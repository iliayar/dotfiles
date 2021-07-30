mod config;

use std::path::Path;
use std::os::unix::fs::FileTypeExt;
use std::sync::Arc;
use tokio::sync::Mutex;
use tokio::time::{sleep, Duration};
use tokio::io::{AsyncWriteExt, AsyncReadExt};
use tokio::fs::{OpenOptions, File};
use tokio::net::UnixListener;
use async_trait::async_trait;

#[derive(Clone, Copy)]
pub struct Color(pub u8, pub u8, pub u8);

impl Color {
    pub fn to_string(&self) -> String {
	format!("#{:X}{:X}{:X}", self.0, self.1, self.2)
    }

    fn color_parse_error_message(s: &str) -> String {
	format!("Invalid color format: {}", s)
    }
    
    pub fn from_string(s: &str) -> Color {
	let error = || Color::color_parse_error_message(s);
	let parse = |s| u8::from_str_radix(s, 16).expect(&error());

	let rest = s.strip_prefix("#").expect(&error());
	let (r, rest) = rest.split_at(2);
	let (g, rest) = rest.split_at(2);
	let (b, _) = rest.split_at(2);
	Color(parse(r),
	      parse(g),
	      parse(b))
    }


    pub fn red() -> Color {
	Color::from_string("#e3276b")
    }
    pub fn green() -> Color {
	Color::from_string("#a6e22e")
    }
    pub fn yellow() -> Color {
	Color::from_string("#f4bf75")
    }
}

impl std::fmt::Display for Color {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
	write!(f, "{}", self.to_string())
    }
}

pub fn xclr(s: &str, c: Color) -> String {
    format!("<fc={}>{}</fc>", c, s)
}

pub fn xfa(s: &str) -> String {
    format!("<fn=1>{}</fn>", s)
}

pub fn xact(s: &str, action: &str) -> String {
    format!("<action={}>{}</action>", action, s)
}

pub async fn get_fifo(name: &str) -> File {
    get_fifo_with_prefix(name, "/tmp").await
}

pub async fn get_fifo_with_prefix(name: &str, prefix: &str) -> File {
    let filename = Path::new(prefix).join(name);
    let error = |msg| format!("{}: {}", msg, filename.to_str().unwrap());
    let create = || unix_named_pipe::create(&filename, Some(0o644))
	.expect(&error("Failed to create FIFO"));

    if filename.exists() {
	if !std::fs::metadata(&filename)
	    .expect(&error("Cannot read FIFO metadata"))
	    .file_type().is_fifo() {
		std::fs::remove_file(&filename)
		    .expect(&error("Remove file on path first"));
		create();
	    }
    } else {
	create();
    }

    OpenOptions::new()
        .write(true)
        .append(true)
        .open(&filename).await
        .expect(&error("Cannot open existing FIFO"))
}

#[async_trait]
pub trait Block {
    fn config(&self) -> BlockConfig;
    fn set_fifo(&mut self, _fifo: File) { }
    async fn update(&self) { }
    async fn command(&self, _cmd: &str) { }
}

pub struct BlockConfig {
    fifo: FIFO,
    interval: UpdateInterval,
}

#[derive(PartialEq)]
pub enum FIFO {
    WithPrefix(String, String),
    WithoutPrefix(String),
    None,
}

pub enum UpdateInterval {
    Once,
    Interval(Duration),
}

struct BlockRunner {
}

impl BlockRunner {
    fn new() -> Self { Self {  } }

    async fn run<B: Block + Send + Sync + 'static>(&mut self, block: B) -> Arc<B>
    {
	let mut block = block;
	let config = block.config();

	if config.fifo != FIFO::None {
	    let fifo = match config.fifo {
		FIFO::WithPrefix(file, prefix) => get_fifo_with_prefix(&file, &prefix).await,
		FIFO::WithoutPrefix(file) => get_fifo(&file).await,
		FIFO::None => unreachable!()
	    };

	    block.set_fifo(fifo);
	}

	let block = Arc::new(block);
	let nblock = block.clone();

	match config.interval {
	    UpdateInterval::Once => tokio::spawn(async move { nblock.update().await }),
	    UpdateInterval::Interval(duration) => tokio::spawn(async move {
		loop {
		    nblock.update().await;
		    sleep(duration).await;
		}
	    }),
	};

	block
    }
}


mod music;
mod dummy;
mod updates;
mod agenda;

fn main() {
    let rt = tokio::runtime::Builder::new_multi_thread()
        .worker_threads(3)
	.enable_time()
	.enable_io()
	.build()
	.unwrap();

    rt.block_on(async {
	let mut runner = BlockRunner::new();

	// let music_block = runner.run(music::block()).await;
	// let _dummy_block = runner.run(dummy::block()).await;
	// let updates_block = runner.run(updates::block()).await;
	let agenda_block = runner.run(agenda::block()).await;

	let socket_path = Path::new(config::SOCKET);
	if socket_path.exists() {
	    tokio::fs::remove_file(socket_path).await.ok();
	}

	let listener = UnixListener::bind(socket_path).unwrap();
	while let Ok(mut stream) = listener.accept().await {
	    let mut data: String = String::new();
	    if let Ok(_) = stream.0.read_to_string(&mut data).await {
		if let  Some((block, cmd)) = data.split_once(|c| c == '\n') {
		    // FIXME: Generalising it would be pure hell
		    let cmd = cmd.to_owned();
		    match block {
			// "music" => {
			//     let nblock = music_block.clone();
			//     tokio::spawn(async move { nblock.command(&cmd).await })
			// },
			// "updates" => {
			//     let nblock = updates_block.clone();
			//     tokio::spawn(async move { nblock.command(&cmd).await })
			// },
			// "dummy" => {
			//     let nblock = dummy_block.clone();
			//     tokio::spawn(async move { nblock.command(&cmd).await })
			// },
			"agenda" => {
			    let nblock = agenda_block.clone();
			    tokio::spawn(async move { nblock.command(&cmd).await })
			},
			_ => continue,
		    };
		}
	    }
	}
    })
}
