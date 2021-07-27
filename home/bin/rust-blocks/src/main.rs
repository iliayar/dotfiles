use std::path::Path;
use std::os::unix::fs::FileTypeExt;
use std::future::Future;
use tokio::time::{sleep, Duration};
use tokio::io::AsyncWriteExt;
use tokio::fs::{OpenOptions, File};
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

    // unix_named_pipe::open_write(&filename).expect(&error("Cannot open existing FIFO"))
    // File::open(&filename).await.expect(&error("Cannot open existing FIFO"))
    OpenOptions::new()
        .write(true)
        .append(true)
        .open(&filename).await
        .expect(&error("Cannot open existing FIFO"))
}

#[async_trait]
pub trait Block {
    fn config(&self) -> BlockConfig;
    async fn update(&mut self, fifo: &mut File);
}

pub struct BlockConfig {
    fifo: FIFO,
    interval: UpdateInterval,
}

pub enum FIFO {
    WithPrefix(String, String),
    WithoutPrefix(String),
}

pub enum UpdateInterval {
    Once,
    Interval(Duration),
}

pub enum Update<Fun, Fut>
where
    Fut: Future<Output = ()>,
    Fun: Fn() -> Fut {
    Once(Fun),
    Interval(Fun, Duration)
}

struct BlockRunner {

}

impl BlockRunner {
    fn new() -> Self { Self {  } }

    async fn run<B: Block + Send + Sync + 'static>(&self, block: B)
    {
	let mut block = block;
	let config = block.config();

	let mut fifo = match config.fifo {
	    FIFO::WithPrefix(file, prefix) => get_fifo_with_prefix(&file, &prefix).await,
	    FIFO::WithoutPrefix(file) => get_fifo(&file).await,
	};

	match config.interval {
	    UpdateInterval::Once => tokio::spawn(async move { block.update(&mut fifo).await }),
	    UpdateInterval::Interval(duration) => tokio::spawn(async move {
		loop {
		    block.update(&mut fifo).await;
		    sleep(duration).await;
		}
	    }),
	};
    }
}


mod music;
mod dummy;

fn main() {
    let rt = tokio::runtime::Builder::new_current_thread()
	.enable_time()
        .enable_io()
	.build()
        .unwrap();

    rt.block_on(async {
	let runner = BlockRunner::new();

	runner.run(music::block()).await;
	runner.run(dummy::block()).await;

	loop { sleep(Duration::from_secs(60 * 60)).await; }
    })
}

// fn main() {
//     let rt = tokio::runtime::Builder::new_current_thread().enable_time().build().unwrap();
//     rt.block_on(async {
// 	let fut1 = async {
// 	    loop {
// 		sleep(Duration::from_secs(1)).await;
// 		println!("Prints every 1 sec");
// 	    }
// 	};
// 	let fut2 = async {
// 	    loop {
// 		sleep(Duration::from_secs(5)).await;
// 		println!("Prints every 5 sec");
// 	    }
// 	};
// 	let fut3 = async {
// 	    loop {
// 		sleep(Duration::from_secs(10)).await;
// 		println!("Prints every 10 sec");
// 	    }
// 	};
// 	rt.spawn(fut1);
// 	rt.spawn(fut2);
// 	rt.spawn(fut3);

// 	loop { sleep(Duration::from_secs(60 * 60)).await; }
//     });
// }
