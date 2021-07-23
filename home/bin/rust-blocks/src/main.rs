use std::fs::{File};
use std::path::{Path};
use std::io::{Write};
use std::os::unix::fs::{FileTypeExt};

use tokio;

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

pub fn xmobar_colorize(s: &str, c: Color) -> String {
    format!("<fc={}>{}</fc>", c, s)
}

pub fn get_fifo(name: &str) -> File {
    get_fifo_with_prefix(name, "/tmp")
}

pub fn get_fifo_with_prefix(name: &str, prefix: &str) -> File {
    let filename = Path::new(prefix).join(name);
    let error = |msg| format!("{}: {}", msg, filename.to_str().unwrap());
    let create = || unix_named_pipe::create(&filename, Some(0o666))
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
    unix_named_pipe::open_write(&filename).expect(&error("Cannot open existing FIFO"))
}

pub trait Block {
    fn info(&self) -> BlockInfo;
}

pub enum FIFO {
    WithPrefix(String, String),
    WithoutPrefix(String)
}

pub struct BlockInfo {
    fifo: FIFO
}

struct BlockRunner {

}

impl BlockRunner {
    fn new() -> Self { Self {  } }

    fn run(block: BlockInfo) {
    }
}


mod music;

#[tokio::main]
async fn main() {
    {
	use music::*;
	let block = block();
    }
}

// fn main() {
//     // tokio::runtime::Runtime::new().unwrap().block_on(async_main());
//     let th1 = std::thread::spawn( || loop {
// 	std::thread::sleep(std::time::Duration::from_secs(1));
// 	println!("Prints every 1 sec");
//     });
//     let th2 = std::thread::spawn( || loop {
// 	std::thread::sleep(std::time::Duration::from_secs(5));
// 	println!("Prints every 5 sec");
//     });
//     let th3 = std::thread::spawn( || loop {
// 	std::thread::sleep(std::time::Duration::from_secs(10));
// 	println!("Prints every 10 sec");
//     });

//     th1.join();
//     th2.join();
//     th3.join();
// }
