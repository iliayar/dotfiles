use std::path::Path;

pub const SOCKET: &str = "/tmp/rust-blocks.sock";

pub fn client() -> String {
    Path::new(&std::env::current_exe().unwrap()).parent().unwrap().join("rust-blocks-client").to_str().unwrap().to_owned()
}
