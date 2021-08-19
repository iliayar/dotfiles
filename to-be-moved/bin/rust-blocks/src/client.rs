mod config;

use tokio::net::UnixStream;
use tokio::io::AsyncWriteExt;
use std::env;

#[tokio::main]
async fn main() {

    let mut args = env::args();
    args.next();

    let block = args.next().expect("Provide block name");
    let command = args.collect::<Vec<String>>().join(" ");

    if let Ok(mut socket) = UnixStream::connect(config::SOCKET).await {

	socket.write_all(format!("{}\n", block).as_bytes()).await.ok();
	socket.write_all(command.as_bytes()).await.ok();

    }
}
