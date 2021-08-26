use super::*;
use tokio::process::Command;

pub struct VpnBlock {
    fifo: Option<Mutex<File>>
}

impl VpnBlock {
    fn new() -> Self {
	Self {
	    fifo: None
	}
    }
}

impl VpnBlock {
    async fn update_impl(&self) {
	let fifo = self.fifo.as_ref().unwrap();

	if let Ok(output) = Command::new("nmcli")
	    .args(&["-f", "GENERAL.STATE", "con", "show", "wireguard"])
	    .output()
	    .await {
		let (cmd, text, color) = if output.stdout.is_empty() {
		    ("up", "down", Color::red())
		} else {
		    ("down", "up", Color::green())
		};
		fifo.lock().await.write_all(format!("{}\n", xact(&format!("{} wireguard: {}", xfa(""), xclr(text, color)),
						 &format!("{} vpn {}", config::client(), cmd))).as_bytes())
		    .await.ok();
	    }
    }
}

#[async_trait]
impl Block for VpnBlock
{
    fn config(&self) -> BlockConfig {
	BlockConfig {
	    fifo: FIFO::WithoutPrefix("vpn.io".to_string()),
	    interval: UpdateInterval::Interval(Duration::from_secs(60 * 10)),
	}
    }

    async fn update(&self) {
	self.update_impl().await;
    }

    fn set_fifo(&mut self, fifo: File) {
	self.fifo.insert(Mutex::new(fifo));
    }

    async fn command(&self, cmd: &str) {
	Command::new("nmcli")
	.args(&["connection", cmd, "wireguard"])
	.spawn()
	.expect("Failed to run command starting vpn")
	.wait()
	.await.ok();
	self.update_impl().await;
    }
}

pub fn block() -> VpnBlock {
    VpnBlock::new()
}
