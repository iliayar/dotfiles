use super::*;
use tokio::process::Command;

pub struct LockBlock {
    fifo: Option<Mutex<File>>
}

const LOCK_SERVICE: &str = "xautolock-session";

impl LockBlock {
    fn new() -> Self {
	Self {
	    fifo: None
	}
    }
}

impl LockBlock {
    async fn udate_impl(&self) {
	let fifo = self.fifo.as_ref().unwrap();
	if let Ok(status) = Command::new("systemctl")
	    .args(&["--user", "--no-pager", "status", LOCK_SERVICE])
	    .status()
	    .await {
		fifo.lock().await.write_all(format!("{}\n",
		    if status.success() {
			xact(&xclr(&xfa(""), Color::green()), &lock_command("disable"))
		    } else {
			xact(&xclr(&xfa(""), Color::red()), &lock_command("enable"))
		    }).as_bytes()).await;
	    }
    }
}

fn lock_command(status: &str) -> String { format!("{} lock {}", config::client(), status) }

#[async_trait]
impl Block for LockBlock
{
    fn config(&self) -> BlockConfig {
	BlockConfig {
	    fifo: FIFO::WithoutPrefix("auto-lock.io".to_string()),
	    interval: UpdateInterval::Interval(Duration::from_secs(60 * 10)),
	}
    }

    async fn update(&self) {
	self.udate_impl().await;
    }

    async fn command(&self, cmd: &str) {
	match cmd {
	    "disable" => {
		Command::new("systemctl")
		    .args(&["--user", "--no-pager", "stop", LOCK_SERVICE])
		    .spawn()
		    .expect("Failed run command to stop lock service")
		    .wait()
		    .await.ok();
		
	    },
	    "enable" => {
		Command::new("systemctl")
		    .args(&["--user", "--no-pager", "start", LOCK_SERVICE])
		    .spawn()
		    .expect("Failed run command to start lock service")
		    .wait()
		    .await.ok();
	    },
	    _ => ()
	};
	self.udate_impl().await;
    }

    fn set_fifo(&mut self, fifo: File) {
	self.fifo = Some(Mutex::new(fifo));
    }
}

pub fn block() -> LockBlock {
    LockBlock::new()
}
