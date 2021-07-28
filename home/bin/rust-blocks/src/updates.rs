use super::*;

use notify_rust::Notification;
use tokio::process::Command;

const ALERT_THRESHOLD: usize = 100;
const WARNING_THRESHOLD: usize = 50;
const AUR_ALERT_THRESHOLD: usize = 10;
const AUR_WARNING_THRESHOLD: usize = 5;

pub struct UpdatesBlock {
    fifo: Option<File>
}

impl UpdatesBlock {
    fn new() -> Self { Self { fifo: None } }
}

async fn update_info(cmd: &mut Command, text: &mut String, color: &mut Color, alert: usize, warning: usize) {
    let proc = cmd.output().await;
    let mut fail = || {
	*color = Color::red();
	*text = "?".to_owned();
    };
    
    if let Ok(proc) = proc {
	if proc.status.success() {
	    let cnt = proc.stdout
		.split(|c| c == &b'\n')
		.count();
	    *color = if cnt >= alert {
		Color::red()
	    } else if cnt >= warning {
		Color::yellow()
	    } else {
		Color::green()
	    };
	    *text = cnt.to_string();
	} else {
	    fail();
	}
    } else {
	fail();
    }
}


#[async_trait]
impl Block for UpdatesBlock
{
    fn config(&self) -> BlockConfig {
	BlockConfig {
	    fifo: FIFO::WithoutPrefix(".updates_data".to_string()),
	    interval: UpdateInterval::Interval(Duration::from_secs(60 * 30)),
	}
    }

    async fn update(&mut self) {
	let fifo = self.fifo.as_mut().unwrap();
	Notification::new().summary("Updates")
	    .body("Fetching pacman and AUR updates")
	    .appname("rust-blocks")
	    .icon("/usr/share/icons/Adwaita/96x96/emblems/emblem-synchronizing-symbolic.symbolic.png")
            .show()
            .unwrap();

	let mut pacman_text: String = "?".to_owned();
	let mut aur_text: String = "?".to_owned();
	let mut pacman_color: Color = Color::green();
	let mut aur_color: Color = Color::green();

	update_info(
	    &mut Command::new("checkupdates"),
	    &mut pacman_text,
	    &mut pacman_color,
	    ALERT_THRESHOLD,
	    WARNING_THRESHOLD).await;

	update_info(
	    &mut Command::new("paru").arg("-Qum"),
	    &mut aur_text,
	    &mut aur_color,
	    AUR_ALERT_THRESHOLD,
	    AUR_WARNING_THRESHOLD).await;

	fifo.write_all(format!("{} {}\n",
			       xclr(&format!("Pacman: {}", pacman_text), pacman_color),
			       xclr(&format!("AUR: {}", aur_text), aur_color))
		       .as_bytes()).await.ok();
    }

    async fn command(&mut self, cmd: &str) {
	println!("Running updates: {}", cmd);
    }

    fn set_fifo(&mut self, fifo: File) {
	self.fifo.insert(fifo);
    }
}

pub fn block() -> UpdatesBlock {
    UpdatesBlock::new()
}
