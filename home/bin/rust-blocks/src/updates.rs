use super::*;

use notify_rust::Notification;
use tokio::process::Command;

const ALERT_THRESHOLD: usize = 100;
const WARNING_THRESHOLD: usize = 50;
const AUR_ALERT_THRESHOLD: usize = 10;
const AUR_WARNING_THRESHOLD: usize = 5;

pub struct UpdatesBlock {
    fifo: Option<Mutex<File>>,
    status: Mutex<UpdateStatus>,
}

impl UpdatesBlock {
    fn new() -> Self {
	Self {
	    fifo: None,
	    status: Mutex::new(UpdateStatus::None),
	}
    }
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

#[derive(PartialEq)]
enum UpdateStatus {
    Updating,
    None
}

async fn run_update(status: &Mutex<UpdateStatus>, args: &[&str]) -> Option<()> {
    *status.lock().await = UpdateStatus::Updating;

    Command::new("urxvt")
	.arg("-e")
	.args(args)
	.spawn().ok()?
	.wait().await.ok()?;

    *status.lock().await = UpdateStatus::None;
    Some(())
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

    async fn update(&self) {
	if self.status.lock().await.eq(&UpdateStatus::Updating) {
	    return;
	}

	let mut fifo = self.fifo.as_ref().unwrap().lock().await;
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

	fn update_cmd(man: &str) -> String { format!("{} updates {}", config::client(), man) }

	fifo.write_all(format!("{} {}\n",
			       xact(&format!("Pacman: {}", xclr(&pacman_text, pacman_color)), &update_cmd("pacman")),
			       xact(&format!("AUR: {}", xclr(&aur_text, aur_color)), &update_cmd("aur")))
		       .as_bytes()).await.ok();
    }

    async fn command(&self, cmd: &str) {
	run_update(
	    &self.status,
	    if cmd == "pacman" {
		&["sudo", "pacman", "-Syyu"]
	    } else if cmd == "aur" {
		&["paru, -Syyu"]
	    } else {
		return
	    }).await;
	self.update().await;
    }

    fn set_fifo(&mut self, fifo: File) {
	self.fifo.insert(Mutex::new(fifo));
    }
}

pub fn block() -> UpdatesBlock {
    UpdatesBlock::new()
}
