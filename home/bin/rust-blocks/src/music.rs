use std::sync::Arc;

use super::*;
use dbus::{
    Message,
    arg::{
	Dict,
	Iter,
	Variant
    },
    message::MatchRule,
    nonblock::{
	Proxy,
	SyncConnection
    }, strings::{
	self,
	Member
    }
};
use dbus_tokio::connection;
use futures::StreamExt;
use tokio::sync::Mutex;

const PLAYER: &str = "spotify";

fn get_name(player: &str) -> String {
    format!("org.mpris.MediaPlayer2.{}", player)
}

#[derive(Debug)]
enum PlaybackStatus {
    Playing,
    Paused,
}

impl PlaybackStatus {
    fn controls(&self) -> String {
	let (color, icon) = match self {
	    &PlaybackStatus::Playing => (Color::green(), ""),
	    &PlaybackStatus::Paused => (Color::yellow(), ""),
	};
	let client = |cmd| format!("{} music {}", config::client(), cmd);
	xclr(&format!("{} {} {}",
		      xact(&xfa(""), &client("prev")),
		      xact(&xfa(icon), &client("play-pause")),
		      xact(&xfa(""), &client("next")),
	), color)
    }
}
fn error(msg: &str) -> String {
    format!("Music block error: {}", msg)
}

fn new_rule(member: &str, path: Option<&str>, sender: Option<&str>, eavesdrop: bool) -> MatchRule<'static> {
    let mut property_rule = MatchRule::new();
    property_rule.eavesdrop = eavesdrop;
    property_rule.msg_type = Some(dbus::MessageType::Signal);
    property_rule.member = Some(Member::new(member)
				.expect(&error(&format!("No member {}", member))));
    if let Some(path) = path {
	property_rule.path = Some(strings::Path::new(path)
				  .expect(&error(&format!("No path {}", path))));

    }
    if let Some(name) = sender {
	property_rule.sender = Some(strings::BusName::new(name)
				    .expect(&error(&format!("No name {}", name))));
    }
    return property_rule;
}

async fn process_playback(msg: Message, fifo: Arc<Mutex<File>>) {
    let mut playback: Option<PlaybackStatus> = None;
    let mut artist: Option<String> = None;
    let mut track: Option<String> = None;
    
    { //        vvvv is not Send
	let mut iter = msg.iter_init();
	iter.read::<String>().ok();
	if let Ok(dict) = iter.read::<Dict<String, Variant<Iter>, _>>() {
	    for (name, Variant(mut iter)) in dict {
		match name.as_str() {
		    "Metadata" => {
			if let Ok(dict) = iter.read::<Dict<String, Variant<Iter>, _>>() {
			    for (name, Variant(mut prop)) in dict {
				match name.as_str() {
				    "xesam:artist" => {
					artist = prop.read::<Vec<String>>().ok().map(|v| v.join(", "));
				    },
				    "xesam:title" => {
					track = prop.read::<String>().ok();
				    },
				    _ => ()
				}
			    }
			}

		    },
		    "PlaybackStatus" => {
			if let Ok(status) = iter.read::<String>() {
			    if status == "Playing" {
				playback.insert(PlaybackStatus::Playing);
			    } else if status == "Paused" {
				playback.insert(PlaybackStatus::Paused);
			    }
			}

		    },
		    _ => ()
		}
	    }
	}
    }
    
    if let Some(((artist, track), playback)) = artist.zip(track).zip(playback) {
	if !artist.is_empty() && !track.is_empty() {
	    fifo.lock().await.write_all(
		format!("{} {}\n",
			xact(&format!("{} - {}", artist, track), "~/.xmonad/xmonadctl 13"),
			playback.controls())
		    .as_bytes()).await
		.expect(&error("Failed to write to fifo in state_stream"));
	}
    }

}

async fn process_state(msg: Message, player_name: &str, fifo: Arc<Mutex<File>>) {
    let mut name = None;
    let mut old_owner = None;
    let mut new_onwer = None;
    {
	let mut iter = msg.iter_init();
	name = iter.read::<String>().ok();
	old_owner = iter.read::<String>().ok();
	new_onwer = iter.read::<String>().ok();
    }
    if let Some(((name, old_owner), _new_owner)) = name.zip(old_owner).zip(new_onwer) {
	if &name == player_name {
	    fifo.lock().await.write_all(
		format!("{} {}\n",
			PLAYER,
			if old_owner.is_empty() {
			    xclr("is running", Color::yellow())
			} else {
			    xclr("is not running", Color::red())
			})
		    .as_bytes()).await
		.expect(&error("Failed to write to fifo in state_stream"));
	}
    }
}


pub struct MusicBlock {
    fifo: Option<Arc<Mutex<File>>>,
    conn: Arc<SyncConnection>,
    proxy: Proxy<'static, Arc<SyncConnection>>,
}

impl MusicBlock {
    fn new() -> Self {
	let (resource, conn) = connection::new_session_sync()
	    .expect(&error("Cannot open dbus connection"));
	
	tokio::spawn(async move {
	    let err = resource.await;
	    panic!("{}", error(&format!("Lost connection to dbus: {}", err)))
	});

	let proxy = Proxy::new(
	    get_name(PLAYER),
	    "/org/mpris/MediaPlayer2",
	    Duration::from_secs(5),
	    conn.clone()
	);

	Self {
	    fifo: None,
	    conn,
	    proxy
	}
    }

    async fn method_call(&self, m: &str) -> Option<()>{
	self.proxy.method_call(&get_name("Player"), m, ()).await.ok()
    }
}

#[async_trait]
impl Block for MusicBlock
{
    fn config(&self) -> BlockConfig {
	BlockConfig {
	    fifo: FIFO::WithoutPrefix(".music_xmobar".to_string()),
	    interval: UpdateInterval::Once,
	}
    }

    async fn update(&self) {

	let fifo = self.fifo.as_ref().unwrap().clone();

	fifo.lock().await.write_all(
	    format!("{} {}\n",
		    PLAYER,
		    xclr("is not running", Color::red()))
		.as_bytes()).await
	    .expect(&error("Failed to write to fifo"));

	let player_name = get_name(PLAYER);

	let state_rule = new_rule("NameOwnerChanged", None, Some("org.freedesktop.DBus"), true);
	let playback_rule = new_rule("PropertiesChanged", None, Some(&player_name), false);

	let (_playback_incoming_signal, playback_stream) = self.conn.add_match(playback_rule).await
	    .expect(&error("Cannot set dbus playback callback"))
	    .stream();

	let (_incoming_signal, state_stream) = self.conn.add_match(state_rule).await
	    .expect(&error("Cannot set dbus state callback"))
	    .stream();

	let state_stream = async {
	    state_stream.for_each(|(msg, ()): (Message, ())| {
		process_state(msg, &player_name, fifo.clone())
	    }).await;
	};

	let playback_stream = async {
	    playback_stream.for_each(|(msg, ()): (Message, ())| {
		process_playback(msg, fifo.clone())
	    }).await;
	};

	tokio::join!(state_stream, playback_stream);
    }

    fn set_fifo(&mut self, fifo: File) {
	self.fifo.insert(Arc::new(Mutex::new(fifo)));
    }

    async fn command(&self, cmd: &str) {
	match cmd {
	    "play" => self.method_call("Play").await,
	    "next" => self.method_call("Next").await,
	    "prev" => self.method_call("Previous").await,
	    "pause" => self.method_call("Pause").await,
	    "play-pause" => self.method_call("PlayPause").await,
	    _ => return
	};
    }
}

pub fn block() -> MusicBlock {
    MusicBlock::new()
}
