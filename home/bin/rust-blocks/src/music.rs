use super::*;
use dbus::{Message, message::MatchRule, strings::{self, Member}};
use dbus_tokio::connection;
use futures::StreamExt;
use pris::Player;
use tokio::sync::Mutex;
use std::sync::Arc;

const PLAYER: &str = "spotify";

pub struct MusicBlock {
}

impl MusicBlock {
    fn new() -> Self { Self {  } }
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

#[async_trait]
impl Block for MusicBlock
{
    fn config(&self) -> BlockConfig {
	BlockConfig {
	    fifo: FIFO::WithoutPrefix(".music_xmobar".to_string()),
	    // interval: UpdateInterval::Interval(Duration::from_secs(1)),
	    interval: UpdateInterval::Once,
	}
    }

    async fn update(&mut self, fifo: &mut File) {

	let player_name = format!("org.mpris.MediaPlayer2.{}", PLAYER);

	let (resource, conn) = connection::new_session_sync()
	    .expect(&error("Cannot open dbus connection"));

	tokio::spawn(async {
	    let err = resource.await;
	    panic!("{}", error(&format!("Lost connection to dbus: {}", err)))
	});

	let state_rule = new_rule("NameOwnerChanged", None, Some("org.freedesktop.DBus"), true);
	let playback_rule = new_rule("PropertiesChanged", None, Some("org.mpris.MediaPlayer2.spotify"), false);
	// let disappear_rule = new_rule("NameLost", None, None, true);


	let player: Option<Player> = None;

	let (_playback_incoming_signal, playback_stream) = conn.add_match(playback_rule).await
	    .expect(&error("Cannot set dbus playback callback"))
	    .stream();

	let (_incoming_signal, state_stream) = conn.add_match(state_rule).await
	    .expect(&error("Cannot set dbus state callback"))
	    .stream();

	let state_stream = async {
	    state_stream.for_each(|(msg, (name, old_owner, new_owner)): (Message, (String, String, String))| {
		let player_name = &player_name;
		async move {
		    if &name == player_name {
			if old_owner.is_empty() {
			    println!("{} opened!", PLAYER);
			} else {
			    println!("{} closed!", PLAYER);
			}
		    }
		}
	    }).await;
	};

	let playback_stream = async {
	    playback_stream.for_each(|(msg, ()): (Message, ())| {
		async move {
		    println!("Playback: {:?}", msg);
		}
	    }).await;
	};

	futures::join!(state_stream, playback_stream);
    }
}

pub fn block() -> MusicBlock {
    MusicBlock::new()
}
