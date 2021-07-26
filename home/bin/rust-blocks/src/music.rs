use super::*;
use dbus::{message::MatchRule, strings::{self, Member}};
use dbus_tokio::connection;
use futures_util::StreamExt;
use pris::Player;
use tokio::sync::Mutex;
use std::sync::Arc;

pub struct MusicBlock {
}

impl MusicBlock {
    fn new() -> Self { Self {  } }
}


fn error(msg: &str) -> String {
    format!("Music block error: {}", msg)
}

fn new_rule(member: &str, path: &str) -> MatchRule<'static> {
    let mut property_rule = MatchRule::new();
    property_rule.member = Some(Member::new(member)
				.expect(&error(&format!("No member {}", member))));
    property_rule.path = Some(strings::Path::new(path)
			      .expect(&error(&format!("No path {}", path))));
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

	let (resource, conn) = connection::new_session_local()
	    .expect(&error("Cannot open dbus connection"));

	tokio::spawn(async {
	    let err = resource.await;
	    panic!("{}", error(&format!("Lost connection to dbus: {}", err)))
	});

	// let mut property_rule = MatchRule::new();
	// property_rule.member = Some(Member::new("PropertiesChanged")
	// 		   .expect(&error("No member PropertiesChanged")));
        // property_rule.path = Some(strings::Path::new("/org/mpris/MediaPlayer2")
        //     .expect(&error("Not path /org/mpris/MediaPlayer2")));
	let mut property_rule = new_rule("PropertiesChanged", "/org/mpris/MediaPlayer2");
	let mut disappear_rule = new_rule("NameLost", "/org/freedesktop/DBus");


	let mut player: Option<Player> = None;

	let (incoming_signal, stream) = conn.add_match(property_rule).await
	    .expect(&error("Canno set dbus callback"))
	    .stream();

	let stream = stream.for_each(|(_, (source,)): (_, (String,))| {
	    async {
		// if player.lock().await.is_none() {
		    // if let Ok(new_player) = Player::try_new("spotify", conn.as_refg).await {
		// println!("Property changed by {:?}", source);
			// player.lock().await.insert(new_player);
		    // }
		// }
	    }
	});

	tokio::join!(stream);

	// conn.remove_match(incoming_signal.token()).await.unwrap();
	// loop { sleep(Duration::from_secs(60 * 60)).await; }
	// println!("I'am alive");
	// fifo.write_all(format!("{}\n", "Test Rust").as_bytes()).await.ok();
	// fifo.flush().await.ok();
	// writeln!(fifo, "Test async Rust");
    }
}

pub fn block() -> MusicBlock {
    MusicBlock::new()
}
