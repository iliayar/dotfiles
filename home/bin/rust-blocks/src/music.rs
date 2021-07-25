use super::*;

pub struct MusicBlock {
}

impl MusicBlock {
    fn new() -> Self { Self {  } }
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

    async fn update(&mut self, fifo: &mut UnixStream) {
	println!("I'am alive");
	fifo.write_all(format!("{}\n", "Test Rust").as_bytes()).await.ok();
	fifo.flush().await.ok();
	// writeln!(fifo, "Test async Rust");
    }
}

pub fn block() -> MusicBlock {
    MusicBlock::new()
}
