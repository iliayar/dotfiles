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
	    interval: UpdateInterval::Interval(Duration::from_secs(1)),
	}
    }

    async fn update(&mut self) {
	println!("I'am alive");
    }
}

pub fn block() -> MusicBlock {
    MusicBlock::new()
}
