use super::*;

pub struct MusicBlock {

}

impl MusicBlock {
    fn new() -> Self { Self {  } }
}

impl Block for MusicBlock {
    fn info(&self) -> BlockInfo {
	BlockInfo {
	    fifo: FIFO::WithoutPrefix(".music_xmobar".to_string()),
	}
    }
}

pub fn block() -> MusicBlock {
    MusicBlock::new()
}
