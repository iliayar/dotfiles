use super::*;

pub struct DummyBlock {
    // fifo: Option<File>
}

impl DummyBlock {
    fn new() -> Self {
	Self {
	    // fifo: None
	}
    }
}

#[async_trait]
impl Block for DummyBlock
{
    fn config(&self) -> BlockConfig {
	BlockConfig {
	    // fifo: FIFO::WithoutPrefix(".dummy".to_string()),
	    fifo: FIFO::None,
	    // interval: UpdateInterval::Interval(Duration::from_secs(1)),
	    interval: UpdateInterval::Interval(Duration::from_secs(1)),
	}
    }

    async fn update(&mut self) {
	// let fifo = self.fifo.as_mut().unwrap();
	println!("Prints every 2 secs");
    }

    // fn set_fifo(&mut self, fifo: File) {
    // 	self.fifo.insert(fifo);
    // }

    async fn command(&mut self, _cmd: &str) { }
}

pub fn block() -> DummyBlock {
    DummyBlock::new()
}
