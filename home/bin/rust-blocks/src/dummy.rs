use super::*;

pub struct DummyBlock {
}

impl DummyBlock {
    fn new() -> Self { Self {  } }
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

    async fn update(&mut self, _fifo: &mut File) {
	println!("Prints every 2 secs");
    }
}

pub fn block() -> DummyBlock {
    DummyBlock::new()
}
