use super::*;

use tokio::process::Command;
use csv::Reader;
use serde::{Deserialize, Deserializer};
use chrono::{DateTime, Utc};

pub struct AgendaBlock {
    // fifo: Option<Mutex<File>>
}

impl AgendaBlock {
    fn new() -> Self {
	Self {
	    // fifo: None
	}
    }
}

#[derive(Debug,Deserialize)]
enum State {
    TODO,
    FIXME,
    DONE,
}

#[derive(Debug)]
struct AgendaRecord {
    file: String,
    headline: String,
    scheduled: String,
    state: State,
    // unknown1: String,
    date: DateTime<Utc>,
    // unknown2: String,
    // unknown3: String,
    // unknown4: String,
    // unknown5: String,
    // unknown6: String,
}

#[derive(Debug, Deserialize)]
struct AgendaRecordHelper(String, String, String, State, String,
			  #[serde(with = "org_date_format")]
			  DateTime<Utc>,
			  String, String, String, String, String);

mod org_date_format {
    use chrono::{DateTime, Utc, TimeZone};
    use serde::{self, Deserialize, Deserializer};

    const FORMAT: &'static str = "%Y-%m-%d";

    pub fn deserialize<'de, D>(
        deserializer: D,
    ) -> Result<DateTime<Utc>, D::Error>
    where
        D: Deserializer<'de>,
    {
        let s = String::deserialize(deserializer)?;
	// FIXME: Only date fails
        Utc.datetime_from_str(&s, FORMAT).map_err(serde::de::Error::custom)
    }
}

impl<'de> Deserialize<'de> for AgendaRecord {
    fn deserialize<D>(deserializer: D) -> Result<Self, <D as Deserializer<'de>>::Error>
    where D: Deserializer<'de> {
	Deserialize::deserialize(deserializer)
	    .map(|a: AgendaRecordHelper|
		 AgendaRecord {
		     file: a.0,
		     headline: a.1,
		     scheduled: a.2,
		     state: a.3,
		     date: a.5,
		 })
    }
}

async fn elisp(fun: &str) -> Option<Vec<u8>> {
    Some(Command::new("/usr/bin/emacs")
	 .args(&["-batch", "-l", "~/bin/lisp/load-org", "-Q", "--eval"])
	 .arg(fun)
	 .output().await
	 .ok()?
	 .stdout)
}

#[async_trait]
impl Block for AgendaBlock
{
    fn config(&self) -> BlockConfig {
	BlockConfig {
	    // fifo: FIFO::WithoutPrefix("agenda.io".to_string()),
	    fifo: FIFO::None,
	    // interval: UpdateInterval::Interval(Duration::from_secs(60 * 5)),
	    interval: UpdateInterval::Once
	}
    }

    async fn update(&self) {
	// let fifo = self.fifo.as_ref().unwrap();
	// fifo.lock().await.write_all(...);
	// println!("Prints every 2 secs");
	if let Some(data) = elisp("(batch-all-agenda)").await {
	    let mut rdr = Reader::from_reader(data.as_slice());
	    for result in rdr.deserialize::<AgendaRecord>() {
		println!("{:?}", result);
	    }
	}
    }

    // fn set_fifo(&mut self, fifo: File) {
    // 	self.fifo.insert(Mutex::new(fifo));
    // }

    async fn command(&self, _cmd: &str) { }
}

pub fn block() -> AgendaBlock {
    AgendaBlock::new()
}
