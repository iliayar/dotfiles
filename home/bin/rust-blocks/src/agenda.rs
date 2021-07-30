use super::*;

use tokio::process::Command;
use csv::Reader;
use serde::{Deserialize, Deserializer};
use chrono::{DateTime, Utc, NaiveDate, NaiveTime, TimeZone};

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
enum AgendaTodo {
    TODO,
    FIXME,
    DONE,
    #[serde(rename = "")]
    NONE,
}

#[derive(Debug,Deserialize)]
#[serde(rename_all = "lowercase")]
enum EventType {
    Scheduled,
    Deadline,
    Block,
    #[serde(rename="past-scheduled")]
    PastScheduled,
    #[serde(rename="upcoming-deadline")]
    UpcomingDeadline,
}

#[derive(Debug)]
enum AgendaTime {
    Exact(DateTime<Utc>),
    Interval(DateTime<Utc>, DateTime<Utc>),
    Day(DateTime<Utc>),
    None,
}

#[derive(Debug)]
struct AgendaRecord {
    category: String,
    head: String,
    event_type: EventType,
    todo: AgendaTodo,
    // unknown1: String,
    date: AgendaTime,
    // unknown2: String, // Time(combined in date field)
    // unknown3: String,
    // unknown4: String,
    priority: u32,
    // unknown6: String,
}

#[derive(Debug, Deserialize)]
struct AgendaRecordHelper(String, String, EventType, AgendaTodo, String, String, String, String, String, u32, String);

impl<'de> Deserialize<'de> for AgendaRecord {
    fn deserialize<D>(deserializer: D) -> Result<Self, <D as Deserializer<'de>>::Error>
    where D: Deserializer<'de> {
	Deserialize::deserialize(deserializer)
	    .map(|a: AgendaRecordHelper| {
		let date = if let Some(date) = NaiveDate::parse_from_str(&a.5, "%Y-%m-%d").ok() {
		    let times: Vec<DateTime<Utc>> = a.6
			.trim_end_matches(|c| c == '.')
			.split("-")
			.flat_map(|s| NaiveTime::parse_from_str(s, "%H:%M").into_iter())
			.map(|time| Utc.from_utc_datetime(&date.and_time(time)))
			.collect();
		    match times.len() {
			2 => AgendaTime::Interval(times[0], times[1]),
			1 => AgendaTime::Exact(times[0]),
			0 => AgendaTime::Day(Utc.from_utc_date(&date).and_hms(23, 59, 59)),
			_ => AgendaTime::None
		    }
		} else {
		    AgendaTime::None
		};

		AgendaRecord {
		    category: a.0,
		    head: a.1,
		    event_type: a.2,
		    todo: a.3,
		    date,
		    priority: a.9
		}
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
	    // for result in rdr.records() {
	    // 	println!("{:?}", result);
	    // }
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
