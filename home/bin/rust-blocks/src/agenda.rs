use super::*;

use tokio::process::Command;
use csv::Reader;
use serde::{Deserialize, Deserializer};
use chrono::{DateTime, Local, NaiveDate, NaiveTime, TimeZone};
use notify_rust::Notification;

pub struct AgendaBlock {
    fifo: Option<Mutex<File>>
}

impl AgendaBlock {
    fn new() -> Self {
	Self {
	    fifo: None
	}
    }
}

#[derive(Debug,Deserialize,Clone)]
enum AgendaTodo {
    TODO,
    FIXME,
    DONE,
    #[serde(rename = "")]
    NONE,
}

#[derive(Debug,Deserialize,Clone)]
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

impl EventType {
    fn notification_type(&self) -> String {
	use EventType::*;
	match self {
	    Scheduled => "Scheduled".to_owned(),
	    Deadline => "Deadline".to_owned(),
	    _ => "NOT IMPLEMENTED".to_owned(),
	}
    }
}

#[derive(Debug,Clone)]
enum AgendaTime {
    Exact(DateTime<Local>),
    Interval(DateTime<Local>, DateTime<Local>),
    Day(DateTime<Local>),
    None,
}

impl AgendaTime {
    fn status_string(&self) -> Option<String> {
	use AgendaTime::*;
	let format = "%H:%M";
	match self {
	    Exact(date) => Some(date.format(format).to_string()),
	    Interval(begin, end) => Some(format!("{} - {}", begin.format(format), end.format(format))),
	    Day(date) => Some(date.format("%m.%d").to_string()),
	    _ => Option::None
	}
    }

    fn status_time(&self) -> Option<&DateTime<Local>> {
	use AgendaTime::*;
	match self {
	    Exact(date) | Interval(date, _) | Day(date) => Some(&date),
	    _ => Option::None
	}
    }
}

#[derive(Debug, Clone)]
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
		    // println!("Time: {}", a.6);
		    let times: Vec<DateTime<Local>> = a.6
			.trim_end_matches(|c| c == '.')
			.split("-")
			.flat_map(|s| NaiveTime::parse_from_str(s, "%H:%M").into_iter())
			.map(|time| Local.from_utc_datetime(&date.and_time(time)))
			.collect();
		    match times.len() {
			2 => AgendaTime::Interval(times[0], times[1]),
			1 => AgendaTime::Exact(times[0]),
			0 => AgendaTime::Day(Local.from_utc_date(&date).and_hms(23, 59, 59)),
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

enum OrgExporter {
    HTML,
    JSON,
    ASCII,
    Org,
    Markdown,
}

impl ToString for OrgExporter {
    fn to_string(&self) -> String {
        use OrgExporter::*;
	let inner = match self {
	    HTML => "org-html-export-as-html nil t nil t",
	    JSON => "ox-json-export-to-buffer nil t nil t",
	    ASCII => "org-ascii-export-as-ascii nil t nil t",
	    Org => "org-org-export-as-org nil t nil t",
	    Markdown => "org-md-export-as-markdown nil t nil",
	};
	format!("(lambda () ({}))", inner)
    }
}

enum LispFunction {
    GetAgenda,
    ExportHeadline(String, String, OrgExporter),
    MarkDone(String, String),
}

impl ToString for LispFunction {
    fn to_string(&self) -> String {
	use LispFunction::*;
        let inner = match self {
	    GetAgenda => "batch-all-agenda".to_owned(),
	    ExportHeadline(file, headline, exporter) => format!("princ-headline-with \"{}\" \"{}\" '{}", file, headline, exporter.to_string()),
	    MarkDone(file, headline) => format!("mark-done \"{}\" \"{}\"", file, headline),
	};
	format!("({})", inner)
    }

}

struct ElispError {
    msg: String
}

impl ElispError {
    fn new(msg: String) -> Self { Self { msg } }
}

impl From<std::io::Error> for ElispError {
    fn from(err: std::io::Error) -> Self {
        ElispError::new(err.to_string())
    }
}

impl From<std::string::FromUtf8Error> for ElispError {
    fn from(err: std::string::FromUtf8Error) -> Self {
        ElispError::new(err.to_string())
    }
}

impl ToString for ElispError {
    fn to_string(&self) -> String {
	self.msg.clone()
    }
}

async fn elisp(fun: LispFunction) -> Result<String, ElispError> {
    let proc = Command::new("/usr/bin/emacs")
	.args(&["-batch", "-l", "~/bin/lisp/load-org", "-Q", "--eval"])
	.arg(fun.to_string())
	.output().await
	.map_err(ElispError::from)?;
    if proc.status.success() {
	Ok(String::from_utf8(proc.stdout).map_err(ElispError::from)?)
    } else {
	Err(ElispError::new(String::from_utf8(proc.stderr).map_err(ElispError::from)?))
    }
}

async fn notify(event: AgendaRecord, expired: Option<chrono::Duration>) {
    // println!("Notifing {:?}", event);
    if let Ok(text) = elisp(LispFunction::ExportHeadline(event.category.clone(), event.head.clone(), OrgExporter::HTML)).await {
	let mut body = format!("<b>{}</b> {}\n{}", event.date.status_string().unwrap(), event.head, &text);
	if let Some(expired) = expired {
	    let hours = expired.num_hours();
	    let minutes = expired.num_minutes() - hours * 60;
	    body.push_str(&format!("EXPIRED FOR {}h {}m", hours, minutes));
	}
	// println!("Showing {:?}", event);
	// FIXME: Rework with async process of ~dunstify~
	let notification_handler = Notification::new()
	    .summary(&event.event_type.notification_type())
	    .body(&body)
	    .appname("rust-blocks")
	    .timeout(60 * 1000)
	    .action("done", &format!("Mark DONE {}", event.head))
	    .show().unwrap();
	let res = tokio::task::spawn_blocking(|| {
	    let mut res: Option<String> = None;
	    notification_handler.wait_for_action(|action| {
		match action {
		    "done" => { res.insert(action.to_owned()); },
		    _ => ()
		}
	    });
	    res
	}).await.ok();
	if let Some(Some(res)) = res {
	    match res.as_str() {
		"done" => {
		    elisp(LispFunction::MarkDone(event.category, event.head)).await.ok();
		},
		_ => return,
	    }
	}
    }
}

async fn check_notify(event: AgendaRecord) {
    let now = Local::now();
    let date = event.date.status_time().unwrap().clone();
    // println!("Date: {}, Now: {}", date, now);
    if date < now {
	notify(event, Some(now.signed_duration_since(date))).await;
    } else {
	let diff = date.signed_duration_since(now).num_minutes();
	if 0 <= diff && diff <= 10 {
	    notify(event.clone(), None).await;
	}
    }

}

async fn check_status(event: &AgendaRecord) -> Option<String> {
    let date = event.date.status_time().unwrap().clone();
    if date < Local::now() {
	return None;
    }
    match event.event_type {
	EventType::Deadline | EventType::Scheduled => (),
	_ => return None
    };
    let time = if let AgendaTime::Day(_) = event.date {
	"Whole day".to_owned()
    } else {
	let mut time = event.date.status_string().unwrap();
	let date = event.date.status_time().unwrap().clone();
	if date.date() != Local::now().date() {
	    time.push_str(&date.format(" %m.%d").to_string())
	}
	time
    };
    Some(format!("{} {}", time, event.head))
}

#[async_trait]
impl Block for AgendaBlock
{
    fn config(&self) -> BlockConfig {
	BlockConfig {
	    fifo: FIFO::WithoutPrefix("agenda.io".to_string()),
	    // fifo: FIFO::None,
	    // interval: UpdateInterval::Interval(Duration::from_secs(60 * 5)),
	    interval: UpdateInterval::Once
	}
    }

    async fn update(&self) {
	let fifo = self.fifo.as_ref().unwrap();
	// fifo.lock().await.write_all(...);

	match elisp(LispFunction::GetAgenda).await {
	    Ok(data) => {
		let mut rdr = Reader::from_reader(data.as_bytes());
		let mut was_status = false;
		for result in rdr.deserialize::<AgendaRecord>() {
		    // println!("{:?}", result);
		    if let Ok(result) = result {
			tokio::spawn(check_notify(result.clone()));
			if !was_status {
			    if let Some(status) = check_status(&result).await {
				fifo.lock().await.write_all(format!("{}\n", status).as_bytes()).await.ok();
				was_status = true;
			    }
			}
		    }

		}
	    },
	    Err(err) => {
		println!("Error!\n{}", err.to_string())
	    }
	}

	// match elisp(LispFunction::MarkDone("Notes".to_owned(), "Rust Agenda 4".to_owned())).await {
	//     Ok(data) => {
	// 	println!("{}", data);
	//     },
	//     Err(err) => {
	// 	println!("Error!\n{}", err.to_string())
	//     }
	// }
    }

    fn set_fifo(&mut self, fifo: File) {
	self.fifo.insert(Mutex::new(fifo));
    }

    async fn command(&self, _cmd: &str) { }
}

pub fn block() -> AgendaBlock {
    AgendaBlock::new()
}
