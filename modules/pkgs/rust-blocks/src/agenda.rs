use super::*;

use tokio::process::Command;
use csv::Reader;
use serde::{Deserialize, Deserializer};
use chrono::{DateTime, Local, NaiveDate, NaiveTime, TimeZone};

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

#[derive(Debug,Deserialize,Clone,PartialEq)]
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
    fn notification_string(&self) -> String {
	use EventType::*;
	match self {
	    Scheduled => "Scheduled".to_owned(),
	    Deadline => "Deadline".to_owned(),
	    PastScheduled => "😱 Scheduled".to_owned(),
	    UpcomingDeadline => "😥 Deadline".to_owned(),
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

impl PartialEq for AgendaTime {
    fn eq(&self, other: &Self) -> bool {
        match (self, other) {
            (Self::Exact(l0), Self::Exact(r0)) => l0 == r0,
            (Self::Interval(l0, l1), Self::Interval(r0, r1)) => l0 == r0 && l1 == r1,
            (Self::Day(l0), Self::Day(r0)) => l0 == r0,
            _ => core::mem::discriminant(self) == core::mem::discriminant(other),
        }
    }
}

impl PartialOrd for AgendaTime {
    fn partial_cmp(&self, other: &Self) -> Option<std::cmp::Ordering> {
	match self {
	    &Self::None => Option::None,
	    &Self::Exact(date) |
	    &Self::Interval(date, _)  |
	    &Self::Day(date) => {
		match other {
		    &Self::None => Option::None,
		    &Self::Exact(other_date) |
		    &Self::Interval(other_date, _) |
		    &Self::Day(other_date) => date.partial_cmp(&other_date)
		}
	    }
	}
    }
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
	    Exact(date) | Interval(_, date) | Day(date) => Some(&date),
	    _ => Option::None
	}
    }

    fn notify_time(&self) -> Option<&DateTime<Local>> {
	use AgendaTime::*;
	match self {
	    Interval(date, _) => Some(&date),
	    _ => self.status_time()
	}
    }
}

#[derive(Debug, Clone)]
struct AgendaRecord {
    category: String,
    head: String,
    event_type: EventType,
    todo: AgendaTodo,
    tags: Vec<String>,
    date: AgendaTime,
    // time: String, // Time(combined in date field)
    // unknown7: String,
    // unknown8: String,
    priority: u32,
    // unknown10: String,
}

#[derive(Debug, Deserialize)]
struct AgendaRecordHelper(String, String, EventType, AgendaTodo, String, String, String, String, String, u32, String);

impl<'de> Deserialize<'de> for AgendaRecord {
    fn deserialize<D>(deserializer: D) -> Result<Self, <D as Deserializer<'de>>::Error>
    where D: Deserializer<'de> {
	Deserialize::deserialize(deserializer)
	    .map(|a: AgendaRecordHelper| {
		debug!("AgendaRecordHelper read: {:?}", a);
		let date = if let Some(date) = NaiveDate::parse_from_str(&a.5, "%Y-%m-%d").ok() {
		    let times: Vec<DateTime<Local>> = a.6
			.trim_end_matches(|c| c == '.')
			.split("-")
			.flat_map(|s| NaiveTime::parse_from_str(s, "%H:%M").into_iter())
			.map(|time| Local.from_local_datetime(&date.and_time(time)).unwrap())
			.collect();
		    match times.len() {
			2 => AgendaTime::Interval(times[0], times[1]),
			1 => AgendaTime::Exact(times[0]),
			0 => AgendaTime::Day(Local.from_local_datetime(&date.and_hms(23, 59, 59)).unwrap()),
			_ => AgendaTime::None
		    }
		} else {
		    AgendaTime::None
		};

		let mut tags: Vec<String> = a.4.split(|c| c == ':').map(|s| s.to_owned()).collect();
		if tags.len() == 1 && tags[0].is_empty() {
		    tags.clear();
		}

		AgendaRecord {
		    category: a.0,
		    head: a.1,
		    event_type: a.2,
		    todo: a.3,
		    date,
		    priority: a.9,
		    tags
		}
	    })
    }
}

enum OrgExporter {
    HTML,
    JSON,
    ASCII,
    Pango,
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
	    Pango => "org-pango-export-as-pango",
	};
	format!("(lambda () ({}))", inner)
    }
}

enum LispFunction {
    GetAgenda,
    ExportHeadline(String, String, OrgExporter),
    ExportHeadlineParent(String, String, OrgExporter),
    MarkDone(String, String),
}

impl ToString for LispFunction {
    fn to_string(&self) -> String {
	use LispFunction::*;
        let inner = match self {
	    GetAgenda => "batch-all-agenda".to_owned(),
	    ExportHeadline(file, headline, exporter) => format!("princ-headline-with \"{}\" \"{}\" '{}", file, headline, exporter.to_string()),
	    ExportHeadlineParent(file, headline, exporter) => format!("princ-headline-parent-with \"{}\" \"{}\" '{}", file, headline, exporter.to_string()),
	    MarkDone(file, headline) => format!("mark-done \"{}\" \"{}\"", file, headline),
	};
	format!("({})", inner)
    }

}

#[derive(Debug)]
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
    let proc = Command::new("emacs")
	.args(&["-batch", "-l", "~/.emacs.d/lisp/load-org", "-Q", "--eval"])
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
    let exporter = if event.tags.contains(&"notify_parent".to_owned()) {
	LispFunction::ExportHeadlineParent
    } else {
	LispFunction::ExportHeadline
    };
    match elisp(exporter(event.category.clone(), event.head.clone(), OrgExporter::Pango)).await {
	Ok(text) => {
	    let mut body = format!("<b>{}</b> {}\n{}", event.date.status_string().unwrap(), event.head, &text);
	    let mut summary = event.event_type.notification_string();
	    if let Some(expired) = expired {
		let hours = expired.num_hours();
		let minutes = expired.num_minutes() - hours * 60;
		body.push_str(&dclr(format!("\nEXPIRED FOR {}h {}m", hours, minutes)).fg(Color::red()).get());
		if event.event_type == EventType::Deadline {
		    summary = format!("⚰ {}", summary);
		}
	    }
	    let notification_handler = dunstify::Notification::new(summary)
		.body(body)
		.appname("rust-blocks")
		.timeout(Duration::from_secs(60))
		.action("done", &format!("Mark DONE {}", event.head))
		.show().await
		.unwrap();
	    if let Some(action) = notification_handler.action().await {
		match action.as_ref() {
		    "done" => {
			elisp(LispFunction::MarkDone(event.category, event.head)).await.ok();
		    },
		    _ => (),
		}
	    }
	},
	Err(err) => error!("Failed to call elisp export function: {}", err.to_string())
    }
}

async fn check_notify(event: AgendaRecord) {
    let now = Local::now();
    let date = event.date.notify_time().unwrap().clone();
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
    let now = Local::now();
    if date < now {
	return None;
    }
    match event.event_type {
	EventType::Deadline | EventType::Scheduled | EventType::Block => (),
	_ => return None
    };
    let time = if let AgendaTime::Day(_) = event.date {
	"Whole day".to_owned()
    } else {
	let mut time = event.date.status_string().unwrap();
	if date.date() != now.date() {
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
	    interval: UpdateInterval::Interval(Duration::from_secs(60 * 5)),
	}
    }

    async fn update(&self) {
	let fifo = self.fifo.as_ref().unwrap();

	match elisp(LispFunction::GetAgenda).await {
	    Ok(data) => {
		debug!("Data from elisp: {:?}", data);
		let mut rdr = Reader::from_reader(data.as_bytes());
		let mut was_status = false;
		let mut records: Vec<AgendaRecord> = rdr.deserialize::<AgendaRecord>().filter_map(|e| e.ok()).collect();
		records.sort_by(|a, b| a.date.partial_cmp(&b.date).unwrap_or(std::cmp::Ordering::Less));
		for result in records {
		    debug!("Get record: {:?}", result);
		    tokio::spawn(check_notify(result.clone()));
		    if !was_status {
			if let Some(status) = check_status(&result).await {
			    fifo.lock().await.write_all(format!("{}\n", status).as_bytes()).await.ok();
			    was_status = true;
			}

		    }
		}

		if !was_status {
		    fifo.lock().await.write_all("Chill\n".as_bytes()).await.ok();
		}
	    },
	    Err(err) => error!("Failed getting agenda: {}", err.to_string())
	}
    }

    fn set_fifo(&mut self, fifo: File) {
	self.fifo.insert(Mutex::new(fifo));
    }

    async fn command(&self, _cmd: &str) { }
}

pub fn block() -> AgendaBlock {
    AgendaBlock::new()
}
