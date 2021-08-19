use super::Color;

use tokio::process::{Command, Child, ChildStdout};
use std::{process::Stdio, time::Duration};
use tokio::io::{BufReader, AsyncBufReadExt};


type Action = (String, String);

#[derive(Clone)]
pub struct Notification {
    summary: String,
    body: Option<String>,
    app: Option<String>,
    urgency: Option<u32>,
    // hints: Vec<Hint>, TODO
    actions: Vec<Action>,
    timeout: Option<Duration>,
    icon: Option<String>,
    raw_icon: Option<String>,
    replace_id: Option<u32>,
}

impl Notification {
    pub fn new<T: AsRef<str>>(summary: T) -> Notification {
	Notification {
	    summary: summary.as_ref().to_owned(),
	    body: None,
	    app: None,
	    urgency: None,
	    actions: vec![],
	    timeout: None,
	    icon: None,
	    raw_icon: None,
	    replace_id: None,
	}
    }

    async fn close_impl(id: u32) {
	if let Ok(mut child) = Command::new("dunstify")
	    .arg("-C")
	    .arg(id.to_string())
	    .spawn() {
		child.wait().await.ok();
	    }
    }

    pub async fn close(&self) {
	if let Some(id) = self.replace_id {
	    Notification::close_impl(id).await;
	}
    }

    pub fn summary<T: AsRef<str>>(&mut self, body: T) -> &mut Self {
        self.summary = body.as_ref().to_owned();
	self
    }

    pub fn body<T: AsRef<str>>(&mut self, body: T) -> &mut Self {
        self.body.insert(body.as_ref().to_owned());
	self
    }

    pub fn appname<T: AsRef<str>>(&mut self, app: T) -> &mut Self {
        self.app.insert(app.as_ref().to_owned());
	self
    }

    pub fn urgency(&mut self, urgency: u32) -> &mut Self {
        self.urgency.insert(urgency);
	self
    }

    pub fn timeout(&mut self, timeout: Duration) -> &mut Self {
        self.timeout.insert(timeout);
	self
    }

    pub fn icon<T: AsRef<str>>(&mut self, icon: T) -> &mut Self {
        self.icon.insert(icon.as_ref().to_owned());
	self
    }

    pub fn raw_icon<T: AsRef<str>>(&mut self, raw_icon: T) -> &mut Self {
        self.raw_icon.insert(raw_icon.as_ref().to_owned());
	self
    }

    pub fn id(&mut self, id: u32) -> &mut Self {
	self.replace_id.insert(id);
	self
    }

    pub fn action<T: AsRef<str>, E: AsRef<str>>(&mut self, name: T, display_name: E) -> &mut Self {
	self.actions.push((name.as_ref().to_owned(), display_name.as_ref().to_owned()));
	self
    }

    pub async fn show(&mut self) -> Result<NotificationHandler, NotificationError> {
	let mut cmd = Command::new("dunstify");
	cmd.arg(self.summary.to_owned());
	if let Some(body) = &self.body {
	    cmd.arg(body);
	}
	if let Some(app) = &self.app {
	    cmd.args(&["-a".to_owned(), app.to_owned()]);
	}

	if let Some(urgency) = &self.urgency {
	    cmd.args(&["-u".to_owned(), urgency.to_string()]);
	}

	for action in self.actions.iter() {
	    cmd.args(&["-A".to_owned(), format!("{},{}", action.0, action.1)]);
	}

	if let Some(timeout) = &self.timeout {
	    cmd.args(&["-t".to_owned(), timeout.as_millis().to_string()]);
	}

	if let Some(icon) = &self.icon {
	    cmd.args(&["-i".to_owned(), icon.to_owned()]);
	}

	if let Some(raw_icon) = &self.raw_icon {
	    cmd.args(&["-i".to_owned(), raw_icon.to_owned()]);
	}

	if let Some(replace_id) = &self.replace_id {
	    cmd.args(&["-r".to_owned(), replace_id.to_string()]);
	}

	cmd.arg("-p"); // Prints notification id
	cmd.arg("-b"); // Blocks until notification is closed
	cmd.stdout(Stdio::piped());

	let child = cmd.spawn()?;
	Ok(NotificationHandler::new(child, self.clone()))
    }

}

#[derive(Debug)]
pub struct NotificationError {
    msg: String
}

impl NotificationError {
    fn new(msg: String) -> Self { Self { msg } }
}

impl From<std::io::Error> for NotificationError {
    fn from(err: std::io::Error) -> Self {
        Self { msg: err.to_string() }
    }
}

impl ToString for NotificationError {
    fn to_string(&self) -> String {
        format!("NotificationError: {}", self.msg)
    }
}

pub struct NotificationHandler {
    child: Child,
    reader: BufReader<ChildStdout>,
    id: Option<u32>,
    notification: Notification,
}

impl<'a> NotificationHandler {
    fn new(mut child: Child, notification: Notification) -> Self {
	let handler = NotificationHandler {
	    reader: BufReader::new(child.stdout.take().unwrap()),
	    child,
	    id: None,
	    notification,
	};

	handler
    }

    pub async fn id(&mut self) -> u32 {
	if let Some(id) = self.id.clone() {
	    return id;
	}
	let mut id_str = String::new();
	self.reader.read_line(&mut id_str).await.ok();
	*self.id.insert(id_str.trim().parse().unwrap())
    }

    pub async fn action(mut self) -> Option<String> {
	self.id().await;
	let mut action = String::new();
	self.reader.read_line(&mut action).await.ok();
	self.closed().await;
	action = action.trim().to_owned();
	match action.as_ref() {
	    "1" | "2" | "3" => None,
	    action => Some(action.to_owned()),
	}
    }

    pub async fn closed(mut self) {
	self.child.wait().await.ok();
    }

    pub async fn close(mut self) {
	let id = self.id().await;
	self.child.kill().await.ok();
	Notification::close_impl(id).await;
    }

    pub async fn update(mut self) -> Notification {
	let id = self.id().await;
	self.child.kill().await.ok();
	self.notification.id(id);
	self.notification
    }
}

pub fn dclr<T: AsRef<str>>(text: T) -> ColorBuilder {
    ColorBuilder::new(text.as_ref().to_owned())
}

pub struct ColorBuilder {
    text: String,
    font: Option<String>,
    background: Option<Color>,
    foreground: Option<Color>,
}

impl ColorBuilder {
    pub fn new(text: String) -> ColorBuilder {
	ColorBuilder {
	    text,
	    font: None,
	    background: None,
	    foreground: None,
	}
    }

    pub fn font<T: AsRef<str>>(&mut self, font: T) -> &mut ColorBuilder {
	self.font.insert(font.as_ref().to_owned());
	self
    } 

    pub fn bg(&mut self, color: Color) -> &mut ColorBuilder {
	self.background.insert(color);
	self
    } 

    pub fn fg(&mut self, color: Color) -> &mut ColorBuilder {
	self.foreground.insert(color);
	self
    } 

    pub fn get(&mut self) -> String {
	format!("<span {} {} {}>{}</span>",
		if let Some(font) = &self.font {
		    format!("font=\"{}\"", font)
		} else { "".to_owned() }
		, if let Some(bg) = self.background {
		    format!("background=\"{}\"", bg)
		} else { "".to_owned() }
		, if let Some(fg) = self.foreground {
		    format!("foreground=\"{}\"", fg)
		} else { "".to_owned() }
		, self.text)
    }
}
