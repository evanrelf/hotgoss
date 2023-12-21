use serde::{Deserialize, Serialize};
use std::{
    fmt::{self, Display},
    io::Write as _,
};

#[derive(Clone, Deserialize, Eq, Hash, PartialEq, Serialize)]
#[serde(transparent)]
pub struct NodeId(String);

impl Display for NodeId {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.0)
    }
}

#[derive(Clone, Copy, Deserialize, Serialize)]
#[serde(transparent)]
pub struct MessageId(usize);

impl MessageId {
    pub fn new() -> Self {
        Self(0)
    }

    pub fn next(&mut self) -> Self {
        let current = *self;
        *self = Self(self.0 + 1);
        current
    }
}

impl Display for MessageId {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.0)
    }
}

#[derive(Deserialize, Serialize)]
pub struct Message<T> {
    pub src: NodeId,
    pub dest: NodeId,
    pub body: T,
}

#[derive(Deserialize)]
#[serde(rename = "init", tag = "type")]
pub struct Init {
    pub msg_id: MessageId,
    pub node_id: NodeId,
    pub node_ids: Vec<NodeId>,
}

#[derive(Serialize)]
#[serde(rename = "init_ok", tag = "type")]
pub struct InitOk {
    pub msg_id: MessageId,
    pub in_reply_to: MessageId,
}

#[derive(Serialize, Deserialize)]
#[serde(untagged)]
pub enum Either<L, R> {
    Left(L),
    Right(R),
}

pub fn receive<T>() -> anyhow::Result<Message<T>>
where
    for<'de> T: Deserialize<'de>,
{
    let mut buffer = String::new();
    std::io::stdin().read_line(&mut buffer)?;
    let message = serde_json::from_str(&buffer)?;
    Ok(message)
}

pub fn send<T>(message: &Message<T>) -> anyhow::Result<()>
where
    T: Serialize,
{
    let json = serde_json::to_string(&message)?;
    println!("{json}");
    std::io::stdout().flush()?;
    Ok(())
}

pub fn handle<Req, Res>(mut handler: impl FnMut(Req) -> anyhow::Result<Res>) -> anyhow::Result<()>
where
    for<'de> Req: Deserialize<'de>,
    Res: Serialize,
{
    let request = receive::<Req>()?;
    let body = handler(request.body)?;
    let response = Message {
        src: request.dest,
        dest: request.src,
        body,
    };
    send::<Res>(&response)?;
    Ok(())
}

pub fn handle_init() -> anyhow::Result<(MessageId, NodeId, Vec<NodeId>)> {
    let msg_id = MessageId::new();

    let mut node_id = None;

    let mut node_ids = None;

    handle(|request: Init| {
        node_id = Some(request.node_id);
        node_ids = Some(request.node_ids);
        Ok(InitOk {
            msg_id,
            in_reply_to: request.msg_id,
        })
    })?;

    Ok((msg_id, node_id.unwrap(), node_ids.unwrap()))
}
