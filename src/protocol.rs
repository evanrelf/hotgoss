use serde::{Deserialize, Serialize};
use std::io::Write as _;

#[derive(Clone, Deserialize, Serialize)]
#[serde(transparent)]
pub struct NodeId(pub String);

#[derive(Clone, Copy, Deserialize, Serialize)]
#[serde(transparent)]
pub struct MessageId(pub usize);

#[derive(Deserialize, Serialize)]
pub struct Message<T> {
    pub src: NodeId,
    pub dest: NodeId,
    pub body: T,
}

#[derive(Deserialize)]
pub struct Init {
    pub r#type: String,
    pub msg_id: MessageId,
    pub node_id: NodeId,
    pub node_ids: Vec<NodeId>,
}

#[derive(Serialize)]
pub struct InitOk {
    pub r#type: String,
    pub msg_id: MessageId,
    pub in_reply_to: MessageId,
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

pub fn handle<Req, Res>(handler: impl Fn(Req) -> anyhow::Result<Res>) -> anyhow::Result<()>
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
