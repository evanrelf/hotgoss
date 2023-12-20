use serde::{Deserialize, Serialize};
use std::io::Write as _;

#[derive(Clone, Deserialize, Serialize)]
#[serde(transparent)]
struct NodeId(String);

#[derive(Clone, Copy, Deserialize, Serialize)]
#[serde(transparent)]
struct MessageId(usize);

#[derive(Deserialize, Serialize)]
struct Message<T> {
    src: NodeId,
    dest: NodeId,
    body: T,
}

#[derive(Deserialize)]
struct Init {
    #[serde(rename = "type")]
    type_: String,
    msg_id: MessageId,
    node_id: NodeId,
    node_ids: Vec<NodeId>,
}

#[derive(Serialize)]
struct InitOk {
    #[serde(rename = "type")]
    type_: String,
    msg_id: MessageId,
    in_reply_to: MessageId,
}

#[derive(Deserialize)]
struct Echo {
    #[serde(rename = "type")]
    type_: String,
    msg_id: MessageId,
    echo: String,
}

#[derive(Serialize)]
struct EchoOk {
    #[serde(rename = "type")]
    type_: String,
    msg_id: MessageId,
    in_reply_to: MessageId,
    echo: String,
}

fn receive<T>() -> anyhow::Result<Message<T>>
where
    for<'de> T: Deserialize<'de>,
{
    let mut buffer = String::new();
    std::io::stdin().read_line(&mut buffer)?;
    let message = serde_json::from_str(&buffer)?;
    Ok(message)
}

fn send<T>(message: &Message<T>) -> anyhow::Result<()>
where
    T: Serialize,
{
    let json = serde_json::to_string(&message)?;
    println!("{json}");
    std::io::stdout().flush()?;
    Ok(())
}

fn handle<Req, Res>(handler: impl Fn(Req) -> anyhow::Result<Res>) -> anyhow::Result<()>
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

pub fn main() -> anyhow::Result<()> {
    let mut msg_id = MessageId(1);

    handle(|request: Init| {
        Ok(InitOk {
            type_: String::from("init_ok"),
            msg_id,
            in_reply_to: request.msg_id,
        })
    })?;

    loop {
        msg_id = MessageId(msg_id.0 + 1);

        handle(|request: Echo| {
            Ok(EchoOk {
                type_: String::from("echo_ok"),
                msg_id,
                in_reply_to: request.msg_id,
                echo: request.echo,
            })
        })?;
    }
}
