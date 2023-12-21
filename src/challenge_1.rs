use crate::protocol::{handle, handle_init, Message, MessageId};
use serde::{Deserialize, Serialize};

#[derive(Deserialize)]
#[serde(rename = "echo", tag = "type")]
struct Echo {
    msg_id: MessageId,
    echo: String,
}

#[derive(Serialize)]
#[serde(rename = "echo_ok", tag = "type")]
struct EchoOk {
    msg_id: MessageId,
    in_reply_to: MessageId,
    echo: String,
}

pub fn main() -> anyhow::Result<()> {
    let (mut msg_id, _, _) = handle_init()?;

    loop {
        handle(|request: Message<Echo>| {
            Ok(EchoOk {
                msg_id: msg_id.next(),
                in_reply_to: request.body.msg_id,
                echo: request.body.echo,
            })
        })?;
    }
}
