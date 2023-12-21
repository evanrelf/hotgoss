use crate::protocol::{handle, handle_init, Message, MessageId};
use serde::{Deserialize, Serialize};

#[derive(Deserialize)]
#[serde(rename = "generate", tag = "type")]
struct Generate {
    msg_id: MessageId,
}

#[derive(Serialize)]
#[serde(rename = "generate_ok", tag = "type")]
struct GenerateOk {
    msg_id: MessageId,
    in_reply_to: MessageId,
    id: String,
}

pub fn main() -> anyhow::Result<()> {
    let (mut msg_id, node_id, _) = handle_init()?;

    loop {
        handle(|request: Message<Generate>| {
            Ok(GenerateOk {
                msg_id: msg_id.next(),
                in_reply_to: request.body.msg_id,
                id: format!("{}-{}", node_id.clone(), msg_id),
            })
        })?;
    }
}
