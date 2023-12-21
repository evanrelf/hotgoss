use crate::protocol::{handle, Init, InitOk, MessageId};
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
    let mut msg_id = MessageId(0);

    let mut node_id = None;

    handle(|request: Init| {
        node_id = Some(request.node_id);
        Ok(InitOk {
            msg_id,
            in_reply_to: request.msg_id,
        })
    })?;

    loop {
        handle(|request: Generate| {
            Ok(GenerateOk {
                msg_id: msg_id.next(),
                in_reply_to: request.msg_id,
                id: format!("{}-{}", node_id.clone().unwrap(), msg_id),
            })
        })?;
    }
}
