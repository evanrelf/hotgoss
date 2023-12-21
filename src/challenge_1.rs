use crate::protocol::{handle, Init, InitOk, MessageId};
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
    let mut msg_id = MessageId(1);

    handle(|request: Init| {
        Ok(InitOk {
            msg_id,
            in_reply_to: request.msg_id,
        })
    })?;

    loop {
        msg_id = MessageId(msg_id.0 + 1);

        handle(|request: Echo| {
            Ok(EchoOk {
                msg_id,
                in_reply_to: request.msg_id,
                echo: request.echo,
            })
        })?;
    }
}
