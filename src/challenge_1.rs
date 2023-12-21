use crate::protocol::{handle, Init, InitOk, MessageId};
use serde::{Deserialize, Serialize};

#[derive(Deserialize)]
struct Echo {
    #[allow(dead_code)]
    r#type: String,
    msg_id: MessageId,
    echo: String,
}

#[derive(Serialize)]
struct EchoOk {
    r#type: String,
    msg_id: MessageId,
    in_reply_to: MessageId,
    echo: String,
}

pub fn main() -> anyhow::Result<()> {
    let mut msg_id = MessageId(1);

    handle(|request: Init| {
        Ok(InitOk {
            r#type: String::from("init_ok"),
            msg_id,
            in_reply_to: request.msg_id,
        })
    })?;

    loop {
        msg_id = MessageId(msg_id.0 + 1);

        handle(|request: Echo| {
            Ok(EchoOk {
                r#type: String::from("echo_ok"),
                msg_id,
                in_reply_to: request.msg_id,
                echo: request.echo,
            })
        })?;
    }
}
