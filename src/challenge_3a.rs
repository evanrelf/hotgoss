use crate::protocol::{handle, handle_init, Either, Message, MessageId, NodeId};
use serde::{Deserialize, Serialize};
use std::collections::HashMap;

#[derive(Deserialize)]
#[serde(rename = "broadcast", tag = "type")]
struct Broadcast {
    msg_id: MessageId,
    message: usize,
}

#[derive(Serialize)]
#[serde(rename = "broadcast_ok", tag = "type")]
struct BroadcastOk {
    msg_id: MessageId,
    in_reply_to: MessageId,
}

#[derive(Deserialize)]
#[serde(rename = "read", tag = "type")]
struct Read {
    msg_id: MessageId,
}

#[derive(Serialize)]
#[serde(rename = "read_ok", tag = "type")]
struct ReadOk {
    msg_id: MessageId,
    in_reply_to: MessageId,
    messages: Vec<usize>,
}

#[derive(Deserialize)]
#[serde(rename = "topology", tag = "type")]
struct Topology {
    msg_id: MessageId,
    #[allow(dead_code)]
    topology: HashMap<NodeId, Vec<NodeId>>,
}

#[derive(Serialize)]
#[serde(rename = "topology_ok", tag = "type")]
struct TopologyOk {
    msg_id: MessageId,
    in_reply_to: MessageId,
}

pub fn main() -> anyhow::Result<()> {
    let mut messages = Vec::new();

    let (mut msg_id, _, _) = handle_init()?;

    handle(|request: Message<Topology>| {
        Ok(TopologyOk {
            msg_id: msg_id.next(),
            in_reply_to: request.body.msg_id,
        })
    })?;

    loop {
        handle(|request: Message<Either<Broadcast, Read>>| {
            Ok(match request.body {
                Either::Left(body) => {
                    messages.push(body.message);
                    Either::Left(BroadcastOk {
                        msg_id: msg_id.next(),
                        in_reply_to: body.msg_id,
                    })
                }
                Either::Right(body) => Either::Right(ReadOk {
                    msg_id: msg_id.next(),
                    in_reply_to: body.msg_id,
                    messages: messages.clone(),
                }),
            })
        })?;
    }
}
