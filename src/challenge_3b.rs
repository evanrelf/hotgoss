use crate::protocol::{handle, handle_init, Either, Message, MessageId, NodeId};
use serde::{Deserialize, Serialize};
use std::collections::{HashMap, HashSet};

#[derive(Clone, Deserialize)]
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

#[derive(Clone, Deserialize)]
#[serde(rename = "read", tag = "type")]
struct Read {
    msg_id: MessageId,
}

#[derive(Serialize)]
#[serde(rename = "read_ok", tag = "type")]
struct ReadOk {
    msg_id: MessageId,
    in_reply_to: MessageId,
    messages: HashSet<usize>,
}

#[derive(Deserialize)]
#[serde(rename = "topology", tag = "type")]
struct Topology {
    msg_id: MessageId,
    #[allow(dead_code)]
    topology: HashMap<NodeId, HashSet<NodeId>>,
}

#[derive(Serialize)]
#[serde(rename = "topology_ok", tag = "type")]
struct TopologyOk {
    msg_id: MessageId,
    in_reply_to: MessageId,
}

struct State {
    msg_id: MessageId,
    node_id: NodeId,
    node_ids: Vec<NodeId>,
    topology: HashMap<NodeId, HashSet<NodeId>>,
    messages: HashSet<usize>,
    gossip: HashMap<NodeId, HashSet<usize>>,
}

impl State {
    fn new(msg_id: MessageId, node_id: NodeId, node_ids: Vec<NodeId>) -> Self {
        Self {
            msg_id,
            node_id,
            node_ids,
            topology: HashMap::new(),
            messages: HashSet::new(),
            gossip: HashMap::new(),
        }
    }
}

pub fn main() -> anyhow::Result<()> {
    let (msg_id, node_id, node_ids) = handle_init()?;

    let mut state = State::new(msg_id, node_id, node_ids);

    handle(|request| handle_topology(&mut state, request))?;

    loop {
        handle(
            |request: Message<Either<Broadcast, Read>>| match request.body {
                Either::Left(ref body) => {
                    let body = body.clone();
                    handle_broadcast(&mut state, request.map(|_| body)).map(Either::Left)
                }
                Either::Right(ref body) => {
                    let body = body.clone();
                    handle_read(&mut state, request.map(|_| body)).map(Either::Right)
                }
            },
        )?;
    }
}

#[allow(clippy::unnecessary_wraps)]
fn handle_topology(state: &mut State, request: Message<Topology>) -> anyhow::Result<TopologyOk> {
    state.topology = request.body.topology;
    state.topology.remove(&state.node_id);

    Ok(TopologyOk {
        msg_id: state.msg_id.next(),
        in_reply_to: request.body.msg_id,
    })
}

#[allow(clippy::unnecessary_wraps)]
fn handle_broadcast(state: &mut State, request: Message<Broadcast>) -> anyhow::Result<BroadcastOk> {
    state.messages.insert(request.body.message);

    state.gossip.entry(request.src).and_modify(|node_messages| {
        node_messages.insert(request.body.message);
    });

    Ok(BroadcastOk {
        msg_id: state.msg_id.next(),
        in_reply_to: request.body.msg_id,
    })
}

fn handle_broadcast_ok(state: &mut State, request: Message<BroadcastOk>) -> anyhow::Result<()> {
    todo!()
}

#[allow(clippy::needless_pass_by_value, clippy::unnecessary_wraps)]
fn handle_read(state: &mut State, request: Message<Read>) -> anyhow::Result<ReadOk> {
    Ok(ReadOk {
        msg_id: state.msg_id.next(),
        in_reply_to: request.body.msg_id,
        messages: state.messages.clone(),
    })
}

fn handle_read_ok(state: &mut State, request: Message<ReadOk>) -> anyhow::Result<()> {
    todo!()
}
