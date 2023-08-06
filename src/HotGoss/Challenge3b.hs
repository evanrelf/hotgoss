module HotGoss.Challenge3b (main) where

import Data.Aeson (FromJSON, ToJSON)
import Data.Data (Data)
import Data.HashMap.Strict qualified as HashMap
import Data.HashSet qualified as HashSet
import HotGoss.Protocol
import HotGoss.Union
import Optics
import Prelude hiding (Read, on, state)
import UnliftIO.Async qualified as Async

data Broadcast = Broadcast
  { msgId :: MessageId
  , message :: Word
  }
  deriving stock (Generic, Data, Show)
  deriving (ToJSON, FromJSON) via MessageBodyJson Broadcast

data BroadcastOk = BroadcastOk
  { msgId :: MessageId
  , inReplyTo :: MessageId
  }
  deriving stock (Generic, Data, Show)
  deriving (ToJSON, FromJSON) via MessageBodyJson BroadcastOk

data Read = Read
  { msgId :: MessageId
  }
  deriving stock (Generic, Data, Show)
  deriving (ToJSON, FromJSON) via MessageBodyJson Read

data ReadOk = ReadOk
  { msgId :: MessageId
  , inReplyTo :: MessageId
  , messages :: HashSet Word
  }
  deriving stock (Generic, Data, Show)
  deriving (ToJSON, FromJSON) via MessageBodyJson ReadOk

data Topology = Topology
  { msgId :: MessageId
  , topology :: HashMap NodeId (HashSet NodeId)
  }
  deriving stock (Generic, Data, Show)
  deriving (ToJSON, FromJSON) via MessageBodyJson Topology

data TopologyOk = TopologyOk
  { msgId :: MessageId
  , inReplyTo :: MessageId
  }
  deriving stock (Generic, Data, Show)
  deriving (ToJSON, FromJSON) via MessageBodyJson TopologyOk

data State = State
  { topology :: HashMap NodeId (HashSet NodeId)
  , messages :: HashSet Word
  , gossip :: HashMap NodeId (HashSet Word)
  }
  deriving stock (Generic)

main :: IO ()
main = do
  (getMessageId, nodeId, nodeIds) <- handleInit

  stateRef <- newIORef State
    { topology = HashMap.empty
    , messages = HashSet.empty
    , gossip = HashMap.fromList (nodeIds <&> (, HashSet.empty))
    }

  let handleTopology :: Topology -> IO TopologyOk
      handleTopology body = do
        msgId <- getMessageId
        atomicModifyIORef' stateRef \state -> (, ()) $
          state & set #topology (HashMap.delete nodeId body.topology)
        pure TopologyOk
          { msgId
          , inReplyTo = body.msgId
          }

  -- TODO: Need access to `Message` for sender's node ID
  let handleBroadcast :: Broadcast -> IO BroadcastOk
      handleBroadcast body = do
        msgId <- getMessageId
        atomicModifyIORef' stateRef \state -> (, ()) $
          state
            & over #messages (HashSet.insert body.message)
            & over #gossip (HashMap.adjust (HashSet.insert body.message) undefined)
        pure BroadcastOk
          { msgId
          , inReplyTo = body.msgId
          }

  let handleRead :: Read -> IO ReadOk
      handleRead body = do
        msgId <- getMessageId
        state <- readIORef stateRef
        pure ReadOk
          { msgId
          , inReplyTo = body.msgId
          , messages = state.messages
          }

  -- TODO: Need ability to handle a message without sending a response
  forever $ handle @_ @(Union '[TopologyOk, BroadcastOk, ReadOk]) $
    case_
      `on` (\msg -> handleTopology msg <&> inject)
      `on` (\msg -> handleRead msg <&> inject)
      `on` (\msg -> handleBroadcast msg <&> inject)
