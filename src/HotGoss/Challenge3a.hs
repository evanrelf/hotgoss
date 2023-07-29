module HotGoss.Challenge3a (main) where

import Data.Aeson (FromJSON, ToJSON)
import Data.Data (Data)
import HotGoss.Protocol
import HotGoss.Union
import Prelude hiding (Read, on)

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
  , messages :: [Word]
  }
  deriving stock (Generic, Data, Show)
  deriving (ToJSON, FromJSON) via MessageBodyJson ReadOk

data Topology = Topology
  { msgId :: MessageId
  , topology :: HashMap NodeId [NodeId]
  }
  deriving stock (Generic, Data, Show)
  deriving (ToJSON, FromJSON) via MessageBodyJson Topology

data TopologyOk = TopologyOk
  { msgId :: MessageId
  , inReplyTo :: MessageId
  }
  deriving stock (Generic, Data, Show)
  deriving (ToJSON, FromJSON) via MessageBodyJson TopologyOk

main :: IO ()
main = do
  messagesRef <- newIORef []

  (getMessageId, _, _) <- handleInit

  handle @Topology \body -> do
    -- TODO: body.topology
    msgId <- getMessageId
    pure TopologyOk
      { msgId
      , inReplyTo = body.msgId
      }

  let handleBroadcast :: Broadcast -> IO BroadcastOk
      handleBroadcast body = do
        msgId <- getMessageId
        atomicModifyIORef' messagesRef \ms -> (body.message : ms, ())
        pure BroadcastOk
          { msgId
          , inReplyTo = body.msgId
          }

  let handleRead :: Read -> IO ReadOk
      handleRead body = do
        msgId <- getMessageId
        messages <- readIORef messagesRef
        pure ReadOk
          { msgId
          , inReplyTo = body.msgId
          , messages
          }

  forever $ handle @_ @(Union '[BroadcastOk, ReadOk]) $
    case_
      `on` (\msg -> handleRead msg <&> inject)
      `on` (\msg -> handleBroadcast msg <&> inject)
