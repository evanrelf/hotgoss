module HotGoss.Challenge3a (main) where

import Data.Aeson (FromJSON, ToJSON)
import Data.Data (Data)
import HotGoss.Protocol
import HotGoss.Union
import Prelude hiding (Read)

data Broadcast = Broadcast
  { msgId :: MessageId
  , inReplyTo :: Omitted
  , message :: Word
  }
  deriving stock (Generic, Data, Show)
  deriving (ToJSON, FromJSON) via MessageJSON Broadcast

data BroadcastOk = BroadcastOk
  { msgId :: MessageId
  , inReplyTo :: MessageId
  }
  deriving stock (Generic, Data, Show)
  deriving (ToJSON, FromJSON) via MessageJSON BroadcastOk

data Read = Read
  { msgId :: MessageId
  , inReplyTo :: Omitted
  }
  deriving stock (Generic, Data, Show)
  deriving (ToJSON, FromJSON) via MessageJSON Read

data ReadOk = ReadOk
  { msgId :: MessageId
  , inReplyTo :: MessageId
  , messages :: [Word]
  }
  deriving stock (Generic, Data, Show)
  deriving (ToJSON, FromJSON) via MessageJSON ReadOk

data Topology = Topology
  { msgId :: MessageId
  , inReplyTo :: Omitted
  , topology :: HashMap NodeId [NodeId]
  }
  deriving stock (Generic, Data, Show)
  deriving (ToJSON, FromJSON) via MessageJSON Topology

data TopologyOk = TopologyOk
  { msgId :: MessageId
  , inReplyTo :: MessageId
  }
  deriving stock (Generic, Data, Show)
  deriving (ToJSON, FromJSON) via MessageJSON TopologyOk

main :: IO ()
main = do
  messagesRef <- newIORef []

  (getMessageId, _, _) <- handleInit

  handle_ @Topology \body -> do
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

  forever $ handle_ @(Union '[Broadcast, Read]) @(Union '[BroadcastOk, ReadOk]) \br ->
    case decompose @Broadcast br of
      Right b -> inject <$> handleBroadcast b
      Left (extract -> r) -> inject <$> handleRead r
