module HotGoss.Challenge3a (main) where

import Data.Aeson (FromJSON, ToJSON)
import Data.Data (Data)
import HotGoss.Protocol
import HotGoss.Union
import Prelude hiding (Read)

data Broadcast = Broadcast
  { msgId :: Word
  , inReplyTo :: Omitted
  , message :: Word
  }
  deriving stock (Generic, Data, Show)
  deriving (ToJSON, FromJSON) via MessageJSON Broadcast

data BroadcastOk = BroadcastOk
  { msgId :: Word
  , inReplyTo :: Word
  }
  deriving stock (Generic, Data, Show)
  deriving (ToJSON, FromJSON) via MessageJSON BroadcastOk

data Read = Read
  { msgId :: Word
  , inReplyTo :: Omitted
  }
  deriving stock (Generic, Data, Show)
  deriving (ToJSON, FromJSON) via MessageJSON Read

data ReadOk = ReadOk
  { msgId :: Word
  , inReplyTo :: Word
  , messages :: [Word]
  }
  deriving stock (Generic, Data, Show)
  deriving (ToJSON, FromJSON) via MessageJSON ReadOk

data Topology = Topology
  { msgId :: Word
  , inReplyTo :: Omitted
  , topology :: HashMap Text [Text]
  }
  deriving stock (Generic, Data, Show)
  deriving (ToJSON, FromJSON) via MessageJSON Topology

data TopologyOk = TopologyOk
  { msgId :: Word
  , inReplyTo :: Word
  }
  deriving stock (Generic, Data, Show)
  deriving (ToJSON, FromJSON) via MessageJSON TopologyOk

main :: IO ()
main = do
  messagesRef <- newIORef []

  getMsgId <- do
    ref <- newIORef 1
    pure $ atomicModifyIORef' ref \x -> (x + 1, x)

  _ <- handleInit

  let handleBroadcast :: Broadcast -> IO BroadcastOk
      handleBroadcast body = do
        msgId <- getMsgId
        atomicModifyIORef' messagesRef \ms -> (body.message : ms, ())
        pure BroadcastOk
          { msgId
          , inReplyTo = body.msgId
          }

  let handleRead :: Read -> IO ReadOk
      handleRead body = do
        msgId <- getMsgId
        messages <- readIORef messagesRef
        pure ReadOk
          { msgId
          , inReplyTo = body.msgId
          , messages
          }

  let handleTopology :: Topology -> IO TopologyOk
      handleTopology body = do
        -- TODO: body.topology
        msgId <- getMsgId
        pure TopologyOk
          { msgId
          , inReplyTo = body.msgId
          }

  forever $ handle @(Union '[Broadcast, Read, Topology]) @(Union '[BroadcastOk, ReadOk, TopologyOk]) \brt -> do
    case decompose @Broadcast brt of
      Right b -> inject <$> handleBroadcast b
      Left rt ->
        case decompose @Read rt of
          Right r -> inject <$> handleRead r
          Left t ->
            inject <$> handleTopology (extract @Topology t)
