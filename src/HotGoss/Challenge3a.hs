module HotGoss.Challenge3a (main) where

import Data.Aeson (FromJSON, ToJSON)
import HotGoss.Protocol
import HotGoss.Union
import Prelude hiding (Read)

data Broadcast = Broadcast
  { msgId :: Maybe Word
  , inReplyTo :: Maybe Word
  , message :: Word
  }
  deriving stock (Generic, Show)
  deriving (ToJSON, FromJSON) via MessageJSON "broadcast" Broadcast

data BroadcastOk = BroadcastOk
  { msgId :: Maybe Word
  , inReplyTo :: Maybe Word
  }
  deriving stock (Generic, Show)
  deriving (ToJSON, FromJSON) via MessageJSON "broadcast_ok" BroadcastOk

data Read = Read
  { msgId :: Maybe Word
  , inReplyTo :: Maybe Word
  }
  deriving stock (Generic, Show)
  deriving (ToJSON, FromJSON) via MessageJSON "read" Read

data ReadOk = ReadOk
  { msgId :: Maybe Word
  , inReplyTo :: Maybe Word
  , messages :: [Word]
  }
  deriving stock (Generic, Show)
  deriving (ToJSON, FromJSON) via MessageJSON "read_ok" ReadOk

data Topology = Topology
  { msgId :: Maybe Word
  , inReplyTo :: Maybe Word
  , topology :: HashMap Text [Text]
  }
  deriving stock (Generic, Show)
  deriving (ToJSON, FromJSON) via MessageJSON "topology" Topology

data TopologyOk = TopologyOk
  { msgId :: Maybe Word
  , inReplyTo :: Maybe Word
  }
  deriving stock (Generic, Show)
  deriving (ToJSON, FromJSON) via MessageJSON "topology_ok" TopologyOk

main :: IO ()
main = do
  messagesRef <- newIORef []

  getMsgId <- do
    ref <- newIORef 1
    pure $ atomicModifyIORef' ref \x -> (x + 1, Just x)

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
