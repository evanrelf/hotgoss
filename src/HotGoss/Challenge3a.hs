module HotGoss.Challenge3a (main) where

import Data.Aeson (FromJSON, ToJSON)
import HotGoss.Protocol
import HotGoss.Union
import Prelude hiding (Read)

data Broadcast = Broadcast
  { msg_id :: Maybe Word
  , in_reply_to :: Maybe Word
  , message :: Word
  }
  deriving stock (Generic, Show)
  deriving (ToJSON, FromJSON) via MessageJSON "broadcast" '[] Broadcast

data BroadcastOk = BroadcastOk
  { msg_id :: Maybe Word
  , in_reply_to :: Maybe Word
  }
  deriving stock (Generic, Show)
  deriving (ToJSON, FromJSON) via MessageJSON "broadcast_ok" '[] BroadcastOk

data Read = Read
  { msg_id :: Maybe Word
  , in_reply_to :: Maybe Word
  }
  deriving stock (Generic, Show)
  deriving (ToJSON, FromJSON) via MessageJSON "read" '[] Read

data ReadOk = ReadOk
  { msg_id :: Maybe Word
  , in_reply_to :: Maybe Word
  , messages :: [Word]
  }
  deriving stock (Generic, Show)
  deriving (ToJSON, FromJSON) via MessageJSON "read_ok" '[] ReadOk

data Topology = Topology
  { msg_id :: Maybe Word
  , in_reply_to :: Maybe Word
  , topology :: HashMap Text [Text]
  }
  deriving stock (Generic, Show)
  deriving (ToJSON, FromJSON) via MessageJSON "topology" '[] Topology

data TopologyOk = TopologyOk
  { msg_id :: Maybe Word
  , in_reply_to :: Maybe Word
  }
  deriving stock (Generic, Show)
  deriving (ToJSON, FromJSON) via MessageJSON "topology_ok" '[] TopologyOk

main :: IO ()
main = do
  messagesRef <- newIORef []

  getMsgId <- do
    ref <- newIORef 1
    pure $ atomicModifyIORef' ref \x -> (x + 1, Just x)

  _ <- handleInit

  let handleBroadcast :: Broadcast -> IO BroadcastOk
      handleBroadcast body = do
        msg_id <- getMsgId
        atomicModifyIORef' messagesRef \ms -> (body.message : ms, ())
        pure BroadcastOk
          { msg_id
          , in_reply_to = body.msg_id
          }

  let handleRead :: Read -> IO ReadOk
      handleRead body = do
        msg_id <- getMsgId
        messages <- readIORef messagesRef
        pure ReadOk
          { msg_id
          , in_reply_to = body.msg_id
          , messages
          }

  let handleTopology :: Topology -> IO TopologyOk
      handleTopology body = do
        -- TODO: body.topology
        msg_id <- getMsgId
        pure TopologyOk
          { msg_id
          , in_reply_to = body.msg_id
          }

  forever $ handle @(Union '[Broadcast, Read, Topology]) @(Union '[BroadcastOk, ReadOk, TopologyOk]) \brt -> do
    case decompose @Broadcast brt of
      Right b -> inject <$> handleBroadcast b
      Left rt ->
        case decompose @Read rt of
          Right r -> inject <$> handleRead r
          Left t ->
            inject <$> handleTopology (extract @Topology t)
