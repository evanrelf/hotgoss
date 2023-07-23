module HotGoss.Challenge1 (main) where

import Data.Aeson (FromJSON, ToJSON)
import Data.Data (Data)
import HotGoss.Protocol

data Echo = Echo
  { msgId :: MessageId
  , inReplyTo :: Omitted
  , echo :: Text
  }
  deriving stock (Generic, Data, Show)
  deriving (ToJSON, FromJSON) via MessageJSON Echo

data EchoOk = EchoOk
  { msgId :: MessageId
  , inReplyTo :: MessageId
  , echo :: Text
  }
  deriving stock (Generic, Data, Show)
  deriving (ToJSON, FromJSON) via MessageJSON EchoOk

main :: IO ()
main = do
  getMsgId <- do
    ref <- newIORef 1
    pure $ atomicModifyIORef' ref \x -> (x + 1, x)

  _ <- handleInit

  forever $ handle @Echo \body -> do
    msgId <- getMsgId
    pure EchoOk
      { msgId
      , inReplyTo = body.msgId
      , echo = body.echo
      }
