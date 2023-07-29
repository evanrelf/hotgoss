module HotGoss.Challenge1 (main) where

import Data.Aeson (FromJSON, ToJSON)
import Data.Data (Data)
import HotGoss.Protocol

data Echo = Echo
  { msgId :: MessageId
  , echo :: Text
  }
  deriving stock (Generic, Data, Show)
  deriving (ToJSON, FromJSON) via MessageBodyJson Echo

data EchoOk = EchoOk
  { msgId :: MessageId
  , inReplyTo :: MessageId
  , echo :: Text
  }
  deriving stock (Generic, Data, Show)
  deriving (ToJSON, FromJSON) via MessageBodyJson EchoOk

main :: IO ()
main = do
  (getMessageId, _, _) <- handleInit

  forever $ handle @Echo \body -> do
    msgId <- getMessageId
    pure EchoOk
      { msgId
      , inReplyTo = body.msgId
      , echo = body.echo
      }
