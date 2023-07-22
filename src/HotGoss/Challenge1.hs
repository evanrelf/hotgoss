module HotGoss.Challenge1 (main) where

import Data.Aeson
import Data.Aeson.Types (Parser)
import HotGoss.Protocol

data Echo = Echo
  { messageId :: Word
  , echo :: Text
  }
  deriving stock (Show)

instance ToJSON Echo where
  toJSON :: Echo -> Value
  toJSON body =
    object
      [ "type" .= ("echo" :: Text)
      , "msg_id" .= body.messageId
      , "echo" .= body.echo
      ]

instance FromJSON Echo where
  parseJSON :: Value -> Parser Echo
  parseJSON =
    withObject "Echo" \o -> do
      type_ :: Text <- o .: "type"
      guard (type_ == "echo")
      messageId <- o .: "msg_id"
      echo <- o .: "echo"
      pure Echo{ messageId, echo }

data EchoOk = EchoOk
  { messageId :: Word
  , inReplyTo :: Word
  , echo :: Text
  }
  deriving stock (Show)

instance ToJSON EchoOk where
  toJSON :: EchoOk -> Value
  toJSON body =
    object
      [ "type" .= ("echo_ok" :: Text)
      , "msg_id" .= body.messageId
      , "in_reply_to" .= body.inReplyTo
      , "echo" .= body.echo
      ]

instance FromJSON EchoOk where
  parseJSON :: Value -> Parser EchoOk
  parseJSON =
    withObject "EchoOk" \o -> do
      type_ :: Text <- o .: "type"
      guard (type_ == "echo_ok")
      messageId <- o .: "msg_id"
      inReplyTo <- o .: "in_reply_to"
      echo <- o .: "echo"
      pure EchoOk{ messageId, inReplyTo, echo }

main :: IO ()
main = do
  messageIdRef <- newIORef 1

  let getMessageId = atomicModifyIORef' messageIdRef \mid -> (mid + 1, mid)

  handle @Init \body -> do
    pure InitOk
      { inReplyTo = body.messageId
      }

  forever $ handle @Echo \body -> do
    messageId <- getMessageId
    pure EchoOk
      { messageId
      , inReplyTo = body.messageId
      , echo = body.echo
      }
