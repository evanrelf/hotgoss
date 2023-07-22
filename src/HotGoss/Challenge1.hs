module HotGoss.Challenge1 (main) where

import Data.Aeson
import Data.Aeson.Types (Parser)
import HotGoss.Protocol

data Echo = Echo
  { msg_id :: Word
  , echo :: Text
  }
  deriving stock (Show)

instance ToJSON Echo where
  toJSON :: Echo -> Value
  toJSON body =
    object
      [ "type" .= ("echo" :: Text)
      , "msg_id" .= body.msg_id
      , "echo" .= body.echo
      ]

instance FromJSON Echo where
  parseJSON :: Value -> Parser Echo
  parseJSON =
    withObject "Echo" \o -> do
      type_ :: Text <- o .: "type"
      guard (type_ == "echo")
      msg_id <- o .: "msg_id"
      echo <- o .: "echo"
      pure Echo{ msg_id, echo }

data EchoOk = EchoOk
  { msg_id :: Word
  , in_reply_to :: Word
  , echo :: Text
  }
  deriving stock (Show)

instance ToJSON EchoOk where
  toJSON :: EchoOk -> Value
  toJSON body =
    object
      [ "type" .= ("echo_ok" :: Text)
      , "msg_id" .= body.msg_id
      , "in_reply_to" .= body.in_reply_to
      , "echo" .= body.echo
      ]

instance FromJSON EchoOk where
  parseJSON :: Value -> Parser EchoOk
  parseJSON =
    withObject "EchoOk" \o -> do
      type_ :: Text <- o .: "type"
      guard (type_ == "echo_ok")
      msg_id <- o .: "msg_id"
      in_reply_to <- o .: "in_reply_to"
      echo <- o .: "echo"
      pure EchoOk{ msg_id, in_reply_to, echo }

main :: IO ()
main = do
  getMsgId <- do
    ref <- newIORef 1
    pure $ atomicModifyIORef' ref \x -> (x + 1, x)

  _ <- handleInit

  forever $ handle @Echo \body -> do
    msg_id <- getMsgId
    pure EchoOk
      { msg_id
      , in_reply_to = body.msg_id
      , echo = body.echo
      }
