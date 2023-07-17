module HotGoss.Challenge1 (main) where

import Data.Aeson
import Data.Aeson.Types (Parser)
import HotGoss.Protocol
import HotGoss.Union

data Echo = Echo
  { messageId :: Word
  , echo :: Text
  }
  deriving stock (Show)

instance ToJSON Echo where
  toJSON :: Echo -> Value
  toJSON echo =
    object
      [ "type" .= ("echo" :: Text)
      , "msg_id" .= echo.messageId
      , "echo" .= echo.echo
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
  toJSON echoOk =
    object
      [ "type" .= ("echo_ok" :: Text)
      , "msg_id" .= echoOk.messageId
      , "in_reply_to" .= echoOk.inReplyTo
      , "echo" .= echoOk.echo
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
main = pure ()
