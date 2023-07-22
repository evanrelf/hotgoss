module HotGoss.Challenge1 (main) where

import Data.Aeson (FromJSON, ToJSON)
import HotGoss.Protocol

data Echo = Echo
  { msg_id :: Word
  , echo :: Text
  }
  deriving stock (Generic, Show)
  deriving (ToJSON, FromJSON) via MessageJSON "echo" '[] Echo

data EchoOk = EchoOk
  { msg_id :: Word
  , in_reply_to :: Word
  , echo :: Text
  }
  deriving stock (Generic, Show)
  deriving (ToJSON, FromJSON) via MessageJSON "echo_ok" '[] EchoOk

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
