module HotGoss.Challenge1 (main) where

import Data.Aeson (FromJSON, ToJSON)
import HotGoss.Protocol

data Echo = Echo
  { msgId :: Word
  , echo :: Text
  }
  deriving stock (Generic, Show)
  deriving (ToJSON, FromJSON) via MessageJSON "echo" Echo

data EchoOk = EchoOk
  { msgId :: Word
  , inReplyTo :: Word
  , echo :: Text
  }
  deriving stock (Generic, Show)
  deriving (ToJSON, FromJSON) via MessageJSON "echo_ok" EchoOk

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
