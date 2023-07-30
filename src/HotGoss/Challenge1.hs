module HotGoss.Challenge1 (main) where

import Data.Aeson (FromJSON, ToJSON)
import Data.Data (Data)
import Data.SOP
import HotGoss.Protocol2

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
  let handleEcho :: s -> Message Exo Echo -> (s, NP (Message Endo) '[EchoOk])
      handleEcho state req = (state, res :* Nil)
        where
        res :: Message Endo EchoOk
        res = replyTo req EchoOk
          { msgId = undefined
          , inReplyTo = req.body.msgId
          , echo = req.body.echo
          }

  node () (Handler handleEcho :* Nil)

  -- (getMessageId, _, _) <- handleInit

  -- forever $ handle @Echo \body -> do
  --   msgId <- getMessageId
  --   pure EchoOk
  --     { msgId
  --     , inReplyTo = body.msgId
  --     , echo = body.echo
  --     }
