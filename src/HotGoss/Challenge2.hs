module HotGoss.Challenge2 (main) where

import Data.Aeson (FromJSON, ToJSON)
import Data.Data (Data)
import Data.Text.Display (display)
import HotGoss.Protocol
import Prelude hiding (id)

data Generate = Generate
  { msgId :: MessageId
  , inReplyTo :: Omitted
  }
  deriving stock (Generic, Data, Show)
  deriving (ToJSON, FromJSON) via MessageBodyJson Generate

data GenerateOk = GenerateOk
  { msgId :: MessageId
  , inReplyTo :: MessageId
  , id :: Text
  }
  deriving stock (Generic, Data, Show)
  deriving (ToJSON, FromJSON) via MessageBodyJson GenerateOk

main :: IO ()
main = do
  (getMessageId, nodeId, _) <- handleInit

  forever $ handle_ @Generate \body -> do
    msgId <- getMessageId
    pure GenerateOk
      { msgId
      , inReplyTo = body.msgId
      , id = display nodeId <> "-" <> display msgId
      }
