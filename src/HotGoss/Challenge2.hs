module HotGoss.Challenge2 (main) where

import Data.Aeson (FromJSON, ToJSON)
import Data.Data (Data)
import HotGoss.Protocol
import Prelude hiding (id)

data Generate = Generate
  { msgId :: MessageId
  , inReplyTo :: Omitted
  }
  deriving stock (Generic, Data, Show)
  deriving (ToJSON, FromJSON) via MessageJSON Generate

data GenerateOk = GenerateOk
  { msgId :: MessageId
  , inReplyTo :: MessageId
  , id :: Text
  }
  deriving stock (Generic, Data, Show)
  deriving (ToJSON, FromJSON) via MessageJSON GenerateOk

main :: IO ()
main = do
  (getMessageId, nodeId, _) <- handleInit

  forever $ handle_ @Generate \body -> do
    msgId <- getMessageId
    pure GenerateOk
      { msgId
      , inReplyTo = body.msgId
      , id = toText nodeId <> "-" <> show msgId
      }
