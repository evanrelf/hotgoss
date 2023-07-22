module HotGoss.Challenge2 (main) where

import Data.Aeson (FromJSON, ToJSON)
import HotGoss.Protocol
import Prelude hiding (id)

data Generate = Generate
  { msg_id :: Word
  }
  deriving stock (Generic, Show)
  deriving (ToJSON, FromJSON) via MessageJSON "generate" '[] Generate

data GenerateOk = GenerateOk
  { msg_id :: Word
  , in_reply_to :: Word
  , id :: Text
  }
  deriving stock (Generic, Show)
  deriving (ToJSON, FromJSON) via MessageJSON "generate_ok" '[] GenerateOk

main :: IO ()
main = do
  getMsgId <- do
    ref <- newIORef 1
    pure $ atomicModifyIORef' ref \mid -> (mid + 1, mid)

  nodeId <- handleInit

  forever $ handle @Generate \generate -> do
    msg_id <- getMsgId
    pure GenerateOk
      { msg_id
      , in_reply_to = generate.msg_id
      , id = nodeId <> "-" <> show msg_id
      }
