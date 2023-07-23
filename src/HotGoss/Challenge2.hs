module HotGoss.Challenge2 (main) where

import Data.Aeson (FromJSON, ToJSON)
import Data.Data (Data)
import HotGoss.Protocol
import Prelude hiding (id)

data Generate = Generate
  { msgId :: Word
  , inReplyTo :: Omitted
  }
  deriving stock (Generic, Data, Show)
  deriving (ToJSON, FromJSON) via MessageJSON Generate

data GenerateOk = GenerateOk
  { msgId :: Word
  , inReplyTo :: Word
  , id :: Text
  }
  deriving stock (Generic, Data, Show)
  deriving (ToJSON, FromJSON) via MessageJSON GenerateOk

main :: IO ()
main = do
  getMsgId <- do
    ref <- newIORef 1
    pure $ atomicModifyIORef' ref \x -> (x + 1, x)

  nodeId <- handleInit

  forever $ handle @Generate \body -> do
    msgId <- getMsgId
    pure GenerateOk
      { msgId
      , inReplyTo = body.msgId
      , id = nodeId <> "-" <> show msgId
      }
