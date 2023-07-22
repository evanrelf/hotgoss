module HotGoss.Challenge2 (main) where

import Data.Aeson
import Data.Aeson.Types (Parser)
import HotGoss.Protocol
import Prelude hiding (id)

data Generate = Generate
  { msg_id :: Word
  }
  deriving stock (Show)

instance ToJSON Generate where
  toJSON :: Generate -> Value
  toJSON body =
    object
      [ "type" .= ("generate" :: Text)
      , "msg_id" .= body.msg_id
      ]

instance FromJSON Generate where
  parseJSON :: Value -> Parser Generate
  parseJSON =
    withObject "Generate" \o -> do
      type_ :: Text <- o .: "type"
      guard (type_ == "generate")
      msg_id <- o .: "msg_id"
      pure Generate{ msg_id }

data GenerateOk = GenerateOk
  { msg_id :: Word
  , in_reply_to :: Word
  , id :: Text
  }
  deriving stock (Show)

instance ToJSON GenerateOk where
  toJSON :: GenerateOk -> Value
  toJSON body =
    object
      [ "type" .= ("generate_ok" :: Text)
      , "in_reply_to" .= body.in_reply_to
      , "id" .= body.id
      ]

instance FromJSON GenerateOk where
  parseJSON :: Value -> Parser GenerateOk
  parseJSON =
    withObject "GenerateOk" \o -> do
      type_ :: Text <- o .: "type"
      guard (type_ == "generate_ok")
      msg_id <- o .: "msg_id"
      in_reply_to <- o .: "inReplyTo"
      id <- o .: "id"
      pure GenerateOk{ msg_id, in_reply_to, id }

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
