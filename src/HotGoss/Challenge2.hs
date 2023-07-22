module HotGoss.Challenge2 (main) where

import Data.Aeson
import Data.Aeson.Types (Parser)
import HotGoss.Protocol
import Prelude hiding (id)

data Generate = Generate
  { messageId :: Word
  }
  deriving stock (Show)

instance ToJSON Generate where
  toJSON :: Generate -> Value
  toJSON body =
    object
      [ "type" .= ("generate" :: Text)
      , "msg_id" .= body.messageId
      ]

instance FromJSON Generate where
  parseJSON :: Value -> Parser Generate
  parseJSON =
    withObject "Generate" \o -> do
      type_ :: Text <- o .: "type"
      guard (type_ == "generate")
      messageId <- o .: "msg_id"
      pure Generate{ messageId }

data GenerateOk = GenerateOk
  { messageId :: Word
  , inReplyTo :: Word
  , id :: Text
  }
  deriving stock (Show)

instance ToJSON GenerateOk where
  toJSON :: GenerateOk -> Value
  toJSON body =
    object
      [ "type" .= ("generate_ok" :: Text)
      , "in_reply_to" .= body.inReplyTo
      , "id" .= body.id
      ]

instance FromJSON GenerateOk where
  parseJSON :: Value -> Parser GenerateOk
  parseJSON =
    withObject "GenerateOk" \o -> do
      type_ :: Text <- o .: "type"
      guard (type_ == "generate_ok")
      messageId <- o .: "msg_id"
      inReplyTo <- o .: "inReplyTo"
      id <- o .: "id"
      pure GenerateOk{ messageId, inReplyTo, id }

main :: IO ()
main = do
  getMessageId <- do
    ref <- newIORef 1
    pure $ atomicModifyIORef' ref \mid -> (mid + 1, mid)

  nodeId <- handleInit

  forever $ handle @Generate \generate -> do
    messageId <- getMessageId
    pure GenerateOk
      { messageId
      , inReplyTo = generate.messageId
      , id = nodeId <> "-" <> show messageId
      }
