module HotGoss.Protocol
  ( Message (..)
  , MessageBody (..)
  )
where

import Data.Aeson
import Data.Aeson.Types (Parser)

data Message a = Message
  { source :: Text
  , destination :: Text
  , body :: MessageBody a
  }

instance ToJSON a => ToJSON (Message a) where
  toJSON :: Message a -> Value
  toJSON message =
    object
      [ "src" .= message.source
      , "dest" .= message.destination
      , "body" .= message.body
      ]

instance FromJSON a => FromJSON (Message a) where
  parseJSON :: Value -> Parser (Message a)
  parseJSON =
    withObject "Message" \o -> do
      source <- o .: "src"
      destination <- o .: "dest"
      body <- o .: "body"
      pure Message{ source, destination, body }

data MessageBody a = MessageBody
  { kind :: Text
  , messageId :: Maybe Int
  , inReplyTo :: Maybe Int
  , fields :: a
  }

instance ToJSON a => ToJSON (MessageBody a) where
  toJSON :: MessageBody a -> Value
  toJSON messageBody =
    object
      [ "type" .= messageBody.kind
      , "msg_id" .= messageBody.messageId
      , "in_reply_to" .= messageBody.inReplyTo
      ]
    `unsafeUnion`
    toJSON messageBody.fields
    where
    unsafeUnion :: Value -> Value -> Value
    unsafeUnion (Object l) (Object r) = Object (l <> r)
    unsafeUnion _ _ = error "unsafeUnion given non-object(s)"

instance FromJSON a => FromJSON (MessageBody a) where
  parseJSON :: Value -> Parser (MessageBody a)
  parseJSON =
    withObject "MessageBody" \o -> do
      kind <- o .: "type"
      messageId <- o .:? "msg_id"
      inReplyTo <- o .:? "in_reply_to"
      fields <- parseJSON (Object o)
      pure MessageBody{ kind, messageId, inReplyTo, fields }
