module HotGoss.Protocol
  ( Message (..)
  )
where

import Data.Aeson
import Data.Aeson.Types (Parser)

data Message a = Message
  { source :: Text
  , destination :: Text
  , body :: a
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
