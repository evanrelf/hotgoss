{-# LANGUAGE DuplicateRecordFields #-}

module HotGoss.Protocol
  ( Message (..)
  , Init (..)
  , InitOk (..)
  )
where

import Data.Aeson
import Data.Aeson.Types (Parser)
import Prelude hiding (init)

data Message a = Message
  { source :: Text
  , destination :: Text
  , body :: a
  }
  deriving stock (Show)

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

data Init = Init
  { type_ :: Text
  , messageId :: Maybe Int
  , nodeId :: Text
  , nodeIds :: [Text]
  }
  deriving stock (Show)

instance ToJSON Init where
  toJSON :: Init -> Value
  toJSON init =
    object
      [ "type" .= init.type_
      , "msg_id" .= init.messageId
      , "node_id" .= init.nodeId
      , "node_ids" .= init.nodeIds
      ]

instance FromJSON Init where
  parseJSON :: Value -> Parser Init
  parseJSON =
    withObject "Init" \o -> do
      type_ <- o .: "type"
      messageId <- o .: "msg_id"
      nodeId <- o .: "node_id"
      nodeIds <- o .: "node_ids"
      pure Init{ type_, messageId, nodeId, nodeIds }

data InitOk = InitOk
  { type_ :: Text
  , inReplyTo :: Int
  }
  deriving stock (Show)

instance ToJSON InitOk where
  toJSON :: InitOk -> Value
  toJSON initOk =
    object
      [ "type" .= initOk.type_
      , "in_reply_to" .= initOk.inReplyTo
      ]

instance FromJSON InitOk where
  parseJSON :: Value -> Parser InitOk
  parseJSON =
    withObject "InitOk" \o -> do
      type_ <- o .: "type"
      inReplyTo <- o .: "in_reply_to"
      pure InitOk{ type_, inReplyTo }
