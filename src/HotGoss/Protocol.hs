module HotGoss.Protocol
  ( Message (..)
  , send
  , receive
  , handle
  , handleInit
  , log
  , Init (..)
  , InitOk (..)
  , Error (..)
  , ErrorCode (..)
  , toErrorCode
  , fromErrorCode
  )
where

import Data.Aeson hiding (Error)
import Data.Aeson.Types (Parser)
import Prelude hiding (error, init)

import qualified Data.ByteString.Lazy as LByteString
import qualified Data.Text.IO as Text
import qualified UnliftIO.Exception as Exception

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

send :: (ToJSON a, MonadIO m) => Message a -> m ()
send message = do
  let bytes = encode message <> "\n"
  liftIO $ LByteString.hPut stdout bytes
  hFlush stdout

receive :: (FromJSON a, MonadIO m) => m (Message a)
receive = do
  bytes <- encodeUtf8 <$> getLine
  either Exception.throwString pure $ eitherDecode' bytes

handle :: (FromJSON a, ToJSON b, MonadIO m) => (a -> m b) -> m ()
handle k = do
  message <- receive
  body <- k message.body
  send Message
    { source = message.destination
    , destination = message.source
    , body
    }

handleInit :: MonadIO m => m Text
handleInit = do
  message <- receive @Init
  send @InitOk Message
    { source = message.destination
    , destination = message.source
    , body =
        InitOk
          { inReplyTo = message.body.messageId
          }
    }
  pure message.destination

log :: MonadIO m => Text -> m ()
log message = do
  liftIO $ Text.hPutStrLn stderr message
  hFlush stdout

data Init = Init
  { messageId :: Word
  , nodeId :: Text
  , nodeIds :: [Text]
  }
  deriving stock (Show)

instance ToJSON Init where
  toJSON :: Init -> Value
  toJSON body =
    object
      [ "type" .= ("init" :: Text)
      , "msg_id" .= body.messageId
      , "node_id" .= body.nodeId
      , "node_ids" .= body.nodeIds
      ]

instance FromJSON Init where
  parseJSON :: Value -> Parser Init
  parseJSON =
    withObject "Init" \o -> do
      type_ :: Text <- o .: "type"
      guard (type_ == "init")
      messageId <- o .: "msg_id"
      nodeId <- o .: "node_id"
      nodeIds <- o .: "node_ids"
      pure Init{ messageId, nodeId, nodeIds }

data InitOk = InitOk
  { inReplyTo :: Word
  }
  deriving stock (Show)

instance ToJSON InitOk where
  toJSON :: InitOk -> Value
  toJSON body =
    object
      [ "type" .= ("init_ok" :: Text)
      , "in_reply_to" .= body.inReplyTo
      ]

instance FromJSON InitOk where
  parseJSON :: Value -> Parser InitOk
  parseJSON =
    withObject "InitOk" \o -> do
      type_ :: Text <- o .: "type"
      guard (type_ == "init_ok")
      inReplyTo <- o .: "in_reply_to"
      pure InitOk{ inReplyTo }

data Error = Error
  { inReplyTo :: Word
  , code :: ErrorCode
  , text :: Maybe Text
    -- TODO: Can include other arbitrary fields
  }
  deriving stock (Show)

instance ToJSON Error where
  toJSON :: Error -> Value
  toJSON body =
    object
      [ "type" .= ("error" :: Text)
      , "in_reply_to" .= body.inReplyTo
      , "code" .= fromErrorCode body.code
        -- TODO: Omit `text` when `Nothing`
      , "text" .= body.text
      ]

instance FromJSON Error where
  parseJSON :: Value -> Parser Error
  parseJSON =
    withObject "Error" \o -> do
      type_ :: Text <- o .: "type"
      guard (type_ == "error")
      inReplyTo <- o .: "in_reply_to"
      code <- toErrorCode <$> o .: "code"
      text <- o .:? "text"
      pure Error{ inReplyTo, code, text }

data ErrorCode
  = Timeout
  | NodeNotFound
  | NotSupported
  | TemporarilyUnavailable
  | MalformedRequest
  | Crash
  | Abort
  | KeyDoesNotExist
  | KeyAlreadyExists
  | PreconditionFailed
  | TransactionConflict
  | Unknown Word
  deriving stock (Show)

toErrorCode :: Word -> ErrorCode
toErrorCode = \case
  0 -> Timeout
  1 -> NodeNotFound
  10 -> NotSupported
  11 -> TemporarilyUnavailable
  12 -> MalformedRequest
  13 -> Crash
  14 -> Abort
  20 -> KeyDoesNotExist
  21 -> KeyAlreadyExists
  22 -> PreconditionFailed
  30 -> TransactionConflict
  n -> Unknown n

fromErrorCode :: ErrorCode -> Word
fromErrorCode = \case
  Timeout -> 0
  NodeNotFound -> 1
  NotSupported -> 10
  TemporarilyUnavailable -> 11
  MalformedRequest -> 12
  Crash -> 13
  Abort -> 14
  KeyDoesNotExist -> 20
  KeyAlreadyExists -> 21
  PreconditionFailed -> 22
  TransactionConflict -> 30
  Unknown n -> n
