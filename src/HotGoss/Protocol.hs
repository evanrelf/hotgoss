{-# LANGUAGE UndecidableInstances #-}

module HotGoss.Protocol
  ( Message (..)
  , MessageJSON (..)
  , CustomJSON (..)
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
import Deriving.Aeson
import GHC.Generics (Rep)
import GHC.TypeLits (KnownSymbol, symbolVal)
import Prelude hiding (error, init)

import qualified Data.Aeson.KeyMap as KeyMap
import qualified Data.ByteString.Lazy as LByteString
import qualified Data.Text.IO as Text
import qualified UnliftIO.Exception as Exception

data Message a = Message
  { src :: Text
  , dest :: Text
  , body :: a
  }
  deriving stock (Generic, Show)
  deriving (ToJSON, FromJSON) via CustomJSON '[] (Message a)

newtype MessageJSON t os a = MkMessageJSON (CustomJSON os a)

instance
  ( KnownSymbol t
  , AesonOptions os
  , Generic a
  , GToJSON Zero (Rep a)
  , GToEncoding Zero (Rep a)
  ) => ToJSON (MessageJSON t os a) where
  toJSON :: MessageJSON t os a -> Value
  toJSON (MkMessageJSON x) =
    case toJSON x of
      Object keyMap -> do
        let type_ = fromString (symbolVal (Proxy @t))
        Object $ KeyMap.insert "type" type_ keyMap
      other -> other

instance
  ( KnownSymbol t
  , AesonOptions os
  , Generic a
  , GFromJSON Zero (Rep a)
  ) => FromJSON (MessageJSON t os a) where
  parseJSON :: Value -> Parser (MessageJSON t os a)
  parseJSON v = MkMessageJSON <$> do
    let expected = symbolVal (Proxy @t)

    v & withObject expected \o -> do
      actual <- o .: "type"
      when (expected /= actual) do
        fail $ "Expected `" <> expected <> "`, got `" <> actual <> "`"

    parseJSON v

send :: (ToJSON a, MonadIO m) => Message a -> m ()
send message = do
  let bytes = encode message <> "\n"
  liftIO $ LByteString.hPut stdout bytes
  hFlush stdout

receive
  :: (HasCallStack, FromJSON a, MonadIO m)
  => m (Message a)
receive = do
  bytes <- encodeUtf8 <$> getLine
  either Exception.throwString pure $ eitherDecode' bytes

handle
  :: (HasCallStack, FromJSON a, ToJSON b, MonadIO m)
  => (a -> m b)
  -> m ()
handle k = do
  message <- receive
  body <- k message.body
  send Message
    { src = message.dest
    , dest = message.src
    , body
    }

handleInit :: (HasCallStack, MonadIO m) => m Text
handleInit = do
  message <- receive @Init
  send @InitOk Message
    { src = message.dest
    , dest = message.src
    , body =
        InitOk
          { in_reply_to = message.body.msg_id
          }
    }
  pure message.dest

log :: MonadIO m => Text -> m ()
log message = do
  liftIO $ Text.hPutStrLn stderr message
  hFlush stdout

data Init = Init
  { msg_id :: Word
  , node_id :: Text
  , node_ids :: [Text]
  }
  deriving stock (Generic, Show)
  deriving (ToJSON, FromJSON) via MessageJSON "init" '[] Init

data InitOk = InitOk
  { in_reply_to :: Word
  }
  deriving stock (Generic, Show)
  deriving (ToJSON, FromJSON) via MessageJSON "init_ok" '[] InitOk

data Error = Error
  { in_reply_to :: Word
  , code :: ErrorCode
  , text :: Maybe Text
    -- TODO: Can include other arbitrary fields
  }
  deriving stock (Generic, Show)
  deriving (ToJSON, FromJSON) via MessageJSON "error" '[] Error

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

instance ToJSON ErrorCode where
  toJSON :: ErrorCode -> Value
  toJSON = toJSON . fromErrorCode

instance FromJSON ErrorCode where
  parseJSON :: Value -> Parser ErrorCode
  parseJSON = fmap toErrorCode . parseJSON

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
