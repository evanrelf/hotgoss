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
  )
where

import Data.Aeson hiding (Error)
import Data.Aeson.Types (Parser)
import Data.Data (Data)
import Deriving.Aeson (CamelToSnake, CustomJSON (..), StringModifier (..))
import Deriving.Aeson.Stock (Snake)
import GHC.Generics (Rep)
import HotGoss.ErrorCode (ErrorCode)

import qualified Data.Aeson.KeyMap as KeyMap
import qualified Data.ByteString.Lazy as LByteString
import qualified Data.Data as Data
import qualified Data.Text.IO as Text
import qualified UnliftIO.Exception as Exception

data Message a = Message
  { src :: Text
  , dest :: Text
  , body :: a
  }
  deriving stock (Generic, Show)
  deriving (ToJSON, FromJSON) via Snake (Message a)

newtype MessageJSON a = MessageJSON (Snake a)

messageType :: forall a s. (Data a, IsString s) => Proxy a -> s
messageType _ =
  Data.dataTypeOf @a (error "unreachable")
  & Data.dataTypeName
  & Data.tyconUQname
  & getStringModifier @CamelToSnake
  & fromString

instance
  ( Generic a
  , Data a
  , GToJSON Zero (Rep a)
  , GToEncoding Zero (Rep a)
  ) => ToJSON (MessageJSON a) where
  toJSON :: MessageJSON a -> Value
  toJSON (MessageJSON x) =
    case toJSON x of
      Object keyMap ->
        Object $ KeyMap.insert "type" (messageType (Proxy @a)) keyMap
      other -> other

instance
  ( Generic a
  , Data a
  , GFromJSON Zero (Rep a)
  ) => FromJSON (MessageJSON a) where
  parseJSON :: Value -> Parser (MessageJSON a)
  parseJSON v = MessageJSON <$> do
    let expected = messageType (Proxy @a)

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
          { inReplyTo = message.body.msgId
          }
    }
  pure message.dest

log :: MonadIO m => Text -> m ()
log message = do
  liftIO $ Text.hPutStrLn stderr message
  hFlush stdout

data Init = Init
  { msgId :: Word
  , nodeId :: Text
  , nodeIds :: [Text]
  }
  deriving stock (Generic, Data, Show)
  deriving (ToJSON, FromJSON) via MessageJSON Init

data InitOk = InitOk
  { inReplyTo :: Word
  }
  deriving stock (Generic, Data, Show)
  deriving (ToJSON, FromJSON) via MessageJSON InitOk

data Error = Error
  { inReplyTo :: Word
  , code :: ErrorCode
  , text :: Maybe Text
    -- TODO: Can include other arbitrary fields
  }
  deriving stock (Generic, Data, Show)
  deriving (ToJSON, FromJSON) via MessageJSON Error
