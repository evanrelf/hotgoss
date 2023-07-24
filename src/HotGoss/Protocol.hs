{-# OPTIONS_GHC -Wno-redundant-constraints #-}

module HotGoss.Protocol
  ( NodeId (..)
  , MessageId (..)
  , Message (..)
  , MessageJSON (..)
  , CustomJSON (..)
  , Omitted
  , pattern Omitted
  , send
  , receive
  , handle
  , handle_
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
import Data.Text.Display (Display)
import Deriving.Aeson
import GHC.Generics (Rep)
import GHC.Records (HasField (..))
import HotGoss.ErrorCode (ErrorCode)

import qualified Data.Aeson.KeyMap as KeyMap
import qualified Data.ByteString.Lazy as LByteString
import qualified Data.Data as Data
import qualified Data.Text.IO as Text
import qualified UnliftIO.Exception as Exception

newtype NodeId = NodeId Text
  deriving stock (Generic, Data, Show, Eq)
  deriving newtype (Display, ToJSON, ToJSONKey, FromJSON, FromJSONKey, Hashable)

newtype MessageId = MessageId Word
  deriving stock (Generic, Data, Show)
  deriving newtype (Display, ToJSON, FromJSON)

data Message a = Message
  { src :: NodeId
  , dest :: NodeId
  , body :: a
  }
  deriving stock (Generic, Show)
  deriving (ToJSON, FromJSON) via
    CustomJSON '[FieldLabelModifier CamelToSnake] (Message a)

newtype MessageJSON a = MessageJSON
  (CustomJSON '[FieldLabelModifier CamelToSnake, OmitNothingFields] a)

type Omitted = Maybe Void

pattern Omitted :: Maybe Void
pattern Omitted = Nothing

messageType :: forall a s. (Data a, IsString s) => Proxy a -> s
messageType _ =
  Data.dataTypeOf @a (error "unreachable")
  & Data.dataTypeName
  & Data.tyconUQname
  & getStringModifier @CamelToSnake
  & fromString

type IsMessage :: Type -> Constraint
type family IsMessage a where
  IsMessage (Message a) = IsMessage a
  IsMessage a = (HasSomeField "msgId" a, HasSomeField "inReplyTo" a)

class HasSomeField x r
instance HasField x r a => HasSomeField x r

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

send :: (IsMessage a, ToJSON a, MonadIO m) => Message a -> m ()
send message = do
  let bytes = encode message <> "\n"
  liftIO $ LByteString.hPut stdout bytes
  hFlush stdout

receive
  :: (HasCallStack, IsMessage a, FromJSON a, MonadIO m)
  => m (Message a)
receive = do
  bytes <- encodeUtf8 <$> getLine
  either Exception.throwString pure $ eitherDecode' bytes

handle
  :: ( HasCallStack
     , IsMessage req
     , IsMessage res
     , FromJSON req
     , ToJSON res
     , MonadIO m
     )
  => (req -> m (res, a))
  -> m a
handle k = do
  req <- receive
  (body, x) <- k req.body
  send Message
    { src = req.dest
    , dest = req.src
    , body
    }
  pure x

handle_
  :: ( HasCallStack
     , IsMessage req
     , IsMessage res
     , FromJSON req
     , ToJSON res
     , MonadIO m
     )
  => (req -> m res)
  -> m ()
handle_ k = handle (fmap (, ()) . k)

handleInit :: (HasCallStack, MonadIO m) => m (m MessageId, NodeId, [NodeId])
handleInit = do
  getMessageId <- do
    ref <- newIORef 1
    pure $ atomicModifyIORef' ref \x -> (x + 1, MessageId x)
  msgId <- getMessageId
  req <- receive @Init
  send @InitOk Message
    { src = req.dest
    , dest = req.src
    , body =
        InitOk
          { msgId
          , inReplyTo = req.body.msgId
          }
    }
  pure (getMessageId, req.body.nodeId, req.body.nodeIds)

log :: MonadIO m => Text -> m ()
log message = do
  liftIO $ Text.hPutStrLn stderr message
  hFlush stdout

data Init = Init
  { msgId :: MessageId
  , inReplyTo :: Omitted
  , nodeId :: NodeId
  , nodeIds :: [NodeId]
  }
  deriving stock (Generic, Data, Show)
  deriving (ToJSON, FromJSON) via MessageJSON Init

data InitOk = InitOk
  { msgId :: MessageId
  , inReplyTo :: MessageId
  }
  deriving stock (Generic, Data, Show)
  deriving (ToJSON, FromJSON) via MessageJSON InitOk

data Error = Error
  { inReplyTo :: MessageId
  , code :: ErrorCode
  , text :: Maybe Text
    -- TODO: Can include other arbitrary fields
  }
  deriving stock (Generic, Data, Show)
  deriving (ToJSON, FromJSON) via MessageJSON Error
