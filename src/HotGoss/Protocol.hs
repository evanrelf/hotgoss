{-# OPTIONS_GHC -Wno-redundant-constraints #-}

module HotGoss.Protocol
  ( Message
  , MessageOrigin (..)
  , MessageBodyJson (..)
  , NodeId (..)
  , MessageId (..)
  , Omitted (Omitted)
  , handle
  , handleInit
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
import qualified UnliftIO.Exception as Exception

data Message o a = Message
  { src :: NodeId
  , dest :: NodeId
  , body :: a
  }
  deriving stock (Generic, Show)

deriving via CustomJSON '[FieldLabelModifier CamelToSnake] (Message Endo a)
  instance ToJSON a => ToJSON (Message Endo a)

deriving via CustomJSON '[FieldLabelModifier CamelToSnake] (Message Exo a)
  instance FromJSON a => FromJSON (Message Exo a)

data MessageOrigin
  = Endo
  | Exo

class HasSomeField x r
instance HasField x r a => HasSomeField x r

class IsMessage a
instance IsMessage a => IsMessage (Message a)
instance (HasSomeField "msgId" a, HasSomeField "inReplyTo" a) => IsMessage a

newtype MessageBodyJson a = MessageBodyJson a
  deriving stock (Generic, Data, Show)
  deriving (ToJSON, FromJSON) via MessageBodyJsonInner (MessageBodyJson a)

newtype MessageBodyJsonInner a = MessageBodyJsonInner
  (CustomJSON '[FieldLabelModifier CamelToSnake, OmitNothingFields] a)

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
  ) => ToJSON (MessageBodyJsonInner a) where
  toJSON :: MessageBodyJsonInner a -> Value
  toJSON (MessageBodyJsonInner x) =
    case toJSON x of
      Object keyMap ->
        Object $ KeyMap.insert "type" (messageType (Proxy @a)) keyMap
      other -> other

instance
  ( Generic a
  , Data a
  , GFromJSON Zero (Rep a)
  ) => FromJSON (MessageBodyJsonInner a) where
  parseJSON :: Value -> Parser (MessageBodyJsonInner a)
  parseJSON v = MessageBodyJsonInner <$> do
    let expected = messageType (Proxy @a)

    v & withObject expected \o -> do
      actual <- o .: "type"
      when (expected /= actual) do
        fail $ "Expected `" <> expected <> "`, got `" <> actual <> "`"

    parseJSON v

newtype NodeId = NodeId Text
  deriving stock (Data, Show, Eq)
  deriving newtype (Display, ToJSON, ToJSONKey, FromJSON, FromJSONKey, Hashable)

newtype MessageId = MessageId Word
  deriving stock (Data, Show)
  deriving newtype (Display, ToJSON, FromJSON)

data Omitted = Omitted
  deriving stock (Data, Show)

instance ToJSON Omitted where
  toJSON _ = Null

instance FromJSON Omitted where
  parseJSON v = maybe Omitted absurd <$> parseJSON v

send :: (IsMessage a, ToJSON a, MonadIO m) => Message Endo a -> m ()
send message = do
  let bytes = encode message <> "\n"
  liftIO $ LByteString.hPut stdout bytes
  hFlush stdout

receive
  :: (HasCallStack, IsMessage a, FromJSON a, MonadIO m)
  => m (Message Exo a)
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
  => (req -> m res)
  -> m ()
handle k = do
  req <- receive
  body <- k req.body
  send Message
    { src = req.dest
    , dest = req.src
    , body
    }

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

data Init = Init
  { msgId :: MessageId
  , inReplyTo :: Omitted
  , nodeId :: NodeId
  , nodeIds :: [NodeId]
  }
  deriving stock (Generic, Data, Show)
  deriving (ToJSON, FromJSON) via MessageBodyJson Init

data InitOk = InitOk
  { msgId :: MessageId
  , inReplyTo :: MessageId
  }
  deriving stock (Generic, Data, Show)
  deriving (ToJSON, FromJSON) via MessageBodyJson InitOk

data Error = Error
  { inReplyTo :: MessageId
  , code :: ErrorCode
  , text :: Maybe Text
    -- TODO: Can include other arbitrary fields
  }
  deriving stock (Generic, Data, Show)
  deriving (ToJSON, FromJSON) via MessageBodyJson Error
