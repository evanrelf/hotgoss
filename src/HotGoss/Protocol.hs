{-# OPTIONS_GHC -Wno-redundant-constraints #-}

module HotGoss.Protocol
  ( Message (..)
  , MessageOrigin (..)
  , IsMessage
  , MessageJSON (..)
  , CustomJSON (..)
  , NodeId (..)
  , MessageId (..)
  , Omitted (Omitted)
  , log
  , send
  , receive
  , handle
  , handle_
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
import Text.Show (Show (..))

import qualified Data.Aeson.KeyMap as KeyMap
import qualified Data.ByteString.Lazy as LByteString
import qualified Data.Data as Data
import qualified Data.Text.IO as Text
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

newtype MessageJSON a = MessageJSON
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

newtype NodeId = NodeId Text
  deriving stock (Data, Show, Eq)
  deriving newtype (Display, ToJSON, ToJSONKey, FromJSON, FromJSONKey, Hashable)

newtype MessageId = MessageId Word
  deriving stock (Data, Show)
  deriving newtype (Display, ToJSON, FromJSON)

-- Workaround until we can use `omit{,ted}Field` from `aeson` 2.2.0.0
newtype Omitted = MkOmitted (Maybe Void)
  deriving stock (Data)
  deriving newtype (ToJSON, FromJSON)

pattern Omitted :: Omitted
pattern Omitted = MkOmitted Nothing

{-# COMPLETE Omitted #-}

instance Show Omitted where
  show _ = "Omitted"

log :: MonadIO m => Text -> m ()
log message = do
  liftIO $ Text.hPutStrLn stderr message
  hFlush stdout

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
