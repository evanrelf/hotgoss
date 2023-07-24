{-# OPTIONS_GHC -Wno-redundant-constraints #-}

module HotGoss.Protocol2
  ( Message
  , MessageOrigin (..)
  , MessageBodyJson (..)
  , CustomJSON (..)
  , NodeId (..)
  , MessageId (..)
  , Omitted (Omitted)
  , Init (..)
  , InitOk (..)
  , Error (..)
  )
where

import Data.Aeson hiding (Result (Error))
import Data.Aeson.Types (Parser)
import Data.Data (Data)
import Data.SOP
import Data.SOP.NP
import Data.SOP.NS
import Data.Text.Display (Display)
import Deriving.Aeson
import GHC.Generics (Rep)
import HotGoss.ErrorCode (ErrorCode)
import Prelude hiding (All, Compose)
import Text.Show (Show (..))

import qualified Data.Aeson as Aeson (Result (..))
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
  deriving stock (Generic, Show, Functor)

deriving via CustomJSON '[FieldLabelModifier CamelToSnake] (Message Endo a)
  instance ToJSON a => ToJSON (Message Endo a)

deriving via CustomJSON '[FieldLabelModifier CamelToSnake] (Message Exo a)
  instance FromJSON a => FromJSON (Message Exo a)

data MessageOrigin
  = Endo
  | Exo

newtype MessageBodyJson a = MessageBodyJson
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
  ) => ToJSON (MessageBodyJson a) where
  toJSON :: MessageBodyJson a -> Value
  toJSON (MessageBodyJson x) =
    case toJSON x of
      Object keyMap ->
        Object $ KeyMap.insert "type" (messageType (Proxy @a)) keyMap
      other -> other

instance
  ( Generic a
  , Data a
  , GFromJSON Zero (Rep a)
  ) => FromJSON (MessageBodyJson a) where
  parseJSON :: Value -> Parser (MessageBodyJson a)
  parseJSON v = MessageBodyJson <$> do
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

data ProtocolException
  = FailedToInit
  | FailedToDecode String
  | UnexpectedMessage Value String
  deriving stock (Show)
  deriving anyclass (Exception)

sendJson :: (ToJSON a, MonadIO m) => a -> m ()
sendJson message = do
  let bytes = encode message <> "\n"
  liftIO $ LByteString.hPut stdout bytes
  hFlush stdout

receiveJson :: (FromJSON a, MonadIO m) => m (Either String a)
receiveJson = eitherDecode' . encodeUtf8 <$> getLine

sendTo :: NodeId -> a -> Message Endo a
sendTo dest body =
  Message
    { src = undefined
    , dest
    , body
    }

replyTo :: Message Exo req -> res -> Message Endo res
replyTo message body =
  Message
    { src = message.dest
    , dest = message.src
    , body
    }

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

data Handler s i where
  Handler
    :: All ToJSON xs
    => (s -> Message Exo i -> (s, NP (Message Endo) xs))
    -> Handler s i

data Handlers s xs where
  Handlers
    :: All (Compose FromJSON (Message Exo)) xs
    => NP (Handler s) xs
    -> Handlers s xs

node
  :: forall s xs m
   . MonadIO m
  => s
  -> Handlers s xs
  -> m ()
node state (Handlers handlers) = do
  getMessageId <- do
    ref <- newIORef 1
    pure $ atomicModifyIORef' ref \x -> (x + 1, MessageId x)

  (nodeId, nodeIds) <-
    receiveJson @(Message Exo Init) >>= \case
      Left err -> Exception.throwIO FailedToInit
      Right req -> do
        msgId <- getMessageId
        let res = replyTo req
              InitOk
                { msgId
                , inReplyTo = req.body.msgId
                }
        sendJson res
        pure (req.body.nodeId, req.body.nodeIds)

  state & fix \loop s -> do
    json <-
      receiveJson @Value >>= \case
        Left err -> Exception.throwIO $ FailedToDecode err
        Right json -> pure json

    case fromJSON @(NS (Message Exo) xs) json of
      Aeson.Error err -> Exception.throwIO $ UnexpectedMessage json err
      Success message -> do
        msgId <- getMessageId
        let s' :: s
            -- ress :: All ToJSON xs => NP (Message Endo) xs
            (s', ress) = undefined s
        undefined
        -- ctraverse__NP (Proxy @ToJSON) sendJson ress
        loop s'

step :: Handler s i -> s -> Message Exo i -> (s, [Message Endo Value])
step (Handler handler) state message =
  let
    (state', np) = handler state message
    values = collapse_NP $ cmap_NP (Proxy @ToJSON) (K . fmap toJSON) $ np
  in
    (state', values)


deriving newtype instance ToJSON a => ToJSON (I a)

deriving newtype instance FromJSON a => FromJSON (I a)

instance All (Compose FromJSON f) xs => FromJSON (NS f xs) where
  parseJSON v =
      unK
    $ traverse__NP (mapKK sequence'_NS)
    $ apInjs'_NP
    $ cpure_NP (Proxy @(Compose FromJSON f))
    $ Comp
    $ parseJSON v
