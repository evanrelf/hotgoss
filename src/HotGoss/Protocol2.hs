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
import Prelude hiding (All, Compose, state)
import Text.Show (Show (..))

import qualified Data.Aeson.KeyMap as KeyMap
import qualified Data.ByteString.Lazy as LByteString
import qualified Data.Data as Data
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
  = FailedToDecode String
  deriving stock (Show)
  deriving anyclass (Exception)

readJson :: (FromJSON a, MonadIO m) => m (Either String a)
readJson = eitherDecode' . encodeUtf8 <$> getLine

writeJson :: (ToJSON a, MonadIO m) => a -> m ()
writeJson message = do
  let bytes = encode message <> "\n"
  liftIO $ LByteString.hPut stdout bytes
  hFlush stdout

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
    :: All ToJSON os
    => (s -> Message Exo i -> (s, NP (Message Endo) os))
    -> Handler s i

node
  :: forall s is m
   . (All (Compose FromJSON (Message Exo)) is, MonadIO m)
  => s
  -> NP (Handler s) is
  -> m ()
node initialState handlers = do
  let receive :: FromJSON a => m a
      receive = do
        readJson >>= \case
          Left err -> Exception.throwIO $ FailedToDecode err
          Right req -> pure req

  getMessageId <- do
    ref <- newIORef 1
    pure $ atomicModifyIORef' ref \x -> (x + 1, MessageId x)

  -- Init
  (nodeId, nodeIds) <- do
    req <- receive @(Message Exo Init)
    msgId <- getMessageId
    let res = replyTo req
          InitOk
            { msgId
            , inReplyTo = req.body.msgId
            }
    writeJson res
    pure (req.body.nodeId, req.body.nodeIds)

  -- Main loop
  initialState & fix \loop state -> do
    req <- receive @(NS (Message Exo) is)
    msgId <- getMessageId
    let (state', ress) = runHandlers handlers state req
    traverse_ writeJson ress
    loop state'

runHandlers
  :: forall s is
   . SListI is
  => NP (Handler s) is
  -> s
  -> NS (Message Exo) is
  -> (s, [Message Endo Value])
runHandlers handlers state message = collapse_NS $ ap_NS handlerFns message
  where
  handlerFns :: NP (Message Exo -.-> K (s, [Message Endo Value])) is
  handlerFns =
    map_NP
      (\handler -> Fn \message -> K $ runHandler handler state message)
      handlers

  runHandler :: Handler s i -> s -> Message Exo i -> (s, [Message Endo Value])
  runHandler (Handler handler) state message =
    let
      (state', np) = handler state message
      values = collapse_NP $ cmap_NP (Proxy @ToJSON) (K . fmap toJSON) np
    in
      (state', values)

deriving newtype instance ToJSON a => ToJSON (I a)

deriving newtype instance FromJSON a => FromJSON (I a)

instance All (Compose FromJSON f) xs => FromJSON (NS f xs) where
  parseJSON :: Value -> Parser (NS f xs)
  parseJSON v =
      unK
    $ traverse__NP (mapKK sequence'_NS)
    $ apInjs'_NP
    $ cpure_NP (Proxy @(Compose FromJSON f))
    $ Comp
    $ parseJSON v
