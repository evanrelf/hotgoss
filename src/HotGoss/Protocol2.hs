{-# LANGUAGE TypeFamilies #-}

module HotGoss.Protocol2
  ( Message
  )
where

import Data.Record.Anon as R
import Data.Record.Anon.Simple as R
import HotGoss.ErrorCode (ErrorCode)
import Prelude hiding (Compose)

import qualified Data.Aeson as Aeson
import qualified Data.ByteString.Lazy as LByteString
import qualified UnliftIO.Exception as Exception

type Message r = Record
  [ "src" := Text
  , "dest" := Text
  , "body" := MessageBody r
  ]

type MessageBody r = Record (Merge
  [ "type" := Text
  , "msg_id" := Maybe Word
  , "in_reply_to" := Maybe Word
  ] r)

type family GetRow record where
  GetRow (Record row) = row

type c ? r =
  ( KnownFields (GetRow (Message r))
  , AllFields (GetRow (Message r)) c
  )

send :: (Aeson.ToJSON a, MonadIO m) => a -> m ()
send message = do
  let bytes = Aeson.encode message <> "\n"
  liftIO $ LByteString.hPut stdout bytes
  hFlush stdout

receive :: (HasCallStack, Aeson.FromJSON a, MonadIO m) => m a
receive = do
  bytes <- encodeUtf8 <$> getLine
  either Exception.throwString pure $ Aeson.eitherDecode' bytes

handle
  :: forall req res m
   . (HasCallStack, Aeson.FromJSON ? req, Aeson.ToJSON ? res, MonadIO m)
  => (MessageBody req -> m (MessageBody res))
  -> m ()
handle k = do
  req <- receive @(Message req)
  body <- k req.body
  let res =
          insert #src req.dest
        $ insert #dest req.src
        $ insert #body body
        $ R.empty
  send @(Message res) res

handleInit :: (HasCallStack, MonadIO m) => m Text
handleInit = do
  req <- receive @Init
  let res =
          insert #src req.dest
        $ insert #dest req.src
        $ insert #body (
              insert #type "init_ok"
            $ insert #msg_id 0
            $ insert #in_reply_to req.body.msg_id
            $ R.empty
          )
        $ R.empty
  send @InitOk res
  pure req.dest

type Init = Message
  [ "node_id" := Text
  , "node_ids" := [Text]
  ]

type InitOk = Message '[]

type Error = Message
  [ "code" := ErrorCode
  , "text" := Maybe Text
  ]
