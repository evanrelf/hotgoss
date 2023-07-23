module HotGoss.ErrorCode
  ( ErrorCode (..)
  , toErrorCode
  , fromErrorCode
  )
where

import qualified Data.Aeson as Aeson
import qualified Data.Aeson.Types as Aeson (Parser)

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

instance Aeson.ToJSON ErrorCode where
  toJSON :: ErrorCode -> Aeson.Value
  toJSON = Aeson.toJSON . fromErrorCode

instance Aeson.FromJSON ErrorCode where
  parseJSON :: Aeson.Value -> Aeson.Parser ErrorCode
  parseJSON = fmap toErrorCode . Aeson.parseJSON

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
