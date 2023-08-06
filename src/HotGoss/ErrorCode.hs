module HotGoss.ErrorCode
  ( ErrorCode (..)
  , toErrorCode
  , fromErrorCode
  )
where

import Data.Aeson qualified as Aeson
import Data.Aeson.Types qualified as Aeson (Parser)
import Data.Data (Data)

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
  | UnknownMaelstrom Word
  | User Word
  deriving stock (Data, Show)

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
  n | n < 1000 -> UnknownMaelstrom n
    | otherwise -> User n

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
  UnknownMaelstrom n -> n
  User n -> n
