module HotGoss.Union
  ( Union (..)
  , Member (..)
  , Members
  , decompose
  , weaken
  , extract
  )
where

import Data.Aeson
import Data.Aeson.Types (Parser)
import GHC.Records (HasField (..))

data Union (r :: [Type]) where
  Zero :: a -> Union (a : r)
  Succ :: Union r -> Union (any : r)

deriving instance Show (Union '[])

deriving instance (Show a, Show (Union r)) => Show (Union (a : r))

class Member a r where
  inject :: a -> Union r
  project :: Union r -> Maybe a

instance Member a (a : r) where
  inject :: a -> Union (a : r)
  inject = Zero

  project :: Union (a : r) -> Maybe a
  project = \case
    Zero x -> Just x
    Succ _ -> Nothing

instance {-# OVERLAPPABLE #-} Member a r => Member a (any : r) where
  inject :: a -> Union (any : r)
  inject = Succ . inject

  project :: Union (any : r) -> Maybe a
  project = \case
    Zero _ -> Nothing
    Succ u -> project u

type family Members as r :: Constraint where
  Members '[] r = ()
  Members (a : as) r = (Member a r, Members as r)

decompose :: Union (a : r) -> Either (Union r) a
decompose = \case
  Zero a -> Right a
  Succ u -> Left u

weaken :: Union r -> Union (any : r)
weaken = Succ

extract :: Union '[a] -> a
extract = \case
  Zero a -> a
  Succ u -> case u of

instance {-# OVERLAPPABLE #-} (HasField x r a, HasField x (Union rs) a)
  => HasField x (Union (r : rs)) a where
  getField = either (getField @x) (getField @x) . decompose

instance HasField x r a => HasField x (Union '[r]) a where
  getField = getField @x . extract

instance ToJSON a => ToJSON (Union '[a]) where
  toJSON :: Union '[a] -> Value
  toJSON = toJSON . extract

instance {-# OVERLAPPABLE #-} (ToJSON a, ToJSON (Union r))
  => ToJSON (Union (a : r)) where
  toJSON :: Union (a : r) -> Value
  toJSON = either toJSON toJSON . decompose

instance FromJSON a => FromJSON (Union '[a]) where
  parseJSON :: Value -> Parser (Union '[a])
  parseJSON v = Zero <$> parseJSON v

instance {-# OVERLAPPABLE #-} (FromJSON a, FromJSON (Union r))
  => FromJSON (Union (a : r)) where
  parseJSON :: Value -> Parser (Union (a : r))
  parseJSON v = Zero <$> parseJSON v <|> Succ <$> parseJSON v
