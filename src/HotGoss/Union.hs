{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}

module HotGoss.Union
  ( Union (..)
  , Member (..)
  , Members
  , decompose
  , weaken
  , extract
  )
where

data Union (r :: [Type]) where
  This :: a -> Union (a : r)
  Next :: Union r -> Union (any : r)

class Member a r where
  inject :: a -> Union r
  project :: Union r -> Maybe a

instance Member a (a : r) where
  inject :: a -> Union (a : r)
  inject = This

  project :: Union (a : r) -> Maybe a
  project = \case
    This x -> Just x
    Next _ -> Nothing

instance {-# OVERLAPPABLE #-} Member a r => Member a (any : r) where
  inject :: a -> Union (any : r)
  inject = Next . inject

  project :: Union (any : r) -> Maybe a
  project = \case
    This _ -> Nothing
    Next u -> project u

type family Members as r :: Constraint where
  Members '[] r = ()
  Members (a : as) r = (Member a r, Members as r)

decompose :: Union (a : r) -> Either (Union r) a
decompose = \case
  This a -> Right a
  Next u -> Left u

weaken :: Union r -> Union (any : r)
weaken = Next

extract :: Union '[a] -> a
extract = \case
  This a -> a
  Next u -> case u of
