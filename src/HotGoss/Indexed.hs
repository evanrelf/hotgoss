module HotGoss.Indexed
  ( IxFunctor (..)
  , IxApplicative (..)
  , IxMonad (..)
  , Ix (..)
  , liftIx
  , unsafeLiftIx

    -- * @-XQualifiedDo@
  , fmap
  , (<*)
  , (*>)
  , (<*>)
  , (>>)
  , (>>=)
  , join
  , pure
  , return
  , fail
  )
where

import Prelude hiding (fmap, (<*), (*>), (<*>), (>>), (>>=), join, pure, return, fail)

class IxFunctor f where
  imap :: (a -> b) -> f i j a -> f i j b

class IxApplicative f where
  ipure :: a -> f i i a
  iap :: f i j (a -> b) -> f j k a -> f i k b

class IxMonad m where
  ibind :: m i j a -> (a -> m j k b) -> m i k b

newtype Ix m i j a = Ix (m a)

instance Functor f => IxFunctor (Ix f) where
  imap :: (a -> b) -> Ix f i j a -> Ix f i j b
  imap = undefined

instance Applicative f => IxApplicative (Ix f) where
  ipure :: a -> Ix f i i a
  ipure = undefined

  iap :: Ix f i j (a -> b) -> Ix f j k a -> Ix f i k b
  iap = undefined

instance Monad m => IxMonad (Ix m) where
  ibind :: Ix m i j a -> (a -> Ix m j k b) -> Ix m i k b
  ibind = undefined

liftIx :: m a -> Ix m i i a
liftIx = coerce

unsafeLiftIx :: m a -> Ix m i j a
unsafeLiftIx = coerce

-- `-XQualifiedDo` -------------------------------------------------------------

fmap :: IxFunctor f => (a -> b) -> f i j a -> f i j b
fmap = imap

(<*) :: ()
(<*) = undefined

(*>) :: ()
(*>) = undefined

(<*>) :: IxApplicative f => f i j (a -> b) -> f j k a -> f i k b
(<*>) = iap

(>>) :: ()
(>>) = (*>)

(>>=) :: IxMonad m => m i j a -> (a -> m j k b) -> m i k b
(>>=) = ibind

join :: ()
join = undefined

pure :: IxApplicative f => a -> f i i a
pure = ipure

return :: IxApplicative f => a -> f i i a
return = pure

fail :: ()
fail = undefined
