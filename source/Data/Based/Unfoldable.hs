module Data.Based.Unfoldable (Unfoldable (..)) where

import Data.Functor.Identity
import Control.Applicative
import Control.Monad.Loops
import Control.Monad.Trans
import Control.Monad.Trans.Maybe
import Data.Proxy
import Data.List.NonEmpty (NonEmpty (..))

invert :: Alternative f => Monad f => f a -> f ()
invert a = optional a >>= maybe (pure mempty) (pure empty)

class Unfoldable f where unfold :: Monad m => m (Maybe a) -> m (Maybe (f a))

instance Unfoldable Proxy where unfold next = runMaybeT $ Proxy <$ invert (MaybeT next)
instance Unfoldable Identity where unfold next = runMaybeT $ Identity <$> MaybeT next <* invert (MaybeT next)
instance Unfoldable Maybe where unfold next = runMaybeT $ lift next <* invert (MaybeT next)
instance Unfoldable NonEmpty where unfold next = runMaybeT $ (:|) <$> MaybeT next <*> lift (unfoldM next)
instance Unfoldable [] where unfold next = runMaybeT $ lift $ unfoldM next
