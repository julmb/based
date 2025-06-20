module Data.Based.Unfoldable (Unfoldable (..)) where

import Control.Applicative
import Control.Monad
import Control.Monad.Trans
import Control.Monad.Trans.Maybe
import Data.Functor
import Data.Functor.Identity
import Data.Proxy
import Data.List.NonEmpty (NonEmpty (..))

invert :: Alternative m => Monad m => m a -> m ()
invert = optional >=> maybe (pure ()) (const empty)

list :: Monad m => m (Maybe a) -> m [a]
list item = go where go = item >>= maybe (pure []) ((<&>) go . (:))

class Unfoldable f where unfold :: Monad m => m (Maybe a) -> m (Maybe (f a))

instance Unfoldable Proxy where unfold next = runMaybeT $ Proxy <$ invert (MaybeT next)
instance Unfoldable Identity where unfold next = runMaybeT $ Identity <$> MaybeT next <* invert (MaybeT next)
instance Unfoldable Maybe where unfold next = runMaybeT $ lift next <* invert (MaybeT next)
instance Unfoldable NonEmpty where unfold next = runMaybeT $ (:|) <$> MaybeT next <*> lift (list next)
instance Unfoldable [] where unfold next = runMaybeT $ lift $ list next
