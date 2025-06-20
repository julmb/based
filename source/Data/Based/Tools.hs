module Data.Based.Tools (fmap2, fmap3, invert, single) where

import Control.Applicative
import Data.Foldable
import Text.Printf

fmap2 :: Functor f => Functor g => (a -> b) -> f (g a) -> f (g b)
fmap2 = fmap . fmap

fmap3 :: Functor f => Functor g => Functor h => (a -> b) -> f (g (h a)) -> f (g (h b))
fmap3 = fmap . fmap . fmap

invert :: Alternative f => Monad f => f a -> f ()
invert a = optional a >>= maybe (pure mempty) (pure empty)

single :: Foldable f => MonadFail m => String -> f a -> m a
single label xs = case toList xs of
    [] -> fail $ printf "expected single %s but found none" label
    [x] -> pure x
    _ -> fail $ printf "expected single %s but found multiple" label
