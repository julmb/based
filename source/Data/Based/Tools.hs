module Data.Based.Tools (fmap2, invert) where

import Control.Applicative

fmap2 :: Functor f => Functor g => (a -> b) -> f (g a) -> f (g b)
fmap2 = fmap . fmap

invert :: Alternative f => Monad f => f a -> f ()
invert a = optional a >>= maybe (pure mempty) (pure empty)
