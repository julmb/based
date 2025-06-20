module Data.Based.Tools (invert) where

import Control.Applicative

invert :: Alternative f => Monad f => f a -> f ()
invert a = optional a >>= maybe (pure mempty) (pure empty)
