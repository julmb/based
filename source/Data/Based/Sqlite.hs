{-# OPTIONS_GHC -Wno-orphans #-}
{-# LANGUAGE OverloadedStrings #-}

module Data.Based.Sqlite
(
    connect, transact,
    runFF, runIF, run1F, runFI, runII, run1I, runF1, runI1, run11
)
where

import Type.Reflection
import Control.Exception
import Control.Monad.Reader
import Data.Functor
import Data.Functor.Identity
import Data.Foldable
import Data.Traversable
import Data.Proxy
import Numeric.Natural
import Database.SQLite.Simple
import Database.SQLite.Simple.ToField
import Database.SQLite.Simple.FromField
import Data.Based.Tools
import Data.Based.Unfoldable

instance ToField Natural where toField = SQLInteger . fromIntegral
instance FromField Natural where
    fromField field = case fieldData field of
        SQLInteger n | n < 0 -> returnError ConversionFailed field "negative INTEGER"
        SQLInteger n -> pure $ fromIntegral n
        _ -> returnError Incompatible field "requires INTEGER"

instance FromRow () where fromRow = pure ()

connect :: FilePath -> ReaderT Connection IO a -> IO a
connect path action = withConnection path $ runReaderT $ run11 "PRAGMA foreign_keys = ON" >> action

transact :: FilePath -> ReaderT Connection IO a -> IO a
transact path action = connect path $ ReaderT $ \ connection -> withTransaction connection $ runReaderT action connection

runFF :: forall f g a b. Typeable g => Traversable f => Unfoldable g => ToRow a => FromRow b =>
    Query -> f a -> ReaderT Connection IO (f (g b))
runFF template parameters = ReaderT go where
    go connection = withStatement connection template $ for parameters . body where
        body statement parameter = withBind statement parameter $ unfold (nextRow statement) >>= maybe failure pure
        failure = throwIO $ ConversionFailed "Rows" (show $ typeRep @g) "unexpected number of rows"

runIF :: Typeable g => Unfoldable g => ToRow a => FromRow b => Query -> a -> ReaderT Connection IO (g b)
runIF template = fmap runIdentity . runFF template . Identity

run1F :: Typeable g => Unfoldable g => FromRow b => Query -> ReaderT Connection IO (g b)
run1F template = runIF template ()

runFI :: Traversable f => ToRow a => FromRow b => Query -> f a -> ReaderT Connection IO (f b)
runFI template = fmap2 runIdentity . runFF template

runII :: ToRow a => FromRow b => Query -> a -> ReaderT Connection IO b
runII template = fmap runIdentity . runFI template . Identity

run1I :: FromRow b => Query -> ReaderT Connection IO b
run1I template = runII template ()

runF1 :: Foldable f => ToRow a => Query -> f a -> ReaderT Connection IO ()
runF1 template = void . runFF @_ @Proxy @_ @() template . toList

runI1 :: ToRow a => Query -> a -> ReaderT Connection IO ()
runI1 template = runF1 template . Identity

run11 :: Query -> ReaderT Connection IO ()
run11 template = runI1 template ()
