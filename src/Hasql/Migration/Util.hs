-- |
-- Module      : Hasql.Migration.Util
-- Copyright   : (c) 2016 Timo von Holtz <tvh@tvholtz.de>,
--               (c) 2014-2016 Andreas Meingast <ameingast@gmail.com>
--
-- License     : BSD-style
-- Maintainer  : tvh@tvholtz.de
-- Stability   : experimental
-- Portability : GHC
--
-- A collection of utilites for database migrations.

{-# LANGUAGE OverloadedStrings #-}

module Hasql.Migration.Util
    ( existsTable
    ) where

import           Hasql.Statement
import qualified Hasql.Encoders as Encoders
import qualified Hasql.Decoders as Decoders
import           Data.Text         (Text)
import qualified Data.Text.Encoding as Text

existsTable :: Statement Text Bool
existsTable =
    fmap (not . null) q
    where
        q = unpreparable sql (Encoders.param (Encoders.nonNullable Encoders.text)) (Decoders.rowList (Decoders.column (Decoders.nullable nameDecoder)))
        sql = "select relname from pg_class where relname = $1"

nameDecoder :: Decoders.Value Text
nameDecoder =
    Decoders.custom Nothing "name" (Just (19, 1003)) [] $ \_ bs ->
        Right $ Text.decodeUtf8 bs
