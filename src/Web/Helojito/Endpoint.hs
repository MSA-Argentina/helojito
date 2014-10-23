{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE OverloadedStrings      #-}
-- |
-- Module      : Web.Helojito.Endpoint
-- Stability   : experimental
-- Portability : POSIX
module Web.Helojito.Endpoint where

import           Data.Aeson            (FromJSON)
import           Data.Text             (Text)

import           Web.Helojito.Client   (Helojito, buildHJRequest)

------------------------------------------------------------------------------
-- | Endpoint maps the id to the returned type on a type level
-- The function dependency @id -> resp@ specifies that @id@ uniquely determines @resp@
class Endpoint id resp | id -> resp where
    endpoint :: id -> Text -- ^ Turn @id@ into path that points to resource

------------------------------------------------------------------------------
-- | Generic function for making requests
getEndpoint :: (Endpoint a b, FromJSON b) => a -> Helojito b
getEndpoint id' = buildHJRequest $ endpoint id'
