{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
-- |
-- Module      : Web.Helojito.Resolution
-- Stability   : experimental
-- Portability : POSIX
module Web.Helojito.Resolution where

import           Control.Applicative     ((<$>), (<*>))
import           Control.Monad           (MonadPlus (mzero))
import           Data.Aeson
import           Data.Text               (Text)

import           Web.Helojito.Endpoint   (Endpoint (endpoint))

------------------------------------------------------------------------------
-- | Types
data Resolution = Resolution {
    resId :: ResolutionId
  , resName :: Text
  , resFinished :: Bool
  } deriving (Show)

data ResolutionList = ResolutionList [Resolution] deriving (Show)

------------------------------------------------------------------------------
-- | ID for a `Resolution` type
newtype ResolutionId = ResolutionId Int deriving (Show, Eq)
data ResolutionListId = ResolutionListId deriving (Show, Eq)

------------------------------------------------------------------------------
-- | Endpoint Instances
instance Endpoint ResolutionListId ResolutionList where
    endpoint _ = "resolutions/"

------------------------------------------------------------------------------
-- | JSON Instances
instance FromJSON Resolution where
  parseJSON (Object o) =
      Resolution <$> (ResolutionId <$> o .: "id")
                 <*> o .: "name"
                 <*> o .: "is_finished"
  parseJSON _ = mzero

instance FromJSON ResolutionList where
  parseJSON = fmap ResolutionList . parseJSON
