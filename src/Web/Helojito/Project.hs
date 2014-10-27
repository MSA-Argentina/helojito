{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
-- |
-- Module      : Web.Helojito.Project
-- Stability   : experimental
-- Portability : POSIX
module Web.Helojito.Project where

import           Control.Applicative     ((<$>), (<*>))
import           Control.Monad           (MonadPlus (mzero))
import           Data.Aeson              (FromJSON (parseJSON), Value (Object), (.:))
import           Data.Text               (Text, append)

import           Web.Helojito.Endpoint   (Endpoint (endpoint))
import           Web.Helojito.Util       (toText)

------------------------------------------------------------------------------
-- | Types
data Project = Project {
    projectId :: ProjectId
  , projectName :: Text
  , projectDescription :: Text
  } deriving (Show)

data ProjectList = ProjectList [Project] deriving (Show)

------------------------------------------------------------------------------
-- | ID for a `Project` type
newtype ProjectId
      = ProjectId Int
      deriving (Show, Eq)
data ProjectListId = ProjectListId deriving (Show, Eq)

------------------------------------------------------------------------------
-- | Endpoint Instances
instance Endpoint ProjectId Project where
    endpoint (ProjectId id') = "project/" `append` toText id' `append` "/"

instance Endpoint ProjectListId ProjectList where
    endpoint _ = "projects/"

------------------------------------------------------------------------------
-- | JSON Instances
instance FromJSON Project where
  parseJSON (Object o) =
      Project <$> (ProjectId <$> o .: "id")
           <*> o .: "name"
           <*> o .: "description"
  parseJSON _ = mzero

instance FromJSON ProjectList where
  parseJSON = fmap ProjectList . parseJSON
