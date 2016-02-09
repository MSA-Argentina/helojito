{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
-- |
-- Module      : Web.Helojito.TaskType
-- Stability   : experimental
-- Portability : POSIX
module Web.Helojito.TaskType where

import           Control.Monad           (MonadPlus (mzero))
import           Data.Aeson
import           Data.Text               (Text)

import           Web.Helojito.Endpoint   (Endpoint (endpoint))

------------------------------------------------------------------------------
-- | Types
data TaskType = TaskType {
    taskTypeId :: TaskTypeId
  , taskTypeName :: Text
  } deriving (Show)

data TaskTypeList = TaskTypeList [TaskType] deriving (Show)

------------------------------------------------------------------------------
-- | ID for a `TaskType` type
newtype TaskTypeId = TaskTypeId Int deriving (Show, Eq)
data TaskTypeListId = TaskTypeListId deriving (Show, Eq)

------------------------------------------------------------------------------
-- | Endpoint Instances
instance Endpoint TaskTypeListId TaskTypeList where
    endpoint _ = "task_types/"

------------------------------------------------------------------------------
-- | JSON Instances
instance FromJSON TaskType where
  parseJSON (Object o) =
      TaskType <$> (TaskTypeId <$> o .: "id")
           <*> o .: "name"
  parseJSON _ = mzero

instance FromJSON TaskTypeList where
  parseJSON = fmap TaskTypeList . parseJSON
