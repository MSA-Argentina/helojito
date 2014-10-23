{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
-- |
-- Module      : Web.Helojito.Task
-- Stability   : experimental
-- Portability : POSIX
module Web.Helojito.Task where

import           Control.Applicative     ((<$>), (<*>))
import           Control.Monad           (MonadPlus (mzero))
import           Data.Aeson              (FromJSON (parseJSON), Value (Object), (.:))
import           Data.Text               (Text, append)

import           Web.Helojito.Endpoint   (Endpoint (endpoint))
import           Web.Helojito.Util       (toText)

------------------------------------------------------------------------------
-- | Types
data Task = Task {
    taskId :: TaskId
  , taskHours :: Float
  , taskName :: Text
  } deriving (Show)

------------------------------------------------------------------------------
-- | ID for a `Task` type
newtype TaskId
      = TaskId Int
      deriving (Show, Eq)

------------------------------------------------------------------------------
-- | Endpoint Instances
instance Endpoint TaskId Task where
    endpoint (TaskId id') = "task/" `append` toText id'

------------------------------------------------------------------------------
-- | JSON Instances
instance FromJSON Task where
  parseJSON (Object o) =
      Task <$> (TaskId <$> o .: "id")
           <*> o .: "name"
           <*> o .: "total_hours"
  parseJSON _ = mzero
