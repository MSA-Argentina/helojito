{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
-- |
-- Module      : Web.Helojito.Task
-- Stability   : experimental
-- Portability : POSIX
module Web.Helojito.Task where

import           Prelude                 hiding (null)
import           Control.Applicative     ((<$>), (<*>))
import           Control.Monad           (MonadPlus (mzero))
import           Data.Aeson
import           Data.Text               (Text, append, null)

import           Web.Helojito.Endpoint   (Endpoint (endpoint))
import           Web.Helojito.Project    (ProjectId(..))
import           Web.Helojito.Util       (toText)

------------------------------------------------------------------------------
-- | Types
data Task = Task {
    taskId :: TaskId
  , taskName :: Text
  , taskHours :: Float
  , taskProject :: ProjectId
  , taskType :: Int
  , taskResolution :: Maybe Int
  , taskDescription :: Text
  , taskDate :: Text
  } deriving (Show)

data TaskList = TaskList [Task] deriving (Show)

------------------------------------------------------------------------------
-- | ID for a `Task` type
newtype TaskId = TaskId Int deriving (Show, Eq)
newtype TaskDayId = TaskDayId Text deriving (Show, Eq)
newtype UpdateTaskId = UpdateTaskId Int deriving (Show, Eq)
data TaskListId = TaskListId deriving (Show, Eq)
data PostTaskId = PostTaskId deriving (Show, Eq)

------------------------------------------------------------------------------
-- | Endpoint Instances
instance Endpoint TaskId Task where
    endpoint (TaskId id') = "tasks/" `append` toText id' `append` "/"

instance Endpoint TaskDayId TaskList where
    endpoint (TaskDayId id') = "tasks_day/" `append` id' `append` "/"

instance Endpoint UpdateTaskId Task where
    endpoint (UpdateTaskId id') = "tasks/" `append` toText id' `append` "/"

instance Endpoint TaskListId TaskList where
    endpoint _ = "tasks/"

instance Endpoint PostTaskId Task where
    endpoint _ = "tasks/"

------------------------------------------------------------------------------
-- | JSON Instances
instance FromJSON Task where
  parseJSON (Object o) =
      Task <$> (TaskId <$> o .: "id")
           <*> o .: "name"
           <*> o .: "total_hours"
           <*> (ProjectId <$> o .: "project")
           <*> o .: "task_type"
           <*> o .: "resolved_as"
           <*> o .: "description"
           <*> o .: "date"
  parseJSON _ = mzero

instance FromJSON TaskList where
  parseJSON = fmap TaskList . parseJSON

instance ToJSON Task where
   toJSON (Task _ n h (ProjectId p) t s d w) =
       object $ [ "name" .= n
                , "total_hours" .= h
                , "project" .= p
                , "task_type" .= t
                , "task_type" .= t
                , "resolved_as" .= s
                , "description" .= d
                ] ++ case null w of
                         True -> []
                         False -> [ "date" .= w ]
