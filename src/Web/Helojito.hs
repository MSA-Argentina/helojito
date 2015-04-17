{-# LANGUAGE OverloadedStrings #-}
-- |
-- Module      : Web.Helojitpo
-- Stability   : experimental
-- Portability : POSIX
module Web.Helojito
       ( -- * Helojito Monad
         runHelojito
         -- * API Calls
       , postTask
       , updateTask
       , getTask
       , getTaskDay
       , getTasks
       , getProjects
       , getTaskTypes
         -- * Types
       , Helojito
       , HelojitoError    (..)
       , ConnConf         (..)
       , Task             (..)
       , TaskId           (..)
       , TaskDayId           (..)
       , UpdateTaskId     (..)
       , TaskList         (..)
       , TaskListId
       , TaskType         (..)
       , TaskTypeId       (..)
       , TaskTypeList     (..)
       , TaskTypeListId
       , Project          (..)
       , ProjectId        (..)
       , ProjectList      (..)
       , ProjectListId
       ) where

import           Web.Helojito.Types
import           Web.Helojito.Client (Helojito, runHelojito, ConnConf(..), HelojitoError(..))

updateTask :: UpdateTaskId -> Task -> Helojito Task
updateTask = putEndpoint

postTask :: Task -> Helojito Task
postTask = postEndpoint PostTaskId

getTask :: TaskId -> Helojito Task
getTask = getEndpoint

getTaskDay :: TaskDayId -> Helojito TaskList
getTaskDay = getEndpoint

getTasks :: Helojito TaskList
getTasks = getEndpoint TaskListId

getTaskTypes :: Helojito TaskTypeList
getTaskTypes = getEndpoint TaskTypeListId

getProjects :: Helojito ProjectList
getProjects = getEndpoint ProjectListId
