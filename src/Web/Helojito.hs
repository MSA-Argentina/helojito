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
       , getTask
       , getTasks
       , getProjects
       , getTaskTypes
       , getResolutions
         -- * Types
       , Helojito
       , HelojitoError    (..)
       , ConnConf         (..)
       , Task             (..)
       , TaskId           (..)
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
       , Resolution       (..)
       , ResolutionId     (..)
       , ResolutionList   (..)
       , ResolutionListId
       ) where

import           Web.Helojito.Types
import           Web.Helojito.Client (Helojito, runHelojito, ConnConf(..), HelojitoError(..))

postTask :: Task -> Helojito Task
postTask = postEndpoint PostTaskId

getTask :: TaskId -> Helojito Task
getTask = getEndpoint

getTasks :: Helojito TaskList
getTasks = getEndpoint TaskListId

getTaskTypes :: Helojito TaskTypeList
getTaskTypes = getEndpoint TaskTypeListId

getProjects :: Helojito ProjectList
getProjects = getEndpoint ProjectListId

getResolutions :: Helojito ResolutionList
getResolutions = getEndpoint ResolutionListId
