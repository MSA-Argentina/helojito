{-# LANGUAGE OverloadedStrings #-}
-- |
-- Module      : Web.Helojitpo
-- Stability   : experimental
-- Portability : POSIX
module Web.Helojito
       ( -- * Helojito Monad
         runHelojito
         -- * API Calls
       , getTask
       , getTasks
       , getProject
         -- * Types
       , Helojito
       , HelojitoError (..)
       , ConnConf      (..)
       , Task          (..)
       , TaskId        (..)
       , TaskList      (..)
       , TaskListId
       , Project       (..)
       , ProjectId     (..)
       ) where

import           Web.Helojito.Types
import           Web.Helojito.Client (Helojito, runHelojito, ConnConf(..), HelojitoError(..))

getTask :: TaskId -> Helojito Task
getTask = getEndpoint

getTasks :: Helojito TaskList
getTasks = getEndpoint TaskListId

getProject :: ProjectId -> Helojito Project
getProject = getEndpoint
