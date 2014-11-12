module Helojito.Actions (
    listProjects
  , listTasks
  , calendarTasks
  , listResolutions
  , listTaskTypes
  , showTask
  , addTask
  , modTask
) where

import           Control.Applicative
import           Data.Aeson          (FromJSON)
import           Data.Maybe          (fromMaybe)
import           Data.Traversable    (traverse)
import           Web.Helojito
import           Helojito.Printers
import           Helojito.Util
import           Text.PrettyPrint    (Doc)
import           Data.Time.Calendar  (Day)


listProjects  :: ConnConf -> IO ()
listProjects c = actionDispatch getProjects pSimpleProjects c

listTaskTypes  :: ConnConf -> IO ()
listTaskTypes c = actionDispatch getTaskTypes pSimpleTaskTypes c

addTask :: Task -> ConnConf -> IO ()
addTask task c = actionDispatch actions pExtraTask c
  where
    actions = ((,) <$> ptask <*> pprojects)
    ptask = postTask task
    pprojects = getProjects

modTask i new_task_d c = actionDispatch actions pExtraTask c
  where
    actions = do
        task <- getTask $ TaskId i
        res <- updateTask (UpdateTaskId i) $ merge task new_task_d
        p <- getProjects
        return (res, p)
    merge (Task i' n h p t s d w) (mn, mh, mp, mt, ms, md, mw) =
        Task i' (fromMaybe n mn)
                (fromMaybe h mh)
                (fromMaybe p mp)
                (fromMaybe t mt)
                (fromMaybe s ms)
                (fromMaybe d md)
                (fromMaybe w mw)

listTasks :: ConnConf -> IO ()
listTasks c = actionDispatch getTasks pSimpleTasks c

calendarTasks :: Day -> ConnConf -> IO ()
calendarTasks day c = actionDispatch actions pSimpleTasks2 c
  where
    actions = traverse getTaskDay week
    week = TaskDayId <$> textWeek day

showTask :: Int -> ConnConf -> IO ()
showTask id' c = actionDispatch actions pExtraTask c
  where
    actions = ((,) <$> ptask <*> pprojects)
    ptask = getTask $ TaskId id'
    pprojects = getProjects

listResolutions :: ConnConf -> IO ()
listResolutions c = actionDispatch getResolutions pSimpleResolutions c

actionDispatch :: FromJSON a => Helojito a -> (a -> Doc) -> ConnConf -> IO ()
actionDispatch actions doc con = do
    resp <- runHelojito actions con
    case resp of
        Left err -> handleError err >> die
        Right stuff -> print (doc stuff) >> exit
  where
    handleError e = case e of
                        (ConnectionError s) -> putStrLn s
                        _ ->  print e
