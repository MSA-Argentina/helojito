module Helojito.Actions (
    listProjects
  , listTasks
  , dayTasks
  , weekTasks
  , monthTasks
  , listTaskTypes
  , showTask
  , addTask
  , modTask
) where

import           Data.Maybe             (fromMaybe)
import           Web.Helojito
import           Helojito.Printers
import           Helojito.Util
import           Data.Time.Calendar     (Day)
import           Network                (withSocketsDo)

import qualified Text.PrettyPrint       as D (render)
import qualified Text.PrettyPrint.Boxes as B (render)


listProjects  :: ConnConf -> IO ()
listProjects c = actionDispatch getProjects p c
  where
    p = D.render . pSimpleProjects

listTaskTypes  :: ConnConf -> IO ()
listTaskTypes c = actionDispatch getTaskTypes p c
  where
    p = D.render . pSimpleTaskTypes

addTask :: Task -> ConnConf -> IO ()
addTask task c = actionDispatch actions p c
  where
    p = D.render . pExtraTask
    actions = ((,) <$> ptask <*> pprojects)
    ptask = postTask task
    pprojects = getProjects

modTask i new_task_d c = actionDispatch actions p c
  where
    p = D.render . pExtraTask
    actions = do
        task <- getTask $ TaskId i
        res <- updateTask (UpdateTaskId i) $ merge task new_task_d
        ps <- getProjects
        return (res, ps)
    merge (Task i' n h p' t d w) (mn, mh, mp, mt, md, mw) =
        Task i' (fromMaybe n mn)
                (fromMaybe h mh)
                (fromMaybe p' mp)
                (fromMaybe t mt)
                (fromMaybe d md)
                (fromMaybe w mw)

listTasks :: ConnConf -> IO ()
listTasks c = actionDispatch getTasks p c
  where
    p = D.render . pSimpleTasks

dayTasks :: Day -> ConnConf -> IO ()
dayTasks day c = actionDispatch actions p c
  where
    actions = getTaskDay id'
    id' = TaskDayId . dayToText $ day
    p = D.render . pDayTasks day

weekTasks :: Day -> ConnConf -> IO ()
weekTasks day c = actionDispatch actions p c
  where
    actions = traverse getTaskDay ids
    ids = map (TaskDayId . dayToText) week
    week = getWeek day
    p = B.render . pWeekTasks week

monthTasks :: Day -> ConnConf -> IO ()
monthTasks day c = actionDispatch actions p c
  where
    actions = traverse getTaskDay ids
    ids = map (TaskDayId . dayToText) month
    month = getMonth day
    p = B.render . pMonthTasks month

showTask :: Int -> ConnConf -> IO ()
showTask id' c = actionDispatch actions p c
  where
    p = D.render . pExtraTask
    actions = ((,) <$> ptask <*> pprojects)
    ptask = getTask $ TaskId id'
    pprojects = getProjects

actionDispatch :: Helojito a -> (a -> String) -> ConnConf -> IO ()
actionDispatch actions out con = do
    resp <- withSocketsDo $ runHelojito actions con
    case resp of
        Left err -> handleError err >> die
        Right stuff -> putStrLn (out stuff) >> exit
  where
    handleError e = case e of
                        (ConnectionError s) -> putStrLn s
                        _ ->  print e
