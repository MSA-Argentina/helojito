module Helojito.Actions (
    listProjects
  , listTasks
  , listResolutions
  , listTaskTypes
  , showTask
  , addTask
) where

import           Control.Applicative
import           Data.Aeson          (FromJSON)
import           Data.Text           (unpack)
import           Data.Text.Encoding  (encodeUtf8)
import           Web.Helojito
import           Helojito.Config
import           Helojito.Printers
import           Helojito.Util
import           Text.PrettyPrint    (Doc)


listProjects  :: Config -> IO ()
listProjects c = actionDispatch getProjects pSimpleProjects c

listTaskTypes  :: Config -> IO ()
listTaskTypes c = actionDispatch getTaskTypes pSimpleTaskTypes c

addTask :: Task -> Config -> IO ()
addTask task c = actionDispatch actions pExtraTask c
  where
    actions = ((,) <$> ptask <*> pprojects)
    ptask = postTask task
    pprojects = getProjects

listTasks  :: Config -> IO ()
listTasks c = actionDispatch getTasks pSimpleTasks c

showTask  :: Int -> Config -> IO ()
showTask id' c = actionDispatch actions pExtraTask c
  where
    actions = ((,) <$> ptask <*> pprojects)
    ptask = getTask $ TaskId id'
    pprojects = getProjects

listResolutions  :: Config -> IO ()
listResolutions c = actionDispatch getResolutions pSimpleResolutions c

actionDispatch :: FromJSON a => Helojito a -> (a -> Doc) -> Config -> IO ()
actionDispatch actions doc conf = do
    resp <- runHelojito actions $ fromC conf
    case resp of
        Left err -> handleError err >> die
        Right stuff -> print (doc stuff) >> exit
  where
    fromC (Config t u) = ConnConf (encodeUtf8 t)
                                  (unpack u)
    handleError e = case e of
                        (ConnectionError s) -> putStrLn s
                        _ ->  print e
