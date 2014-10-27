module Helojito.Actions (
    listProjects
  , listTasks
  , showTask
  , addTask
) where

import           Control.Applicative
import           Data.Text           (unpack)
import           Data.Text.Encoding  (encodeUtf8)
import           Web.Helojito
import           Network.HTTP.Client (HttpException(..))
import           Helojito.Config
import           Helojito.Printers
import           Helojito.Util


listProjects  :: Config -> IO ()
listProjects c = do
    resp <- runHelojito getProjects $ fromC c
    case resp of
        Left err -> handleError err >> die
        Right (ProjectList projects) -> mapM_ (print . pSimpleProject) projects >> exit

addTask :: Config -> IO ()
addTask = undefined

listTasks  :: Config -> IO ()
listTasks c = do
    resp <- runHelojito getTasks $ fromC c
    case resp of
        Left err -> handleError err >> die
        Right (TaskList tasks) -> mapM_ (print . pSimpleTask) tasks >> exit

showTask  :: Config -> Int -> IO ()
showTask c id' = do
    resp <- runHelojito ((,) <$> ptask <*> pprojects) $ fromC c
    case resp of
        Left err -> handleError err >> die
        Right (task, projects) -> print (pExtraTask task projects) >> exit
  where
    ptask = getTask $ TaskId id'
    pprojects = getProjects

fromC :: Config -> ConnConf
fromC (Config t u) = ConnConf (encodeUtf8 t)
                              (unpack u)

handleError :: HelojitoError -> IO ()
handleError (ConnectionError e) = handleHttp e
handleError e = print e


handleHttp :: HttpException -> IO ()
handleHttp (StatusCodeException status response cookie) = print status
handleHttp (FailedConnectionException2 host port _ _) = putStrLn $ "Connection refused with " ++ host ++ ":" ++ show port ++ ", is the server running?"
handleHttp e = print e
