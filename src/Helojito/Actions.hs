module Helojito.Actions (
    listTasks
  , showTask
  , addTask
) where

import           Data.Text          (unpack)
import           Data.Text.Encoding (encodeUtf8)
import           Web.Helojito
import           Helojito.Config
import           Helojito.Printers
import           Helojito.Util


addTask :: Config -> IO ()
addTask = undefined

listTasks  :: Config -> IO ()
listTasks c = do
        resp <- runHelojito getTasks $ fromC c
        case resp of
            Left err -> print err
            Right (TaskList tasks) -> mapM_ (print . pSimpleTask) tasks

showTask  :: Config -> Maybe Int -> IO ()
showTask c mid = do
        case mid of
            Nothing -> putStrLn "ID has to be a number" >> die
            Just id' -> do
                resp <- runHelojito (getTask $ TaskId id') $ fromC c
                case resp of
                    Left err -> print err >> die
                    Right task -> print (pExtraTask task) >> exit

fromC :: Config -> ConnConf
fromC (Config t u) = ConnConf (encodeUtf8 t)
                              (unpack u)
