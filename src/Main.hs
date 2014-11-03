module Main where

import           Data.Text          (pack)
import           Helojito.Actions
import           Helojito.Options
import           Helojito.Config
import qualified Web.Helojito.Types as H


main :: IO ()
main = do
    conf <- readConf
    opts <- getOptions

    case subcommand opts of
        TaskCommand TaskList -> listTasks conf
        TaskCommand (TaskPrint n) -> showTask n conf
        TaskCommand args@(TaskAdd {}) -> addTask (taskBuilder args) conf
        ProjectCommand _ -> listProjects conf
        ResCommand _ -> listResolutions conf
        TaskTypeCommand _ -> listTaskTypes conf

taskBuilder :: TaskOpts -> H.Task
taskBuilder (TaskAdd n h p t s d w) =
    H.Task (H.TaskId 99) (pack n) h (H.ProjectId p) t s (pack d) (pack w)
