module Main where

import           Control.Applicative
import           Data.Text           (pack, unpack)
import           Data.Text.Encoding  (encodeUtf8)
import           Data.Time.Clock     (getCurrentTime, utctDay)
import           Helojito.Actions
import           Helojito.Options
import           Helojito.Config
import           Helojito.Util
import qualified Web.Helojito        as H


main :: IO ()
main = do
    conf <- readConf
    opts <- getOptions
    today <- utctDay <$> getCurrentTime

    let con = let (Config t u) = conf in H.ConnConf (encodeUtf8 t) (unpack u)

    case subcommand opts of
        TaskCommand TaskList -> listTasks con
        TaskCommand (TaskCalendar (Just d)) -> calendarTasks (toDay d) con
        TaskCommand (TaskCalendar Nothing) -> calendarTasks today con
        TaskCommand (TaskPrint n) -> showTask n con
        TaskCommand args@(TaskAdd {}) -> addTask (taskBuilder args) con
        TaskCommand (TaskMod i n h p t s d w) -> modTask i (pack <$> n, h, H.ProjectId <$> p, t, Just <$> s, pack <$> d, pack <$> w) con
        ProjectCommand _ -> listProjects con
        ResCommand _ -> listResolutions con
        TaskTypeCommand _ -> listTaskTypes con

taskBuilder :: TaskOpts -> H.Task
taskBuilder (TaskAdd n h p t s d w) =
    H.Task (H.TaskId (-1)) (pack n) h (H.ProjectId p) t s (pack d) (pack w)
taskBuilder _ = error "What"
