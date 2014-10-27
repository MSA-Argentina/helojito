module Main where

import           Helojito.Actions
import           Helojito.Options
import           Helojito.Config

main :: IO ()
main = do
    conf <- readConf
    opts <- getOptions

    case subcommand opts of
        TaskCommand List -> listTasks conf
        TaskCommand (Print n) -> showTask conf n
        TaskCommand Add -> addTask conf
        ProjectCommand List -> listProjects conf
