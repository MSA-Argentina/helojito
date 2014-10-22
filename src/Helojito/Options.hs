module Helojito.Options (
    getOptions
  , Options(..)
  , Command(..)
) where

import Options.Applicative


data Options = Options
    { subcommand :: Command
    , verbose :: Bool }

data Command = Task CommandOpts | Project CommandOpts
data CommandOpts = List | Add

getOptions :: IO Options
getOptions = customExecParser (prefs showHelpOnError) optsParserInfo

optsParserInfo :: ParserInfo Options
optsParserInfo = info (helper <*> optsParser) (fullDesc <> progDesc "Relojito CLI utility")

optsParser :: Parser Options
optsParser = Options
    <$> subparser (
          (command "task"
              (info (helper <*> taskParser) (progDesc "list/add tasks")))
       <> (command "project"
              (info (helper <*> projectParser) (progDesc "list projects"))))
    <*> switch
          (long "verbose"
        <> help "Be loud")

taskParser :: Parser Command
taskParser = Task <$> subOptsParser

projectParser :: Parser Command
projectParser = Project <$> subOptsParser

subOptsParser :: Parser CommandOpts
subOptsParser = subparser (
                   (command "list"
                            (info (helper <*> pure List) (progDesc "list elements")))
                <> (command "add"
                     (info (helper <*> pure Add) (progDesc "add element"))))
