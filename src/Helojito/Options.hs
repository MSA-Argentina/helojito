module Helojito.Options (
    getOptions
  , Options     (..)
  , Command     (..)
  , CommandOpts (..)
) where

import Options.Applicative


data Options = Options
    { subcommand :: Command
    , verbose :: Bool }

data Command = TaskCommand CommandOpts | ProjectCommand CommandOpts
data CommandOpts = List | Add | Print Int

getOptions :: IO Options
getOptions = customExecParser (prefs showHelpOnError) optsParserInfo

optsParserInfo :: ParserInfo Options
optsParserInfo = info (helper <*> optsParser) (fullDesc <> progDesc "Relojito CLI utility")

optsParser :: Parser Options
optsParser = Options
    <$> subparser (
          command "task"
              (info (helper <*> taskParser) (progDesc "list/add tasks"))
       <> command "project"
              (info (helper <*> projectParser) (progDesc "list projects")))
    <*> switch
          (long "verbose"
        <> help "Be loud")

taskParser :: Parser Command
taskParser = TaskCommand <$> subOptsParser

projectParser :: Parser Command
projectParser = ProjectCommand <$> subOptsParser

subOptsParser :: Parser CommandOpts
subOptsParser = subparser (
                   command "list"
                            (info (helper <*> pure List) (progDesc "list elements"))
                <> command "show"
                            (info (helper <*> printParser) (progDesc "show element"))
                <> command "add"
                     (info (helper <*> pure Add) (progDesc "add element")))

printParser :: Parser CommandOpts
printParser = Print <$> argument intOption  (metavar "TASK_ID")

intOption :: ReadM Int
intOption = eitherReader $ \arg -> case reads arg of
    [(r, "")] -> return r
    _       -> Left $ "Cannot parse Int `" ++ arg ++ "'"

