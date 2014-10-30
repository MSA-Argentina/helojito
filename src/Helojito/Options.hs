module Helojito.Options (
    getOptions
  , Options     (..)
  , Command     (..)
  , TaskOpts    (..)
) where

import Options.Applicative


type Name = String
type Hours = Float
type Project = Int
type Type = Int
type Solved = Int
type Description = String
type Date = String

data Options = Options
    { subcommand :: Command }
    deriving (Show)

data Command =
    TaskCommand TaskOpts
  | ProjectCommand ProjectOpts
  | ResCommand ResOpts
  | TaskTypeCommand TaskTypeOpts
    deriving (Show)

data TaskOpts =
    TaskList
  | TaskAdd Name Hours Project Type Solved Description Date
  | TaskPrint Int
    deriving (Show)

data ProjectOpts = ProjectList deriving (Show)
data ResOpts = ResList deriving (Show)
data TaskTypeOpts = TaskTypeList deriving (Show)

getOptions :: IO Options
getOptions = customExecParser (prefs showHelpOnError) optsParserInfo

optsParserInfo :: ParserInfo Options
optsParserInfo = info optsParser (fullDesc <> progDesc "Relojito CLI tool")

optsParser :: Parser Options
optsParser = Options
    <$> subparser (
          command "task"
              (info taskParser (progDesc "operate on tasks"))
       <> command "project"
              (info projectParser (progDesc "operate on projects"))
       <> command "type"
              (info typeParser (progDesc "operate on task types"))
       <> command "resolution"
              (info resParser (progDesc "operate on resolution types")))

taskParser :: Parser Command
taskParser = TaskCommand <$> taskOptsParser

projectParser :: Parser Command
projectParser = ProjectCommand <$> projectOptsParser

resParser :: Parser Command
resParser = ResCommand <$> resOptsParser

typeParser :: Parser Command
typeParser = TaskTypeCommand <$> typeOptsParser

projectOptsParser :: Parser ProjectOpts
projectOptsParser = subparser (
                      command "list"
                               (info (pure ProjectList) (progDesc "list elements")))

resOptsParser :: Parser ResOpts
resOptsParser = subparser (
                      command "list"
                               (info (pure ResList) (progDesc "list elements")))

typeOptsParser :: Parser TaskTypeOpts
typeOptsParser = subparser (
                      command "list"
                               (info (pure TaskTypeList) (progDesc "list elements")))

taskOptsParser :: Parser TaskOpts
taskOptsParser = subparser (
                   command "list"
                            (info (pure TaskList) (progDesc "list tasks"))
                <> command "show"
                            (info printParser (progDesc "show task information"))
                <> command "add"
                     (info addParser (progDesc "add a task")))

addParser :: Parser TaskOpts
addParser = TaskAdd <$> strOption (long "name"
                                  <> short 'n'
                                  <> metavar "NAME")
                    <*> option (num :: ReadM Float) (long "hours"
                               <> short 't'
                               <> metavar "HOURS")
                    <*> option (num :: ReadM Int) (long "project"
                               <> short 'p'
                               <> metavar "PROJECT_ID")
                    <*> option (num :: ReadM Int) (long "type"
                               <> short 'y'
                               <> metavar "TASK_TYPE_ID")
                    <*> option (num :: ReadM Int) (long "resolve"
                               <> short 'r'
                               <> metavar "RESOLVED_AS_ID")
                    <*> strOption (long "description"
                               <> short 'd'
                               <> metavar "DESCRIPTION"
                               <> value "")
                    <*> strOption (long "date"
                               <> short 'w'
                               <> metavar "DATE"
                               <> value "")

printParser :: Parser TaskOpts
printParser = TaskPrint <$> argument (num :: ReadM Int)  (metavar "TASK_ID")

num :: (Read a, Num a) => ReadM a
num = eitherReader $ \arg -> case reads arg of
    [(r, "")] -> return r
    _       -> Left $ "cannot parse number '" ++ arg ++ "'"
