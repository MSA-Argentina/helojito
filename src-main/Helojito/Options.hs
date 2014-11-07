module Helojito.Options (
    getOptions
  , Options     (..)
  , Command     (..)
  , TaskOpts    (..)
  , Date
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
  | TaskCalendar (Maybe Date)
  | TaskAdd
      Name
      Hours
      Project
      Type
      (Maybe Solved)
      Description
      Date
  | TaskMod
      Int
      (Maybe Name)
      (Maybe Hours)
      (Maybe Project)
      (Maybe Type)
      (Maybe Solved)
      (Maybe Description)
      (Maybe Date)
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
                <> command "calendar"
                            (info calParser (progDesc "list tasks in a calendar"))
                <> command "show"
                            (info printParser (progDesc "show task information"))
                <> command "add"
                     (info addParser (progDesc "add a task"))
                <> command "mod"
                     (info modParser (progDesc "modify a task")))

printParser :: Parser TaskOpts
printParser = TaskPrint <$> argument (num :: ReadM Int)  (metavar "TASK_ID")

calParser :: Parser TaskOpts
calParser = TaskCalendar <$> optional (strArgument (metavar "TASK_ID"))

addParser :: Parser TaskOpts
addParser = TaskAdd <$> strOption (long "name"
                                  <> short 'n'
                                  <> metavar "NAME")
                    <*> option (num :: ReadM Float) (long "hours"
                               <> short 'h'
                               <> metavar "HOURS")
                    <*> option (num :: ReadM Int) (long "project"
                               <> short 'p'
                               <> metavar "PROJECT_ID")
                    <*> option (num :: ReadM Int) (long "type"
                               <> short 't'
                               <> metavar "TASK_TYPE_ID")
                    <*> optional (option (num :: ReadM Int) (long "resolve"
                                         <> short 'r'
                                         <> metavar "RESOLVED_AS_ID"))
                    <*> strOption (long "description"
                               <> short 'd'
                               <> metavar "DESCRIPTION"
                               <> value "")
                    <*> strOption (long "when"
                               <> short 'w'
                               <> metavar "DATE"
                               <> value "")

modParser :: Parser TaskOpts
modParser = TaskMod <$> argument (num :: ReadM Int) (metavar "TASK_ID")
                    <*> optional (strOption (long "name"
                                            <> short 'n'
                                            <> metavar "NAME"))
                    <*> optional (option (num :: ReadM Float) (long "hours"
                                         <> short 'h'
                                         <> metavar "HOURS"))
                    <*> optional (option (num :: ReadM Int) (long "project"
                                         <> short 'p'
                                         <> metavar "PROJECT_ID"))
                    <*> optional (option (num :: ReadM Int) (long "type"
                                         <> short 't'
                                         <> metavar "TASK_TYPE_ID"))
                    <*> optional (option (num :: ReadM Int) (long "resolve"
                                         <> short 'r'
                                         <> metavar "RESOLVED_AS_ID"))
                    <*> optional (strOption (long "description"
                                         <> short 'd'
                                         <> metavar "DESCRIPTION"))
                    <*> optional (strOption (long "when"
                                         <> short 'w'
                                         <> metavar "DATE"))

num :: (Read a, Num a) => ReadM a
num = eitherReader $ \arg -> case reads arg of
    [(r, "")] -> return r
    _       -> Left $ "cannot parse number '" ++ arg ++ "'"
