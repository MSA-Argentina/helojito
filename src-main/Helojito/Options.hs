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
type Description = String
type Date = String
type Hash = String

data Options = Options
    { subcommand :: Command }
    deriving (Show)

data Command =
    TaskCommand TaskOpts
  | ProjectCommand ProjectOpts
  | TaskTypeCommand TaskTypeOpts
    deriving (Show)

data TaskOpts =
    TaskList
  | TaskWeek (Maybe Date)
  | TaskMonth (Maybe Date)
  | TaskDay (Maybe Date)
  | TaskAdd
      Name
      Hours
      Project
      Type
      Description
      Date
  | TaskMod
      Int
      (Maybe Name)
      (Maybe Hours)
      (Maybe Project)
      (Maybe Type)
      (Maybe Description)
      (Maybe Date)
  | TaskCommit
      Hash
      Hours
      Project
      Type
      Description
  | TaskPrint Int
    deriving (Show)

data ProjectOpts = ProjectList deriving (Show)
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
              (info typeParser (progDesc "operate on task types")))

taskParser :: Parser Command
taskParser = TaskCommand <$> taskOptsParser

projectParser :: Parser Command
projectParser = ProjectCommand <$> projectOptsParser

typeParser :: Parser Command
typeParser = TaskTypeCommand <$> typeOptsParser

projectOptsParser :: Parser ProjectOpts
projectOptsParser = subparser (
                      command "list"
                               (info (pure ProjectList) (progDesc "list elements")))

typeOptsParser :: Parser TaskTypeOpts
typeOptsParser = subparser (
                      command "list"
                               (info (pure TaskTypeList) (progDesc "list elements")))

taskOptsParser :: Parser TaskOpts
taskOptsParser = subparser (
                   command "list"
                            (info (pure TaskList) (progDesc "list tasks"))
                <> command "day"
                            (info dayParser (progDesc "list day tasks in a calendar"))
                <> command "week"
                            (info weekParser (progDesc "list week tasks in a calendar"))
                <> command "month"
                            (info monthParser (progDesc "list month tasks"))
                <> command "show"
                            (info printParser (progDesc "show task information"))
                <> command "add"
                     (info addParser (progDesc "add a task"))
                <> command "mod"
                     (info modParser (progDesc "modify a task"))
                <> command "commit"
                     (info commitParser (progDesc "add a task from a commit")))

printParser :: Parser TaskOpts
printParser = TaskPrint <$> argument (num :: ReadM Int)  (metavar "TASK_ID")

weekParser :: Parser TaskOpts
weekParser = TaskWeek <$> optional (strArgument (metavar "DATE"))

monthParser :: Parser TaskOpts
monthParser = TaskMonth <$> optional (strArgument (metavar "DATE"))

dayParser :: Parser TaskOpts
dayParser = TaskDay <$> optional (strArgument (metavar "DATE"))

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
                    <*> optional (strOption (long "description"
                                         <> short 'd'
                                         <> metavar "DESCRIPTION"))
                    <*> optional (strOption (long "when"
                                         <> short 'w'
                                         <> metavar "DATE"))

commitParser :: Parser TaskOpts
commitParser = TaskCommit <$> strArgument (metavar "HASH")
                          <*> option (num :: ReadM Float) (long "hours"
                                     <> short 'h'
                                     <> metavar "HOURS")
                          <*> option (num :: ReadM Int) (long "project"
                                     <> short 'p'
                                     <> metavar "PROJECT_ID")
                          <*> option (num :: ReadM Int) (long "type"
                                     <> short 't'
                                     <> metavar "TASK_TYPE_ID")
                          <*> strOption (long "description"
                                     <> short 'd'
                                     <> metavar "DESCRIPTION"
                                     <> value "")

num :: (Read a, Num a) => ReadM a
num = eitherReader $ \arg -> case reads arg of
    [(r, "")] -> return r
    _       -> Left $ "cannot parse number '" ++ arg ++ "'"
