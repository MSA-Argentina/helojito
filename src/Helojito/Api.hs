{-# LANGUAGE OverloadedStrings, DeriveGeneric #-}
module Helojito.Api (
    apiCall
) where

import Control.Lens
import qualified Data.ByteString.Char8 as B
import Data.Aeson
import Data.Text
import Data.Text.Encoding (encodeUtf8)
import GHC.Generics
import Network.Wreq

import qualified Helojito.Options as O


data Task = Task { name :: Text
                 , description :: Text
                 , total_hours :: Float } deriving (Show, Generic)

instance FromJSON Task
instance ToJSON Task

apiCall :: String -> Text -> O.Command -> IO ()
apiCall base_url token command = do
    let endpoint = case command of
            O.Task O.List -> "tasks/"
            O.Task O.Add -> "task/"
            O.Task O.Print -> "task/"

            O.Project sub -> ""

    let url = base_url ++ endpoint
    let baseopts = defaults & header "Authorization" .~  ["Token " `B.append` encodeUtf8 token]

    case command of
        O.Task O.List -> printList =<< build url baseopts
        O.Task O.Add -> printOne =<< build url baseopts
        O.Task O.Print -> printOne =<< build url baseopts
        O.Project _ -> error "meh"

build :: FromJSON a => String -> Options -> IO a
build url baseopts = do
    r <- getWith baseopts url
    case eitherDecode (r ^. responseBody) of
        Left err -> putStrLn "Malformed JSON" >> error err
        Right o -> return o

printList :: [Task] -> IO ()
printList = mapM_ (putStrLn . simpleFormat)

printOne :: Task -> IO ()
printOne = putStrLn . moreFormat


simpleFormat :: Task -> String
simpleFormat (Task n _ t) = unpack $ "Task: " `append` n `append` " - Horas: " `append` (pack $ show t)

moreFormat :: Task -> String
moreFormat (Task n d t) = unpack $ "Task: " `append` n `append` "\n  Horas: " `append` (pack $ show t) `append` "\n  Descripción: " `append` d
