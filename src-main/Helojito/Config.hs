{-# LANGUAGE OverloadedStrings, DeriveGeneric #-}
module Helojito.Config (
    readConf
  , Config   (..)
) where

import Data.Aeson
import qualified Data.ByteString.Lazy as B
import Data.Text
import GHC.Generics
import System.Directory

data Config = Config { token :: !Text
                     , apiurl :: !Text } deriving (Show, Generic)

instance FromJSON Config
instance ToJSON Config

readConf :: IO Config
readConf = do
    home <- getHomeDirectory
    contents <- B.readFile (home ++ "/.helojito")
    let ejson = eitherDecode contents
    case ejson of
        Left err -> putStrLn "Config parse error:" >> error err
        Right conf -> return conf
