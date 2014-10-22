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

import Helojito.Util


data Task = Task { name :: Text
                 , description :: Text
                 , total_hours :: Float } deriving (Show, Generic)

instance FromJSON Task
instance ToJSON Task

apiCall :: String -> Text -> IO ()
apiCall url token = do
    let baseopts = defaults & header "Authorization" .~  ["Token " `B.append` encodeUtf8 token]
    r <- getWith baseopts url
    let ejson = eitherDecode (r ^. responseBody) :: Either String [Task]
    case ejson of
        Left err -> putStrLn "Malformed JSON" >> putStrLn err >> die
        Right o -> print o
