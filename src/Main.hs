{-# LANGUAGE OverloadedStrings #-}
module Main where

import Prelude

import Data.Text as T
import Control.Exception as E
import Network.HTTP.Client (HttpException(..))

import Helojito.Api
import Helojito.Util
import Helojito.Config as C
import Helojito.Options as O


httpHandler :: HttpException -> IO ()
httpHandler (FailedConnectionException2 _ _ _ _) = putStrLn "Could not connect to host." >> die
httpHandler (StatusCodeException s _ _) = print s >> die
httpHandler e = throwIO e

main :: IO ()
main = do
    conf <- C.readConf
    opts <- getOptions

    let token' = token conf
    let base_url = T.unpack $ url conf
    let command = subcommand opts

    apiCall base_url token' command `E.catch` httpHandler
