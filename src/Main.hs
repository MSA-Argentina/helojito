module Main where

import Web.Helojito

import Control.Monad

import Helojito.Options
import Helojito.Config

main :: IO ()
main = do
    conf <- readConf
    opts <- getOptions

    print =<< runHelojito (liftM2 (,) one two)
  where
    one = getTask (TaskId 4)
    two = getTask (TaskId 1)
