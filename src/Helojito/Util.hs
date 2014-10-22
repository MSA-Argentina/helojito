module Helojito.Util (
    die
) where

import System.Exit

die :: IO a
die = exitWith (ExitFailure 1)
