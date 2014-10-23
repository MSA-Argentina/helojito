module Helojito.Util (
    die
  , exit
) where

import System.Exit

die :: IO ()
die = exitWith (ExitFailure 1)

exit :: IO ()
exit = exitSuccess
