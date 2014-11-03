module Helojito.Util where

import           Data.Text (Text, unpack)
import           Text.PrettyPrint (Doc, text)
import           System.Exit


die :: IO ()
die = exitWith (ExitFailure 1)

exit :: IO ()
exit = exitSuccess

toDoc :: Text -> Doc
toDoc = text . unpack
