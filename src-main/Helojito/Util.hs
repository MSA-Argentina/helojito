module Helojito.Util where

import           Data.Text          (Text, unpack)
import           Text.PrettyPrint   (Doc, text)
import           Helojito.Options   (Date)
import           System.Exit
import           Data.String.Utils  (split)
--import           Data.Time.Clock
import           Data.Time.Calendar
--import           Data.Time.Calendar.OrdinalDate


die :: IO ()
die = exitWith (ExitFailure 1)

exit :: IO ()
exit = exitSuccess

toDoc :: Text -> Doc
toDoc = text . unpack

dateWeek :: Day -> [Day]
dateWeek = undefined

toDay :: Date -> Day
toDay s = fromGregorian (fromIntegral y) m d
  where
    [y, m, d] = map (fromIntegral . read) $ split "-" s :: [Int]
