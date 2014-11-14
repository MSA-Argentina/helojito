module Helojito.Util where

import           Data.Text          (Text, pack, unpack)
import           Text.PrettyPrint   (Doc, text)
import           Helojito.Options   (Date)
import           System.Exit
import           Data.String.Utils  (split)
import           System.Locale
import           Data.Time.Format
import           Data.Time.Calendar
import           Data.Time.Calendar.OrdinalDate


die :: IO ()
die = exitWith (ExitFailure 1)

exit :: IO ()
exit = exitSuccess

toDoc :: Text -> Doc
toDoc = text . unpack

dayToText :: Day -> Text
dayToText = pack . formatTime defaultTimeLocale "%Y-%m-%d"

getWeek :: Day -> [Day]
getWeek day = [addDays o monday | o <- [0..6]]
  where
    monday = fromMondayStartWeek year week 1
    (year, _, _) = toGregorian day
    (week, _) = mondayStartWeek day

toDay :: Date -> Day
toDay s = fromGregorian (fromIntegral y) m d
  where
    [y, m, d] = map read $ split "-" s :: [Int]

toDayName :: Day -> String
toDayName = formatTime defaultTimeLocale "%A"

toShortDate :: Day -> String
toShortDate = formatTime defaultTimeLocale "%m-%d"
