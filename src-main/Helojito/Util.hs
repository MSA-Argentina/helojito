module Helojito.Util where

import           Data.Text          (Text, pack, unpack)
import           Data.List.Split    (split, keepDelimsL, whenElt)
import           Text.PrettyPrint   (Doc, text)
import           Helojito.Options   (Date)
import           System.Exit
import qualified Data.String.Utils  as SU
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

getMonth :: Day -> [Day]
getMonth day = [fromGregorian year month d | d <- [1..top]]
  where
    (year, month, _) = toGregorian day
    top = gregorianMonthLength year month

toDay :: Date -> Day
toDay s = fromGregorian (fromIntegral y) m d
  where
    [y, m, d] = map read $ SU.split "-" s :: [Int]

toDayName :: Day -> String
toDayName = formatTime defaultTimeLocale "%A"

toDayNumber :: Day -> String
toDayNumber = formatTime defaultTimeLocale "%d"

toShortDate :: Day -> String
toShortDate = formatTime defaultTimeLocale "%m-%d"

chunkLeftWhen :: (a -> Bool) -> [a] -> [[a]]
chunkLeftWhen f = split (keepDelimsL $ whenElt f)

isSunday :: Day -> Bool
isSunday = (==) 0 . snd . sundayStartWeek
