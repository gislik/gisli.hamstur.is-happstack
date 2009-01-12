module AppState.AppDate where

import AppState.Types
import Data.Char (isNumber)
import System.Time (ClockTime(TOD))
import Data.Time


toAppDate :: UTCTime -> AppDate
toAppDate (UTCTime d dt) = AppDate (toModifiedJulianDay d) $ (read.takeWhile isNumber.show) dt

fromAppDate :: AppDate -> UTCTime
fromAppDate (AppDate d s) =  UTCTime (ModifiedJulianDay d) (secondsToDiffTime s)
fromAppDate NoAppDate = UTCTime (ModifiedJulianDay 0) (secondsToDiffTime 0)
