module Encounter.Logger (logger) where

import Data.Text           (Text, unpack)
import Data.Time.LocalTime (ZonedTime(..), getZonedTime)
import Data.Time.Format    (formatTime, defaultTimeLocale)
-- import Data.Time.Format.Locale

import Text.Printf         (printf)
-- import System.Locale       (defaultTimeLocale)


cleanCalendar :: ZonedTime -> String
cleanCalendar = formatTime defaultTimeLocale "%Y-%m-%d %H:%M:%S"

logger :: Text -> IO ()
logger msg = do
    zt <- getZonedTime
    printf "[%s] %s\n" (cleanCalendar zt) (unpack msg)
