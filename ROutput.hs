-- |Outputs measurement data ready to R's read.csv. Some may call it foRmat :-)

module ROutput where

import Data.List
import Data.Time.Format (formatTime)
import Data.Time.Clock (UTCTime)
import System.Locale (defaultTimeLocale)
import Data.Maybe (catMaybes)
import MeasurementTools (getMeasurements)

measurementToR :: (UTCTime,Double) -> String
measurementToR (stamp,temp) = intercalate "," [unixStamp,show temp]
    where unixStamp = formatTime defaultTimeLocale "%s" stamp

measurementsToR ms = intercalate "\n" $ (header:map measurementToR ms)
    where header = "timestamp,value"

-- |Converts files in a directory to R compatible CSV format. Ignores
-- |failed measurements.
convertDirToR opts fromDir outFile = do
  ms <- getMeasurements opts fromDir
  let goodMs = sort $ catMaybes ms
  writeFile outFile $ measurementsToR goodMs
