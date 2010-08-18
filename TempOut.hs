module TempOut where

import Control.Monad (liftM,liftM2)
import System.Directory
import Data.Time.Format
import Data.Time.Clock (UTCTime)
import System.Locale (defaultTimeLocale)
import Text.Parsec
import Text.Parsec.ByteString
import qualified Data.ByteString as B
import Parsing (perhapsParseFile)

data TempInfo = TempInfo UTCTime Float  deriving (Show, Eq, Ord)

data MeasurementOpts = MeasurementOpts {
      parser      :: GenParser Char () Float
    , stampFormat :: String
    , pathSuffix  :: FilePath
}

tempOut = MeasurementOpts {
            parser = readW1Temp
          , stampFormat = "%FT%RZ.txt"
          , pathSuffix = "temp-out"
          }

getTemps :: MeasurementOpts -> FilePath -> IO [Maybe TempInfo]
getTemps opts dir = do
  files <- getDirectoryContents fullDir
  let realFiles = filter fileEntrySieve files
  mapM (getTempOrDie opts fullDir) realFiles
    where fullDir = concat [dir,"/",pathSuffix opts]

-- |Filters *nix "bogus" path entries from directory listings.
fileEntrySieve x = not $ elem x [".",".."]

getTemp :: MeasurementOpts -> FilePath -> FilePath -> IO (Maybe TempInfo)
getTemp opts dir f = do
  temp <- perhapsParseFile (parser opts) fullPath
  return $ liftM2 TempInfo (fileDate opts f) temp
    where fullPath = concat [dir,"/",f]

-- |More pedant function for parsing. Fails on a single error.
getTempOrDie :: MeasurementOpts -> FilePath -> FilePath -> IO (Maybe TempInfo)
getTempOrDie opts dir f = do
  temp <- perhapsParseFile (parser opts) fullPath
  res <- liftM2 TempInfo (fileDate opts f) temp
  return $ Just res
    where fullPath = concat [dir,"/",f]

fileDate :: (Monad m) => MeasurementOpts -> FilePath -> m UTCTime
fileDate opts f = case parseTime defaultTimeLocale (stampFormat opts) f of
               Just a -> return a
               Nothing -> fail $ "Can not parse date from " ++ f

readW1Temp :: (Read a,Floating a) => GenParser Char () a
readW1Temp = do
  skipBeginning
  checkCRC
  newline
  skipBeginning
  temp <- readTempText
  newline
  eof
  return $ (read temp) / 1000 -- Original string is in "millicelsius" scale.

skipHexByte = do
  count 2 hexDigit
  space

skipBeginning = do
  count 9 skipHexByte
  
checkCRC = do
  string ": crc="
  count 2 hexDigit
  space
  string "YES" <?> "correct CRC"

readTempText = do
  string "t="
  many1 digit
