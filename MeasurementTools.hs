module MeasurementTools where

import Control.Monad (liftM,liftM2)
import System.Directory
import Data.Time.Format
import Data.Time.Clock (UTCTime)
import System.Locale (defaultTimeLocale)
import Text.Parsec
import Text.Parsec.ByteString
import qualified Data.ByteString as B
import Parsing (perhapsParseFile)

data MeasurementOpts a = MeasurementOpts {
      parser      :: GenParser Char () a -- ^Parser to use to parse a single file.
    , stampFormat :: String           -- ^Time stamp format on file name.
    , pathSuffix  :: FilePath         -- ^Directory, where measurements are in.
    , relaxed     :: Bool             -- ^Are we happy with Nothings if parsing fails?
}

type TimeTuple a = (UTCTime,a)

-- |Reads measurements from given directory with given parsers etc. If
-- you need a more relaxed parsing, switch getMeasurementOrDie to
-- getMeasurement.
getMeasurements :: MeasurementOpts a -> FilePath -> IO [Maybe (TimeTuple a)]
getMeasurements opts dir = do
  files <- getDirectoryContents fullDir
  let realFiles = filter fileEntrySieve files
  mapM (getMeasurement opts fullDir) realFiles
    where fullDir = concat [dir,"/",pathSuffix opts]


-- |Filters *nix "bogus" path entries from directory listings.
fileEntrySieve x = not $ elem x [".",".."]

-- |Reads a single file from a given directory with given
-- parsers. This function just picks the right function depending on
-- given options. See getMeasurementRelaxed and getMeasurementOrDie
-- for more information. NB: Yo dawg, I've put monad in your monad so
-- you can return while you return!
getMeasurement :: MeasurementOpts a        -- ^Options.
               -> FilePath                 -- ^Directory path
               -> FilePath                 -- ^File path
               -> IO (Maybe (TimeTuple a)) -- ^Returns: Tuple in monads <3.
getMeasurement o | relaxed o == True = getMeasurementRelaxed o
                 | otherwise         = getMeasurementOrDie o

-- |Quiet parser function. Doesn't fail in IO monad if parsing error,
-- just returns Nothing. See getMeasurementRelaxed for parameter documentation.
getMeasurementRelaxed :: MeasurementOpts a -> FilePath -> FilePath
                      -> IO (Maybe (TimeTuple a))
getMeasurementRelaxed opts dir f = do
  temp <- perhapsParseFile (parser opts) fullPath
  return $ liftM2 timeTuple (fileDate opts f) temp
    where fullPath = concat [dir,"/",f]

-- |More pedant function for parsing. Fails on a single error. See
-- getMeasurementRelaxed for parameter documentation.
getMeasurementOrDie :: MeasurementOpts a -> FilePath -> FilePath
                    -> IO (Maybe (TimeTuple a))
getMeasurementOrDie opts dir f = do
  temp <- perhapsParseFile (parser opts) fullPath
  res <- liftM2 timeTuple (fileDate opts f) temp
  return $ Just res
    where fullPath = concat [dir,"/",f]

-- |Parses given file name to get a date out of it.
fileDate :: (Monad m) => MeasurementOpts a -> FilePath -> m UTCTime
fileDate opts f = case parseTime defaultTimeLocale (stampFormat opts) f of
               Just a -> return a
               Nothing -> fail $ "Can not parse date from " ++ f

-- |Helper function to produce (time,data) pairs. Restricted types a
-- |bit to keep compile-time error messages helpful.
timeTuple :: UTCTime -> a -> (UTCTime,a)
timeTuple x y = (x,y)

