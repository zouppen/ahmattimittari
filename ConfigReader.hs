{-# LANGUAGE TypeSynonymInstances #-}
module ConfigReader where

import Text.Parsec.ByteString
import Network.URI (URI,parseURI)
import Data.Maybe (fromJust)
import Data.Char (isSpace)
import W1Temp (readW1Temp)
import SPITemp (readBoxTemp)

pairs = [("W1",readW1Temp)
        ,("Spi",readBoxTemp)
        ]

type MeasurementParser = GenParser Char () Double

-- Taken from Real world Haskell and modified to understand 
instance Read MeasurementParser where
    -- readsPrec is the main function for parsing input
    readsPrec _ rawValue = 
        -- We pass tryParse a list of pairs.  Each pair has a string
        -- and the desired return value.  tryParse will try to match
        -- the input to one of these strings.
        tryParse pairs
        where tryParse [] = []    -- If there is nothing left to try, fail
              tryParse ((attempt, result):xs) =
                      -- Compare the start of the string to be parsed to the
                      -- text we are looking for.
                      if (take (length attempt) value) == attempt
                         -- If we have a match, return the result and the
                         -- remaining input
                         then [(result, drop (length attempt) value)]
                         -- If we don't have a match, try the next pair
                         -- in the list of attempts.
                         else tryParse xs
              value = dropWhile isSpace rawValue

-- So ugly that we need to reconsider using a custom config file.
instance Read URI where
  readsPrec n = (map (change parse)).(str n)
    where 
      str = readsPrec :: (Int -> ReadS String)
      parse = fromJust.parseURI
      change f (a,rest) = (f a, rest)

data Config = Config { collectors  :: [Collector]
                     , databaseURL :: URI
                     } deriving (Read)

data Collector = Collector 
  { device :: FilePath          -- ^File name to parse
  , parser :: MeasurementParser -- ^Parser to use to parse a single file.
  , name   :: String            -- ^Name (to JSON records).
  } deriving (Read)

readConfigFromFile :: FilePath -> IO Config
readConfigFromFile f = readFile f >>= return.read
