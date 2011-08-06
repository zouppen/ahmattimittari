module JsonCollectors where

import Data.Either
import Text.JSON
import ConfigReader
import Parsing (perhapsParseFile)
import Database.CouchDB

type Entry = (String, JSValue)

-- |Performs a measurement and writes the results to local CouchDB. Sweet!
measureToDatabase :: FilePath -> IO ()
measureToDatabase conffile = do
  conf <- readConfigFromFile conffile 
  json <- measure conf
  (doc,_) <- runCouchDB' $ newDoc (db "measurements") json
  putStrLn $ "Measurement ready, id=" ++ show doc

-- |Performs measurement by the instructions in config file
measure :: [Collector] -> IO JSValue
measure collectors = do
  collected <- mapM doParse collectors
  -- Collects errors to a separate object to ease fetching.
  let errors = ("errors",makeObj (lefts collected))
  return $ makeObj (errors:rights collected)

-- |Collects a single measurement and puts successful ones to right
-- and errors to left.
doParse :: Collector -> IO (Either Entry Entry)
doParse a = do
  catch act err
  where
    act = do
      value <- perhapsParseFile (parser a) (device a)
      return $ Right (name a, showJSON value)
    err x = return $ Left (name a, showJSON $ show x)
