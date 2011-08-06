module JsonCollectors where

import Data.Either
import Text.JSON
import ConfigReader
import Parsing (perhapsParseFile)

measure :: FilePath -> IO JSValue
measure conffile = do
  confs <- readConfigFromFile conffile 
  collected <- mapM doParse confs
  -- Collects errors to a separate object to ease fetching.
  let errors = ("errors",makeObj (lefts collected))
  return $ makeObj (errors:rights collected)

doParse a = do
  catch act err
  where
    act = do
      value <- perhapsParseFile (parser a) (device a)
      return $ Right (name a, showJSON value)
    err x = return $ Left (name a, showJSON $ show x)
