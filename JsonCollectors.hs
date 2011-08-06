module JsonCollectors where

import Text.JSON
import ConfigReader
import Parsing (perhapsParseFile)

-- measure :: FilePath -> 
measure conffile = do
  confs <- readConfigFromFile conffile 
  list <- mapM doParse confs
  return $ makeObj list
  where doParse a = do
          value <- perhapsParseFile (parser a) (device a) 
          return (name a,showJSON value)
