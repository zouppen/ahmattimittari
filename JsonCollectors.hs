module JsonCollectors where

import ConfigReader
import Parsing (perhapsParseFile)

-- measure :: FilePath -> 
measure conffile = do
  confs <- readConfigFromFile conffile 
  mapM doParse confs
  where doParse a = perhapsParseFile (parser a) (device a)
  