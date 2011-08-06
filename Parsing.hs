module Parsing where

import Text.Parsec.ByteString

-- |Parses file contents and puts the results in a monad of your
-- disposal. File reading related errors are put on IO monad, anyway.
perhapsParseFile :: Parser a -> FilePath -> IO a
perhapsParseFile parser f = do
  res <- parseFromFile parser f
  case res of
    Left e -> fail $ show e
    Right a -> return a
