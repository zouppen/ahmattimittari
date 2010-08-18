module Parsing where

import Text.Parsec.ByteString

-- |Parses file contents and puts the results in a monad of your
-- disposal. File reading related errers are put on IO monad, anyway.
perhapsParseFile :: (Monad m) => Parser a -> FilePath -> IO (m a)
perhapsParseFile parser f = do
  res <- parseFromFile parser f
  return $ case res of
             Left e -> fail $ show e
             Right a -> return a
