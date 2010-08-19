-- |Parses SPI bus temperature data from Linux sysfs. 

module SPITemp where

import Text.Parsec.ByteString
import Text.Parsec

readBoxTemp = do
  try readSPITemp <|> readLegacyBoxTemp

readLegacyBoxTemp = do
  newline
  string "The temp is: "
  temp <- readFloat
  string " degrees Celcius"
  return temp

readSPITemp = do
  temp <- readFloat
  newline
  eof
  return temp

readFloat :: (Read a,Floating a) => GenParser Char () a
readFloat = do
  sign <- option ' ' $ char '-'
  a <- many1 digit
  b <- char '.'
  c <- many1 digit

  return $ read $ sign:a++b:c
