-- |Parses SPI bus temperature data from Linux sysfs. 

module SPITemp where

import Text.Parsec.ByteString
import Text.Parsec
import Control.Monad (when)

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

  let floatText=sign:a++b:c

  when (floatText == " 0.00") $ fail "Glitch temperature (0.00 degrees)"
  return $ read $ floatText
