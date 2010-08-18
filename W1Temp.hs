-- |Parses Dallas 1-wire bus temperature data from Linux sysfs. 

module W1Temp where

import Text.Parsec.ByteString
import Text.Parsec

readW1Temp :: (Read a,Floating a) => GenParser Char () a
readW1Temp = do
  skipBeginning
  checkCRC
  newline
  skipBeginning
  temp <- readTempText
  newline
  eof
  return $ (read temp) / 1000 -- Original string is in "millicelsius" scale.

skipHexByte = do
  count 2 hexDigit
  space

skipBeginning = do
  count 9 skipHexByte
  
checkCRC = do
  string ": crc="
  count 2 hexDigit
  space
  string "YES" <?> "correct CRC"

readTempText = do
  string "t="
  many1 digit
