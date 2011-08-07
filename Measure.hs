module Main where

import System.Environment (getArgs)
import Control.Monad (unless)
import JsonCollectors (measureToDatabase)

main = do
  args <- getArgs
  unless (length args == 1) $ fail "Usage: measure conf"
  measureToDatabase (args !! 0)
