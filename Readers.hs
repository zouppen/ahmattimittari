module Readers where

import W1Temp (readW1Temp)
import TempOut

tempOut = MeasurementOpts {
            parser = readW1Temp
          , stampFormat = "%FT%RZ.txt"
          , pathSuffix = "temp-out"
          , relaxed = False
          }
