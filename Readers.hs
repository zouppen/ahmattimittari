module Readers where

import W1Temp (readW1Temp)
import SPITemp (readBoxTemp)
import MeasurementTools

tempOut = MeasurementOpts {
            parser = readW1Temp
          , stampFormat = "%FT%RZ.txt"
          , pathSuffix = "temp-out"
          , relaxed = False
          }

tempBox = MeasurementOpts {
            parser = readBoxTemp
          , stampFormat = "%FT%RZ.txt"
          , pathSuffix = "temp-box"
          , relaxed = False
          }
