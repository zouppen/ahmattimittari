# Ahmattimittari

Ahmattimittari collects measurements from either flat files or devices
from sysfs. New version can read device files and write to a CouchDB
database. Older implementation reads files in a directory and produces
content compatible with GNU R. In the future these implementations are
going to be merged somehow.

## CouchDB writer mode

Before compiling make sure you have patched version of CouchDB which
supports authentication. The patch pending to mainline. Meanwhile you
may grab the sources from
https://zouppen@github.com/zouppen/haskell-couchdb.git .

Compiling Ahmattimittari is very straightforward:

    ghc --make Measure.hs

There is an example configuration `collectors.conf` which has both
Dallas 1-wire bus (W1) receptor and SPI bus receptor. You may have
multiple types of collectors mixed.

You need to configure CouchDB, too. You can run it in Admin Party Mode
or in authenticated mode. Login credentials are embedded to HTTP URL
as usual.

Running the collector:

    ./Measure collectors.conf

If you need to run it periodically, use `cron` or similar program.

## Directory traversal mode

Simple usage:

    ghci Readers.hs Helpers.hs

```haskell
:m +Data.List Helpers
a <- getMeasurements tempOut "/home/joell/projektit/ahma/measurements"
printBeautifully $ sort a
```

If you don't mind some parsing errors (because of CRC errors etc.) you
may put tempOut{relaxed=True} instead of tempOut.

To produce very simple output for R, this can be used:

```haskell
writeFile "/home/joell/projektit/ahma/poista-out.txt" $ unlines $ map (\(x,y) -> concat ["\"",show x,"\",",show y]) $ catMaybes $sort a
```

### Plotting with GNU R

To produce output compatible with GNU R

    ghci ROutput.hs Readers.hs

```haskell
:m +Readers MeasurementTools
convertDirToR tempOut{relaxed=True} "/home/joell/projektit/ahma/measurements" "/home/joell/projektit/ahma/tempOut.csv"

And in GNU R:

```r
source("tempPlot.r")
a <- read.temp("tempOut.csv")
b <- read.temp("tempBox.csv")
multitempplot(a,b)
```

You can use PDF output, too. Good values are:

```r
pdf(file="tempplot.pdf",10,7)
multitempplot(a,b)
dev.off()
```
