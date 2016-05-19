## stress test

library(photobiology)
library(photobiologyWavebands)
library(photobiologyInOut)
library(hyperSpec)
library(lattice)

test0.spct <- rbindspct(list(sun1 = sun.spct, sun2 = sun.spct * 2))
subset2mspct(test0.spct, idx.var = "spct.idx")
subset2mspct(test0.spct)

oldwd <- setwd("data-raw/libRadtran")

# read 5 spectra

test.spct <- read_libradtran_vesa("libradtran-multi.dat")
test.spct

# read 5 spectra keeping all columns

test.spct <- read_libradtran_vesa("libradtran-multi.dat", simplify = F)
test.spct

# split2source_mspct(test.spct)
# "time" is the original character variable read from the file (normally dropped)

test.mspct <- subset2mspct(test.spct, idx.var = "time")
test.mspct

# "datetime" is a time variable defining an instant such as POSIXct
test.mspct <- subset2mspct(test.spct, idx.var = "time")
test.mspct

## read 240 spectra

test.spct <- read_libradtran_vesa("libradtran-multi-long.dat")

test.mspct <- subset2mspct(test.spct, idx.var = "datetime", drop.idx = FALSE)

## 21 summaries for each of the 240 spectra

my.q.irrads <- q_irrad(test.mspct, c(Plant_bands(), UV_bands(), list(PAR())))
my.e.irrads <- e_irrad(test.mspct, c(Plant_bands(), UV_bands(), list(PAR(), GEN_G(), PG())))
dplyr::left_join(my.q.irrads, my.e.irrads)

## convert to hyperSpec object and use the hyperSpec package

test.hspct <- mspct2hyperSpec(test.mspct, spct.data.var = "s.e.irrad")
test.hspct
plot(test.hspct)
plot(test.hspct, "spcmeansd")
