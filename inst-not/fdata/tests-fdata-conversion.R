library(fda.usc)
library(photobiologyInOut)
library(ggspectra)
library(magrittr)

load("CF.01.spct.Rda")
load("CF.24.spct.Rda")
load("CF.50.spct.Rda")

getMultipleWl(CF.01.spct) # 3 spectra
getMultipleWl(CF.24.spct) # 500 spectra
getMultipleWl(CF.50.spct) # 1 spectrum

fda.mspct <- source_mspct(list(mean = fdata2spct(func.mean(spct2fdata(CF.24.spct))),
                               median = fdata2spct(func.med.FM(spct2fdata(CF.24.spct))),
                               obs = CF.24.spct))
                          
autoplot(fda.mspct$obs) +
  geom_line(data = fda.mspct$mean, colour = "red") +
  geom_line(data = fda.mspct$median, colour = "blue") +
  coord_cartesian(xlim = c(450, 550))

