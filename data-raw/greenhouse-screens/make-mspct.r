library(readxl)
library(photobiology)
library(ggspectra)

screens.tb <- read_xlsx("./data-raw/greenhouse-screens/ScreensNets_irrad_trans.xlsx", sheet = "database", na = "NA")

colnames(screens.tb)

transmittance.spct <- screens.tb[ , c("Wavelength", "FilterFactor", "Company", "FilterName")]
names(transmittance.spct)[1:2] <- c("w.length", "Tfr")
transmittance.spct <- as.filter_spct(transmittance.spct, 
                                     multiple.wl = length(unique(transmittance.spct$FilterName)),
                                     idfactor = "FilterName")
screens.mspct <- subset2mspct(transmittance.spct)

screens.mspct <- clean(screens.mspct)
screens.mspct <- trim_wl(screens.mspct, range = c(305, 900))
screens.mspct <- despike(screens.mspct)

screens.mspct <- msmsply(screens.mspct, smooth_spct, method = "supsmu", strength = 3)

autoplot(screens.mspct[1:5], annotations = c("-", "peaks"))

object.size(screens.mspct)

screens.mspct <- thin_wl(screens.mspct, max.wl.step = 10)
object.size(screens.mspct)

autoplot(screens.mspct[1:5], annotations = c("-", "peaks"))

save(screens.mspct, file = "./data-raw/greenhouse-screens/screens-mspct.rda", compression_level = 9)
