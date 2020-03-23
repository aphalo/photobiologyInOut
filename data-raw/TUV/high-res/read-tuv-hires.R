library(photobiologyInOut)
library(ggspectra)

photon_as_default()

tuv_z0_200du.spct <- read_qtuv_txt("tuv-z0.txt")
tuv_z0_300du.spct <- read_qtuv_txt("tuv-z0-300.txt")
tuv_z30_200du.spct <- read_qtuv_txt("tuv-z30.txt")
tuv_z30_300du.spct <- read_qtuv_txt("tuv-z30-300.txt")
tuv_z60_200du.spct <- read_qtuv_txt("tuv-z60.txt")
tuv_z60_300du.spct <- read_qtuv_txt("tuv-z60-300.txt")
tuv_z75_200du.spct <- read_qtuv_txt("tuv-z75.txt")
tuv_z75_300du.spct <- read_qtuv_txt("tuv-z75-300.txt")

tuv_200du.mspct <- source_mspct(list(z0 = tuv_z0_200du.spct,
                                     z30 = tuv_z30_200du.spct,
                                     z60 = tuv_z60_200du.spct,
                                     z75 = tuv_z75_200du.spct))

tuv_300du.mspct <- source_mspct(list(z0 = tuv_z0_300du.spct,
                                     z30 = tuv_z30_300du.spct,
                                     z60 = tuv_z60_300du.spct,
                                     z75 = tuv_z75_300du.spct))

autoplot(tuv_300du.mspct$z75)
autoplot(tuv_200du.mspct$z75)

autoplot(tuv_300du.mspct$z60)
autoplot(tuv_200du.mspct$z60)

autoplot(tuv_300du.mspct$z30)
autoplot(tuv_200du.mspct$z30)

autoplot(tuv_300du.mspct$z0)
autoplot(tuv_200du.mspct$z0)

autoplot(tuv_300du.mspct)
autoplot(tuv_200du.mspct)

save(tuv_200du.mspct, tuv_300du.mspct, file = "tuv-spectra.rda")
