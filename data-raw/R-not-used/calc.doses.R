library(photobiology)
library(photobiologygg)
library(photobiologyFilters)
library(lubridate)
library(ggplot2)

# oldwd <- setwd("~/Lettuce_2012/Spectral_data/sari_v20140613")
# exposure_0106.spct <- read.table(file = "2012-06-01_cum.hel",", skip = 3, col.names = c("w.length", "s.e.irrad"))
# exposure_0106.spct$w.length <- exposure_0106.spct$w.length/10
# exposure_0106.spct <- as.source_spct(exposure_0106.spct, time.unit = "day")
# plot(exposure_0106.spct, unit.out = "photon" )
# e_irrad(exposure_0106.spct, list(GEN_G(300), GEN_T(300), CIE(), FLAV(), PAR()))

oldwd <- setwd("~/Lettuce_2012/Spectral_data/sari_v20140613")

file.names <- c(
"2012-06-01_cum.hel",
"2012-06-02_cum.hel",
"2012-06-03_cum.hel",
"2012-06-04_cum.hel",
"2012-06-05_cum.hel",
"2012-06-06_cum.hel",
"2012-06-07_cum.hel",
"2012-06-08_cum.hel",
"2012-06-09_cum.hel",
"2012-06-10_cum.hel",
"2012-06-11_cum.hel",
"2012-06-12_cum.hel",
"2012-06-13_cum.hel",
"2012-06-14_cum.hel",
"2012-06-15_cum.hel",
"2012-06-16_cum.hel",
"2012-06-17_cum.hel",
"2012-06-18_cum.hel",
"2012-06-19_cum.hel",
"2012-06-20_cum.hel",
"2012-06-21_cum.hel",
"2012-06-22_cum.hel",
"2012-06-23_cum.hel",
"2012-06-24_cum.hel",
"2012-06-25_cum.hel",
"2012-06-26_cum.hel",
"2012-06-27_cum.hel",
"2012-06-28_cum.hel",
"2012-06-29_cum.hel",
"2012-06-30_cum.hel",
"2012-07-01_cum.hel",
"2012-07-02_cum.hel",
"2012-07-03_cum.hel",
"2012-07-04_cum.hel",
"2012-07-05_cum.hel",
"2012-07-06_cum.hel",
"2012-07-07_cum.hel",
"2012-07-08_cum.hel",
"2012-07-09_cum.hel",
"2012-07-10_cum.hel",
"2012-07-11_cum.hel",
"2012-07-12_cum.hel",
"2012-07-13_cum.hel",
"2012-07-14_cum.hel",
"2012-07-15_cum.hel")

spectra.dates <- ymd(grep("_cum.hel", "", file.names, fixed=TRUE))
list.of.spectra <- list()
for (f in file.names) {
temporary.spct <- read.table(file = f, skip = 3, col.names = c("w.length", "s.e.irrad"))
temporary.spct$w.length <- temporary.spct$w.length/10
temporary.spct <- as.source_spct(temporary.spct, time.unit = "day")
data.name <- paste("Anders_", f, ".spct", sep = "")
data.name <- gsub("-", ".", data.name, fixed = FALSE)
assign(data.name, temporary.spct)
list.of.spectra[[data.name]] <- get(data.name, inherits = FALSE)
}

lettuce.mspct <- source_mspct(list.of.spectra)

# filters.mspct <- filter_mspct(list(UVAB = polythene.new.spct, UV0 = uv.226.new.spct,
#                                     Green0 = rose.pink.new.spct, Blue0 = canary.yellow.new.spct,
#                                     SimShade = moss.green.new.spct,
#                                     none = clear.spct))

spectra_with_filters <- convolve_each(lettuce.mspct, filters.mspct)

ambient_doses_joules_day <- e_irrad(lettuce.mspct, list(GEN_G = GEN_G(300), GEN_T = GEN_T(300), CIE = CIE(), FLAV = FLAV(), PAR_energy = PAR()))
ambient_doses_joules_day$date <- spectra.dates
ambient_doses_joules_day$filter <- "none"
pe_doses_joules_day <- e_irrad(convolve_each(lettuce.mspct, polythene.new.spct), list(GEN_G = GEN_G(300), GEN_T = GEN_T(300), CIE = CIE(), FLAV = FLAV(), PAR_energy = PAR()))
pe_doses_joules_day$date <- spectra.dates
pe_doses_joules_day$filter <- "polythene"
pet_doses_joules_day <- e_irrad(convolve_each(lettuce.mspct, polyester.new.spct), list(GEN_G = GEN_G(300), GEN_T = GEN_T(300), CIE = CIE(), FLAV = FLAV(), PAR_energy = PAR()))

all_doses_joules_day <- rbind(ambient_doses_joules_day, pe_doses_joules_day)
ggplot(all_doses_joules_day, aes(x = date, y = GEN_G, colour = filter)) + geom_line() + ylim(0, NA)

min(doses_joules_day$GEN_G)
which.min(doses_joules_day$GEN_G)
range(doses_joules_day$GEN_G)
mean(doses_joules_day$GEN_G)

lettuce_photon_ratios <- q_ratio(lettuce.mspct, list(UVB(), UVA(), Blue()), PAR())
pe_photon_ratios_PAR <- q_ratio(convolve_each(lettuce.mspct, polythene.new.spct), list(UVB(), UVA(), Blue()), list(PAR()))
pe_photon_ratios_other <- q_ratio(convolve_each(lettuce.mspct, polythene.new.spct), list(UVB()), list(UVA(), Blue(), PAR()))
