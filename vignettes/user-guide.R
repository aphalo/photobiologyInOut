## ----setup, include=FALSE, cache=FALSE---------------
library(knitr)
# Are the packages used in examples installed?
eval_chunks <- requireNamespace("ggspectra", quietly = TRUE) &&
                 requireNamespace("photobiologyWavebands", quietly = TRUE)
# eval_colorSpec <- requireNamespace("colorSpec", quietly = TRUE) && eval_chunks
eval_colorSpec <- eval_chunks
eval_pavo <- requireNamespace("pavo", quietly = TRUE) && eval_chunks
eval_hyperSpec <- requireNamespace("hyperSpec", quietly = TRUE) && eval_chunks

opts_chunk$set(fig.align='center', fig.show='hold',
               fig.width=7, fig.height=6, size="footnotesize",
               eval=eval_chunks)

options(replace.assign = TRUE, width = 55,
        warnPartialMatchAttr = FALSE,
        warnPartialMatchDollar = FALSE,
        warnPartialMatchArgs = FALSE)
# setting TZ may be needed in some geographic locations as some Windows TZ 
# strings are not recognized by all versions of R
Sys.setenv(TZ = 'UTC')

## ----message=FALSE-----------------------------------
library(photobiology)
library(photobiologyWavebands)
library(photobiologyInOut)
library(lubridate)
library(ggspectra)
library(readr)
library(colorSpec)
if (eval_pavo) {library(pavo)}
if (eval_hyperSpec) {library(hyperSpec)}

## ----------------------------------------------------
# plot defaults
theme_set(theme_bw()) # ggplot2
set_annotations_default(annotations = c("+", "title:what:when"))
# decrease lines printed
options(tibble.print_max = 5,
        tibble.print_min = 3,
        photobiology.strict.range = NA_integer_)

## ----------------------------------------------------
jaz.raw.file <- 
  system.file("extdata", "spectrum.jaz", 
              package = "photobiologyInOut", mustWork = TRUE)
jazraw.spct <- read_oo_jazdata(file = jaz.raw.file)
jazraw.spct <- trim_wl(jazraw.spct, range = c(250, 900))

## ----------------------------------------------------
autoplot(jazraw.spct)

## ----------------------------------------------------
when_measured(jazraw.spct)

## ----------------------------------------------------
getInstrDesc(jazraw.spct)

## ----------------------------------------------------
getInstrSettings(jazraw.spct)

## ----------------------------------------------------
jaz.s.irrad.file <- 
  system.file("extdata", "spectrum.JazIrrad", 
              package = "photobiologyInOut", mustWork = TRUE)
jaz.spct <- read_oo_jazirrad(file = jaz.s.irrad.file)
jaz0.spct <- jaz.spct
jaz.spct <- trim_wl(jaz.spct, range = c(290, 800))

## ----------------------------------------------------
autoplot(jaz.spct)

## ----------------------------------------------------
jaz.spct <- fshift(jaz0.spct, range = c(255, 290), f = "mean")
jaz.spct <- trim_wl(jaz.spct, range = c(290, 800))
autoplot(jaz.spct)

## ----------------------------------------------------
jaz.spct <- smooth_spct(jaz.spct)
autoplot(jaz.spct)

## ----------------------------------------------------
e_irrad(jaz.spct, PAR())       # W m-2

## ----------------------------------------------------
autoplot(read_oo_jazirrad(file = jaz.s.irrad.file))

## ----------------------------------------------------
autoplot(read_oo_jazirrad(file = jaz.s.irrad.file),
     range = c(250,850))

## ----------------------------------------------------
autoplot(smooth_spct(read_oo_jazirrad(file = jaz.s.irrad.file)),
     range = c(250,850))

## ----------------------------------------------------
q.raw.file <- 
  system.file("extdata", "spectrum.SSIrrad", 
              package = "photobiologyInOut", mustWork = TRUE)
autoplot(read_oo_ssirrad(file = q.raw.file))

## ----------------------------------------------------
file.name <- 
    system.file("extdata", "enlighten-wasatch-scope.csv",
                package = "photobiologyInOut", mustWork = TRUE)
              
wasatch.raw.spct <- 
    read_wasatch_csv(file = file.name, extra.cols = "drop")

## ----------------------------------------------------
summary(wasatch.raw.spct)

## ----------------------------------------------------
autoplot(wasatch.raw.spct)

## ----------------------------------------------------
ava.raw.file <- 
  system.file("extdata", "spectrum-avaspec.csv", 
              package = "photobiologyInOut", mustWork = TRUE)
autoplot(read_avaspec_csv(file = ava.raw.file),
     range = c(280, 900), unit.out = "photon")

## ----------------------------------------------------
macam.raw.file <- 
  system.file("extdata", "spectrum.DTA", 
              package = "photobiologyInOut", mustWork = TRUE)
autoplot(read_macam_dta(file = macam.raw.file))

## ----------------------------------------------------
licor_espd.file <- 
  system.file("extdata", "LI-180-irradiance.txt", 
              package = "photobiologyInOut", mustWork = TRUE)
li180.spct <- read_li180_txt(file = licor_espd.file)

## ----------------------------------------------------
li180.spct
cat(comment(li180.spct))
getInstrDesc(li180.spct)
getInstrSettings(li180.spct)
autoplot(li180.spct, unit.out = "photon")

## ----------------------------------------------------
licor.file <- 
  system.file("extdata", "spectrum.PRN", 
              package = "photobiologyInOut", mustWork = TRUE)
licor.spct <- read_licor_prn(file = licor.file, tz = "Europe/Helsinki")

## ----------------------------------------------------
licor.spct
cat(comment(licor.spct))
autoplot(licor.spct, unit.out = "photon")

## ----------------------------------------------------
licor.file <- 
  system.file("extdata", "reflectance.PRN", 
              package = "photobiologyInOut", mustWork = TRUE)
licor.spct <- read_licor_prn(file = licor.file, s.qty = "Rfr")

## ----------------------------------------------------
licor.spct
cat(comment(licor.spct))
autoplot(licor.spct)

## ----------------------------------------------------
  file.name <- 
    system.file("extdata", "spectrum-psi-spectrapen-SP.csv", 
                package = "photobiologyInOut", mustWork = TRUE)
  psi.mspct <- read_spectrapen_csv(file = file.name,
                                  tz = "UTC")
  summary(psi.mspct)
  autoplot(psi.mspct, annotations = "")

## ----------------------------------------------------
summary(psi.mspct[["spct.14"]])
autoplot(psi.mspct[["spct.14"]])

## ----------------------------------------------------
  file.name <- 
    system.file("extdata", "cid-spectravue-Rpc-Measurements.csv", 
                package = "photobiologyInOut", mustWork = TRUE)
  cid_Rpc.spct <- read_cid_spectravue_csv(file = file.name)
  summary(cid_Rpc.spct)
  autoplot(smooth_spct(cid_Rpc.spct, method = "supsmu"), 
           range = c(400, 1000), annotations = "") %+%
    ylim(0, 0.55)

## ----------------------------------------------------
cs.day.file <- 
  system.file("extdata", "cr6-day.dat", 
              package = "photobiologyInOut", mustWork = TRUE)

## ----eval=FALSE--------------------------------------
# # not run
# read_lines(yoctopuce_hour.file, n_max = 10)

## ----------------------------------------------------
day.dat <- read_csi_dat(file = cs.day.file)
day.dat

## ----------------------------------------------------
cs_hour.file <- 
  system.file("extdata", "cr6-hour.dat", 
              package = "photobiologyInOut", mustWork = TRUE)
hour.dat <- read_csi_dat(file = cs_hour.file)
ggplot(hour.dat, aes(TIMESTAMP, PAR_Den_Avg)) + geom_line()

## ----------------------------------------------------
yoctopuce_hour.file <- 
  system.file("extdata", "yoctopuce-data.csv", 
              package = "photobiologyInOut", mustWork = TRUE)

## ----eval=FALSE--------------------------------------
# # not run
# read_lines(yoctopuce_hour.file, n_max = 10)

## ----------------------------------------------------
hour.dat <- read_yoctopuce_csv(file = yoctopuce_hour.file)
ggplot(hour.dat, aes(ISO.time, temperature.avg)) + geom_line()

## ----------------------------------------------------
tuv.file <- 
  system.file("extdata", "usrout.txt", 
              package = "photobiologyInOut", mustWork = TRUE)
tuv.spct <- read_tuv_usrout(file = tuv.file,
                            date = ymd("2014-03-21"))
summary(subset(tuv.spct, spct.idx == "A"))
tuv.spct

## ----fig.height=10-----------------------------------
autoplot(tuv.spct, annotations = c("colour.guide")) +
  facet_wrap(~as.character(date), ncol = 2)

## ----------------------------------------------------
tuv.mspct <- subset2mspct(tuv.spct)
summary(tuv.mspct)
autoplot(tuv.mspct)

## ----------------------------------------------------
tuv_nd.spct <- read_tuv_usrout(file = tuv.file)
when_measured(tuv_nd.spct)

## ----------------------------------------------------
qtuv.file <- 
  system.file("extdata", "qtuv.txt", 
              package = "photobiologyInOut", mustWork = TRUE)
qtuv.spct <- read_qtuv_txt(file = qtuv.file)
summary(qtuv.spct)
qtuv.spct

## ----------------------------------------------------
uvspec.2col.file <- 
  system.file("extdata", "uvspec-plain-2col.dat", 
              package = "photobiologyInOut", mustWork = TRUE)
lrt.df <- read.table(file = uvspec.2col.file,
                     col.names = c("w.length", "s.e.irrad"))
uvspec.01.spct <- source_spct(w.length = lrt.df$w.length,
                               s.e.irrad = lrt.df$s.e.irrad * 1e-3)
summary(uvspec.01.spct)
cat(comment(uvspec.01.spct))
autoplot(uvspec.01.spct, range = c(250, 2500), unit.out = "photon")

## ----------------------------------------------------
uvspec.disort.file <- 
  system.file("extdata", "uvspec-disort.dat", 
              package = "photobiologyInOut", mustWork = TRUE)
uvspec.02.spct <- read_uvspec_disort(uvspec.disort.file)
summary(uvspec.02.spct)
cat(comment(uvspec.02.spct))
autoplot(uvspec.02.spct, unit.out = "photon")

## ----------------------------------------------------
ggplot(uvspec.02.spct) +
  geom_line() +
  geom_line(aes(y = s.e.irrad.diff), linetype = "dashed")

## ----------------------------------------------------
uvspec.disort.inp.file <- 
  system.file("extdata", "uvspec-disort.inp", 
              package = "photobiologyInOut", mustWork = TRUE)
comment(uvspec.02.spct) <- paste(comment(uvspec.02.spct),
                                 read_file(uvspec.disort.inp.file),
                                 sep = "\n\n")
cat(comment(uvspec.02.spct))

## ----------------------------------------------------
uvspec.multi.file <- 
  system.file("extdata", "uvspec-multi.dat", 
              package = "photobiologyInOut", mustWork = TRUE)
lbr.multi.spct <- read_uvspec_disort_vesa(uvspec.multi.file)
print(lbr.multi.spct, n = 5)

## ----------------------------------------------------
fmi.file <- 
  system.file("extdata", "2014-08-21_cum.hel", 
              package = "photobiologyInOut", mustWork = TRUE)
z.spct <- read_fmi_cum(fmi.file)
class_spct(z.spct)
when_measured(z.spct)
z.spct

## ----------------------------------------------------
fmi.files <- 
  system.file("extdata", c("2014-08-21_cum.hel", "2014-08-21_cum.hel"),
              package = "photobiologyInOut", mustWork = TRUE)
z.mspct <- read_m_fmi_cum(fmi.files)
class(z.mspct)
when_measured(z.mspct)
z.mspct

## ----eval=FALSE--------------------------------------
# fmi.files <- list.files(".", "*cum.hel")
# fmi.files <- paste(".", fmi.files, sep = "")
# z1.mspct <- read_m_fmi_cum(fmi.files)
# class(z1.mspct)
# when_measured(z1.mspct)
# z1.mspct

## ----message=FALSE-----------------------------------
# because of Google's query limits call will frequently fail without a key
# my.geocode <- ggmap::geocode("Kumpula, Helsinki, Finland", source = "google")
my.geocode <- data.frame(lon = 24.96474, lat = 60.20911)
z2.mspct <-
  read_m_fmi_cum(fmi.files,
                 geocode = my.geocode)
class(z2.mspct)
when_measured(z2.mspct)
where_measured(z2.mspct)
z2.mspct

## ----------------------------------------------------
fmi.file <- 
  system.file("extdata", "2013-05-01.hel", 
              package = "photobiologyInOut", mustWork = TRUE)
z3.mspct <- read_fmi2mspct(fmi.file)
class(z3.mspct)[1:2]
when_measured(z3.mspct[[1]])
length(z3.mspct)
names(z3.mspct)
when_measured(z3.mspct[[1]])
what_measured(z3.mspct[[1]])

## ----------------------------------------------------
fred.file <- 
  system.file("extdata", "FReDflowerID_157.csv", 
              package = "photobiologyInOut", mustWork = TRUE)
fred.spct <- read_FReD_csv(file = fred.file, 
                           label = "Gazania heterochaeta",
                           geocode = data.frame(lat = -28.8751, lon = 17.2293))

## ----------------------------------------------------
fred.spct
cat(comment(fred.spct))
autoplot(fred.spct)

## ----------------------------------------------------
aster.file <- 
  system.file("extdata", "drygrass-spectrum.txt", 
              package = "photobiologyInOut", mustWork = TRUE)
aster.spct <- read_ASTER_txt(file = aster.file)

## ----------------------------------------------------
aster.spct
cat(comment(aster.spct))
autoplot(aster.spct)

## ----eval=eval_hyperSpec-----------------------------
z2.hspct <- mspct2hyperSpec(z2.mspct, "s.e.irrad")
class(z2.hspct)
# plot(z2.hspct)

## ----eval=eval_hyperSpec-----------------------------
data(laser)
class(laser)
laser
plot(laser)

## ----eval=eval_hyperSpec-----------------------------
wl(laser) <- list (
  wl = 1e7 / (1/405e-7 - wl (laser)),
  label = expression (lambda / nm)
)
laser
plot(laser)
laser.mspct <-
  hyperSpec2mspct(laser, "source_spct", "s.e.irrad", multiplier = 1e-3)
ggplot(laser.mspct[[1]]) +
  geom_line() +
  stat_peaks(geom = "text", vjust = -1, label.fmt = "%.6g nm", color = "red")

## ----eval = eval_colorSpec---------------------------
# bug that needs to be fixed
fluorescent.mspct <- colorSpec2mspct(colorSpec::Fs.5nm)
print(fluorescent.mspct, n = 3, n.members = 3)

## ----eval = eval_colorSpec---------------------------
colorSpec2mspct(colorSpec::Hoya)

## ----eval = eval_colorSpec---------------------------
fluorescent.spct <- colorSpec2spct(colorSpec::Fs.5nm)
autoplot(fluorescent.spct, annotations = "")

## ----eval = eval_colorSpec---------------------------
colorSpec2chroma_spct(colorSpec::xyz1931.5nm)

## ----eval = eval_colorSpec---------------------------
sun.cspec <- spct2colorSpec(sun.spct)
plot(sun.cspec, col = "blue")

## ----eval = eval_colorSpec---------------------------
spct2colorSpec(yellow_gel.spct)

## ----eval = eval_colorSpec---------------------------
chroma_spct2colorSpec(beesxyzCMF.spct)

## ----------------------------------------------------
spct_CCT(white_led.source_spct) # correlated color temperature
spct_CRI(white_led.source_spct) # color rendition index
spct_CRI(white_led.source_spct, named = TRUE)
spct_SSI(white_led.source_spct, sun.spct) # spectral similarity index

## ----eval = eval_pavo--------------------------------
data(sicalis)
class(sicalis)
names(sicalis)

## ----eval = eval_pavo--------------------------------
sicalis.mspct <- rspec2mspct(sicalis, "reflector_spct", "Rpc")
summary(sicalis.mspct[[1]])
summary(sicalis.mspct[[2]])
summary(sicalis.mspct[[3]])

## ----eval = eval_pavo--------------------------------
ggplot(rbindspct(sicalis.mspct[1:3])) +
  aes(linetype = spct.idx) +
  ylim(0,0.3) +
  geom_line()

## ----eval = eval_pavo--------------------------------
print(sicalis.mspct[c(TRUE, FALSE, FALSE)])
ggplot(rbindspct(sicalis.mspct[c(TRUE, FALSE, FALSE)])) +
  aes(linetype = spct.idx) +
  ylim(0,0.15) +
  geom_line() +
  ggtitle("'crown' reflectance spectra")

## ----eval = eval_pavo--------------------------------
refl.by.band <- reflectance(sicalis.mspct, w.band = list(Red(), Green(), Blue(), UVA()))
refl.by.band$body.part <- rep(c("crown", "throat", "breast"), 7)

## ----eval = eval_pavo--------------------------------
refl.red <- reflectance(sicalis.mspct, w.band = Red())
names(refl.red)[2] <- "red.reflectance"
refl.red$body.part <- rep(c("crown", "throat", "breast"), 7)
ggplot(refl.red, aes(x = body.part, y = red.reflectance)) +
  stat_summary(fun.data = "mean_se", color = "red") +
  geom_point(alpha = 0.5)

## ----------------------------------------------------
jaz.irrad.comma.file <- 
  system.file("extdata", "spectrum-comma.JazIrrad", 
              package = "photobiologyInOut", mustWork = TRUE)
my.locale <- locale(decimal_mark = ",", tz = "Europe/Helsinki")
jaz00.spct <- read_oo_jazirrad(file = jaz.irrad.comma.file,
                               locale = my.locale)

## ----------------------------------------------------
jaz00.spct

## ----------------------------------------------------
jaz.s.irrad.file <- 
  system.file("extdata", "spectrum.JazIrrad", 
              package = "photobiologyInOut", mustWork = TRUE)

## ----warning=FALSE-----------------------------------
jaz01.spct <- read_oo_jazirrad(file = jaz.s.irrad.file,
                               date = NULL)
when_measured(jaz01.spct)

## ----warning=FALSE-----------------------------------
jaz02.spct <- read_oo_jazirrad(file = jaz.s.irrad.file,
                               date = ymd_hms("2015-11-15 12:00:00"))
when_measured(jaz02.spct)

## ----warning=FALSE-----------------------------------
jaz03.spct <- read_oo_jazirrad(file = jaz.s.irrad.file,
                               date = now())
when_measured(jaz03.spct)

## ----message=FALSE,warning=FALSE---------------------
my.geocode <- data.frame(lon = 25.02006, lat = 60.22525)
jaz04.spct <- read_oo_jazirrad(file = jaz.s.irrad.file,
                               geocode = my.geocode)
jaz04.spct
where_measured(jaz04.spct)

