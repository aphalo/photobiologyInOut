library(ggspectra)
library(photobiologyWavebands)

OL.df <- read.table("optronics-OL731-2.txt", # nombre del archivo
                    skip = 36, # number of rows to skip 
                    col.names = c("w.length", "s.e.irrad", "x"))[ , -3]
OL.df[["s.e.irrad"]] <- OL.df[["s.e.irrad"]] * 10000 # W/cm-2/nm -> W/m2/nm
OL.spct <- as.source_spct(OL.df)

# figura
autoplot(OL.spct, unit.out = "photon")

# irradiancias en umol m-2 s-1
q_irrad(OL.spct, UV_bands(), scale.factor = 1e6)
q_irrad(OL.spct, PAR(), scale.factor = 1e6)

# irradiancias biológicamente efectivas en W m-2
e_irrad(OL.spct, list(PG(), CIE(), GEN_G()))

