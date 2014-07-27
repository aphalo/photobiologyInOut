library(photobiologyInOut)

oldwd <- setwd("test")

test.ls <- read.xlsx("204.xlsx")
D65.spct <- test.ls[["D65"]][-(1:3), 1:2]
names(D65.spct) <- c("w.length", "s.e.irrad")
setSourceSpct(D65.spct)
D65.spct[ , w.length := as.integer(w.length)]
D65.spct[ , s.e.irrad := as.numeric(s.e.irrad)]
setkey(D65.spct, w.length)
D65.dt <- setDT(copy(D65.spct))
class(D65.dt)
D65.data <- setDF(copy(D65.spct))
class(D65.data)
setSourceSpct(D65.spct)
class(D65.spct)
save(D65.spct, D65.dt, D65.data, file="D65.data.rda")
setwd(oldwd)
