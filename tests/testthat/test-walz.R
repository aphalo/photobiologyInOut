library("photobiology")
library("lubridate")

context("read Walz LSA-2050 xlsx file")

test_that("consolidated data are read correctly", {
  
  file.name <- 
    system.file("extdata", "walz-lsa-2050-prunus.xlsx",
                package = "photobiologyInOut",
                mustWork = TRUE)
  lsa_default.df <- read_walz_lsa_xlsx(file.name)
  
  expect_equal(lsa_default.df,
               read_walz_lsa_xlsx(file.name, 
                                  returned.data = "consolidated"))
  
  expect_s3_class(lsa_default.df, "data.frame")
  expect_equal(nrow(lsa_default.df), 20)
  expect_equal(ncol(lsa_default.df), 44)
  expect_equal(colnames(lsa_default.df),
               c("Time", "Type", "Number", "Marker", "IFo", "IFm", "Fo", "Fm", 
               "I310", "I365", "I450", "I530", "I615", "F310", "F365", "F450",
               "F530", "F615", "FV.FM", "Q310", "Q365", "Q450", "Q530",
               "AFLAV", "AANTH", "I700", "I770", "Transmittance", "Absorbance", 
               "nmol.cm2", "µg.cm2", "NBI", "Satellite.num", "DoP", 
               "Latitude", "Longitude", "Height", "Leaf.Az.", "Leaf.Sl.", 
               "Sun.Az.", "Sun.Elev.", "AoI", "Incidence", "SAT.F.ls"))
  expect_true(all(sapply(lsa_default.df[ , -c(1:4, 44)], class) == "numeric"))
  expect_type(lsa_default.df[["SAT.F.ls"]], "list")
  expect_s3_class(lsa_default.df[["SAT.F.ls"]], "AsIs")
  expect_s3_class(lsa_default.df[["Time"]], "POSIXct")

  expect_s3_class(lsa_default.df[["SAT.F.ls"]][[1]], "data.frame")
  expect_named(lsa_default.df[["SAT.F.ls"]][[1]], c( "t", "SAT.F"))
  expect_true(all(sapply(lsa_default.df[["SAT.F.ls"]][[1]], class) == "numeric"))
  expect_true(!any(is.na(lsa_default.df[["SAT.F.ls"]][[1]])))
})

test_that("visible data are read correctly", {
  
  file.name <- 
    system.file("extdata", "walz-lsa-2050-prunus.xlsx",
                package = "photobiologyInOut",
                mustWork = TRUE)
  lsa_default.df <- read_walz_lsa_xlsx(file.name,
                                       returned.data = "visible")
  
  expect_s3_class(lsa_default.df, "data.frame")
  expect_equal(nrow(lsa_default.df), 20)
  expect_equal(ncol(lsa_default.df), 17)
  expect_equal(colnames(lsa_default.df),
               c("Time", "Type", "Number", "Marker", "Fo", "Fm", 
                 "FV.FM", "Q310", "Q365", "Q450", "Q530",
                 "AFLAV", "AANTH",
                 "nmol.cm2", "µg.cm2", "NBI", "Incidence"))
  expect_true(all(sapply(lsa_default.df[ , -c(1:4)], class) == "numeric"))
  expect_s3_class(lsa_default.df[["Time"]], "POSIXct")
})

test_that("measured data are read correctly", {
  
  file.name <- 
    system.file("extdata", "walz-lsa-2050-prunus.xlsx",
                package = "photobiologyInOut",
                mustWork = TRUE)
  lsa_default.df <- read_walz_lsa_xlsx(file.name,
                                       returned.data = "measured")
  
  expect_s3_class(lsa_default.df, "data.frame")
  expect_equal(nrow(lsa_default.df), 20)
  expect_equal(ncol(lsa_default.df), 43)
  expect_equal(colnames(lsa_default.df),
               c("Time", "Type", "Number", "Marker", "IFo", "IFm", "Fo", "Fm", 
                 "I310", "I365", "I450", "I530", "I615", "F310", "F365", "F450",
                 "F530", "F615", "FV.FM", "Q310", "Q365", "Q450", "Q530",
                 "AFLAV", "AANTH", "I700", "I770", "Transmittance", "Absorbance", 
                 "nmol.cm2", "µg.cm2", "NBI", "Satellite.num", "DoP", 
                 "Latitude", "Longitude", "Height", "Leaf.Az.", "Leaf.Sl.", 
                 "Sun.Az.", "Sun.Elev.", "AoI", "Incidence"))
  expect_true(all(sapply(lsa_default.df[ , -c(1:4)], class) == "numeric"))
  expect_s3_class(lsa_default.df[["Time"]], "POSIXct")
})

test_that("raw data are read correctly", {
  
  file.name <- 
    system.file("extdata", "walz-lsa-2050-prunus.xlsx",
                package = "photobiologyInOut",
                mustWork = TRUE)
  lsa_default.df <- read_walz_lsa_xlsx(file.name,
                                       returned.data = "raw")
  
  expect_s3_class(lsa_default.df, "data.frame")
  expect_equal(nrow(lsa_default.df), 20)
  expect_equal(ncol(lsa_default.df), 24)
  expect_equal(colnames(lsa_default.df),
               c("Time", "Type", "Number", "Marker", "IFo", "IFm", 
                 "I310", "I365", "I450", "I530", "I615", "I700", "I770", 
                 "Satellite.num", "DoP", 
                 "Latitude", "Longitude", "Height", "Leaf.Az.", "Leaf.Sl.", 
                 "Sun.Az.", "Sun.Elev.", "AoI", "SAT.F.ls"))
  expect_true(all(sapply(lsa_default.df[ , -c(1:4, 24)], class) == "numeric"))
  expect_type(lsa_default.df[["SAT.F.ls"]], "list")
  expect_s3_class(lsa_default.df[["SAT.F.ls"]], "AsIs")
  expect_s3_class(lsa_default.df[["Time"]], "POSIXct")
})

test_that("data are read correctly into list", {
  
  file.name <- 
    system.file("extdata", "walz-lsa-2050-prunus.xlsx",
                package = "photobiologyInOut",
                mustWork = TRUE)
  lsa_default.ls <- read_walz_lsa_xlsx(file.name,
                                       returned.data = "list")
  
  expect_type(lsa_default.ls, "list")
  expect_equal(length(lsa_default.ls), 2)
  expect_named(lsa_default.ls, c("Measure", "SAT Chart"))
  
  expect_equal(nrow(lsa_default.ls[[1]]), 20)
  expect_equal(ncol(lsa_default.ls[[1]]), 43)
  expect_equal(colnames(lsa_default.ls[[1]]),
               c("Time", "Type", "Number", "Marker", "IFo", "IFm", "Fo", "Fm", 
                 "I310", "I365", "I450", "I530", "I615", "F310", "F365", "F450",
                 "F530", "F615", "FV.FM", "Q310", "Q365", "Q450", "Q530",
                 "AFLAV", "AANTH", "I700", "I770", "Transmittance", "Absorbance", 
                 "nmol.cm2", "µg.cm2", "NBI", "Satellite.num", "DoP", 
                 "Latitude", "Longitude", "Height", "Leaf.Az.", "Leaf.Sl.", 
                 "Sun.Az.", "Sun.Elev.", "AoI", "Incidence"))
  expect_true(all(sapply(lsa_default.ls[[1]][ , -c(1:4)], class) == "numeric"))
  expect_s3_class(lsa_default.ls[[1]][["Time"]], "POSIXct")
  
  expect_equal(nrow(lsa_default.ls[[2]]), 20)
  expect_equal(ncol(lsa_default.ls[[2]]), 51)
  expect_equal(colnames(lsa_default.ls[[2]]),
               c("Time", "0", "50", "100", "150", "200", "250", "300", "350",
                 "400", "450", "500", "550", "600", "650", "700", "750", "800",
                 "850", "900", "950", "1000", "1050", "1100", "1150", "1200", 
                 "1250", "1300", "1350", "1400", "1450", "1500", "1550", "1600",
                 "1650", "1700", "1750", "1800", "1850", "1900", "1950", "2000",
                 "2050", "2100", "2150", "2200", "2250", "2300", "2350", "2400",
                 "2450"))
  expect_true(all(sapply(lsa_default.ls[[2]][ , -1], class) == "numeric"))
  expect_s3_class(lsa_default.ls[[2]][["Time"]], "POSIXct")
})
