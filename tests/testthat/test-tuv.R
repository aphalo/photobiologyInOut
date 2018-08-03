library("photobiology")
library("photobiologyInOut")
library("lubridate")
library("readr")

context("import from TUV")

test_that("read Quick TUV", {

  file.name <- 
    system.file("extdata", "qtuv.txt", 
                package = "photobiologyInOut", mustWork = TRUE)

  qtuv.spct <- 
    read_qtuv_txt(file = file.name,
                  ozone.du = 300)
  
  expect_equal(nrow(qtuv.spct), 140L)
  expect_equal(ncol(qtuv.spct), 7L)
  expect_equal(qtuv.spct[["w.length"]][1L], 280.5, tolerance = 0.0001)
  expect_equal(qtuv.spct[["w.length"]][140L], 419.5, tolerance = 0.0001)
  expect_is(qtuv.spct[["w.length"]], "numeric")
  expect_equal(sum(is.na(qtuv.spct[["w.length"]])), 0)
  expect_true(all(sign(qtuv.spct[["w.length"]]) > 0))
  expect_is(qtuv.spct[[2]], "numeric")
  expect_equal(sum(is.na(qtuv.spct[["s.e.irrad"]])), 0)
  expect_is(qtuv.spct, "source_spct")
  expect_named(
    qtuv.spct,
    c(
      "w.length",
      "s.e.irrad",
      "s.e.irrad.dir",
      "s.e.irrad.diff.down",
      "s.e.irrad.diff.up",
      "angle",
      "date"
    )
  )
  expect_equal(format(getWhenMeasured(qtuv.spct)),
               format(ymd("2015-06-30")))
  expect_true(all(is.na(getWhereMeasured(qtuv.spct))))
  expect_named(getWhereMeasured(qtuv.spct), c("lon", "lat"))
  expect_equal(getWhatMeasured(qtuv.spct), 
               "Quick TUV spectral simulation File: qtuv.txt ")
  expect_equal(getTimeUnit(qtuv.spct), "second")
  expect_equal(length(comment(qtuv.spct)), 1L)

  
  file.name <- 
    system.file("extdata", "qtuv-long.txt", 
                package = "photobiologyInOut", mustWork = TRUE)
  
  qtuv_long.spct <- 
    read_qtuv_txt(file = file.name,
                  ozone.du = 300)
  
  expect_equal(nrow(qtuv_long.spct), 500L)
  expect_equal(ncol(qtuv_long.spct), 7L)
  expect_equal(qtuv_long.spct[["w.length"]][1L], 280.5, tolerance = 0.0001)
  expect_equal(qtuv_long.spct[["w.length"]][500L], 779.5, tolerance = 0.0001)
  expect_is(qtuv_long.spct[["w.length"]], "numeric")
  expect_equal(sum(is.na(qtuv_long.spct[["w.length"]])), 0)
  expect_true(all(sign(qtuv_long.spct[["w.length"]]) > 0))
  expect_is(qtuv_long.spct[["s.e.irrad"]], "numeric")
  expect_equal(sum(is.na(qtuv_long.spct[["s.e.irrad"]])), 0)
  expect_is(qtuv_long.spct, "source_spct")
  expect_named(
    qtuv_long.spct,
    c(
      "w.length",
      "s.e.irrad",
      "s.e.irrad.dir",
      "s.e.irrad.diff.down",
      "s.e.irrad.diff.up",
      "angle",
      "date"
    )
  )
  expect_equal(getWhenMeasured(qtuv_long.spct),
               ymd_hms("2018-08-01 12:00:00", tz = "UTC"))
  expect_equal(getWhereMeasured(qtuv_long.spct), 
               data.frame(lon = -15, lat = 60))
  expect_equal(getWhatMeasured(qtuv_long.spct), 
               "Quick TUV spectral simulation File: qtuv-long.txt ")
  expect_equal(getTimeUnit(qtuv_long.spct), "second")
  expect_equal(length(comment(qtuv_long.spct)), 1)
  
  file.name <- 
    system.file("extdata", "qtuv-no-spct.txt", 
                package = "photobiologyInOut", mustWork = TRUE)
  
  expect_warning(qtuv_empty.spct <- 
                   read_qtuv_txt(file = file.name,
                                 ozone.du = 300))

  expect_equal(nrow(qtuv_empty.spct), 0L)
  expect_equal(ncol(qtuv_empty.spct), 2L)
  expect_is(qtuv_empty.spct[["w.length"]], "numeric")
  expect_is(qtuv_empty.spct[["s.e.irrad"]], "numeric")
  expect_is(qtuv_empty.spct, "source_spct")
  expect_named(qtuv_empty.spct, c("w.length", "s.e.irrad"))
  expect_true(all(is.na(getWhenMeasured(qtuv_empty.spct))))
  expect_true(all(is.na(getWhereMeasured(qtuv_empty.spct))))
  expect_named(getWhereMeasured(qtuv_empty.spct), c("lon", "lat"))
  expect_true(is.na(getWhatMeasured(qtuv_empty.spct)))
  expect_equal(getTimeUnit(qtuv_empty.spct), "second")
  expect_equal(length(comment(qtuv_empty.spct)), 0L)
  
})



test_that("read TUV", {
  
  file.name <- 
    system.file("extdata", "usrout.txt", 
                package = "photobiologyInOut", mustWork = TRUE)

  usrout.spct <- read_tuv_usrout(file = file.name,
                                 ozone.du = 300,
                                 geocode = data.frame(lon = -15, lat = 60),
                                 tz = "UTC")
  
  expect_equal(nrow(usrout.spct), 3856)
  expect_equal(ncol(usrout.spct), 5)
  expect_equal(usrout.spct[["w.length"]][1], 280.5, tolerance = 0.000001)
  expect_equal(usrout.spct[["w.length"]][3856], 761.5, tolerance = 0.000001)
  expect_is(usrout.spct[["w.length"]], "numeric")
  expect_equal(sum(is.na(usrout.spct[["w.length"]])), 0)
  expect_true(all(sign(usrout.spct[["w.length"]]) > 0))
  expect_is(usrout.spct[["s.e.irrad"]], "numeric")
  expect_equal(sum(is.na(usrout.spct[["s.e.irrad"]])), 0)
  expect_is(usrout.spct, "source_spct")
  expect_named(usrout.spct, c("w.length", "spct.idx", "s.e.irrad",      
                               "angle", "date"))
  expect_equal(length(getWhenMeasured(usrout.spct)), 8L)
  expect_equal(getWhereMeasured(usrout.spct), 
               data.frame(lon = -15, lat = 60))
  expect_equal(getWhatMeasured(usrout.spct), "TUV spectral simulation File: usrout.txt ")
  expect_equal(length(comment(usrout.spct)), 1L)
  
  
  usrouta.spct <- read_tuv_usrout(file = file.name,
                                 ozone.du = 300,
                                 tz = "UTC")
  
  expect_true(all(is.na(getWhereMeasured(usrouta.spct))))
  expect_named(getWhereMeasured(usrouta.spct), c("lon", "lat"))
  
  file.name <- 
    system.file("extdata", "usrout2.txt", 
                package = "photobiologyInOut", mustWork = TRUE)
  
  usrout2.spct <- read_tuv_usrout(file = file.name,
                                 ozone.du = 300,
                                 geocode = data.frame(lon = -15, lat = 60),
                                 tz = "UTC")
  
  expect_equal(nrow(usrout2.spct), 12220)
  expect_equal(ncol(usrout2.spct), 5)
  expect_equal(usrout2.spct[["w.length"]][1], 290.25, tolerance = 0.000001)
  expect_equal(usrout2.spct[["w.length"]][12220], 759.75, tolerance = 0.000001)
  expect_is(usrout2.spct[["w.length"]], "numeric")
  expect_equal(sum(is.na(usrout2.spct[["w.length"]])), 0)
  expect_true(all(sign(usrout2.spct[["w.length"]]) > 0))
  expect_is(usrout2.spct[["s.e.irrad"]], "numeric")
  expect_equal(sum(is.na(usrout2.spct[["s.e.irrad"]])), 0)
  expect_is(usrout2.spct, "source_spct")
  expect_named(usrout2.spct, c("w.length", "spct.idx", "s.e.irrad",      
                              "angle", "date"))
  expect_equal(length(getWhenMeasured(usrout2.spct)), 13L)
  expect_equal(getWhereMeasured(usrout2.spct), 
               data.frame(lon = -15, lat = 60))
  expect_equal(getWhatMeasured(usrout2.spct), "TUV spectral simulation File: usrout2.txt ")
  expect_equal(length(comment(usrout.spct)), 1L)
})


