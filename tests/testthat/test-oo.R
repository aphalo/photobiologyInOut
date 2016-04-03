library("photobiology")
library("photobiologyInOut")
library("lubridate")

context("read Ocean Optics")

test_that("jazz", {

  suppressWarnings(jaz.spct <- read_oo_jazirrad(file = "data-test/spectrum.JazIrrad"))
  
  expect_equal(nrow(jaz.spct), 2048)
  expect_equal(ncol(jaz.spct), 2)
  expect_equal(jaz.spct[1, 1], 188.8252, tolerance = 0.0001)
  expect_equal(jaz.spct[2048, 1], 1033.148, tolerance = 0.0001)
  expect_is(jaz.spct[[1]], "numeric")
  expect_equal(sum(is.na(jaz.spct[[1]])), 0)
  expect_true(all(sign(jaz.spct[[1]]) > 0))
  expect_is(jaz.spct[[2]], "numeric")
  expect_equal(sum(is.na(jaz.spct[[2]])), 0)
  expect_is(jaz.spct, "source_spct")
  expect_named(jaz.spct, c("w.length", "s.e.irrad"))
  expect_equal(as.numeric(getWhenMeasured(jaz.spct)), 
               as.numeric(ymd_hms("2015-02-03 07:44:41")))
  expect_equal(getWhereMeasured(jaz.spct), 
               data.frame(lon = NA_real_, lat = NA_real_))
  expect_equal(getWhatMeasured(jaz.spct), NA)
  expect_equal(getTimeUnit(jaz.spct), "second")
  expect_gt(length(comment(jaz.spct)), 0)
})


test_that("SpectraSuite", {
  
  suppressWarnings(ss.spct <- read_oo_sstxt(file = "data-test/spectrum.SSIrrad"))
  
  expect_equal(nrow(ss.spct), 1044)
  expect_equal(ncol(ss.spct), 2)
  expect_equal(ss.spct[1, 1], 199.08)
  expect_equal(ss.spct[1044, 1], 998.61)
  expect_is(ss.spct[[1]], "numeric")
  expect_equal(sum(is.na(ss.spct[[1]])), 0)
  expect_true(all(sign(ss.spct[[1]]) > 0))
  expect_is(ss.spct[[2]], "numeric")
  expect_equal(sum(is.na(ss.spct[[2]])), 0)
  expect_is(ss.spct, "source_spct")
  expect_named(ss.spct, c("w.length", "s.e.irrad"))
  expect_equal(as.numeric(getWhenMeasured(ss.spct)), 
               as.numeric(ymd_hms("2013-05-06 13:13:40")))
  expect_equal(getWhereMeasured(ss.spct), 
               data.frame(lon = NA_real_, lat = NA_real_))
  expect_equal(getWhatMeasured(ss.spct), NA)
  expect_equal(getTimeUnit(ss.spct), "second")
  expect_gt(length(comment(ss.spct)), 0)
})
