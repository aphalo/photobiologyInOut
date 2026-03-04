library("photobiology")
library("lubridate")

context("read ASD ascii file tab separated")

test_that("irradiance is read correctly", {

  file.name <- 
    system.file("extdata", "irrad-sky.asd.txt", 
                package = "photobiologyInOut", mustWork = TRUE)
  asd.source_spct <- read_asd_tsv(file = file.name)
  
  expect_equal(nrow(asd.source_spct), 2150)
  expect_equal(ncol(asd.source_spct), 2)
  expect_equal(asd.source_spct[1, 1],  351, tolerance = 0.001)
  expect_equal(asd.source_spct[2150, 1], 2500, tolerance = 0.001)
  expect_is(asd.source_spct[[1]], "numeric")
  expect_equal(sum(is.na(asd.source_spct[[1]])), 0)
  expect_true(all(sign(asd.source_spct[[1]]) > 0))
  expect_is(asd.source_spct[[2]], "numeric")
#  expect_true(all(sign(asd.source_spct[[2]]) >= 0)) # data are not clean
  expect_equal(sum(is.na(asd.source_spct[[2]])), 0)
  expect_is(asd.source_spct, "source_spct")
  expect_named(asd.source_spct, c("w.length", "s.e.irrad"))
  expect_equal(getWhenMeasured(asd.source_spct), 
               ymd_hms("2024-05-21 11:33:07 UTC"))
  expect_equivalent(getWhereMeasured(asd.source_spct), 
                    data.frame(lon = NA_real_, lat = NA_real_, address = NA_character_, 
                               stringsAsFactors = FALSE))
  expect_gt(length(getWhatMeasured(asd.source_spct)), 0)
  expect_gt(length(comment(asd.source_spct)), 0)
  expect_equal(as.numeric(e_irrad(asd.source_spct, w.band = c(351, 2000))), 
               526.0794, tolerance = 0.001)
  expect_equal(labels(e_irrad(asd.source_spct, w.band = c(351, 2000))), 
               "E_range.351.2000")
})

test_that("reflectance is read correctly", {
  
  file.name <- 
    system.file("extdata", "reflec-panel-50pc.asd.txt", 
                package = "photobiologyInOut", mustWork = TRUE)
  asd.reflector_spct <- read_asd_tsv(file = file.name)
  
  expect_equal(nrow(asd.reflector_spct), 2150)
  expect_equal(ncol(asd.reflector_spct), 2)
  expect_equal(asd.reflector_spct[1, 1],  351, tolerance = 0.001)
  expect_equal(asd.reflector_spct[2150, 1], 2500, tolerance = 0.001)
  expect_is(asd.reflector_spct[[1]], "numeric")
  expect_equal(sum(is.na(asd.reflector_spct[[1]])), 0)
  expect_true(all(sign(asd.reflector_spct[[1]]) > 0))
  expect_is(asd.reflector_spct[[2]], "numeric")
  #  expect_true(all(sign(asd.reflector_spct[[2]]) >= 0)) # data are not clean
  expect_equal(sum(is.na(asd.reflector_spct[[2]])), 0)
  expect_is(asd.reflector_spct, "reflector_spct")
  expect_named(asd.reflector_spct, c("w.length", "Rfr"))
  expect_equal(getWhenMeasured(asd.reflector_spct), 
               ymd_hms("2024-05-21 11:14:40 UTC"))
  expect_equivalent(getWhereMeasured(asd.reflector_spct), 
                    data.frame(lon = NA_real_, lat = NA_real_, address = NA_character_, 
                               stringsAsFactors = FALSE))
  expect_gt(length(getWhatMeasured(asd.reflector_spct)), 0)
  expect_gt(length(comment(asd.reflector_spct)), 0)
  expect_equal(as.numeric(reflectance(clip_wl(asd.reflector_spct, c(351, 1300)))), 
               0.4883091, tolerance = 0.0000001)
  expect_equal(labels(reflectance(clip_wl(asd.reflector_spct, c(351, 1300)))), 
               "Rfr(wl)_Total")
})

test_that("raw-counts are read correctly", {
  
  file.name <- 
    system.file("extdata", "DN-gravel.asd.txt", 
                package = "photobiologyInOut", mustWork = TRUE)
  asd.raw_spct <- read_asd_tsv(file = file.name)
  
  expect_equal(nrow(asd.raw_spct), 2150)
  expect_equal(ncol(asd.raw_spct), 2)
  expect_equal(asd.raw_spct[1, 1],  351, tolerance = 0.001)
  expect_equal(asd.raw_spct[2150, 1], 2500, tolerance = 0.001)
  expect_is(asd.raw_spct[[1]], "numeric")
  expect_equal(sum(is.na(asd.raw_spct[[1]])), 0)
  expect_true(all(sign(asd.raw_spct[[1]]) > 0))
  expect_is(asd.raw_spct[[2]], "numeric")
  #  expect_true(all(sign(asd.raw_spct[[2]]) >= 0)) # data are not clean
  expect_equal(sum(is.na(asd.raw_spct[[2]])), 0)
  expect_is(asd.raw_spct, "raw_spct")
  expect_named(asd.raw_spct, c("w.length", "counts"))
  expect_equal(getWhenMeasured(asd.raw_spct), 
               ymd_hms("2024-05-23 17:17:01 UTC"))
  expect_equivalent(getWhereMeasured(asd.raw_spct), 
                    data.frame(lon = NA_real_, lat = NA_real_, address = NA_character_, 
                               stringsAsFactors = FALSE))
  expect_gt(length(getWhatMeasured(asd.raw_spct)), 0)
  expect_gt(length(comment(asd.raw_spct)), 0)
})

test_that("arbitrary data is read correctly", {
  
  file.name <- 
    system.file("extdata", "DN-gravel.asd.txt", 
                package = "photobiologyInOut", mustWork = TRUE)
  asd.generic_spct <- read_asd_tsv(file = file.name, s.qty = "zz")
  
  expect_equal(nrow(asd.generic_spct), 2150)
  expect_equal(ncol(asd.generic_spct), 2)
  expect_equal(asd.generic_spct[1, 1, drop = TRUE],  351, tolerance = 0.001)
  expect_equal(asd.generic_spct[2150, 1, drop = TRUE], 2500, tolerance = 0.001)
  expect_is(asd.generic_spct[[1]], "numeric")
  expect_equal(sum(is.na(asd.generic_spct[[1]])), 0)
  expect_true(all(sign(asd.generic_spct[[1]]) > 0))
  expect_is(asd.generic_spct[[2]], "numeric")
  #  expect_true(all(sign(asd.generic_spct[[2]]) >= 0)) # data are not clean
  expect_equal(sum(is.na(asd.generic_spct[[2]])), 0)
  expect_is(asd.generic_spct, "generic_spct")
  expect_named(asd.generic_spct, c("w.length", "zz"))
  expect_equal(getWhenMeasured(asd.generic_spct), 
               ymd_hms("2024-05-23 17:17:01 UTC"))
  expect_equivalent(getWhereMeasured(asd.generic_spct), 
                    data.frame(lon = NA_real_, lat = NA_real_, address = NA_character_, 
                               stringsAsFactors = FALSE))
  expect_gt(length(getWhatMeasured(asd.generic_spct)), 0)
  expect_gt(length(comment(asd.generic_spct)), 0)
})

