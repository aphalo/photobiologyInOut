library("photobiology")
library("photobiologyInOut")
library("lubridate")

context("read ENLIGHTEN .csv file)")

test_that("single spectrum (energy)", {

  file.name <- 
    system.file("extdata", "enlighten-wasatch-raman.csv", 
                package = "photobiologyInOut", mustWork = TRUE)
  wasatch.spct <- read_wasatch_csv(file = file.name)
  
  expect_equal(nrow(wasatch.spct), 1024)
  expect_equal(ncol(wasatch.spct), 2)
  expect_equal(wasatch.spct[1, 1],  792.33, tolerance = 0.01)
  expect_equal(wasatch.spct[1024, 1], 1067.8, tolerance = 0.01)
  expect_is(wasatch.spct[[1]], "numeric")
  expect_equal(sum(is.na(wasatch.spct[[1]])), 0)
  expect_true(all(sign(wasatch.spct[[1]]) > 0))
  expect_is(wasatch.spct[[2]], "numeric")
  expect_equal(sum(is.na(wasatch.spct[[2]])), 0)
  expect_is(wasatch.spct, "source_spct")
  expect_named(wasatch.spct, c("w.length", "s.e.irrad"))
  expect_is(getWhenMeasured(wasatch.spct), "POSIXct")
  expect_equivalent(getWhereMeasured(wasatch.spct), 
                    data.frame(lon = NA_real_, lat = NA_real_, address = NA_character_, 
                               stringsAsFactors = FALSE))
  expect_gt(length(getWhatMeasured(wasatch.spct)), 0)
  expect_gt(length(comment(wasatch.spct)), 0)
})

