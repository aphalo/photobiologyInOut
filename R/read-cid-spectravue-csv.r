#' Read File Saved by CID's SpectraVue.
#'
#' Read wavelength and spectral data from Measurements.CSV files exported from CID
#' Bio-Sciences' SpectraVue CI-710s leaf spectrometer, importing them into R.
#' Available metadata is also extracted from the file.
#'
#' \code{read_cid_spectravue_csv()} only accepts "row oriented" CSV files. These
#' may contain multiple spectra, one per row.
#'
#' @param file character
#' @param ... additional arguments passed to the constructor of the `filter_spct`
#'   object.
#' @param scale.factor numeric vector of length 1, or length equal to the number
#'   of data rows (= number of spectra). Numeric multiplier applied to returned
#'   spectral values.
#' @param date a \code{POSIXct} object to use to set the \code{"when.measured"}
#'   attribute. If \code{NULL}, the default, the date and time are extracted
#'   from the file.
#' @param geocode A data frame with columns \code{lon} and \code{lat} used to
#'   set attribute \code{"where.measured"}. If \code{NULL}, the default, the
#'   geocode is extracted from the file, if present, and if \code{NA} the
#'   "where.measured" attribute is not set.
#' @param label character string. If \code{NULL}, the default, the value of the
#'   "tag" present in the file is used, and if \code{NA} the "what.measured"
#'   attribute is not set.
#' @param tz character Time zone is by default that of the machine's locale.
#' @param locale The locale controls defaults that vary from place to place. The
#'   default locale is US-centric (like R), but you can use
#'   \code{\link[readr]{locale}} to create your own locale that controls things
#'   like the default time zone, encoding, decimal mark, big mark, and day/month
#'   names.
#'
#' @details SpectrVue's row-wise spectral Measurements.CSV files contain columns
#'   with metadata on the right edge, followed by columns with data for each
#'   wavelength. The value in column "Mode" indicates the quantity measured,
#'   decoded into Tfr, Rfr or A. The data in each row in the CSV file are read
#'   and stored in a `filter_spct` object. These objects are collected into a
#'   single `filter_mspct` object to be returned.
#'
#' @note SpectraVue creates four files for each measurement, from these, this
#'   function reads the one with name ending in "Measurements.CSV". The first
#'   part of the file name gives the time of measurement.
#'
#' @return An object of class \code{filter_spct}.
#'
#' @export
#' @references \url{https://cid-inc.com/}
#' 
read_cid_spectravue_csv <- function(file,
                                    date = NULL,
                                    geocode = NULL,
                                    label = NULL,
                                    tz = NULL,
                                    locale = readr::default_locale(),
                                    scale.factor = 1,
                                    range = c(380, 1000),
                                    ...) {
  if (!grepl("Measurements.csv$", file, ignore.case = TRUE)) {
    warning("Only processed measurements CSV files from CID's SpectraVue CI-710s",
            "leaf spectrometer are supported.")
    return(photobiology::filter_mspct())
  }
  
  if (is.null(tz)) {
    tz <- locale$tz
  }
  
  if (is.character(date)) {
    date <- anytime::anytime(date, tz = tz)
    message("Dates and times from file overriden by user!")
  }
  
  # read file to memory
  file_data <- readr::read_file(file)

  # read metadata from all rows
  headers <- readr::read_csv(file = file_data, 
                             col_names = TRUE, 
                             skip = 0,
                             col_types = "ccdnnccccd", 
                             col_select = 1:10)
  
  names(headers)[3:5] <- c("integ.time", "boxcar", "num.scans") # ms, number, number

  headers[["var.name"]] <- c(Transmittance = "Tpc",
                             Reflectance = "Rpc",
                             Absorbance = "A",
                             "Abs Reflectance" = "Rpc",
                             "Abs Transmittance" = "Tpc")[headers[["Mode"]]]
  
  headers[["when.measured"]] <- 
    if(is.null(date)) {
      lubridate::mdy_hms(headers[["Date"]], tz = tz)
    } else {
      date
    }
  
  headers[["where.measured"]] <-
    if (is.null(geocode) || is.na(geocode)) {
      photobiology::na_geocode()
    } else {
      geocode
    }
  
  # fill missing Tags
  selector <- is.na(headers[["Tag"]])
  headers[["Tag"]][selector] <- paste("Tag", 1:sum(selector), sep = "")
  
  # read wavelengths from top row
  w.length <- as.numeric(readr::read_csv(file = file_data, 
                                         col_names = FALSE,
                                         n_max = 1, 
                                         skip = 0,
                                         col_types = "d", 
                                         col_select = 11:2058))
  
  # read spectral data starting at row 2
  data <- t(readr::read_csv(file = file_data, 
                                     col_names = FALSE,
                                     skip = 1,
                                     col_types = "d", 
                                     col_select = 11:2058))
  
  zz <- photobiology::generic_mspct()
  for (col in 1:ncol(data)) {
    z <- list(w.length, data[ , col]) # data is a matrix!
    names(z) <- c("w.length", headers[["var.name"]][col])

    old.opts <- options("photobiology.strict.range" = NA_integer_)
    if (headers[["var.name"]][col] %in% c("Afr", "Tfr", "Apc", "Tpc", "A")) {
      z <- photobiology::as.filter_spct(z, ...)
    } else if (headers[["var.name"]][col] %in% c("Rfr", "Rpc")) {
      z <- photobiology::as.reflector_spct(z, ...)
    } else {
      z <- photobiology::as.generic_spct(z, ...)
    }
    z <- clip_wl(x = z, range = range)
    options(old.opts)
    
    comment(z) <-
      paste(paste("CID Bio-Science SpectraVue CI-710s file: '", basename(file), 
                  "' with Mode '", headers[["Mode"]], "'\nImported on ", 
                  lubridate::now(tzone = "UTC"), " UTC\n", sep = ""),
            paste(headers[col, ], collapse = "\n"), sep = "\n")
    
    photobiology::setWhenMeasured(z, headers[col, "when.measured"])
    photobiology::setWhereMeasured(z, photobiology::na_geocode())
    photobiology::setWhatMeasured(z, paste(headers[["Mode"]][col], headers[["Tag"]][col], label, sep = ", "))
    photobiology::setHowMeasured(z, "CID Bio-Science SpectraVue CI-710s leaf spectrometer")
    
    instr.descriptor <- list(time = headers[["when.measured"]][col],
                             spectrometer.name = "CI-710s",
                             spectrometer.sn = NA_character_,
                             bench.grating = "default",
                             bench.filter = "default",
                             bench.slit = "default",
                             max.counts = 65535,
                             n.pixels = 2048)
    
    photobiology::setInstrDesc(z, instr.descriptor)
    
    instr.settings <- list(calibration.id =  headers[["CalibrationID"]][col],
                           pix.selector = TRUE,
                           integ.time = headers[["integ.time"]][col] * 1e3, # ms -> us
                           num.scans = headers[["num.scans"]][col],
                           tot.time = headers[["integ.time"]][col] * 
                             headers[["num.scans"]][col] * 1e3, # ms -> us
                           rel.signal = NA_real_,
                           boxcar.width = headers[["boxcar"]][col],
                           linearized = TRUE,
                           dark.subtracted = TRUE)
    
    photobiology::setInstrSettings(z, instr.settings)
    
    zz[[headers[["Tag"]][col]]] <- z
    
  }
  
  if (length(zz) == 1) {
    zz[[1]]
  } else {
    zz
  }
}
