#' Read File Saved by ASD's conversion tool
#' 
#' Read the spectral data and parse the header of a energy irradiance, a
#' reflectance, a transmittance data, a raw-detector-counts data or arbitrary
#' data file as output by the text conversion tool for ASD spectrometers when
#' set for tab separated output. ASD's field spectrometers measure VIS and SWIR
#' radiation.
#' 
#' @param file character string Path to the file to be read, following R's use
#'   of forward slashes as separator for folder names.
#' @param date a \code{POSIXct} object to use to set the \code{"when.measured"}
#'   attribute. If \code{NULL}, the default, the date is extracted from the file
#'   header.
#' @param geocode A data frame with columns \code{lon} and \code{lat}, and
#'   optionally \code{address} used to set attribute \code{"where.measured"}.
#' @param label character string to which to set the \code{"what.measured"}
#'   attribute. If \code{NULL} the value of \code{basename(file)} is used, and
#'   if \code{NA} the \code{"what.measured"} attribute is not set.
#' @param tz character A time zone recognized by R. If \code{NULL} it is
#'   extracted from \code{locale}. If \code{""}, the default, the local time
#'   zone is used.
#' @param locale	The locale controls defaults that vary from place to place. The
#'   default locale is US-centric (like R), with time in UTC but you can use
#'   \code{\link[readr]{locale}} to create your own locale that controls things
#'   like the default time zone, encoding, decimal mark, big mark, and day/month
#'   names. Its value must match that used to write the imported file, which is
#'   not necessarily the default one or the local one.
#' @param s.qty character string The name of the quantity to be read as by
#'   object constructors from 'photobiology'. If \code{NULL} quantity is guessed
#'   from file header.
#' @param scale.factor numeric Multiplier to be applied to the spectral data so
#'   that it matches \code{s.qty} in base of expression and units. In most cases
#'   the default of 1.0 is correct.
#' @param range a numeric vector of length two, or any other object for which
#'   function \code{range()} will return a range of wavelengths expressed in
#'   nanometres.
#' 
#' @details The header of the file is first decoded and parsed to extract the
#'   time of data acquisition and serial number of the spectrometer, and to
#'   locate the start of the spectral data. The time in the header is the local
#'   time with no time zone information. Thus with the default \code{tz = ""}
#'   the time is re-expressed as UTC with a correction. When file import takes
#'   place on a different time zone than the measurement the measurement time
#'   zone must be supplied by the user as an argument to parameter \code{tz} or
#'   as part of the \code{locale} after setting \code{tz = NULL}. The metadata
#'   fields in the header are located by text matching.
#'
#'   Spectral irradiance is returned as an object of class
#'   \code{\link[photobiology]{source_spct}}, reflectance as
#'   \code{\link[photobiology]{reflector_spct}}, transmittance as
#'   \code{\link[photobiology]{filter_spct}}, raw detector counts as
#'   \code{\link[photobiology]{raw_spct}}, and arbitrary spectral values as
#'   \code{\link[photobiology]{generic_spct}} objects with metadata stored in
#'   attributes \code{when.measured}, \code{what.measured}, and
#'   \code{how.measured}. The spectral quantity is guessed from the header
#'   metadata by default, but an argument to \code{s.qty} can be passed to
#'   override this default.
#'
#'   The value stored in the \code{how.measured} attribute includes the
#'   spectrometer serial number extracted from the file header. If an argument
#'   is passed to parameter \code{geocode}, its value is saved in attribute
#'   \code{where.measured} (currently the geocode is not extracted from the file
#'   header). The object's \code{comment} always gives a text that includes the
#'   file name, time of import, function name and the version of packages
#'   'photobiology' and 'photobiologyInOut' used. The file header in whole is
#'   copied to attribute \code{file.header}.
#'
#' @return A \code{source_spct} object with a column \code{s.e.irrad} with
#'   spectral energy irradiance in \eqn{W m^{-2} nm^{-1}}, or a
#'   \code{reflector_spct} object with a column \code{Rfr} with fractional
#'   reflectance, or a \code{filter_spct} object with a column \code{Tfr} with
#'   fractional total transmittance. In all cases a column \code{w.length}
#'   contains wavelengths in nanometres and the attributes \code{comment},
#'   \code{what.measured}, \code{when.measured}, \code{how.measured},
#'   \code{where.measured} and \code{file.header} containing metadata for the
#'   spectrum.
#' 
#' @export
#' 
#' @references \url{https://www.malvernpanalytical.com/}
#' 
#' @examples
#'
#' # energy spectral irradiance file
#' 
#'  file.name <-
#'    system.file("extdata", "irrad-sky.asd.txt", 
#'                package = "photobiologyInOut", 
#'                mustWork = TRUE)
#'                 
#'  asd.source_spct <- 
#'    read_asd_tsv(file = file.name,
#'                 tz = "Europe/Helsinki")
#'  
#'  class_spct(asd.source_spct)
#'  summary(asd.source_spct)
#'  getWhenMeasured(asd.source_spct)
#'  getWhatMeasured(asd.source_spct)
#'  getHowMeasured(asd.source_spct)
#'  cat(comment(asd.source_spct))
#'  e_irrad(asd.source_spct)
#'  q_irrad(asd.source_spct, w.band = c(400, 700), 
#'          scale.factor = 1e6)
#'  
#'  asd_clipped.source_spct <- 
#'    read_asd_tsv(file = file.name,
#'                 tz = "Europe/Helsinki",
#'                 range = c(400, 700))
#'  
#'  class_spct(asd_clipped.source_spct)
#'  summary(asd_clipped.source_spct)
#' 
#' # spectral reflectance file
#' 
#'  file.name <-
#'    system.file("extdata", "reflec-panel-50pc.asd.txt", 
#'                package = "photobiologyInOut",
#'                mustWork = TRUE)
#'                 
#'  asd.reflector_spct <- 
#'    read_asd_tsv(file = file.name,
#'                 tz = "Europe/Helsinki")
#'  
#'  class_spct(asd.reflector_spct)
#'  summary(asd.reflector_spct)
#'  getWhenMeasured(asd.reflector_spct)
#'  getWhatMeasured(asd.reflector_spct)
#'  getHowMeasured(asd.reflector_spct)
#'  cat(comment(asd.reflector_spct))
#' 
#'  asd_clipped.reflector_spct <- 
#'    read_asd_tsv(file = file.name,
#'                tz = "Europe/Helsinki",
#'                range = c(400, 700))
#'  
#'  class_spct(asd_clipped.reflector_spct)
#'  summary(asd_clipped.reflector_spct)
#'  
#'  # Raw-counts data
#' 
#'  file.name <-
#'    system.file("extdata", "DN-gravel.asd.txt", 
#'                package = "photobiologyInOut",
#'                mustWork = TRUE)
#'                 
#'  asd.raw_spct <- 
#'    read_asd_tsv(file = file.name,
#'                 tz = "Europe/Helsinki")
#'  
#'  class_spct(asd.raw_spct)
#'  summary(asd.raw_spct)
#'  getWhenMeasured(asd.raw_spct)
#'  getWhatMeasured(asd.raw_spct)
#'  getHowMeasured(asd.raw_spct)
#'  cat(comment(asd.raw_spct))
#' 
read_asd_tsv <- function(file,
                        date = NULL,
                        geocode = NULL,
                        label = NULL,
                        tz = NULL,
                        locale = readr::default_locale(),
                        s.qty = NULL,
                        scale.factor = 1,
                        range = NULL) {
  if (is.null(tz)) {
    tz <- locale$tz
  }
  
  file_header <- scan(file = file, nlines = 50,
                      skip = 0, what="character",
                      blank.lines.skip = FALSE, # to get start of data
                      sep = "\n", quiet = TRUE)
  
  NonASCII <- tools::showNonASCII(file_header)
  if (length(NonASCII) > 0L) {
    warning("Found non-ASCII characters in file header: ",
            NonASCII,
            "replacing with ' '.")
    file_header <- iconv(file_header, to = "ASCII", sub = " ")
  }
  if (!any(grepl("ASD spectrum file", file_header, fixed = TRUE))) {
    warning("Input file'", basename(file),
            "'lacks the expected ASD header: skipping!!")
    return(photobiology::source_spct())
  }
  if (is.null(s.qty)) {
    if (any(grepl("Data is compared to a white reference:",
                  file_header, 
                  fixed = TRUE)
            )) {
      s.qty <- "Rfr"
    } else if (any(grepl("There was a remote cosine receptor attached",
                         file_header,
                         fixed = TRUE)) &&
               any(grepl("Data is not compared to a white reference",
                         file_header,
                         fixed = TRUE))) {
                           s.qty <- "s.e.irrad"
    } else if (any(grepl("There was no foreoptic attached",
                         file_header,
                         fixed = TRUE)) &&
               any(grepl("Data is not compared to a white reference",
                         file_header,
                         fixed = TRUE))) {
      s.qty <- "counts"
    }
  } 
  spct.class <- switch(s.qty,
                       s.e.irrad = "source_spct",
                       Rfr = "reflector_spct",
                       Tfr = "filter_spct",
                       counts = "raw_spct",
                       "generic_spct")

  ln.idx <- which(grepl("^The instrument number was", file_header))
  sr.sn <- trimws(sub("The instrument number was", "", file_header[ln.idx], fixed = TRUE))

  if (is.null(date)) {
    ln.idx <- which(grepl("^Spectrum saved:",
                          file_header))
    line.date <- gsub("Spectrum saved: | at", "", file_header[ln.idx])
    date <- lubridate::mdy_hms(line.date, tz = tz)
  }

  to.skip <- which(grepl("^Wavelength", file_header))
  stopifnot("Header parsing failure" = 
              !is.na(to.skip) && is.integer(to.skip) && length(to.skip == 1))
  original.name <- gsub("^Wavelength", "", file_header[[to.skip]])
  original.name <- trimws(original.name)
  
  z <- utils::read.table(
    file = file,
    header = TRUE,
    col.names = c("w.length", s.qty),
    skip = to.skip,
    colClasses = "numeric"
  )
  
  z[[s.qty]] <- z[[s.qty]] * scale.factor

  old.opts <- options("photobiology.strict.range" = NA_integer_)
  if (spct.class == "source_spct") {
    z <- photobiology::as.source_spct(z, time.unit = "second")
    if (!is.null(range)) {
      z <- photobiology::clip_wl(z, range)
    }
  } else if (spct.class == "reflector_spct") {
    z <- photobiology::as.reflector_spct(z)
    if (!is.null(range)) {
      z <- photobiology::clip_wl(z, range)
    }
  } else if (spct.class == "filter_spct") {
    z <- photobiology::as.filter_spct(z, Tfr.type = "total")
    if (!is.null(range)) {
      z <- photobiology::clip_wl(z, range)
    }
  } else if (spct.class == "raw_spct") {
    z <- photobiology::as.raw_spct(z)
    if (!is.null(range)) {
      z <- photobiology::clip_wl(z, range)
    }
  } else if (spct.class == "generic_spct") {
    z <- photobiology::as.generic_spct(z)
    if (!is.null(range)) {
      z <- photobiology::clip_wl(z, range)
    }
  } else {
    warning("Returning a data frame!!")
  }
  
  options(old.opts)

  comment(z) <-
    paste("Imported ASD text-converted file '", basename(file),
          "' with quantity name '", original.name,
          "' imported on ",
          lubridate::round_date(lubridate::now(tzone = "UTC")), " UTC ",
          "with function 'read_asd_tsv()'.\n",
          "R packages 'photobiologyInOut' ",
          utils::packageVersion(pkg = "photobiologyInOut"),
          " and 'photobiology' ",
          utils::packageVersion(pkg = "photobiology"),
          " were used.", sep = "")

  how.measured <- paste("Measured with ASD spectrometer with s.n. ",
                         sr.sn, " and ASD software.", sep = "")
  
  photobiology::setHowMeasured(z, how.measured)
  photobiology::setWhenMeasured(z, date)
  if (!is.null(geocode)) {
    photobiology::setWhereMeasured(z, geocode)
  }
  label.file <- paste("File: ", basename(file), sep = "")
  if (is.null(label)) {
    label <- paste(label.file, "\nQty: '", original.name, "'", sep = "")
  } else if (!is.na(label)) {
    label <- paste(label.file, label, sep = "\n")
  }
  photobiology::setWhatMeasured(z, label)
  attr(z, "file.header") <- file_header[1:to.skip]
  z
}
