#' Read File Saved by Wasatch's Enlighten.
#' 
#' Reads and parses the header of a processed data file as output by
#' Enlighten to extract the whole header field. The time field is
#' retrieved and decoded as well additional metadata.
#' 
#' @param file character 
#' @param scale.factor numeric vector of length 1, or length equal to the number
#'   of rows (= detector pixels). Numeric multiplier applied to returned
#'   spectral values.
#' @param date a \code{POSIXct} object to use to set the \code{"when.measured"}
#'   attribute. If \code{NULL}, the default, the date is extracted from the
#'   file header.
#' @param geocode A data frame with columns \code{lon} and \code{lat} used to
#'   set attribute \code{"where.measured"}.
#' @param label character string, but if \code{NULL} the value of \code{file} is
#'   used, and if \code{NA} the "what.measured" attribute is not set.
#' @param tz character Time zone is by default read from the file.
#' @param locale	The locale controls defaults that vary from place to place. The
#'   default locale is US-centric (like R), but you can use
#'   \code{\link[readr]{locale}} to create your own locale that controls things
#'   like the default time zone, encoding, decimal mark, big mark, and day/month
#'   names.
#'   
#' @note Tested only with two example files.
#' 
#' @return A source_spct object.
#' @export
#' @references \url{https://wasatchphotonics.com/}
#'   \url{https://wasatchphotonics.com/product-category/software/}
#' @keywords misc
#' 
read_wasatch_csv <- function(file,
                             scale.factor = 1,
                             date = NULL,
                             geocode = NULL,
                             label = NULL,
                             tz = NULL,
                             locale = readr::default_locale()) {
  if (is.null(tz)) {
    tz <- locale$tz
  }
  
  label.file <- paste("File: ", basename(file), sep = "")
  if (is.null(label)) {
    label <- label.file
  } else if (!is.na(label)) {
    label <- paste(label.file, label, sep = "\n")
  }
  
  line01 <- scan(file = file, nlines =  1, skip = 0, what="character")
  if (line01[1] != "ENLIGHTEN") {
    warning("Input file was not created by ENLIGHTEN as expected: skipping")
    return(photobiology::source_spct())
  }
  file_header <- scan(file = file, nlines = 40, 
                      skip = 0, what="character", sep = "\n")
  NonASCII <- tools::showNonASCII(file_header)
  if (length(NonASCII) > 0L) {
    warning("Found non-ASCII characters in file header: ", 
            NonASCII,
            "replacing with '.'.")
    file_header <- iconv(file_header, to = "ASCII", sub = " ")
  }
  
  npixels <- as.integer(sub("Pixel Count,", "", 
                            file_header[32], fixed = TRUE))
  
  if (is.null(date)) {
    line19 <- sub("Timestamp,", "", file_header[19])
    if (is.null(tz)) {
      tz <- tz(lubridate::now())
      warning("Using computer's current TZ setting: ", tz)
    }
    date <- lubridate::ymd_hms(line19, tz = tz)
  }
  
  i <- 30L
  
  while(!grepl("wavelength", tolower(file_header[i]))) {
    i <- i + 1L
  }
  
  z <- readr::read_csv(
    file = file,
    col_names = c("w.length", "s.e.irrad"),
    skip = i + 1L,
    n_max = npixels,
    col_types = readr::cols(),
    locale = locale
  )
  
  z[["s.e.irrad"]] <- z[["s.e.irrad"]] * scale.factor

  old.opts <- options("photobiology.strict.range" = NA_integer_)
  z <- photobiology::as.source_spct(z, time.unit = "second")
  options(old.opts)

  comment(z) <-
    paste(paste("Wasatch Enlighten irradiance file '", file, "' imported on ", 
                lubridate::now(tzone = "UTC"), " UTC", sep = ""),
          paste(file_header, collapse = "\n"), 
          sep = "\n")
  
  photobiology::setWhenMeasured(z, date)
  photobiology::setWhereMeasured(z, geocode)
  photobiology::setWhatMeasured(z, label)
  attr(z, "file.header") <- file_header
  z
}
