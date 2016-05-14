#' Read libRadtran output file.
#' 
#' Reads and parses the header of a text file output by libRadtran for a solar 
#' spectrum simulation to extract the header and spectral data. The time field
#' is converted to a date.
#' 
#' @param file character string
#' @param date a \code{POSIXct} object, but if \code{NULL} the date stored in
#'   file is used, and if \code{NA} no date variable is added
#' @param geocode A data frame with columns \code{lon} and \code{lat}.
#' @param tz character Time zone is by default read from the file.
#' @param locale	The locale controls defaults that vary from place to place. The
#'   default locale is US-centric (like R), but you can use
#'   \code{\link[readr]{locale}} to create your own locale that controls things
#'   like the default time zone, encoding, decimal mark, big mark, and day/month
#'   names.
#' @param simplify logical Remove redundant columns from returned object.
#'   
#'   
#' @return a source_spct object, possibly containing several spectra in long
#'  form and a datetime column.
#'   
#' @references \url{http://www.r4photobiology.info}
#'
#' @note Tested only libRadtran version 2.0
#' 
#' @export
#' 
read_libradtran_vesa <- function(file, 
                                 date = NULL,
                                 geocode = NULL,
                                 tz = NULL,
                                 locale = readr::default_locale(),
                                 simplify = TRUE) {
  if (is.null(tz)) {
    tz <- locale$tz
  }
  x <- readr::read_table(file = file,
                         col_names = c("w.length", "day", "time", 
                                       "s.e.irrad.dir", "s.e.irrad.diff"),
                         col_types = "dccdd",
                         locale = locale)
  # the statement below triggers a NOTE but I hevn't yet worked out how to
  # convert this to use mutate_() for SE
  z <- dplyr::mutate(x, datetime = lubridate::ymd_hms(paste(day, time), tz = tz), 
                     s.e.irrad = (s.e.irrad.dir + s.e.irrad.diff) * 1e-3)
  datetimes <- unique(z[["datetime"]])
  num.spectra <- length(datetimes)
  if (simplify && num.spectra == 1) {
    z <- dplyr::select_(z, "w.length", 
                        lazyeval::interp(~starts_with(x), x = "s.e.irrad"))
  } else if (simplify && num.spectra > 1) {
    z <- dplyr::select_(z, "w.length", "datetime", 
                        lazyeval::interp(~starts_with(x), x = "s.e.irrad"))
  }
  photobiology::setSourceSpct(z, time.unit = "second", multiple.wl = num.spectra)
  setWhenMeasured(z, datetimes)
  setWhereMeasured(z, geocode)
  setWhatMeasured(z, "libRadtran spectral simulation")
  comment(z) <- paste("TUV:", file, sep = "\n")
  z
}

