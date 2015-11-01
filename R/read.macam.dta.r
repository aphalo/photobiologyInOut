#' Read \code{.DTA} File Saved by Macam's Software.
#' 
#' Reads and parses the header of a processed data file as output by the PC1800
#' program to extract the whole header remark field and also check whether data
#' is in photon or energy based units. The time field is ignored as it does not
#' contain year information.
#' 
#' @param file character string
#' @param date a \code{POSIXct} object, but if \code{NULL} the date stored in
#'   file is used, and if \code{NA} no date variable is added
#' @param geocode A data frame with columns \code{lon} and \code{lat}.
#' @param tz character Time zone used for interpreting times saved in the
#'   file header.
#'   
#' @return A source_spct object.
#' @export
#' @author Pedro J. Aphalo
#' @references \url{http://www.r4photobiology.info}
#' @keywords misc
#' 
read_macam_dta <- function(file = "spectrum.DTA",
                            date = NULL,
                            geocode = NULL,
                            tz = Sys.timezone(location = FALSE)) {
  file_header <- scan(file = file, nlines = 3, skip = 0, what = "character")
  if (is.null(date)) {
    date <- lubridate::dmy(sub(pattern = "@", replacement = "",
                               x = file_header[1], fixed = TRUE), 
                           tz = tz)
    time <- lubridate::hms(sub(pattern = '@', replacement = "", 
                               x = file_header[2], fixed = TRUE),
                           tz = tz)
    date <- date + time
  }
  z <- scan(file = file, 
                   what = list(w.length = double(), s.e.irrad = double()),
                   skip = 3)
  
  comment(z) <- paste("MACAM:", paste(file_header, collapse = "\n"), sep = "\n")
  
  old.opts <- options("photobiology.strict.range" = NA)
  z <- photobiology::as.source_spct(z, time.unit = "second")
  options(old.opts)
  if (!is.na(date)) {
    photobiology::setWhenMeasured(z, date)
  }
  if (!is.null(geocode) && !is.na(geocode)) {
    photobiology::setWhereMeasured(z, geocode)
  }
  z
}
