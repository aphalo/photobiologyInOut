#' Read \code{.csv} File Saved by Aavnates' Software for AvaSpec.
#' 
#' Reads and parses the header of a processed data file, but the header
#' has little useful information.
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
read_avaspec_csv <- function(file = "spectrum.csv",
                            date = NULL,
                            geocode = NULL,
                            tz = Sys.timezone()) {
  file_header <- scan(file = file, nlines = 6, skip = 0, what = "character")
  # watt / cm ?
  if (length(grep("Watt/cm", file_header[2], fixed = TRUE))) {
    mult <- 10e-4
  } else {
    mult <- NA
  }
  
  z <- readr::read_csv(file = file,
                       col_names = c("w.length", "s.e.irrad"),
                       skip = 6)
  dots <- list(~s.e.irrad * mult)
  z <- dplyr::mutate_(z, .dots = stats::setNames(dots, "s.e.irrad"))
  
  comment(z) <- paste("AvaSpec:", paste(file_header, collapse = "\n"), sep = "\n")
  
  old.opts <- options("photobiology.strict.range" = NA)
  z <- photobiology::as.source_spct(z, time.unit = "second")
  options(old.opts)
  if (!is.null(date) && !is.na(date)) {
    photobiology::setWhenMeasured(z, date)
  }
  if (!is.null(geocode) && !is.na(geocode)) {
    photobiology::setWhereMeasured(z, geocode)
  }
  z
}
