#' Read File Saved by Ocean Optics' SpectraSuite.
#' 
#' Reads and parses the header of a processed data file as output by
#' SpectraSuite to extract the whole header remark field The time field is
#' retireved
#' 
#' @param file character string
#' @param date a \code{POSIXct} object, but if \code{NULL} the date stored in
#'   file is used, and if \code{NA} no date variable is added
#' @param geocode A data frame with columns \code{lon} and \code{lat}.
#' @param tz character Time zone is by default read from the file.
#'   
#' @return A source_spct object.
#' @export
#' @author Pedro J. Aphalo
#' @references \url{http://www.r4photobiology.info}
#' @keywords misc
#' 
read_oo_sstxt <- function(file = "spectrum.SSIrrad",
                          date = NULL,
                          geocode = NULL,
                          tz = NULL) {
  line01 <- scan(file = file, nlines =  1, skip = 0, what="character")
  if (line01[1] != "SpectraSuite") {
    warning("Input file was not created by SpectrSuite as expected: skipping")
    return(NA)
  }
  file_header <- scan(file = file, nlines = 16, 
                      skip = 0, what="character", sep = "\n")
  
  npixels <- as.integer(sub("Number of Pixels in Processed Spectrum: ", "", 
                            file_header[16], fixed = TRUE))
  
  if (is.null(date)) {
    line03 <- sub("Date: [[:alpha:]]{3} ", "", file_header[3])
    if (is.null(tz)) {
      tz <- sub("^(.{16})([[:upper:]]{3,4})(.{5})$", "\\2", line03)
      if (nchar(tz) == 4) {
        tz <- sub("S", "", tz)
      }
    }
    date <- lubridate::parse_date_time(line03, "m*!d! hms y", tz = tz)
  }
  
  z <- readr::read_tsv(
    file = file,
    col_names = c("w.length", "s.e.irrad"),
    skip = 17,
    n_max = npixels
  )
  
  dots <- list(~s.e.irrad * 1e-2) # uW cm-2 nm-1 -> W m-2 nm-1
  z <- dplyr::mutate_(z, .dots = setNames(dots, "s.e.irrad"))

  comment(z) <-
    paste("Ocean Optics:", paste(file_header, collapse = "\n"), sep = "\n")
  
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
