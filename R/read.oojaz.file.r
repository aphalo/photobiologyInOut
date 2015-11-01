#' Read File Saved by Ocean Optics' Jaz spectrometer.
#' 
#' Reads and parses the header of a processed data file as output by
#' SpectraSuite to extract the whole header remark field The time field is
#' retireved
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
read_oo_jazzirrad <- function(file = "spectrum.JazIrrad",
                              date = NULL,
                              geocode = NULL,
                              tz = Sys.timezone(location = FALSE)) {
  line01 <-
    scan(
      file = file,
      nlines =  1,
      skip = 0,
      what = "character"
    )
  if (line01[1] != "Jaz") {
    warning("Input file was not created by a Jaz spectrometer as expected: skipping")
    return(NA)
  }
  file_header <-
    scan(
      file = file,
      nlines = 18,
      skip = 0,
      what = "character",
      sep = "\n"
    )
  
  if (is.null(date)) {
    line03 <- sub("Date: [[:alpha:]]{3} ", "", file_header[3])
    date <-
      lubridate::parse_date_time(line03, "m*!d! hms y", tz = tz)
  }
  
  #  data_header <- scan(file = file, nlines = 1, skip = 20, what = "character")
  col_names <-
    c("w.length", "s.e.irrad.dark", "s.e.irrad.uc", "s.e.irrad")
  z <- readr::read_table(
    file = file,
    col_names = col_names,
    col_types = "dd--",
    skip = 20,
    n_max = 2047
  )
  
  z <-
    dplyr::mutate(z, s.e.irrad = s.e.irrad * 1e-2) # uW cm-2 nm-1 -> W m-2 nm-1
  
  comment(z) <-
    paste("Ocean Optics:", paste(file_header, collapse = "\n"), sep = "\n")
  
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
