#' Read \code{.PRN} File(s) Saved by LI-COR's PC1800 Program.
#' 
#' Reads and parses the header of a processed data file as output by the PC1800
#' program to extract the whole header remark field and also check whether data
#' is in photon or energy based units. The time field is ignored as it does not
#' contain year information.
#' 
#' @param file Path to file as a character string.
#' @param date a \code{POSIXct} object, but if \code{NULL} the date stored in
#'   file is used, and if \code{NA} no date variable is added.
#' @param geocode A data frame with columns \code{lon} and \code{lat}.
#' @param tz character Time zone used for interpreting times saved in the
#'   file header.
#'   
#' @return \code{read_licor_prn()} returns a \code{source_spct} object with
#'   \code{time.unit} attribute set to \code{"second"} and \code{when.measured}
#'   attribute set to the date-time extracted from the file name, or supplied.
#' @export
#' @author Pedro J. Aphalo
#' @references \url{http://www.r4photobiology.info}
#' @keywords misc
#'   
#' @note The LI-1800 spectroradiometer does not store the year as part of the
#' data, only month, day, and time of day. Because of this, in the current
#' version, if \code{NULL} is the argument to date, year is set to 0000.
#' 
read_licor_prn <- function(file,
                           date = NULL,
                           geocode = NULL,
                           tz = Sys.timezone()) {
  file_header <- scan(
    file = file,
    nlines = 7,
    skip = 0,
    what = "character",
    sep = "\n"
  )
  
  if (is.null(date)) {
    line05 <- sub("Date:", "", file_header[5])
    date <- lubridate::parse_date_time(line05, "m*!d! hm", tz = tz)
  }
  
  if (!is.na(match("(QNTM)", file_header[2], nomatch = FALSE))) {
    unit.in <- "photon"
  } else {
    unit.in <- "energy"
  }
  
  if (unit.in == "photon") {
    col_names <- c("w.length", "s.q.irrad")
    mult <- 1e-6 # umol -> mol
  } else if (unit.in == "energy") {
    col_names <- c("w.length", "s.e.irrad")
    mult <- 1.0 # joule -> joule
  }
  
  z <-
    readr::read_table(file,
                      col_names = col_names,
                      col_types = "dd",
                      skip = 7)
  
  if (mult != 1) {
    dots <- list(~s.q.irrad * mult)
    z <- dplyr::mutate_(z, .dots = stats::setNames(dots, "s.q.irrad"))
  }
  
  comment(z) <-
    paste("LICOR LI-1800:", paste(file_header, collapse = "\n"), sep = "\n")
  
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

#' @rdname read_licor_prn
#' @param files A list or vector of character strings.
#' @export
#' @return Function \code{read_m_licor_prn()} returns a source_mspct object
#'   containing one spectrum per file read.
#'   
#' @details Function \code{read_m_licor_prn()} calls \code{red_licor_file()} 
#'   for each file in \code{files}. See \code{\link[readr]{read_table}} for
#'   a description of valid arguments for \code{files}.
#' 
read_m_licor_prn <- function(files,
                             date = NULL,
                             geocode = NULL,
                             tz = Sys.timezone(location = FALSE)) {
  list.of.spectra <- list()
  for (f in files) {
    spct.name <- tolower(sub(".PRN", "", f))
    
    list.of.spectra[[spct.name]] <-
      read_licor_prn(
        file = f,
        date = date,
        geocode = geocode,
        tz = tz
      )
  }
  photobiology::source_mspct(list.of.spectra)
}

