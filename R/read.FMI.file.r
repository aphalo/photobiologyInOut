#' Read spectral data from a usrout.txt file generated with TUV simulation
#' model.
#' 
#' Reads and parses the header of a text file output by the TUV program to
#' extract the header and spectral data. The time field is converted to a date.
#' 
#' @usage read_tuv_file(file = "usrout.txt", range = NULL, low.limit = NULL,
#'   high.limit = NULL, unit.out = "energy", date = NULL)
#'   
#' @param file character string
#' @param range a numeric vector of length two, or any other object for which
#'   function range() will return two
#' @param low.limit shortest wavelength to be kept (defaults to shortest
#'   w.length in input)
#' @param high.limit longest wavelength to be kept (defaults to longest w.length
#'   in input)
#' @param unit.out character string with one of "energy", "photon" or "both"
#' @param date a \code{POSIXct} object, but if \code{NULL} the date stored in
#'   file is used, and if \code{NA} no date variable is added
#' @param use.hinges logical When trimming, whether to insert and interpoalted 
#'   value at the boundaries or not.
#'   
#' @return a source.spct object obtained by 'melting' the TUV file, and adding a
#'   factor \code{spctrum}, and variables \code{zenith.angle} and \code{date}.
#'   
#' @author Pedro J. Aphalo
#' @references \url{http://www.r4photobiology.info}
#' @keywords misc
#' 
#' @details
#' Algorithm:
#' \enumerate{
#'  \item read file header
#'  \item read spectral data (as many columns as present)
#'  \item convert dataframne to data.table
#'  \item melt data.table and add angles and dates (from header)
#'  \item trim the spectrum according to arguments
#'  \item add the remark from file header as a comment() to object
#'  \item return the source.spct object
#' }
#' 
#' @export
#' 

read_tuv_file <- function(file = "usrout.txt", 
                          range = NULL, low.limit = NULL, high.limit = NULL, 
                          unit.out="energy", 
                          date = NULL,
                          use.hinges = FALSE) {
  file_header <- scan(file = file, nlines = 5, what = "character", sep = "\n" )
  hours <- scan(text = sub(pattern = "wc, nm", 
                           replacement = "", x = file_header[4], fixed=TRUE))
  num.spectra <- length(hours)
  
  minutes <- trunc((hours - trunc(hours)) * 60)
  seconds <- trunc((minutes - trunc(minutes)) * 60)

  lubridate::hour(date) <- trunc(hours) 
  lubridate::minute(date) <- trunc(minutes)
  lubridate::second(date) <- trunc(seconds)
  
  angles <- scan(text = sub(pattern = "sza = ", replacement = "", 
                            x = file_header[5], fixed=TRUE))
  
  wide.dt <- read.table(file=file, header=FALSE, skip=5, 
                        col.names=c("w.length", LETTERS[1:num.spectra]))
  
  setGenericSpct(wide.dt)
  wide.dt <-
    trim_spct(
      wide.dt,
      range = range,
      low.limit = low.limit,
      high.limit = high.limit,
      use.hinges = use.hinges
    )
  
  wl.length <- wide.dt[ , length(wide.dt[["w.length"]])]

  out.spct <- rehape2::melt(wide.dt, id.vars = "w.length", 
                   value.name = "s.e.irrad", variable.name = "spct.idx")
  setSourceSpct(out.spct, time.unit = "second")

  out.spct[["angle"]] <- rep(angles, rep(wl.length, num.spectra))
  out.spct[["data"]] <- rep(as.POSIXct(date), rep(wl.length, num.spectra))
  if (unit.out=="energy") {
    q2e(out.spct, action = "replace", byref = TRUE)
  } else if (unit.out=="photon") {
    e2q(out.spct, action = "replace", byref = TRUE)
  } else if (unit.out=="both") {
    q2e(out.spct, action = "add", byref = TRUE)
    e2q(out.spct, action = "add", byref = TRUE)
  } else {
    warning("Unrecognized argument to 'unit.out' ", unit.out, " keeping data as is.")
  }
  
  comment(out.spct) <- paste("TUV:", paste(file_header, collapse = "\n"), sep = "\n")
  return(out.spct)
}

