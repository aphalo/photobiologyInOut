##' Read spectral data from one .PRN file created with LI-COR's PC1800 program.
##' 
##' Reads and parses the header of a processed data file as output by the PC1800 program
##' to extract the whole header remark field and also check whether data is in photon or energy based units.
##' The time field is ignored as it does not contain year information.
##' 
##' @usage read_macam_file(file = "spectrum.DTA", 
##'                             range = NULL, low.limit = NULL, high.limit = NULL, 
##'                             unit.out = "energy", 
##'                             date = NA)
##' 
##' @param file character string
##' @param range a numeric vector of length two, or any other object for which function range() will return two
##' @param low.limit shortest wavelength to be kept (defaults to shortest w.length in input)
##' @param high.limit longest wavelength to be kept (defaults to longest w.length in input)
##' @param unit.out character string with one of "energy", "photon" or "both"
##' @param date a \code{POSIXct} object, but if \code{NULL} the date stored in file is used, and if \code{NA} no date variable is added 
##' 
##' @return A source.spct object.
##' @export
##' @author Pedro J. Aphalo
##' @references \url{http://www.r4photobiology.info}
##' @keywords misc
##' 
##' @details
##' Algorithm:
##' \enumerate{
##'  \item read file header
##'  \item read spectral data
##'  \item convert dataframne to source.spct
##'  \item convert spectral data if needed
##'  \item trim the spectrum according to arguments
##'  \item add the remark from file header as a comment() to object
##'  \item return the source.spct object
##' }
##' 

read_macam_file <- function( file = "spectrum.DTA", 
                                 range = NULL, low.limit = NULL, high.limit = NULL, 
                                 unit.out="energy", 
                                 date = NA){
  file_header <- scan(file=file, nlines=2, skip=0, what="character")
  if (is.null(date)) {
    date <- lubridate::dmy(sub(pattern="@", replacement="", x=file_header[1], fixed=TRUE))
    time <- lubridate::hms(sub(pattern='@', replacement="", x=file_header[2], fixed=TRUE))
    date <- date + time
  }
  unit.in <- "energy"
  out.spct <- scan(file = file, 
                   what = list(w.length = double(), s.e.irrad = double()),
                   skip=3)
  
  setSourceSpct(out.spct, time.unit = "second")
  if (!is.na(date)) {
    out.spct[["date"]] <- date
  }
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
  comment(out.spct) <- paste("MACAM:", paste(file_header, collapse = "\n"), sep = "\n")
  out.spct <- trim_spct(out.spct, range = range, low.limit = low.limit, high.limit = high.limit)
  return(out.spct)
}
