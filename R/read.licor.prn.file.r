##' Read spectral data from one .PRN file created with LI-COR's PC1800 program.
##' 
##' Reads and parses the header of a processed data file as output by the PC1800 program
##' to extract the whole header remark field and also check whether data is in photon or energy based units.
##' The time field is ignored as it does not contain year information.
##' 
##' @usage read_licor_prn_file(file = "spectrum.PRN", 
##'                             range = NULL, low.limit = NULL, high.limit = NULL, 
##'                             unit.out = "energy", 
##'                             date = lubridate::today())
##' 
##' @param file character string
##' @param range a numeric vector of length two, or any other object for which function range() will return two
##' @param low.limit shortest wavelength to be kept (defaults to shortest w.length in input)
##' @param high.limit longest wavelength to be kept (defaults to longest w.length in input)
##' @param unit.out character string with one of "energy", "photon" or "both"
##' @param date a \code{lubridate} compatible date corresponding to simulated spectrum
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

read_licor_prn_file <- function( file = "spectrum.PRN", 
                                 range = NULL, low.limit = NULL, high.limit = NULL, 
                                 unit.out="energy", 
                                 date = lubridate::today()){
  file_header <- scan(file=file, nlines=5, skip=0, 
                      what=list(file_name_original="character", 
                                remark="character", 
                                limits="character", 
                                interval="character", 
                                date.time="character") )
  parsed_remark <- sub(pattern="REM: ", replacement="", x=file_header$remark, fixed=TRUE)
  parsed_remark <- sub(pattern='(QNTM)', replacement="", x=parsed_remark, fixed=TRUE)
  parsed_remark <- str_trim(parsed_remark)
  message(parsed_remark)
  if (!is.na(match("(QNTM)", file_header$remark, nomatch=FALSE))) {
    unit.in <- "photon"
  } else {
    unit.in <- "energy"
  }
  
  if (unit.in=="photon") {
    out.spct <- read.table(file, header=FALSE, skip=7, col.names=c("w.length", "s.q.irrad"))
    out.spct$s.q.irrad <- out.spct$s.q.irrad * 1e-6 # convert from umol to mol
  } else if (unit.in=="energy") {
    out.spct <- read.table(file, header=FALSE, skip=7, col.names=c("w.length", "s.e.irrad"))
  } else {
    stop("unrecognized unit.in")
  }
  
  setSourceSpct(out.spct, time.unit = "second")
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
  setattr(out.spct, "comment", paste("LICOR LI-1800:", paste(file_header, collapse = "\n"), sep = "\n"))
  out.spct <- trim_spct(out.spct, range = range, low.limit = low.limit, high.limit = high.limit)
  return(out.spct)
}
