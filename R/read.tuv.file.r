##' Read spectral data from a usrout.txt file generated with TUV simulation model.
##' 
##' Reads and parses the header of a text file output by the TUV program
##' to extract the header and spectral data. The time field is converted to a date.
##' 
##' @usage read_tuv_file(file = "usrout.txt", 
##'                      ange = NULL, low.limit = NULL, high.limit = NULL, 
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
##' @return a source.spct object obtained by 'melting' the TUV file, and adding a factor
##' \code{spctrum}, and variables \code{zenith.angle} and \code{date}.
##' 
##' @author Pedro J. Aphalo
##' @references \url{http://www.r4photobiology.info}
##' @keywords misc
##' 
##' @details
##' Algorithm:
##' \enumerate{
##'  \item read file header
##'  \item read spectral data (as many columns as present)
##'  \item convert dataframne to data.table
##'  \item melt data.table and add angles and dates (from header)
##'  \item trim the spectrum according to arguments
##'  \item add the remark from file header as a comment() to object
##'  \item return the source.spct object
##' }
##' 
##' @export
##' 

read_tuv_file <- function(file = "usrout.txt", 
                          range = NULL, low.limit = NULL, high.limit = NULL, 
                          unit.out="energy", 
                          date = lubridate::today()) {
  file_header <- scan(file=file, nlines=5, what="character", sep = "\n" )
  hours <- scan(text = sub(pattern = "wc, nm", replacement = "", x = file_header[4], fixed=TRUE), what = "double")
  num.spectra <- length(hours)
  
  minutes <- trunc((hours - trunc(hours)) * 60)
  seconds <- trunc((minutes - trunc(minutes)) * 60)

  lubridate::hour(date) <- trunc(hours) 
  lubridate::minute(date) <- trunc(minutes)
  lubridate::second(date) <- trunc(seconds)
  
  angles <- scan(text = sub(pattern = "sza = ", replacement = "", x = file_header[5], fixed=TRUE), what = "double")
  
  wide.dt <- read.table(file=file, header=FALSE, skip=5, col.names=c("w.length", LETTERS[1:num.spectra]))
  wl.length <- length()
  setDT(wide.dt)
  wl.length <- wide.dt[ , length(w.length)]
  out.spct <- melt(wide.dt, id.vars = "w.length", value.name = "s.e.irrad", variable.name = "spectrum")
  setkey(out.spct, spectrum)
  out.spct[levels(spectrum), angle := rep(angles, rep(wl.length, num.spectra))]
  out.spct[levels(spectrum), date := rep(times, rep(wl.length, num.spectra))]
  
  setSourceSpct(out.spct, time.unit = "second")
  setattr(out.spct, "comment", paste("TUV:", paste(file_header, collapse = "\n"), sep = "\n"))
  trim_spct(out.spct, low.limit=low.limit, high.limit=high.limit)
  return(out.spct)
}

