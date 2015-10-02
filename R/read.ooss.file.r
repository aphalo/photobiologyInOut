#' Read spectral data from one .txt file created with Ocean Optics'
#' SpectraSuite program.
#' 
#' Reads and parses the header of a processed data file as output by
#' SpectraSuite to extract the whole header remark field The time field is
#' retireved
#' 
#' @param file character string
#' @param range a numeric vector of length two, or any other object for which
#'   function range() will return two
#' @param low.limit shortest wavelength to be kept (defaults to shortest 
#'   w.length in input)
#' @param high.limit longest wavelength to be kept (defaults to longest 
#'   w.length in input)
#' @param unit.out character string with one of "energy", "photon" or "both"
#' @param date a \code{POSIXct} object, but if \code{NULL} the date stored in 
#'   file is used, and if \code{NA} no date variable is added
#' @param use.hinges logical When trimming, whether to insert and interpoalted 
#'   value at the boundaries or not.
#'   
#' @return A source.spct object.
#' @export
#' @author Pedro J. Aphalo
#' @references \url{http://www.r4photobiology.info}
#' @keywords misc
#' 
#' @details
#' Algorithm:
#' \enumerate{
#'  \item read file header
#'  \item read spectral data
#'  \item convert dataframne to source.spct
#'  \item convert spectral data if needed
#'  \item trim the spectrum according to arguments
#'  \item add the remark from file header as a comment() to object
#'  \item return the source.spct object
#' }
#' 

read_ooss_file <- function( file = "spectrum.txt", 
                            range = NULL, low.limit = NULL, high.limit = NULL, 
                            unit.out="energy", 
                            date = NA,
                            use.hinges = FALSE){
  line01 <- scan(file = file, nlines =  1, skip = 0, what="character")
  if (line01[1] != "SpectraSuite") {
    warning("Input file was not created by SpectrSuite as expected: skipping")
    return(NA)
  }
  file_header <- scan(file = file, nlines = 15, 
                      skip = 0, what="character", sep = "\n")
  
  if (is.null(date)) {
    line03 <- sub("Date: [[:alpha:]]{3} ", "", file_header[3])
    time_zone <- sub("^(.{16})([[:upper:]]{3,4})(.{5})$", "\\2", line03)
    if (nchar(time_zone) == 4) {
      time_zone <- sub("S", "", time_zone)
    } # the S is for summer time but it is not needed
#    line03 <- sub("[[:upper:]]{3,4}[[:space:]]", "", line03)
#    as.POSIXct(line03, format="%B %d %H:%M:%S %Y", tz="EET")
    date <- lubridate::parse_date_time(line03, "m*!d! hms y", tz = time_zone)
  }
  
  out.spct <- read.table(file = file, header=FALSE, skip=17, 
                         comment.char = ">",
                         col.names=c("w.length", "s.e.irrad"),
                         dec = ".")
  
  setSourceSpct(out.spct, time.unit = "second")
  out.spct[["s.e.irrad"]] <-  
               out.spct[["s.e.irrad"]] * 1e-2 # uW cm-2 nm-1 -> W m-2 nm-1
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
    warning("Unrecognized argument to 'unit.out' ", 
            unit.out, " keeping data as is.")
  }
  comment(out.spct) <- paste("Ocean Optics:", 
                             paste(file_header, collapse = "\n"), sep = "\n")
  out.spct <-
    trim_spct(
      out.spct,
      range = range,
      low.limit = low.limit,
      high.limit = high.limit,
      use.hinges = use.hinges
    )
  return(out.spct)
}
