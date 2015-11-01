#' Read TUV output file.
#' 
#' Reads and parses the header of a text file output by the TUV program to
#' extract the header and spectral data. The time field is converted to a date.
#' 
#' @param file character string
#' @param date a \code{POSIXct} object, but if \code{NULL} the date stored in
#'   file is used, and if \code{NA} no date variable is added
#' @param geocode A data frame with columns \code{lon} and \code{lat}.
#' @param tz character Time zone is by default read from the file.
#'   
#'   
#' @return a source_spct object obtained by 'melting' the TUV file, and adding
#'   a factor \code{spct.idx}, and variables \code{zenith.angle} and
#'   \code{date}.
#'   
#' @author Pedro J. Aphalo
#' @references \url{http://www.r4photobiology.info}
#' @keywords misc
#'
#' @note Tested only with TUV versison 5.0.
#' 
#' @export
#' 
read_tuv_usrout <- function(file = "usrout.txt", 
                            date = NULL,
                            geocode = NULL,
                            tz = "UTC") {
  file_header <- scan(file = file, nlines = 5, what = "character", sep = "\n" )
  hours <- scan(text = sub(pattern = "wc, nm", replacement = "",
                           x = file_header[4], fixed = TRUE))
  num.spectra <- length(hours)
  
  minutes <- trunc((hours - trunc(hours)) * 60)
  seconds <- trunc((minutes - trunc(minutes)) * 60)

  lubridate::hour(date) <- trunc(hours) 
  lubridate::minute(date) <- trunc(minutes)
  lubridate::second(date) <- trunc(seconds)
  
  angles <- scan(text = sub(pattern = "sza = ", replacement = "", 
                            x = file_header[5], fixed = TRUE))
  
  wide.dt <- read.table(file = file, header = FALSE, skip = 5, 
                        col.names = c("w.length", LETTERS[1:num.spectra]))
  
  setGenericSpct(wide.dt, multiple.wl = Inf)
  wide.dt <-
    trim_spct(
      wide.dt,
      range = range,
      low.limit = low.limit,
      high.limit = high.limit,
      use.hinges = use.hinges
    )
  
  wl.length <- length(wide.dt[["w.length"]])

  out.spct <- reshape2::melt(wide.dt, id.vars = "w.length", 
                             value.name = "s.e.irrad", variable.name = "spct.idx")
  setSourceSpct(out.spct, time.unit = "second", multiple.wl = Inf)

  out.spct[["angle"]] <- with(out.spct, rep(angles, rep(wl.length, num.spectra)))
  out.spct[["date"]] <- with(out.spct, rep(as.POSIXct(date), rep(wl.length, num.spectra)))
  
  old.opts <- options("photobiology.strict.range" = NA)
  
  setSourceSpct(out.spct, time.unit = "second", multiple.wl = num.spectra)
  
  if (unit.out == "energy") {
    q2e(out.spct, action = "replace", byref = TRUE)
  } else if (unit.out == "photon") {
    e2q(out.spct, action = "replace", byref = TRUE)
  } else if (unit.out == "both") {
    q2e(out.spct, action = "add", byref = TRUE)
    e2q(out.spct, action = "add", byref = TRUE)
  } else {
    warning("Unrecognized argument to 'unit.out' ", unit.out, " keeping data as is.")
  }
  
  comment(out.spct) <- paste("TUV:", paste(file_header, collapse = "\n"), sep = "\n")
  options(old.opts)
  return(out.spct)
}

