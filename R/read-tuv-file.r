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
                            date = lubridate::today(),
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
  
  wide.df <- readr::read_table(file = file, skip = 5, 
                               col_names = c("w.length", LETTERS[1:num.spectra]))
  
  wl.length <- length(wide.df[["w.length"]])

  z <- reshape2::melt(wide.df, id.vars = "w.length", 
                      value.name = "s.e.irrad", variable.name = "spct.idx")
  
  z[["angle"]] <- with(z, rep(angles, rep(wl.length, num.spectra)))
  z[["date"]] <- with(z, rep(as.POSIXct(date), rep(wl.length, num.spectra)))
  
  comment(z) <- paste("TUV:", paste(file_header, collapse = "\n"), sep = "\n")

  old.opts <- options("photobiology.strict.range" = NA)
  photobiology::setSourceSpct(z, time.unit = "second", multiple.wl = num.spectra)
  options(old.opts)
  z
}
