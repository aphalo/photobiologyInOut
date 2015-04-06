##' Read spectral data from .PRN files created with LI-COR's PC1800 program.
##' 
##' Reads and parses the header of a processed data file as output by the PC1800 program
##' to extract the whole header remark field and also check whether data is in photon or energy based units.
##' The time field is ignored as it does not contain year information.
##' 
##' @usage read_licor_prn_files(in.path="./", out.path=NULL, file.name.patt="*.PRN", 
##'                             range = NULL, low.limit = NULL, high.limit = NULL, 
##'                             unit.out="energy", 
##'                             date = lubridate::today())
##' 
##' @param in.path The path to the folder containing the text files with processed 
##' data from the spectrometer.
##' @param out.path The path to the folder where to save the .Rda files
##' @param file.name.patt The pattern to be matched when searching for data files.
##' @param range a numeric vector of length two, or any other object for 
##' which function range() will return two
##' @param low.limit shortest wavelength to be kept (defaults to shortest w.length in input)
##' @param high.limit longest wavelength to be kept (defaults to longest w.length in input)
##' @param unit.out character string with one of "energy", "photon" or "both"
##' @param date a \code{lubridate} compatible date corresponding to simulated spectrum
##' 
##' @export
##' @author Pedro J. Aphalo
##' @references \url{http://www.r4photobiology.info}
##' @keywords misc
##' 
##' @return A list of character strings giving the names of the 
##' source.spct objects created.
##' 
##' @details
##' This fucntion calls \code{red_licor_prn_file()} for each file in the path 
##' and matching the patterm. Each object created is also saved to an .rda file
##' unless \code{out.path} is \code{nULL}. 
##' 
##' Paths need to end in a (forward) slash, even under Windows. Under Windows you may need
##' to install Unix/Linux style command line utilities for this function to work. For example
##' install the RTools corresponding to the R version you are using. Under OS X or Linux,
##' all needed uitlities are part of the operating system.
##' 

read_licor_prn_files <- function(in.path="./", out.path=NULL, file.name.patt="*.PRN", 
                                 range = NULL, low.limit = NULL, high.limit = NULL, 
                                 unit.out="energy", 
                                 date = lubridate::today()){
  old.path <- getwd()
  setwd(in.path)
  df.names.vec <- NULL
  file.list <- system(paste('ls', file.name.patt), intern=TRUE)
  for (file.name in file.list) {
    message(paste("Reading: ", file.name))

    df.name <- sub(pattern=".PRN", replacement=".spct", x=file.name)

    assign(df.name, 
           read_licor_prn_file(file = file.name,
                               range = range, high.limit = high.limit, low.limit = low.limit,
                               unit.out = unit.out,
                               date = date) )
    if (!is.null(out.path)) {
      save(list=df.name, file=paste(out.path, df.name, ".rda", sep=""))
     }
  }
  setwd(old.path)
  return(df.names.vec)
}
