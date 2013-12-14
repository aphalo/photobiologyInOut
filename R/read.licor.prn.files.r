##' Read spectral data from .PRN files created with LI-COR's PC1800 program.
##' 
##' Reads and parses the header of a processed data file as output by the PC1800 program
##' to extract the whole header remark field and also check whether data is in photon or energy based units.
##' The time field is ignored as it does not contain year information.
##' 
##' @usage read_licor_prn_files(in.path=".", out.path="../data/", file.name.patt="*.PRN", trim.wl=NULL)
##' @param in.path The path to the folder containing the text files with processed data from the spectrometer.
##' @param out.path The path to the folder where to save the .Rda files
##' @param file.name.patt The pattern to be matched when searching for data files.
##' @param trim.wl Numeric value indicating threshold wavelength (nm) above which data is discarded.
##' 
##' @return Returns a character vector with the names of the data.frames created.
##' @export
##' @author Pedro J. Aphalo
##' @references \url{http://r4photobiology.wordpress.com}
##' @keywords misc
##' @note Example head of a file showing the expected format:\\
##' "FILE:FL2"
##' "REM: TLD 36W/865       (QNTM)"
##' "LIMS: 300- 900NM"
##' "INT:  1NM"
##' "DATE:08/23 16:32"
##' "MIN:  300NM  1.518E-04"
##' "MAX:  546NM  7.491E-01"
##' 
##' @details
##' Read all available spectral data files in the directory (folder) pointed to by in.path
##' assumes that all files with ".PRN" are spectral-data files from LI-COR spectroradiometer 
##' STEPS
##' 1) get matching file names
##' 2) for each file
##'  a) Read file header
##'  b) check whether unit.in is "photon" or "energy" 
##'  c) read spectral data
##'  d) calculate data in the other unit base
##'  e) create a data.frame
##'  e') if trim.wl != NULL, then discard data for longer wavelengths
##'  f) add the remark from file header as a comment() to the dataframe
##'  g) save the dataframe as an .Rda file with a name based on .PRN file name
##'  h) save the extracted header as a .txt file
##' 3) goto 2, for next file
##' Paths need to end in a (forward) slash, even under Windows. Under Windows you may need
##' to install Unix/Linux style command line utilities for this function to work. For example
##' install the RTools corresponding to the R version you are using. Under OS X or Linux,
##' all needed uitlities are part of the operating system.
##' 

read_licor_prn_files <- function(in.path=".", out.path="../data/", file.name.patt="*.PRN", trim.wl=NULL){
  old.path <- getwd()
  setwd(in.path)
  df.names.vec <- NULL
  file.list <- system(paste('ls', file.name.patt), intern=TRUE)
  for (file.name in file.list) {
    print(paste("Reading: ", file.name))
    raw_file_header <- scan(file=file.name, nlines=4, skip=0, what=list(file_name_original="character", remark="character", limits="character", interval="character") )
    parsed_remark <- sub(pattern="REM: ", replacement="", x=raw_file_header$remark, fixed=TRUE)
    parsed_remark <- sub(pattern='(QNTM)', replacement="", x=parsed_remark, fixed=TRUE)
    parsed_remark <- str_trim(parsed_remark)
    print(parsed_remark)
    if (!is.na(match("(QNTM)", raw_file_header$remark, nomatch=FALSE))) {unit.in="photon"} else {unit.in="energy"}

    df.name <- paste(sub(pattern=".PRN", replacement="", x=file.name), "data", sep=".")
    df.name <- tolower(df.name)

    df.names.vec <- c(df.names.vec, df.name)
    if (unit.in=="photon") {
      df.temp <- read.table(file.name, header=FALSE, skip=7, col.names=c("w.length", "s.q.irrad"))
      df.temp$s.e.irrad <- with(df.temp, as_energy(w.length, s.q.irrad * 1e-6))
    } else if (unit.in=="energy") {
      df.temp <- read.table(file.name, header=FALSE, skip=7, col.names=c("w.length", "s.e.irrad"))
      df.temp$s.e.irrad <- with(df.temp, as_quantum_mol(w.length, s.e.irrad))
    } else {
      stop("unrecognized unit.in")
    }
    if (!is.null(trim.wl)) {
      df.temp <- df.temp[df.temp$w.length <= trim.wl,]
    }
    comment(df.temp) <- paste("LICOR LI-1800:", parsed_remark)
    assign(df.name, df.temp)
    save(list=df.name, file=paste(out.path, df.name, ".rda", sep=""))
    str_cat <- NULL
    for (str in raw_file_header){
      str_cat <- paste(str_cat, str)
    }
    print(str_cat)
    cat(str_cat, file=paste(out.path, df.name, ".txt", sep=""))
  }
  setwd(old.path)
  return(df.names.vec)
}
