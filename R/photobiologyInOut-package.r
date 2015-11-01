#' @title Read Spectral Data from Foreign Files.
#' 
#' @description This package provides functions for reading output files
#'   containing spectral data from spectrometers and their associated software,
#'   and some simulation models. The package is designed to complement package
#'   \code{photobiology}.
#' 
#' @section Warning!: 
#' Most of the file formats supported are not standardized, and are a moving
#' target because of changes in instrument firmware and support software. In
#' addition the output format, especially with models, can depend on settings
#' that users can alter. So do check that import is working as expected, and
#' if not, please please raise a bug or support ticket and upload one 
#' example of an incorrectly decoded file.
#' 
#' @note Paths need to end in a (forward) slash, even under Windows. Under Windows
#' you may need to install Unix/Linux style command line utilities for this
#' function to work. For example install the RTools corresponding to the R
#' version you are using. Under OS X or Linux, all needed uitlities are part of
#' the operating system.
#' 
#' @docType package
#' @keywords misc
#' @name photobiologyInOut-package
#' @author Pedro J. Aphalo
#' @details
#' \tabular{ll}{
#' Package: \tab photobiologyInOut\cr
#' Type: \tab Package\cr
#' Version: \tab 0.3.3\cr
#' Date: \tab 2015-11-01\cr
#' License: \tab GPL (>= 3)\cr
#' URL: \tab \url{https://bitbucket.org/aphalo/photobiologyInOut}\cr
#' BugReports: \tab \url{https://bitbucket.org/aphalo/photobiologyInOut}\cr
#' }
#' 
#' @references
#' \url{http://www.r4photobiology.info/}
#' 
#' @import photobiology

NULL
