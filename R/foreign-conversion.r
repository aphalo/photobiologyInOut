#' Convert 'hyperSpec::hyperSpec' objects
#' 
#' Convert between 'hyperSpec::hyperSpec' objects and collection of spectra
#' objects (xxxx_mspct) as defined in package 'photobiology' preserving all
#' information. Collection of spectra objects can be easily converted into
#' spectral objects or into tidy data frames.
#' 
#' @note hyperSpec objects use memory more efficiently than spectral objects
#' of the classes defined in package 'photobiology' while these are more
#' flexible as they are derived from 'tbl_df' and in turn from 'data.frame'.
#' 
#' @param x hyperSpec object
#' @param member.class character One of the classes defined in package
#'   'photobiology'.
#' @param spct.data.var character The name to be used for the 'spc' data when
#'   constructing the spectral objects.
#' @param spc.multiplier numeric A multiplier to be applied to the 'spc' data to
#'   do unit or
#'   scale conversion. For example "a.u." units in some examples in package
#'   'hyperSpec' seem to have scale factors applied.
#' @param ... currently ignored.
#' 
#' @export
#' 
hyperSpec2mspct <- function(x, 
                            member.class = "filter_spct", 
                            spct.data.var = "A", 
                            spc.multiplier = 1,
                            ...) {
  stopifnot(inherits(x, "hyperSpec"))
  y <- cbind(hyperSpec::wl(x), t(x$spc) * spc.multiplier) 
  colnames(y) <- c("w.length", paste("spc", 1:nrow(x), sep = ""))
  y <- dplyr::as_data_frame(y)
  z <- split2mspct(x = y, 
                   member.class = member.class, 
                   spct.data.var = spct.data.var)
  other.vars <- setdiff(colnames(x), "spc")
  for (var in other.vars) {
    for (r in 1:nrow(x)) {
      z[[r]][[var]] <- rep(x@data[[var]][r], hyperSpec::nwl(x))
    }
  }
  comment(z) <- paste('Converted from "hyperSpec" object\n',
                      'dim: ', 
                      paste(names(dim(x)), dim(x), collapse = " "),
                      '\ncolnames: ', paste(colnames(x), collapse = ", "),
                      sep = "")
  z
}