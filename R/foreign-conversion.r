#' @export
#' 
hyperSpec2mspct <- function(x, 
                            member.class = "filter_spct", 
                            spct.data.var = "A", 
                            multiplier = 1,
                            ...) {
  stopifnot(inherits(x, "hyperSpec"))
#  data.members <- ncol(x) # these are not of same length!
#  data.dim <- dim(x$spc)
  y <- cbind(hyperSpec::wl(x), t(x$spc) * multiplier)
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
                      'dim: ', dim(x),
                      'colnames: ', colnames(x)
                      )
  z
}