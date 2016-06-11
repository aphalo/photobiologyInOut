
# hyperSpec ---------------------------------------------------------------

#' Convert 'hyperSpec::hyperSpec' objects
#' 
#' Convert between 'hyperSpec::hyperSpec' objects and collection of spectra
#' objects (xxxx_mspct) as defined in package 'photobiology' preserving all
#' information. Collection of spectra objects can be easily converted into
#' long spectral objects or into tidy data frames.
#' 
#' @note hyperSpec objects use memory more efficiently than spectral objects
#' of the classes defined in package 'photobiology' while these are more
#' flexible as they are derived from 'tbl_df' and in turn from 'data.frame'.
#' 
#' @param x hyperSpec object
#' @param member.class character One of the spectrum classes defined in package
#'   'photobiology'.
#' @param spct.data.var character The name to be used for the 'spc' data when
#'   constructing the spectral objects.
#' @param multiplier numeric A multiplier to be applied to the 'spc' data to
#'   do unit or
#'   scale conversion. For example "a.u." units in some examples in package
#'   'hyperSpec' seem to have scale factors applied.
#' @param ... currently ignored.
#' 
#' @export
#' 
#' @examples 
#' 
#' library(hyperSpec)
#' data(laser)
#' wl(laser) <- 
#' list (wl = 1e7 / (1/405e-7 - wl (laser)),
#'       label = expression (lambda / nm))
#' laser.mspct <- hyperSpec2mspct(laser, "source_spct", "s.e.irrad")
#' class(laser.mspct)
#' 
hyperSpec2mspct <- function(x, 
                            member.class, 
                            spct.data.var,
                            multiplier = 1,
                            ...) {
  stopifnot(inherits(x, "hyperSpec"))
  # spc data (spectra) are stored as rows in a matrix, consequently
  # we transpose the matrix so that each spectrum is in a column
  y <- cbind(hyperSpec::wl(x), t(x$spc) * multiplier) 
  colnames(y) <- c("w.length", paste("spc", 1:nrow(x), sep = ""))
  y <- tibble::as_data_frame(y)
  z <- split2mspct(x = y, 
                   member.class = member.class, 
                   spct.data.var = spct.data.var)
  other.vars <- setdiff(colnames(x), "spc")
  for (r in 1:length(z)) { # nrow(x) is same as length(z)
    for (var in other.vars) {
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

#' @rdname hyperSpec2mspct
#' 
#' @export
#' 
mspct2hyperSpec <- function(x, 
                            spct.data.var,
                            multiplier = 1,
                            ...) {
  stopifnot(is.any_mspct(x))
  spct.names <- names(x)
  spct.selector <- rep(TRUE, length(x))
  for (i in 1:length(x)) {
    temp <- x[[i]]
    s.column <- temp[[spct.data.var]] * multiplier
    wl.current <- temp[["w.length"]]
    if (i == 1L) {
      mat <- s.column
      wl.prev <- wl.current
    } else {
      if (!all(wl.current == wl.prev)) {
        spct.selector[i] <- FALSE
        next()
      }
      mat <- rbind(mat, s.column) # as row!
    }
  }
  methods::new("hyperSpec", 
      spc = mat, 
      wavelength = wl.prev, 
      data = data.frame(spct_name = factor(spct.names[spct.selector])), 
      labels = list(spct = expression("spct_name"), 
                    spc = expression("I / a.u."),
                    .wavelength = expression("lambda/nm")))
}


# pavo --------------------------------------------------------------------

#' Convert "pavo::rspec' objects
#' 
#' Convert between 'pavo::rspec' objects and collection of spectra
#' objects (xxxx_mspct) as defined in package 'photobiology' preserving all
#' information. Collection of spectra objects can be easily converted into
#' long spectral objects or into tidy data frames.
#' 
#' @note hyperSpec objects use memory more efficiently than spectral objects
#' of the classes defined in package 'photobiology' while these are more
#' flexible as they are derived from 'tbl_df' and in turn from 'data.frame'.
#' 
#' @param x rpec object
#' @param member.class character One of the spectrum classes defined in package
#'   'photobiology'.
#' @param spct.data.var character The name to be used for the 'spc' data when
#'   constructing the spectral objects.
#' @param multiplier numeric A multiplier to be applied to the 'rspc' data 
#'   to do unit or scale conversion.
#' @param ... currently ignored.
#' 
#' @note Objects of class "pavo::rspec" do not contain metadata or class data
#'   from which the quantity measured and the units of expression could be 
#'   obtained. When using this function the user needs to use parameter 
#'   \code{multiplier} to convert the data to what is expected by the object
#'   constructors defined in package 'photobiology' and use parameter
#'   \code{spct.data.var} to select the quantity.
#' 
#' @export
#' 
#' @examples 
#' 
#' library(pavo)
#' data(sicalis)
#' sicalis.mspct <- rspec2mspct(sicalis)
#' class(sicalis.mspct)
#' 
rspec2mspct <- function(x, 
                        member.class = "reflector_spct", 
                        spct.data.var = "Rpc", 
                        multiplier = 1,
                        ...) {
  stopifnot(pavo::is.rspec(x))
  spct.names <- colnames(x)[-1]
  z <- split2mspct(x = x, 
                   member.class = member.class, 
                   spct.data.var = spct.data.var,
                   w.length.var = "wl")
  names(z) <- spct.names
  comment(z) <- paste('Converted from "pavo::rspec" object\n',
                      'dim: ', 
                      paste(names(dim(x)), dim(x), collapse = " "),
                      '\ncolnames: ', paste(colnames(x), collapse = ", "),
                      sep = "")
  z
}


# colorSpec ---------------------------------------------------------------

#' Convert 'colorSpec::colorSpec' objects
#' 
#' Convert between 'colorSpec::colorSpec' objects and spectral
#' objects (xxxx_spct) as defined in package 'photobiology' preserving all
#' information.
#' 
#' @param x colorSpec object
#' @param multiplier numeric A multiplier to be applied to the 'spc' data to
#'   do unit or
#'   scale conversion. For example "a.u." units in some examples in package
#'   'hyperSpec' seem to have scale factors applied.
#' @param ... currently ignored.
#' 
#' @export
#' 
#' @examples 
#' 
#' library(hyperSpec)
#' data(laser)
#' wl(laser) <- 
#' list (wl = 1e7 / (1/405e-7 - wl (laser)),
#'       label = expression (lambda / nm))
#' laser.mspct <- hyperSpec2mspct(laser, "source_spct", "s.e.irrad")
#' class(laser.mspct)
#' 
colorSpec2spct <- function(x, multiplier = 1, ...) {
  stopifnot(multiplier > 0)
  spct.type <- colorSpec::type(x)
  spct.quantity <- colorSpec::quantity(x)
  spct.metadata <- attr(x, "metadata", exact = TRUE)
  comment.spct <- paste('Converted from "colorSpec::colorSpec" object\n',
                        '"type": ', spct.type, "\n",
                        '"quantity": ', spct.quantity, "\n",
                        '"metadata path": ', spct.metadata[["path"]], "\n",
                        '"metadata header": \n', 
                        paste(spct.metadata[["header"]], collapse = "\n"), sep = "")
  if (length(colorSpec::numSpectra(x)) > 1) {
    warning("colorSpec object contains multiple spectra, use 'colorSpec2mspct()'")
    return(generic_spct())
  }
  if (spct.type == "light") {
    if (spct.quantity == 'power') {
      z <- source_spct(w.length = colorSpec::wavelength(x), 
                       s.e.irrad = as.numeric(x) * multiplier)
    } else if (spct.quantity == 'photons/sec') {
      z <- source_spct(w.length = colorSpec::wavelength(x), 
                       s.q.irrad = as.numeric(x) * multiplier)
    } else {
      stop("unkown 'quantity': ", spct.quantity)
    }
  } else if (spct.type == 'responsivity.light') {
    if (spct.quantity %in% c('power->electrical', 'power->neural', 'power->action')) {
      z <- response_spct(w.length = colorSpec::wavelength(x), 
                         s.e.response = as.numeric(x) * multiplier)
    } else if (spct.quantity %in% c('photons->electrical', 'photons->neural', 'photons->action')) {
      z <- response_spct(w.length = colorSpec::wavelength(x), 
                         s.q.response = as.numeric(x) * multiplier)
    } else {
      stop("unkown 'quantity': ", spct.quantity)
    }
  } else if (spct.type == 'material') {
    if (spct.quantity == 'reflectance') {
      z <- reflector_spct(w.length = colorSpec::wavelength(x), 
                          Rfr = as.numeric(x) * multiplier)
    } else if (spct.quantity == 'transmittance') {
      z <- filter_spct(w.length = colorSpec::wavelength(x), 
                       Tfr = as.numeric(x) * multiplier)
    } else if (spct.quantity == 'absorbance') {
      z <- filter_spct(w.length = colorSpec::wavelength(x), 
                       A = as.numeric(x)  * multiplier)
    } else {
      stop("unkown 'quantity': ", spct.quantity)
    }
  } else {
    z <- generic_spct(w.length = colorSpec::wavelength(x), 
                      s.data = as.numeric(x) * multiplier)
  }
  comment(z) <- comment.spct
  scaled <- ifelse(multiplier == 1, FALSE, multiplier)
  setScaled(z, scaled)
  z
}

# #' @rdname colorSpec2spct
# #' 
# #' @export
# #' 
# colorSpec2mspct <- function(x, multiplier = 1, ...) {
#   z <- list()
#   for (name in colorSpec::specnames(x)) {
#     z[[name]] <- colorSpec2spct(x[ , name], 
#                                 multiplier = multiplier,
#                                 ...)
#   }
#   generic_mspct(z, class = class(z[[1]]))
# }