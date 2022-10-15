# build docs without fetching dates of releases from CRAN?
options(pkgdown.internet = FALSE)
pkgdown::build_site()
