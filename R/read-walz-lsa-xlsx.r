#' Read '.xlsx' file(s) saved by Walz's LSA-2050 leaf analyser
#' 
#' Read the two worksheets contained in the workbook file downloaded by Walz's
#' LSA software from an LSA-2050 instrument. Combine the data into a single
#' data frame and add metadata as its attributes.
#' 
#' @param file Path to file as a character string.
#' @param label character string, but if \code{NULL} the value of \code{file} is
#'   used, and if \code{NA} the "what.measured" attribute is not set.
#' @param data_skip integer Number of records (rows) to skip from the actual
#'   data block.
#' @param n_max integer Maximum number of records to read.
#' @param na character Vector of strings to interpret as missing values. Set 
#'   this option to character() to indicate no missing values.
#' @param marker.rename character A named vector with new \code{Marker} values 
#'   named according to the Markers set in the LSA-2050.
#' @param returned.data character If \code{"consolidated"} return all data in a
#'   single data frame, with the "SAT chart" data as a list column containing
#'   one data frame per sample. Other values return a data frame containing a
#'   subset of the columns: with \code{"measured"}, return only the data in the
#'   worksheet named "Measured", with \code{"visible"} return only data from
#'   columns visible in the worksheet named "Measured", and \code{"raw"} returns
#'   only data stored in both worksheets as numeric values, i.e., ignores values
#'   computed by formulas included in the worksheet. \emph{For debugging:} if
#'   \code{"list"}, return a named list with two member data frames, each
#'   containing the data from one worksheet.
#' @param drop.positions logical If \code{TRUE} the columns related to leaf
#'   position are deleted, and if \code{FALSE} they are retained. If \code{NULL}
#'   they are dropped only if they contain no finite data.
#' @param ... Further named arguments currently passed to 
#' \code{\link[readxl]{read_excel}()}.
#'   
#' @details The worksheets contain formulas for computations, on import the
#'   results of the computations previously stored in the workbook are returned.
#'   The raw data values are imported and well as calibration values. The 
#'   If the names of the worksheets in the workbook do not
#'   match the expected ones, a warning is issued. As the \code{Date} and
#'   \code{Time} columns contain identical data (displayed differently) for
#'   efficiency, only \code{Time} is retained in the returned object. Units and
#'   bases of expression are not modified. Some column names are edited to 
#'   comply with R object-naming rules.
#'
#'   The named vector passed as argument to \code{marker.rename} if not
#'   \code{NULL}, is used to rename the values set as Marker in the LSA-2050
#'   (single capital letters) into informative character strings.
#'   \code{marker.rename} can contain a superset of the required mappings but 
#'   must contain all those used in the imported workbook.
#'   
#' @return The value returned depends on the argument passed to parameter
#'   \code{returned.data}. With the default, \code{"consolidated"}, all the data
#'   from both worksheets are returned in a single data frame with 44 columns.
#'   With arguments \code{"measured"} (43 columns), \code{"visible"} (17
#'   columns) and \code{"raw"} (24 columns) different variable subsets from the
#'   consolidated data frame are returned. Finally for debugging, with
#'   \code{"list"}, a list containing one data frame with the data from each
#'   worksheet is returned.
#' 
#' @section Warning!: The computed values for the formulas in the worksheets
#'   need to be present in the imported workbook for their import to succeed.
#'   If they are missing (zeros displayed) in the worksheet in place of the
#'   results from calculations, the zeros are replaced by \code{NA}. The 
#'   workbooks saved using the LSA software seem to contain the computed 
#'   values. Missing cached values from formulas can be restored both in
#'   Excel and in LibreOffice. In LibreOffice open the workbook file, use 
#'   \strong{data > calculate > recalculate hard}
#'   to recalculate all formulas and then save the file. (In LibreOffice a
#'   manually triggered hard recalculation is also needed for computed values to
#'   be displayed.) The LSA workbook file included in 'photobiologyInOut' and
#'   used in the examples and tests has been recalculated and saved in
#'   LibreOffice.
#'
#' @note This function has been tested with the output from LSA software
#'   version 1.09 and an example data file downloaded from Walz's website.
#'   To avoid silent bugs the code relies on column names rather than positional
#'   indexing. It is expected to tolerate reordering of columns and addition of
#'   columns compared to the format of the tested workbooks. If column naming is
#'   changed in future versions, triggering of error and warning messages
#'   can be expected.
#' 
#' @references \url{https://www.campbellsci.eu/}
#' 
#' @export
#' 
#' @examples
#' 
#'  file.name <-
#'    system.file("extdata", "walz-lsa-2050-prunus.xlsx", 
#'                package = "photobiologyInOut", 
#'                mustWork = TRUE)
#'                
#'  # import everything hidden and visible in workbook
#'  lsa.df1 <- read_walz_lsa_xlsx(file.name)
#'  colnames(lsa.df1)
#'  plot(lsa.df1$SAT.F.ls[[1]], type = "b")
#'  cat(comment(lsa.df1))
#'  
#'  # minimal import, only values visible in workbook
#'  # also changing the letter markers into informative labels 
#'  lsa.df2 <-
#'   read_walz_lsa_xlsx(file.name,
#'                      returned.data = "visible",
#'                      marker.rename = c(A = "adaxial", B = "abaxial"))
#'  colnames(lsa.df2)
#'  cat(comment(lsa.df2))
#'  
read_walz_lsa_xlsx <- function(file, 
                               label = NULL,
                               data_skip = 0, 
                               n_max = Inf, 
                               na = c("", "NA", "NAN"),
                               marker.rename = NULL,
                               returned.data = "consolidated",
                               drop.positions = NULL,
                               ...) {
  
  sheet_names <- readxl::excel_sheets(file)
  if (!all(sheet_names == c("Measure", "SAT Chart"))) {
    warning("Unexpected worksheet names \"",
            paste(sheet_names, collapse = "\", \""),
            "\" in file: ", basename(file))
  }
  
  label.file <- paste("File: ", basename(file), sep = "")
  if (is.null(label)) {
    label <- label.file
  } else if (!is.na(label)) {
    label <- paste(label.file, label, sep = "\n")
  }
  
  # read worksheet 1
  
  wrks1 <- readxl::read_excel(path = file,
                              sheet = 1,
                              na = na,
                              n_max = n_max,
                              .name_repair = "universal_quiet")
  # Worksheet uses "Satellite #" which is not a valid R name
  colnames(wrks1)[colnames(wrks1) == "Satellite.."] <- "Satellite.num"
  # worksheet columns Date and Time differ only in display format
  # we delete Date if identical to Time
  if (all(wrks1[["Date"]][-(1:3)] == wrks1[["Time"]][-(1:3)])) {
    wrks1[["Date"]] <- NULL
  }
  wrks1.head <- wrks1[1:3, ]
  wrks1 <- wrks1[-(1:3), -which(colnames(wrks1) %in% c("MesRef", "Model"))]
  
  # check if computed values have been imported
  if (all(wrks1[["mg.cm2"]] == 0) && all(wrks1[["AFLAV"]] == 0)) {
    if (returned.data != "raw") {
      warning("Cached computed values for formulas missing in file \"",
              basename(file), "\": NAs imported. ",
              "(Recalculate and resave workbook in Excel or LibreOffice.)")
      # replace filler zeros by NAs in computed variables
      zero.cols <- 
        grep("^Time$|^Type$|^Number$|^Marker$|^Q|^F[omV2-7]|^Tran|^Absor|^A[AF]|cm2$|^NBI$|^Incidence$",
              colnames(wrks1), value = TRUE)
      for (col in zero.cols) {
        wrks1[[col]] <- ifelse(wrks1[[col]] == 0, NA_real_, wrks1[[col]])
      }
    }
  }
  drop.positions <- drop.positions %||% all(is.na(wrks1[["Satellite.num"]]))
  if (drop.positions) {
    message("Dropping of geocode dependent data not yet implemented.")
  }
  
  if (length(marker.rename)) {
    if (all(unique(wrks1[["Marker"]]) %in% names(marker.rename))) {
      wrks1[["Marker"]] <- marker.rename[wrks1[["Marker"]]]
    } else {
      warning("'marker.rename' lacks mapping(s) for marker(s): \"",
              paste(setdiff(unique(wrks1[["Marker"]]), 
                            names(marker.rename)),
                    collapse = "\", \""),
              "\". Skipping!")
    }
  }
  
  attr(wrks1, "table.header") <- wrks1.head
  
  if (returned.data != "measured") {
    # read worksheet 2
    
    wrks2 <- readxl::read_excel(path = file,
                                sheet = 2,
                                na = na,
                                n_max = n_max,
                                .name_repair = "unique_quiet")
    
    # worksheet columns Date and Time differ only in display format
    # we delete Date if identical to Time
    if (all(wrks2[["Date"]][-(1:3)] == wrks2[["Time"]][-(1:3)])) {
      wrks2[["Date"]] <- NULL
    }
    wrks2.head <- wrks2[1:3, ]
    if (all(is.na(wrks2.head))) {
      wrks2 <- wrks2[-(1:3), ]
    }
    # we delete imported columns that had no head title and no data in the worksheet
    for (col in grep("^\\.\\.\\.[0-9]+", colnames(wrks2), value = TRUE)) {
      if (all(is.na(wrks2[[col]]))) {
        wrks2[[col]] <- NULL
      }
    }
  }
  
  # Assemble object to return
  comment.txt <- paste("Data from Walz LSA-2050 in file \"",
                       basename(file),
                       "\" imported on ",
                       format(lubridate::today()),
                       " and selected as \"", returned.data, 
                       "\" by function 'read_walz_lsa_xlsx()' from",
                       " 'photobiologyInOut' (==", 
                       utils::packageVersion("photobiologyInOut"), ").",
                       sep = "")
  if (returned.data == "list") {
    z <- list(wrks1, wrks2)
    names(z) <- sheet_names
    comment(z) <- comment.txt
    z
  } else if (returned.data %in% c("consolidated", "raw")) {
    # named list of numeric vectors, each giving the time series for a sample
    z <- as.list(as.data.frame(t(wrks2[ , -1])))
    SAT.time <- as.numeric(colnames(wrks2)[-1])
    # convert each vector into a data frame by adding a time column
    z <- lapply(X = z, 
                FUN = function(x) {data.frame(t = SAT.time, SAT.F = x)}    )
    names(z) <- wrks2[[1]]
    # add list as a column
    wrks1[["SAT.F.ls"]] <- I(z)
    # store times shared by all samples as an attribute
    attr(wrks1, "SAT.times.ms") <- SAT.time
    comment(wrks1) <- comment.txt
    if (returned.data == "raw") {
      selector <- 
        grepl("^Time$|^Type$|^Number$|^Marker$|^IF|^I[3-8]|^Satellite|^DoP|^Latitude|^Longitude|^Height$|^Leaf|^Sun|^AoI|^SAT",
              colnames(wrks1))
      wrks1 <- wrks1[ , selector]
    }
    wrks1
  } else if (returned.data == "measured") {
    comment(wrks1) <- comment.txt
    wrks1
  } else if (returned.data == "visible") {
    selector <- 
      grepl("^Time$|^Type$|^Number$|^Marker$|^Q|^F[omV]|^A[AF]|cm2$|^NBI$|^Incidence$",
            colnames(wrks1))
    wrks1 <- wrks1[ , selector]
    comment(wrks1) <- comment.txt
    wrks1
  }
}
