
read_CIE_csv <- function(file.name) {
  
  metadata.file.name <- paste(file.name, "metadata.json", sep = "_")
  base.name <- basename(file.name)
  
  if (grepl("^CIE_illum", base.name)) {
    member.class <- "source_spct"
    spct.data.var <- "s.e.irrad"
  } else if (grepl("^CIE_sle", base.name)) {
    member.class <- "response_spct"
    spct.data.var <- "s.e.response"
  }
  
  cie_metadata.ls <- jsonlite::fromJSON(metadata.file.name)
  
  col.names <- cie_metadata.ls$datatableInfo$columnHeaders$title
  
  cie.tb <- read.csv(file.name, 
                     header = FALSE,
                     col.names = col.names)
  
  z <- 
    photobiology::split2mspct(cie.tb, 
                              member.class = member.class,
                              spct.data.var = spct.data.var,
                              w.length.var = "lambda")
  
  photobiology::setScaled(z, scaled = TRUE)
  
  photobiology::what_measured(z) <- cie_metadata.ls$titles$title
  photobiology::how_measured(z) <-  cie_metadata.ls$relatedItems$titles[[1]]
  
  comment(z) <- 
    with(cie_metadata.ls,
         paste(identifier$identifierType, ": ", identifier$identifier,
               "; ", alternateIdentifiers$alternateIdentifierType, ": ", 
               alternateIdentifiers$alternateIdentifier,
               "; units: ", 
               paste(datatableInfo$columnHeaders$unit, collapse = ", "),
               "\n", publisher,
               ". ", rightsList$rightsIdentifier,
               sep = "")       
    )
  
  z
}

# CIE_D55_2018.mspct <- read_CIE_csv("./inst-not/CIE-JSON/CIE_illum_D55.csv")
# CIE_D75_2018.mspct <- read_CIE_csv("./inst-not/CIE-JSON/CIE_illum_D75.csv")
# CIE_LEDs_2018.mspct <- read_CIE_csv("./inst-not/CIE-JSON/CIE_illum_LEDs.csv")
# 
# autoplot(CIE_D55_2018.mspct) + geom_point()
# autoplot(CIE_D75_2018.mspct) + geom_point()
# autoplot(CIE_LEDs_2018.mspct)
