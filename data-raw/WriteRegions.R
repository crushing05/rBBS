RegionsForZipFiles <- GetRegions(ZipFiles = TRUE)

code_lookup <- read.csv("data-raw/code_lookup.csv")

code_lookup <- dplyr::select(code_lookup, -group)


devtools::use_data(RegionsForZipFiles, code_lookup, overwrite=TRUE, internal = TRUE)
