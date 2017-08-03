# Get Route data (i.e. routes metadata)
#' @export GetRoutes
GetRoutes <- function(Dir="ftp://ftpext.usgs.gov/pub/er/md/laurel/BBS/DataFiles/") {
  routes=GetUnzip(ZipName=paste0(Dir, "Routes.zip"), FileName="routes.csv")
  routes <- dplyr::mutate(routes, routeID = as.numeric(paste(countrynum,
                                                formatC(statenum, width = 2, format = "d", flag = "0"),
                                                formatC(Route, width = 3, format = "d", flag = "0"), sep="")))
  routes
}
