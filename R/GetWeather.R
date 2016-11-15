# Get Weather data (i.e. which routes were surveyed each year)
#' @export GetWeather
GetWeather <- function(Dir="ftp://ftpext.usgs.gov/pub/er/md/laurel/BBS/DataFiles/") {
  weather=GetUnzip(ZipName=paste0(Dir, "Weather.zip"), FileName="weather.csv")
  weather <- mutate(weather, routeID = as.numeric(paste(countrynum,
                                                formatC(statenum, width = 2, format = "d", flag = "0"),
                                                formatC(Route, width = 3, format = "d", flag = "0"), sep="")))
  weather  
}
