# Function to query 10 or 50 stop data for all species in a year
#' @export GetRouteData
#' @importFrom RCurl getURL
#' @importFrom plyr ldply
GetRouteData=function(AOU=NULL, countrynum=NULL, states=NULL, year = NULL, weather=NULL, routes=NULL, 
                      TenStops = TRUE, 
                      Dir="ftp://ftpext.usgs.gov/pub/er/md/laurel/BBS/DataFiles/") {
  
  if(TenStops) {
    DirData <- paste0(Dir, "States/")
    CountString <- "^count"
  } else {
    if(any(year<1997)) stop("Data only available from 1997: pre-1997 data not integrated into this function for 50 stop data (yet)")
    DirData <- paste0(Dir, "50-StopData/1997ToPresent_SurveyWide/")
    CountString <- "^stop"
  }
  if(!is.null(countrynum) & any(!(countrynum%in%c(124, 484, 840)))) stop("countrynum should be either 124 (Canada), 484 (Mexico), or 840 (USA)")
  
  GetDat <- function(file, dir, year, AOU, countrynum, states) {
    dat=GetUnzip(ZipName=paste0(dir, file), FileName=gsub("^Fifty", "fifty", gsub("zip", "csv", file)))
    names(dat) <- tolower(names(dat))
    if(is.null(year)) {  UseYear <- TRUE  } else {  UseYear <- dat$year%in%year  }
    if(is.null(AOU)) {  UseAOU <- TRUE  } else {  UseAOU <- dat$aou%in%AOU  }
    if(is.null(countrynum)) {  UseCountry <- TRUE  } else {  UseCountry <- dat$countrynum%in%countrynum  }
    if(is.null(states)) {  UseState <- TRUE  } else {  UseState <- dat$statenum%in%states  }
    Use <- UseYear & UseAOU & UseCountry & UseState
    if(sum(Use)>0) {
      dat <- dplyr::mutate(dat, routeID = as.numeric(paste(countrynum,
                                                    formatC(statenum, width = 2, format = "d", flag = "0"),
                                                    formatC(route, width = 3, format = "d", flag = "0"), sep="")))
      dat=subset(dat, subset=Use)
      return(dat)      
    } else return(NULL)
  }
  
# Only use the files we want
  CountriesToUse <- if(!is.null(countrynum)) {
    RegionsForZipFiles$countrynum%in%countrynum 
  } else {
    TRUE
  }
  StatesToUse <- if(!is.null(states)) {
    RegionsForZipFiles$RegionCode%in%states 
  } else {
    TRUE
  }
  ToUse <- CountriesToUse & StatesToUse
  if(TenStops) {
    Files <- RegionsForZipFiles$FileName10stop[ToUse]
    Missing <- ToUse & is.na(RegionsForZipFiles$FileName10stop)
  } else { # 50 stop
    Files <- unique(RegionsForZipFiles$FileName50stop[ToUse])
    Missing <- ToUse & is.na(RegionsForZipFiles$FileName50stop)
  }
  
  if(length(Files)==0) stop("No data for the states specified")
  if(any(is.na(Files))) warning(paste0("No data for these states: ", paste(RegionsForZipFiles$'State/Prov/TerrName'[Missing], collapse=", ")))
  
  Data.lst <- sapply(Files[!is.na(Files)], GetDat, dir=DirData, year=year, AOU=AOU, countrynum=countrynum, states=states, simplify=FALSE)
  Data <- ldply(Data.lst)
  Data <- dplyr::filter(Data, rpid == 101)
  Data <- dplyr::select(Data, -.id, -countrynum, -statenum, -route, -rpid)
  Data <- dplyr::rename(Data, Year = year)
  
  
  Data
}

# Also: add a vars option, to only return some variables
#   Try <- Get50RouteData(countrynum=NULL, states=c(89, 40:60), weather=NULL, routes=NULL, AOU=c(4050, 3850), year=2010, Zeroes=FALSE, Dir=NULL)