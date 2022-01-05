#' idfeez
#' @description This function allows you to identify location in which EEZ from a numeric vector.
#' @param lon Input the longitude.
#' @param lat Input the latitude.
#' @param ac logical. If \code{TRUE} will return full name of EEZ.
#' @export
#' @import sf
#' @import sp
#' @import rgdal
#' @examples
#' idfeez(141,23)



idfeez<-function(lon,lat,ac=TRUE){

loc<-data.frame(lon=lon,lat=lat)
sptacsat<- SpatialPoints(coordinates(loc))

proj4string(sptacsat) <-suppressWarnings(CRS("+init=epsg:4326"))

eezdata <- get("eez_rg")
sptacsat_O<- suppressWarnings(spTransform(sptacsat, CRS(proj4string(eezdata))))
idx<- over(sptacsat_O,eezdata)
loc2<-idx$ISO_3digit

if(ac){
  loc2<-idx$EEZ
  return(loc2)
}

return(loc2)

}


