#' @title idfport
#' @description This function allows you to identify port name from a numeric vector.
#' @param lon Input the longitude.
#' @param lat Input the latitude.
#' @export
#' @import sf
#' @import sp
#' @import rgdal
#' @examples
#' idfport(121.8006,25.14065)


idfport<-function(lon,lat){

loc<-data.frame(lon=lon,lat=lat)
sptacsat<- SpatialPoints(coordinates(loc))

proj4string(sptacsat) <-suppressWarnings(CRS("+init=epsg:4326"))

portdata <- get("port")
sptacsat_O<- suppressWarnings(spTransform(sptacsat, CRS(proj4string(portdata))))
idx<- over(sptacsat_O,portdata)
loc2<-idx$id
return(loc2)
}


