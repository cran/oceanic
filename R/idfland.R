#' @title idfland
#' @description This function allows you to identify location in which land or ocean.
#' @param lon Input the longitude.
#' @param lat Input the latitude.
#' @export
#' @import sf
#' @import spData
#' @examples
#' idfland(22,-5)



idfland<-function(lon,lat){

loc<-data.frame(lon=lon,lat=lat)
pts <- st_as_sf(loc, coords=1:2, crs=4326)
wd <- read_sf(system.file("shapes/world.gpkg", package="spData"))
loc$ps<-!is.na(as.numeric(st_intersects(pts, wd)))
loc$iden<-ifelse(loc$ps=="TRUE","land","ocean")
loc2<-loc$iden

return(loc2)

}


