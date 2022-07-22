#' @title sixtytoten
#' @description This function allows you to transfer the coordinate system from sexagesimal to decimal 
#' @param lon Input the longitude.
#' @param lat Input the latitude.
#' @export
#' @import sf
#' @import spData
#' @examples
#' sixtytoten(121.49,25.58)



sixtytoten<-function(lon,lat){


lon<-ifelse(lon==-180,-179,lon)
lon<-ifelse(lon==180,179,lon)

a1<-ifelse(lat>=0,(trunc(abs(lat)/5)+1)*2+71,(trunc(abs(lat)/5)+1)*2+72)

a2<-ifelse(lon<0,trunc(abs(lon)/5)*2+1,trunc(abs(lon)/5)*2+2)

farea<-a1*100+a2

return(farea)
}

