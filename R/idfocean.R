#' @title idfocean
#' @description Return The Pacific Ocean(PAC), Indian Ocean(IND) or Atlantic Ocean(ATL) of your coordinate.
#' @import sf
#' @import sp
#' @import methods
#' @param lon Input the longitude.
#' @param lat Input the latitude.
#' @return the ocean of \code{lon} and \code{lat}.
#' @export
#' @examples
#' idfocean(125,20)

idfocean <- function(lon,lat){
  #PAC
    p1 = list(rbind(c(-180,-70),c(-70,-70),c(-70,5),c(-77,7.75),c(-79,9.25),c(-81,8.5),c(-82,8.5),c(-91,17.5),c(-101,17.5),c(-101,70),c(-180,70),c(-180,-70)))
    p2 = list(rbind(c(150,-70),c(180,-70),c(180,70),c(99,70),c(99,8.5),c(103,2.5),c(103,-2.5),c(106,-6),c(109,-7.5),c(113.46667,-8),c(129,-8),c(129,-15),c(150,-28),c(150,-70)))
    p = st_multipolygon(list(p1,p2))
  #IND
    i = st_polygon(list(rbind(c(42,48),c(42,30),c(20,30),c(20,5),c(20,-70),c(150,-70),c(150,-30),c(129,-20),c(129,-8),c(113.46667,-8),c(109,-7.5),c(106,-6),c(105.14,-5),c(99,2.4),c(103,2.4),c(99,8.5),c(99,70),c(42,48))))
  #ATL
    a = st_polygon(list(rbind(c(-101,70),c(-101,17.5),c(-91,17.5),c(-82,8.5),c(-81,8.5),c(-79,9.25),c(-77,7.75),c(-70,5),c(-70,-70),c(20,-70),c(20,30),c(-5,30),c(-5,40),c(6,48),c(42,48),c(99,70),c(-101,70))))
  #glb_sf
    glb <- st_sfc(p,i,a)
    sf <- data.frame(name=c("PAC","IND","ATL")) %>% st_set_geometry(glb)
  #lon,lat In
    d <- data.frame(x=lon,y=lat) 
	d <- st_as_sf(d,coords = c("x","y"))
  #intersect
    idx <- st_join(d, sf, join=st_intersects)
  #data Out
    return (idx$name)
}
