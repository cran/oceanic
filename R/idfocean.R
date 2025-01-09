#' @title idfocean
#' @description Return The Pacific Ocean(PAC), Indian Ocean(IND) or Atlantic Ocean(ATL) of your coordinate.
#' @import sf
#' @import methods
#' @param lon Input the longitude.
#' @param lat Input the latitude.
#' @return the ocean of \code{lon} and \code{lat}.
#' @export
#' @examples
#' idfocean(125,20)

idfocean <- function(lon, lat) {
  pac1 <- st_polygon(list(rbind(c(-180, -70), c(-70, -70), c(-70, 5), c(-77, 7.75), c(-79, 9.25), c(-81, 8.5),
                                c(-82, 8.5), c(-91, 17.5), c(-101, 17.5), c(-101, 70), c(-180, 70), c(-180, -70))))
  pac2 <- st_polygon(list(rbind(c(150, -70), c(180, -70), c(180, 70), c(99, 70), c(99, 8.5), c(103, 2.5),
                                c(103, -2.5), c(106, -6), c(109, -7.5), c(113.46667, -8), c(129, -8), 
                                c(129, -15), c(150, -28), c(150, -70))))
  pac <- st_multipolygon(list(pac1, pac2))
  ind <- st_polygon(list(rbind(c(42, 48), c(42, 30), c(20, 30), c(20, 5), c(20, -70), c(150, -70), 
                               c(150, -30), c(129, -20), c(129, -8), c(113.46667, -8), c(109, -7.5), 
                               c(106, -6), c(105.14, -5), c(99, 2.4), c(103, 2.4), c(99, 8.5), 
                               c(99, 70), c(42, 48))))
  atl <- st_polygon(list(rbind(c(-101, 70), c(-101, 17.5), c(-91, 17.5), c(-82, 8.5), c(-81, 8.5), 
                               c(-79, 9.25), c(-77, 7.75), c(-70, 5), c(-70, -70), c(20, -70), 
                               c(20, 30), c(-5, 30), c(-5, 40), c(6, 48), c(42, 48), c(99, 70), c(-101, 70))))
  glb <- st_sfc(pac, ind, atl, crs = 4326)
  ocean_sf <- st_sf(name = c("PAC", "IND", "ATL"), geometry = glb)
  points_sf <- st_as_sf(data.frame(lon = lon, lat = lat), coords = c("lon", "lat"), crs = 4326)
  results <- st_join(points_sf, ocean_sf, join = st_intersects)
  return(ifelse(!is.na(results$name), results$name, "-"))
}
