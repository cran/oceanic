#' @title idfport
#' @description This function allows you to identify port name from a numeric vector.
#' @param lon Input the longitude.
#' @param lat Input the latitude.
#' @export
#' @import sf
#' @import sp
#' @examples
#' idfport(121.8006,25.14065)


idfport <- function(lon,lat) {

  port <- get("port")
  port_sf <- st_as_sf(port)
  port_sf <- st_set_crs(port_sf,NA)

  point <- data.frame(lon=lon, lat=lat)
  point_sf <- st_as_sf(point, coords = c("lon", "lat"))


  result <- suppressWarnings(st_intersection(port_sf, point_sf))

  if (nrow(result) > 0) {
    return(result$id)
  } else {
    return("-")
  }
}
