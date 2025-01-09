#' @title idfport
#' @description This function allows you to identify port name from a numeric vector.
#' @param lon Input the longitude.
#' @param lat Input the latitude.
#' @export
#' @import sf
#' @examples
#' idfport(121.8006,25.14065)

idfport <- function(lon, lat) {

  ports <- get("port_sf")
  port_sf <- st_as_sf(ports)


  if (any(!st_is_valid(port_sf))) {
    port_sf <- st_make_valid(port_sf)
  }


  point <- data.frame(lon = lon, lat = lat)
  point_sf <- st_as_sf(point, coords = c("lon", "lat"), crs = 4326)


  st_crs(port_sf) <- st_crs(point_sf)


  result <- suppressWarnings(st_intersection(port_sf, point_sf))

  if (nrow(result) > 0) {
    return(result$id[1])
  } else {
    return("-")
  }
}
