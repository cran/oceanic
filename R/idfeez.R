#' @title idfeez
#' @description This function allows you to identify location in which EEZ from a numeric vector.
#' @param lon Input the longitude.
#' @param lat Input the latitude.
#' @param ac logical. If \code{TRUE} will return full name of EEZ.
#' @export
#' @import sf
#' @import methods
#' @examples
#' idfeez(141,23)



idfeez <- function(lon, lat, ac = TRUE) {
  loc <- data.frame(lon = lon, lat = lat)
  
  # 創建 sf 點對象
  sptacsat <- st_as_sf(loc, coords = c("lon", "lat"), crs = 4326)
  
  # 獲取 eezdata 並轉換為 sf 對象
  eezdata <- get("eez_rg")
  eez_rg3 <- st_as_sf(eezdata)
  st_crs(eez_rg3) <- 4326
  
  # 找到點所在的多邊形
  idx <- st_join(sptacsat, eez_rg3)
  loc2 <- idx$ISO_3digit
  
  if (ac) {
    loc2 <- idx$EEZ
    return(loc2)
  }
  
  return(loc2)
}
