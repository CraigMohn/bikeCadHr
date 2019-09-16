#' plot ride track data on a map
#'
#' \code{mapWindow} Return a 4-tuple box from tracks and maps
#'
#' Create a 4-tuple rectangle of lons and lats that is a specified map
#'   in a list of maps or is a minimal enclosure of the track pts
#'
#' @param geodf data frame or tibble containing at least: position_lat.dd,
#'    position_lon.dd
#' @param definedmaps list of name,bounds pairs of maps
#' @param usemap name of map in list of defined maps
#' @param margin.factor percentage to expand map area beyond limits of
#'    the track in each direction, when autodetermining region to plot
#' @param ...  other arguments
#'
#' @return c(min lon, max lon, min lat, max lat)
#'
#' @export
mapWindow <- function(geodf,definedmaps,usemap,margin.factor=0.05,...) {

  colnames(geodf) <- gsub("position_lat.dd","lat",colnames(geodf))
  colnames(geodf) <- gsub("position_lon.dd","lon",colnames(geodf))
  geodf <- geodf[!is.na(geodf$lat),]
  if (nrow(geodf)> 0) {
    lat.max <- max(geodf$lat)
    lat.min <- min(geodf$lat)
    lon.max <- max(geodf$lon)
    lon.min <- min(geodf$lon)
  }
  # expand the map area beyond the limits of the tracks
  marginfactor <- margin.factor
  # use maps if specified
  if (!missing(definedmaps)) {
    if (!missing(usemap)) {
      u.map <- definedmaps[[usemap]]
      if (is.null(u.map)) {
        warning(paste0("Exactly where did you define map ",usemap,"??"))
        return(NULL)
      }
      cat("\nUsing defined map area ",usemap)
      lat.min <- u.map[["lat"]][1]
      lat.max <- u.map[["lat"]][2]
      lon.min <- u.map[["lon"]][1]
      lon.max <- u.map[["lon"]][2]
    } else {
       for (i in seq_along(definedmaps)) {
         u.map <- definedmaps[[i]]
         if (lat.min >= u.map[["lat"]][1] & lat.max <= u.map[["lat"]][2] &
             lon.min >= u.map[["lon"]][1] & lon.max <= u.map[["lon"]][2]) {
           cat("\nusing defined map area ",names(definedmaps)[[i]])
           lat.min <- u.map[["lat"]][1]
           lat.max <- u.map[["lat"]][2]
           lon.min <- u.map[["lon"]][1]
           lon.max <- u.map[["lon"]][2]
           break
         }
       }
    }
    marginfactor <- 0.0
   }

   latwide <- lat.max - lat.min
   lonwide <- lon.max - lon.min
   map.lat.max.dd <- lat.max+latwide*marginfactor
   map.lat.min.dd <- lat.min-latwide*marginfactor
   map.lon.max.dd <- lon.max+lonwide*marginfactor
   map.lon.min.dd <- lon.min-lonwide*marginfactor
   return(c(map.lon.min.dd,map.lon.max.dd,
                              map.lat.min.dd,map.lat.max.dd))
}
