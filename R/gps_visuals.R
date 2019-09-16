#' plot ride track data on a map
#'
#' \code{map_rides} Plot lat/long data on maps, optionally using colored
#'   dots to indicate speed
#'
#' Create a map displaying the tracks specified.  Tracks may be drawn all in one
#'   specified color, with each separate track drawn in a different color from
#'   a specified palette, or as a series of closely spaced dots (which may
#'   be prodded into appearing as a line through judicious choice of point
#'   size and alpha) with the color varying based on speed and the chosen
#'   palette.
#'
#' @param geodf data frame or tibble containing at least: position_lat.dd,
#'    position_lon.dd,(or lat,lon)(both numeric,decimal degrees),
#'    startbutton.date(int),startbutton.time(int),segment(numeric),
#'    and speed.m.s for speed coloring
#' @param rgl use rgl openGL to draw 3D track in viewer
#' @param maptitle string containing title
#' @param ... arguments for 3d rendering calls
#'
#' @return NULL
#'
#' @export
map_rides <- function(geodf,maptitle,
                      rgl=FALSE,
                      ...) {

  colnames(geodf) <- gsub("position_lat.dd","lat",colnames(geodf))
  colnames(geodf) <- gsub("position_lon.dd","lon",colnames(geodf))
  geodf <- geodf[!is.na(geodf$lat),]

  mapwdw <- mapWindow(geodf,...)
  maptrack2d::draw2d(geodf,mapWindow=mapwdw,maptitle=maptitle,...)
  if (rgl) {
    maptrack3d::draw3dMap(paths=geodf,
                          mapWindow=mapwdw,
                          ...)
  }
  return(NULL)
}
