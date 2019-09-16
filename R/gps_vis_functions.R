pointsFromMatrix <- function(dataMat) {
  dmCol <- ncol(dataMat)
  dmRow <- nrow(dataMat)
  row <- matrix(rep(seq(1:dmRow),dmCol),ncol=dmCol)
  col <- t(matrix(rep(seq(1:dmCol),dmRow),ncol=dmRow))
  dmPoint <- !is.na(as.vector(dataMat))
  return(as_tibble(list(x=as.vector(col)[dmPoint],
                        y=as.vector(row)[dmPoint],
                        z=as.vector(dataMat)[dmPoint])))
}

yRatio <- function(rrr) {
  xmin <- rrr@extent@xmin
  xmax <- rrr@extent@xmax
  ymin <- rrr@extent@ymin
  ymax <- rrr@extent@ymax
  return(yRatioPts(xmin,xmax,ymin,ymax))
}
yRatioPts <- function(xmin,xmax,ymin,ymax) {
  width <-
    (raster::pointDistance(cbind(xmin,ymin),cbind(xmax,ymin),lonlat=TRUE) +
     raster::pointDistance(cbind(xmin,ymax),cbind(xmax,ymax),lonlat=TRUE)) / 2
  height <-
    (raster::pointDistance(cbind(xmin,ymin),cbind(xmin,ymax),lonlat=TRUE) +
     raster::pointDistance(cbind(xmax,ymin),cbind(xmax,ymax),lonlat=TRUE)) / 2
  return(height/width)
}
