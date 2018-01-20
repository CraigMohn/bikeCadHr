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
#' @param outfile name of output file, extension is either .tiff or .jpg
#' @param maptitle string containing title
#' @param definedmaps list, each named entry is a list of two vectors of
#'    length two, named lat and lon, containing the min and max of the
#'    latitude andf longitude of the map area
#' @param usemap string identifying area from list \eqn{definedmaps} or
#' @param maptype map to use as background (\code{"maptoolkit-topo"} or
#'       \code{"bing"} or \code{"osm"} or \code{"stamen-terrain"} or
#'       \code{"esri-topo"}  or \code{"stamen-watercolor"} or \code{"mapbox"} or
#'       \code{"esri"} or \code{"osm-public-transport"} or \code{"opencyclemap"}
#'       or \code{"apple-iphoto"} or \code{"skobbler"})
#' @param minTiles minimum number of tiles fetched for map, larger is slower
#'    but better quality
#' @param mapsize pixel size of map created
#' @param fine.map if true, use c(7680,4800) for map size
#' @param margin.factor percentage to expand map area beyond limits of
#'    the track in each direction, when autodetermining region to plot
#' @param draw.speed if true, draw track(s) as a series of points whose color
#'    indicates travel speed
#' @param line.color the color to draw the lines of the tracks,
#'    if a palette is specified (\code{"plasma"} or \code{"viridis"} or
#'     \code{"rainbow"} or \code{"heat"} or \code{"red-blue"})is specified,
#'    each track supplied will be assigned a color from that palette
#' @param line.width the width of the line for the tracks
#' @param line.alpha the opacity of the line
#' @param speed.color palette to use to represent speed on plot
#'     (\code{"speedcolors"} or \code{"red-blue-green"} or
#'     \code{"rainbow"} or \code{"plasma"} or \code{"magma"} or
#'     \code{"heat"} )
#' @param speed.alpha opacity of the speed line if \eqn{speed.color} specified
#' @param speed.ptsize size for the symbols used to plot the speed line,
#'    doubled if \eqn{fine.map} is set true.
#' @param speed.pch the character to use plotting the points of the speed line
#' @param jpeg.quality the "quality" of the JPEG image, a percentage. Smaller
#'    values will give more compression but also more degradation of the image
#'
#' @return NULL
#'
#' @export
map_rides <- function(geodf,outfile,maptitle,definedmaps,usemap,
                      maptype="maptoolkit-topo",minTiles=50,
                      mapsize=c(1600,1200),fine.map=FALSE,margin.factor=0.08,
                      draw.speed=FALSE,
                      line.color="magenta",line.width=3,line.alpha=0.8,
                      speed.color="speedcolors",speed.alpha=0.7,
                      speed.ptsize=6,speed.pch=19,
                      jpeg.quality=90) {

  ##  geodf: a tibble or dataframe containing at least:
        #  position_lat.dd,position_lon.dd,(or lat,lon)(both numeric),
        #  startbutton.time(int),segment(numeric)
  start.hour <- start.time <- startbutton.date <-  NULL
  stoplabels <- timestamp.s <- NULL
  if (substr(outfile,nchar(outfile)-4,nchar(outfile))==".tiff") {
    outfiletype <- "tiff"
  } else if (substr(outfile,nchar(outfile)-3,nchar(outfile))==".jpg") {
    outfiletype <- "jpeg"
  } else {
    stop("invalid output filetype")
  }
  if (fine.map) mapsize <- c(7680,4800)
  if (fine.map) speed.ptsize <- 2*speed.ptsize

  colnames(geodf) <- gsub("position_lat.dd","lat",colnames(geodf))
  colnames(geodf) <- gsub("position_lon.dd","lon",colnames(geodf))
  geodf <- geodf[!is.na(geodf$lat),]
  if (nrow(geodf)> 0) {
    geodf$start.time <-
         dateTimeStr(geodf$startbutton.date,geodf$startbutton.time)
    lat.max <- max(geodf$lat)
    lat.min <- min(geodf$lat)
    lon.max <- max(geodf$lon)
    lon.min <- min(geodf$lon)
    # expand the map area beyond the limits of the tracks
    marginfactor <- margin.factor
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
    if (min(lat.max-lat.min,lon.max-lon.min) > .005){
      latwide <- lat.max - lat.min
      lonwide <- lon.max - lon.min
      map.lat.max.dd <- lat.max+latwide*marginfactor
      if (!missing(maptitle)) map.lat.max.dd <- lat.max+latwide*0.05
      map.lat.min.dd <- lat.min-latwide*marginfactor
      map.lon.max.dd <- lon.max+lonwide*marginfactor
      map.lon.min.dd <- lon.min-lonwide*marginfactor
      map <- OpenStreetMap::openmap(c(map.lat.max.dd, map.lon.min.dd),
                        c(map.lat.min.dd, map.lon.max.dd),
                        type=maptype,minNumTiles=minTiles)
      #native mercator-for longlat add:
      #                map <- openproj(map,projection="+proj=longlat")
    } else {
     cat("\n",outfile," not created, map too small")
        return(NULL)
    }
    map.lat.min <- map$bbox[["p2"]][2] * (180 / (2 ^ 31))
    map.lat.max <- map$bbox[["p1"]][2] * (180 / (2 ^ 31))
    map.lon.min <- map$bbox[["p1"]][1] * (180 / (2 ^ 31))
    map.lon.max <- map$bbox[["p2"]][1] * (180 / (2 ^ 31))
    aspectcorrect <- 1  #  using mercator, better than latlon at northern lats
    aspectratio <- (map.lon.max - map.lon.min)/(map.lat.max-map.lat.min)
    if (aspectratio > mapsize[1]/mapsize[2]) {
      mapwidth <- mapsize[1]
      mapheight <- mapsize[1]/aspectratio
    } else {
      mapwidth <- mapsize[2]*aspectratio
      mapheight <- mapsize[2]
    }
    #  expand limits so any segments that have their middle cut out will
    #        be wrongly rendered, but off-map
    #    (except perhaps near corners, not worth fixing now, maybe never)
    mapdf <- geodf[geodf$lon>=map.lon.min.dd-.01 &
                   geodf$lon<=map.lon.max.dd+.01 &
                   geodf$lat>=map.lat.min.dd-.01 &
                   geodf$lat<=map.lat.max.dd+.01,]
    trackstarts <- unique(mapdf$start.time)
    cat("\noutfile=",outfile)
    if (outfiletype=="jpeg") {
      jpeg(outfile, width = mapwidth,height=mapheight,quality=jpeg.quality)
    } else if (outfiletype=="tiff") {
      tiff(outfile, width = mapwidth, height = mapheight,
           type="cairo",compression="zip+p")
    }
    par(mar = rep(0,4))
    plot(map)
    if (!missing(maptitle)) title(main=as.character(maptitle),
           cex.main=2*mapheight/800,col="gray57",line=-3*(mapheight/800))
    if (!draw.speed) {
      if (line.color=="plasma") {
        colorvec <- viridis::plasma(length(trackstarts),begin=0.0,end=0.7)
      } else if (line.color=="viridis") {
        colorvec <- viridis::viridis(length(trackstarts),begin=0.1,end=0.9)
      } else if (line.color=="rainbow") {
        colorvec <- rainbow(length(trackstarts),start=0.2,end=0.9)
      } else if (line.color=="heat") {
        colorvec <- heat.colors(length(trackstarts))
      } else if (line.color=="red-blue") {
        colorvec <- colorRampPalette(c("red","blue"))(101)

      } else {
        colorvec <- rep(line.color,length(trackstarts))
      }
      for (trkstarttime in trackstarts) {
        draw.color <- colorvec[which(trackstarts == trkstarttime)]
        for (seg in unique(mapdf[mapdf$start.time==trkstarttime,]$segment)) {
          use = mapdf$start.time==trkstarttime & mapdf$segment==seg &
               (lead_one(mapdf$segment)==mapdf$segment |
                lag_one(mapdf$segment)==mapdf$segment) # can't do 1 pt lines
          if(sum(use)>0) {
            temp <-
               OpenStreetMap::projectMercator(mapdf$lat[use],mapdf$lon[use])
            lines(temp[,1], temp[,2],type = "l",
            col = scales::alpha(draw.color, line.alpha), lwd = line.width)
          }
        }
     }
    } else {
      if (speed.color=="plasma") {
        spdcolors <- rev(plasma(101))
      } else if (speed.color=="magma") {
        spdcolors <- rev(magma(101))
      } else if (speed.color=="heat") {
        spdcolors <- rev(heat.colors(101))
      } else if (speed.color=="rainbow") {
        spdcolors <- (rainbow(101,start=0.15,end=1))
      } else if (speed.color=="red-blue-green") {
        spdcolors <- colorRampPalette(c("red","blue","green"))(101)
      } else {
        spdcolors <- colorRampPalette(c("red","orange","cornflowerblue",
                      "dodgerblue","blue","darkorchid","purple","magenta"))(101)
      }
      speed <- mapdf$speed.m.s*2.23694
      speed[speed>40] <- 40
      speed[speed<3] <- 3
      colorvec <- spdcolors[floor(100*(speed - 3)/37) + 1]
      temp <- OpenStreetMap::projectMercator(mapdf$lat, mapdf$lon)
      points(temp[,1], temp[,2], pch=speed.pch, col=alpha(colorvec,speed.alpha),
             lwd=speed.ptsize)
    }
    dev.off()
  }
  return(NULL)
}
#' plot heartrate, cadence, speed and more using color-driven
#'   data summaries to convey performance patterns
#'
#'
#' \code{plot_elev_profile_plus} creates a plot using the elevation profile
#'   and colored bands to compactly display the relationship between terrain,
#'   speed, cadence and heartrate on a given ride
#'
#' Generates a plot which compactly displays the elevation profile and its
#'    relationship with speed, cadence and heartrate.  Also displays clock
#'    information and shows location and approximate length of stops.
#'
#' @param track data frame or tibble containing track details including
#'    location and elevation data, as well as heartrate and cadence
#' @param summary data frame or tibble containing track summary from the
#'    data frames generated by \code{read_ride}
#' @param savefn name of output file, format is determined by the file
#'    extension specified, .pdf, .png or .jpg
#' @param title title printed over top center of map
#' @param palette palette to use in graphic (\code{"plasma"} or \code{"magma"}
#'    or \code{"inferno"} or \code{"viridis"})
#' @param vertical.multiplier default vertical exaggeration factor.  Default
#'    ranges from 25 to 60 depending on the length being plotted
#' @param ppm override calculated default number of points per mile
#' @param elevation.shape shape to use for drawing the elevation plot
#' @param show.stops draw row with short and long stops
#' @param show.time draw row with 15 minute and hour marks
#' @param show.summary display summary results if supplied
#' @param cad.cont display cadence as a continuos color map
#' @param hr.low heartrates below this are same color
#' @param hr.high heartrates above this are same color
#' @param hr.color.low set color for hr.low and lower
#' @param hr.color.high set color for hr.highand higher
#'    colors are from same palette as speeds, number is speed corresponding
#'    to the desired limit on the range of heartrates
#' @param cad.low low cadence limit
#' @param cad.target target cadence range minimum
#' @param cad.cont.low lower cadence limit for continuous color, all
#'    lower cadences are displayed as same color
#' @param cad.cont.high upper cadence limit for continuous color, all
#'    higher cadences are displayed as same color
#' @param cad.color.low set color for cadence at cad.low or below
#' @param cad.color.mid set color for cadence above low but below target
#' @param cad.color.high set color for cadence above target
#' @param hr.smooth.bw.meters bandwidth (in meters) for smoothing kernel
#'    for heartrate data
#' @param hr.smooth.nn number of points (on each side) to use in smoothing
#'    kernel for heartrate data
#' @param cad.smooth.bw.meters bandwidth (in meters) for smoothing kernel
#'    for cadence data
#' @param cad.smooth.nn number of points (on each side) to use in
#'    smoothing kernel for cadence data
#' @param min.numpoints pad out the plot on the right if too short
#'
#' @return a ggplot object
#'
#' @export
plot_elev_profile_plus <- function(track,summary,savefn,title="Ride",
          palette="plasma",vertical.multiplier=NA,ppm=NA,elevation.shape=46,
          show.stops=TRUE,show.time=TRUE,show.summary=TRUE,cad.cont=TRUE,
          hr.low=100,hr.high=170,hr.color.low=9,hr.color.high=31,
          cad.low=65,cad.target=88,cad.cont.low=60,cad.cont.high=100,
          cad.color.low=3,cad.color.mid=9,cad.color.high=15,
          hr.smooth.bw.meters=6,hr.smooth.nn=5,
          cad.smooth.bw.meters=20,cad.smooth.nn=15,
          min.numpoints=1000) {


  return(
    plot_profile(track,summary,savefn,title,palette,
                  verticalMultiplier=vertical.multiplier,ppm,
                  elevationShape=elevation.shape,
                  hrDistance=TRUE,cadDistance=TRUE,
                  hrTime=TRUE,cadTime=FALSE,
                  showTime=FALSE,showSummary=show.summary,
                  showStops=show.stops,
                  hrLow=hr.low,hrHigh=hr.high,hrColorLow=hr.color.low,
                  hrColorHigh=hr.color.high,
                  cadLow=cad.low,cadTarget=cad.target,
                  cadCont=cad.cont,
                  cadContLow=cad.cont.low,cadContHigh=cad.cont.high,
                  cadColorLow=cad.color.low,cadColorMid=cad.color.mid,
                  cadColorHigh=cad.color.high,
                  hrSmoothBW=hr.smooth.bw.meters,hrSmoothNN=hr.smooth.nn,
                  cadSmoothBW=cad.smooth.bw.meters,
                  cadSmoothNN=cad.smooth.nn,
                  minNumPoints=min.numpoints,
                             imperial=TRUE)

  )
}
#' plot heartrate, cadence, speed and more using color-driven
#'   data summaries to convey performance patterns
#'
#'
#' \code{plot_profile} creates a plot which uses the elevation profile
#'   and colored bands to compactly display the relationship between terrain,
#'   speed, cadence and heartrate on a given ride
#'
#' Generates a plot which compactly displays the elevation profile and its
#'    relationship with speed, cadence and heartrate.  Also displays clock
#'    information and shows location and approximate length of stops.
#'
#' @param track data frame or tibble containing track details including
#'    location and elevation data, as well as heartrate and cadence
#' @param summary data frame or tibble containing track summary from the
#'    data frames generated by \code{read_ride}
#' @param savefn name of output file, format is determined by the file
#'    extension specified, .pdf, .png or .jpg
#' @param title title printed over top center of map
#' @param palette palette to use in graphic (\code{"plasma"} or \code{"magma"}
#'    or \code{"inferno"} or \code{"viridis"})
#' @param verticalMultiplier default vertical exaggeration factor.  Default
#'    ranges from 25 to 60 depending on the length being plotted
#' @param ppm override calculated default number of points per mile
#' @param elevationShape shape to use for drawing the elevation plot
#' @param hrDistance display HR data scaled by distance if available
#' @param cadDistance display cadence data scaled by distance if available
#' @param powerDistance display power data scaled by distance if available
#' @param hrTime display HR data scaled by time if available
#' @param cadTime display cadence data scaled by time if available
#' @param powerTime display power data scaled by time if available
#' @param showStops draw row with short and long stops on distance axis
#' @param showTime draw time axis - use FALSE to suppress,
#' @param showSummary display summary results if supplied
#' @param cadCont display cadence as a continuos color map
#' @param hrLow heartrates below this are same color
#' @param hrHigh heartrates above this are same color
#' @param hrColorLow set color for hrLow and lower
#' @param hrColorHigh set color for hrHigh and higher
#'    colors are from same palette as speeds, number is the speed corresponding
#'    to the desired limit on the range of heartrates
#' @param cadLow low cadence limit
#' @param cadTarget target cadence range minimum
#' @param cadContLow lower cadence limit for continuous color, all
#'    lower cadences are displayed as same color
#' @param cadContHigh upper cadence limit for continuous color, all
#'    higher cadences are displayed as same color
#' @param cadColorLow set color for cadence at cadLow or below
#' @param cadColorMid set color for cadence above low but below target
#' @param cadColorHigh set color for cadence above target
#' @param powerLow power outputs below this are same color
#' @param powerHigh power outputs above this are same color
#' @param powerColorLow set color for powerLow and lower
#' @param powerColorHigh set color for powerHigh and higher
#' @param hrSmoothBW bandwidth (in seconds) for smoothing kernel
#'    for heartrate data
#' @param hrSmoothNN number of points (on each side) to use in smoothing
#'    kernel for heartrate data
#' @param cadSmoothBW bandwidth (in seconds) for smoothing kernel
#'    for cadence data
#' @param cadSmoothNN number of points (on each side) to use in
#'    smoothing kernel for cadence data
#' @param powerSmoothBW bandwidth (in seconds) for smoothing kernel
#'    for power data
#' @param powerSmoothNN number of points (on each side) to use in smoothing
#'    for power data
#' @param elevSmoothBWMeters bandwidth (in meters) for elevation smoothing
#' @param stopToleranceMeters tolerance in meters for determining stop
#' @param minSecsRolling restarts shorter than this number of seconds
#'    considered to be part of stop
#' @param minNumPoints pad out the plot on the right if too short
#' @param imperial use mi and ft instead of km and m
#'
#' @return a ggplot object
#'
#' @export
plot_profile <- function(track,summary,savefn,title="Ride starting ",
                         palette="plasma",
                         verticalMultiplier=NA,ppm=NA,
                         elevationShape=46,
                         hrDistance=TRUE,cadDistance=TRUE,powerDistance=TRUE,
                         hrTime=TRUE,cadTime=TRUE,powerTime=TRUE,
                         showTime=TRUE,showSummary=TRUE,
                         showStops=TRUE,
                         hrLow=100,hrHigh=170,
                         hrColorLow=11,hrColorHigh=26,
                         cadLow=65,cadTarget=88,
                         cadCont=TRUE,cadContLow=60,cadContHigh=100,
                         cadColorLow=3,cadColorMid=9,cadColorHigh=15,
                         powerLow=75,powerHigh=350,
                         powerColorLow=2,powerColorHigh=20,
                         hrSmoothBW=6,hrSmoothNN=6,
                         cadSmoothBW=10,cadSmoothNN=10,
                         powerSmoothBW=20,powerSmoothNN=20,
                         elevSmoothBWMeters=15,
                         stopToleranceMeters=5,
                         minSecsRolling=5,
                         minNumPoints=3000,
                         imperial=TRUE) {
  ##  what will we add below the profile
  cadDistance <- cadDistance & any(!is.na(track$cadence.rpm))
  hrDistance <- hrDistance & any(!is.na(track$heart_rate.bpm))
  powerDistance <- powerDistance & any(!is.na(track$power.watts))
  cadTime <- cadTime & any(!is.na(track$cadence.rpm))
  hrTime <- hrTime & any(!is.na(track$heart_rate.bpm))
  powerTime <- powerTime & any(!is.na(track$power.watts))
  cadTime <- cadTime & showTime
  hrTime <- hrTime & showTime
  powerTime <- powerTime & showTime
  showHr <- hrDistance | hrTime
  showCad <- cadDistance | cadTime
  showPower <- powerDistance | powerTime
  ##  set up numeric time (in seconds) and smoothed variables
  walltime <- as.numeric(difftime(track$timestamp.s,track$timestamp.s[1],
                                  units="secs"))
  if (imperial) {
    distance  <- milesFromMeters(track$distance.m)
    stopDistLim <- milesFromMeters(stopToleranceMeters)
    elevsmbw <- milesFromMeters(elevSmoothBWMeters)
  }
  else {
    distance <- kmFromMeters(track$distance.m)
    stopDistLim <- kmFromMeters(stopToleranceMeters)
    elevsmbw <- kmFromMeters(elevSmoothBWMeters)
  }
  # grab the structure of starts and stops
  startsAndStops <- segSummary(time=walltime,dist=distance,
                               segment=track$segment,stopped=track$stopped,
                               stopDistTolerance=stopDistLim,
                               stopRunLength=minSecsRolling)

  #  note that may be multiple records at same distance.  smoothing
  #    algorithm will weight equally.
  elevsm <- smoothData(yvec=track$altitude.m,xvar=distance,
                       bw=elevsmbw,nneighbors=18,kernel="epanechnikov")
  if (showCad) {
    cadzero <- track$cadence.rpm == 0
    cadna <- is.na(track$cadence.rpm)
    cadencetemp <- track$cadence.rpm
    cadencetemp[cadzero] <- NA
    cadencesm <- smoothDataSegments(yvec=cadencetemp,xvar=walltime,
                                    segment=track$segment,
                                    bw=cadSmoothBW,nneighbors=cadSmoothNN,
                                    kernel="triangular")
    cadencesm[cadzero] <- 0.0
    cadencesm[cadna] <- NA
  } else {
    cadencesm <- rep(NA,length(walltime))
  }
  if (showHr) {
    hrsm <- smoothDataSegments(yvec=track$heart_rate.bpm,xvar=walltime,
                               segment=track$segment,
                               bw=hrSmoothBW,nneighbors=hrSmoothNN,
                               kernel="epanechnikov")
  } else {
    hrsm <- rep(NA,length(walltime))
  }
  if (showPower) {
    powzero <- track$power.watts == 0
    powna <- is.na(track$power.watts)
    powertemp <- track$power.watts
    powertemp[powzero] <- NA
    powersm <- smoothDataSegments(yvec=powertemp,xvar=walltime,
                                    segment=track$segment,
                                    bw=powerSmoothBW,nneighbors=powerSmoothNN,
                                    kernel="epanechnikov")
    powersm[powzero] <- 0
    powersm[powna] <- NA
  } else {
    powersm <- rep(NA,length(walltime))
  }
  speedsm <- smoothDataSegments(yvec=track$speed.m.s,xvar=walltime,
                                segment=track$segment,
                                bw=4,nneighbors=4,
                                kernel="epanechnikov")
  if (imperial) {
    elevsm <- feetFromMeters(elevsm)
    speedsm <- milesFromMeters(speedsm)*3600
  }
  else {
    speedsm <- kmFromMeters(speedsm)*3600
  }
  dist <- distance[length(distance)]
  hours <- walltime[length(walltime)]/3600
  npoints <- numPointsXAxis(dist,ppm,imperial)
  distPerPoint <- dist/(npoints-1)
  hoursPerPoint <- hours/(npoints-1)
  ngraphpoints <- max(minNumPoints,npoints)
  grlist <- drawProfile(distancevec=distance,
                        elevationvec=elevsm,
                        speedvec=speedsm,
                        distPerPoint,palette,
                        vertMult=verticalMultiplier,
                        npoints,minNumPoints,
                        elevationShape,imperial,
                        hrDistance,cadDistance,powerDistance,
                        hrTime,cadTime,powerTime,showTime)
  if (showSummary & !missing(summary))
    grlist <- drawSummary(grlist,summary,title)

  if (!showTime) {
    grlist <- drawXAxis(grlist,distance,
                        startsAndStops=startsAndStops,
                        showStops,distPerPoint,
                        imperial=imperial,underLine=TRUE)
  }
  if (cadDistance) grlist <- drawCadence(grlist,cadencesm,distance,
                                         segment=track$segment,
                                         cadLow,cadTarget,
                                         cadCont,cadContLow,cadContHigh,
                                         cadColorLow,cadColorMid,cadColorHigh,
                                         minNumPoints,showlegend=TRUE)

  if (hrDistance) grlist <- drawHr(grlist,hrsm,distance,
                                   segment=track$segment,
                                   hrLow,hrHigh,
                                   hrColorLow,hrColorHigh,
                                   minNumPoints,showlegend=TRUE)

  if (powerDistance) grlist <- drawPower(grlist,powersm,distance,
                                   segment=track$segment,
                                   powerLow,powerHigh,
                                   powerColorLow,powerColorHigh,
                                   minNumPoints,showlegend=TRUE)

  if (showTime) {
    #  draw opaque white background where faint gridlines aren't helpful
    grlist[["g"]] <- grlist[["g"]] +
      ggplot2::geom_rect(xmin=0,xmax=grlist[["xmax"]],
                         ymax=grlist[["ymin"]],
                         ymin=grlist[["ybottom"]],
                         color="white",fill="white")

    grlist <- drawXAxis(grlist,distance,startsAndStops=startsAndStops,
                        showStops=FALSE,distPerPoint,imperial=imperial,
                        lineAtZero=!(cadDistance|hrDistance|powerDistance))
    grlist <- drawXTConnect(grlist,distance,walltime,
                            startsAndStops=startsAndStops,
                            distPerPoint,hoursPerPoint)
    grlist <- drawTAxis(grlist,walltime,
                        startsAndStops=startsAndStops,
                        distPerPoint,hoursPerPoint)
  }

  if (cadTime) grlist <- drawCadence(grlist,cadencesm,walltime,
                                     segment=track$segment,
                                     cadLow,cadTarget,
                                     cadCont,cadContLow,cadContHigh,
                                     cadColorLow,cadColorMid,cadColorHigh,
                                     minNumPoints,showlegend=!cadDistance)
  if (hrTime) grlist <- drawHr(grlist,hrsm,walltime,
                               segment=track$segment,
                               hrLow,hrHigh,
                               hrColorLow,hrColorHigh,
                               minNumPoints,showlegend=!hrDistance)

  if (powerTime) grlist <- drawPower(grlist,powersm,walltime,
                                         segment=track$segment,
                                         powerLow,powerHigh,
                                         powerColorLow,powerColorHigh,
                                         minNumPoints,showlegend=!powerDistance)



#  this is key to having text being kept at an appropriate size.
  ymax <- grlist[["ymax"]]
  ymin <- grlist[["ymin"]]
  vertmult <- grlist[["vertmult"]]
  ngraphpoints <- grlist[["ngraphpoints"]]
  g <- grlist[["g"]]

  plot.width <- 0.5*(ngraphpoints/600)+0.75
  plot.height <- 0.5*(((ymax-ymin)*vertmult/(5280*distPerPoint))/600)*1.1 + 2

  if (!missing(savefn))
    ggsave(savefn,width=plot.width,height=plot.height,
           units="in",dpi=600,scale=1.3,limitsize=FALSE)
  return(g)
}

