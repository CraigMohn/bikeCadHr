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
#' @param plotly use plotly to draw 3D track in viewer
#' @param rgl use rgl openGL to draw 3D track in viewer
#' @param rasterDir character location of base directory to load and save raster files
#' @param localElevFile file containing raster object with elevations on lat/lon
#' @param plot3DSize number which is rough target for size of grid for plotly/rgl
#' @param featureDataSource character,  "Raster" to load saved raster data
#'    from directory specified , "none" to show none
#' @param townLevel numeric, display towns ranked this number or higher:
#'    3=all towns     5=larger towns (in US >50k)
#' @param roadLevel numeric, display roads ranked this number or higher:
#'    2=Service Drive, Bike Path, etc      3=Local Street
#'    4=Secondary Hwy                      5=Primary Hwy/Transit
#' @param waterALevel numeric, display areal water ranked this number or higher:
#'    2=res/treatmentpond/pit/quarry       3=lake/pond/swamp/stream
#'    4=class 2 or 3 bigger than 1k ha     5=named lake/pond/swamp/stream
#'    6=large lake/pond/swamp/stream       7=Ocean/Bay/Est/Sound
#'    8=glacier
#' @param waterLLevel numeric, display linear water ranked this number or higher:
#'    2=canal/ditch                        3=braided stream
#'    4=stream/river                       5=named stream/river
#'    6=named stream/river containing the string "RIV"
#' @param rglColorScheme name of color scheme from
#'     c("default","beach","viridis","plasma","terrain","oleron","snow","oslo",
#'       "desert","lajolla","niccoli","bright",
#'       "bing","maptoolkit-topo","nps","apple-iphoto")
#' @param useImageRaster logical, use the image raster from saved openStreetmap
#'    colrings of the map surface
#' @param plot3DVertScale number which will multiply vertical scaling for plotly/rgl
#' @param rglColorMaxElev the upper limit of elevation, anything above this will
#'      be colored the same in the rgl
#' @param rglShininess number controlling surface shininess in rgl rendering
#' @param rglLights logical indicating whether to turn on lighting
#' @param rglLightPars string containing parameters for rgl.light call
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
#' @return NULL or plotly object
#'
#' @export
map_rides <- function(geodf,outfile,maptitle,definedmaps,usemap,
                      rasterDir=NULL,
                      plotly=FALSE,rgl=FALSE,localElevFile="",
                      plot3DSize=300,plot3DVertScale=1,
                      featureDataSource="none",
                      townLevel=3,roadLevel=4,waterALevel=4,waterLLevel=5,
                      rglColorScheme="default",useImageRaster=FALSE,
                      rglColorMaxElev=3000,
                      rglShininess=0,rglLightPars="viewpoint.rel=TRUE",
                      rglLights=TRUE,
                      maptype="osm",minTiles=50,
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
  } else if (outfile=="none") {
    outfiletype <- "none"
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

    latwide <- lat.max - lat.min
    lonwide <- lon.max - lon.min
    map.lat.max.dd <- lat.max+latwide*marginfactor
    if (!missing(maptitle)) map.lat.max.dd <- lat.max+latwide*0.05
    map.lat.min.dd <- lat.min-latwide*marginfactor
    map.lon.max.dd <- lon.max+lonwide*marginfactor
    map.lon.min.dd <- lon.min-lonwide*marginfactor
    if (outfiletype != "none") {
      if (min(lat.max-lat.min,lon.max-lon.min) > .005){
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
    }

    #  expand limits so any segments that have their middle cut out will
    #        be wrongly rendered, but off-map
    #    (except perhaps near corners, not worth fixing now, maybe never)
    mapdf <- geodf[geodf$lon>=map.lon.min.dd-.01 &
                   geodf$lon<=map.lon.max.dd+.01 &
                   geodf$lat>=map.lat.min.dd-.01 &
                   geodf$lat<=map.lat.max.dd+.01,]
    trackstarts <- unique(mapdf$start.time)

    if (nrow(mapdf) > 0) {
      if (draw.speed) {
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
                                          "dodgerblue","blue","darkorchid",
                                          "purple","magenta"))(101)
        }
        speed <- mapdf$speed.m.s*2.23694
        speed[speed>40] <- 40
        speed[speed<3] <- 3
        mapdf$colorvec <- spdcolors[floor(100*(speed - 3)/37) + 1]
      } else {
        if (line.color=="plasma") {
          mapcvec <- viridis::plasma(length(trackstarts),begin=0.0,end=0.7)
        } else if (line.color=="viridis") {
          mapcvec <- viridis::viridis(length(trackstarts),begin=0.1,end=0.9)
        } else if (line.color=="rainbow") {
          mapcvec <- rainbow(length(trackstarts),start=0.2,end=0.9)
        } else if (line.color=="heat") {
          mapcvec <- heat.colors(length(trackstarts))
        } else if (line.color=="red-blue") {
          mapcvec <- colorRampPalette(c("red","blue"))(101)
        } else {
          mapcvec <- rep(line.color,length(trackstarts))
        }
        mapdf$colorvec <- mapcvec[match(mapdf$start.time, trackstarts)]
      }
    }

    if (outfiletype!="none") {
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
      if (nrow(mapdf > 0)) {
        if (!draw.speed) {
          for (trkstarttime in trackstarts) {
            draw.color <- mapdf$colorvec[which(trackstarts == trkstarttime)]
            for (seg in unique(mapdf[mapdf$start.time==trkstarttime,]$segment)) {
              use = mapdf$start.time==trkstarttime & mapdf$segment==seg &
                   (lead_one(mapdf$segment)==mapdf$segment |
                    lag_one(mapdf$segment)==mapdf$segment) # can't do 1 pt lines
              if(sum(use)>0) {
                temp <-
                   OpenStreetMap::projectMercator(mapdf$lat[use],mapdf$lon[use])
                lines(temp[,1], temp[,2],type = "l",
                      col = scales::alpha(mapdf$colorvec[use][[1]], line.alpha),
                      lwd = line.width)
              }
            }
          }
        } else {
          temp <- OpenStreetMap::projectMercator(mapdf$lat, mapdf$lon)
          points(temp[,1],
                 temp[,2],
                 pch=speed.pch,
                 col=scales::alpha(mapdf$colorvec,speed.alpha),
                 lwd=speed.ptsize)
        }
      }
      dev.off()
    }
    if (rgl) {
      maptrack3d::draw3dMap(paths=mapdf,
                  trackCurve=TRUE,
                  trackCurveElevFromRaster=TRUE,
                  trackCurveHeight=15,
                  mapWindow=c(map.lon.min.dd,map.lon.max.dd,
                              map.lat.min.dd,map.lat.max.dd),
                  USStatevec=NULL,
                  rasterFileSetNames=localElevFile,
                  elevDataSource="Raster",
                  featureDataSource=featureDataSource,
                  townLevel=townLevel,roadLevel=roadLevel,
                  waterALevel=waterALevel,waterLLevel=waterLLevel,
                  vScale=plot3DVertScale,maxElev=rglColorMaxElev,
                  rglColorScheme=rglColorScheme,useImageRaster=useImageRaster,
                  rasterDir=rasterDir,
                  res3d=2400)
    }
    p <- NULL
    if (plotly) {
      elevations <- NULL
      elevations <- raster::raster(paste0(rasterDir,"/",
                                          localElevFile,"/",
                                          localElevFile,"elevs.grd"))
      ttt <- raster::crop(elevations,
                          c(map.lon.min.dd,map.lon.max.dd,
                            map.lat.min.dd,map.lat.max.dd))
      ttt[is.na(ttt[])] <- 0
      if (nrow(mapdf) > 0) {
        tt <- raster::rasterize(cbind(mapdf$lon,mapdf$lat),ttt,mask=TRUE)
        ppp <- raster::as.matrix(tt)
        ppp <- ppp[,ncol(ppp):1]
        pathpts <- pointsFromMatrix(ppp)
      } else {
        pathpts <- pointsFromMatrix(ttt)[1,]  #pick 1 point from map for track
      }
      yscale <- yRatio(ttt)
      zscale <- 0.15*plot3DVertScale
      print("building plotly image")
      if (localElevFile != "") {
        mmm <- raster::as.matrix(ttt)
        mmm <- mmm[,ncol(mmm):1]  #  flip east/west since row 1 is top
        pathpts$z <- pathpts$z + 10
        ax <- list(title="longitude",zeroline=FALSE,
                   showline=FALSE,showticklabels=FALSE,showgrid=FALSE)
        ay <- list(title="latitude",zeroline=FALSE,
                   showline=FALSE,showticklabels=FALSE,showgrid=FALSE)
        az <- list(title="elevation",zeroline=FALSE,
                   showline=FALSE,showticklabels=FALSE,showgrid=FALSE)
        p <- plotly::plot_ly(z = ~mmm,
                             colors = c("blue","yellow")) %>%
          plotly::add_surface(opacity=1.0)
        if(nrow(mapdf)> 0) {
             p <- plotly::add_trace(p,data=pathpts,
                                    x = ~x ,
                                    y = ~y,
                                    z= ~z,
                                    type = "scatter3d",
                                    mode = "markers",
                                    marker = list(size = 2,
                                                  color = "black"),
                                                  opacity=1)
        }
        p <- plotly::layout(p,scene=list(xaxis=ax,yaxis=ay,zaxis=az,
                                       aspectmode = "manual",
                                       aspectratio = list(x=1,y=yscale,z=zscale),
                                       camera=list(up=c(0,1,0),
                                                   eye=c(0,1.25,0)) ) )
      } else {
        p <- plotly::plot_ly(mapdf,
                        x = ~lon,
                        y = ~lat,
                        z = ~altitude.m,
                        type = 'scatter3d', mode = 'markers',
                        opacity = 1,
                        marker = list(size = 1.5, color = ~colorvec)) %>%
             plotly::layout(scene = list(aspectmode = "manual",
                            aspectratio = list(x=1, y=yscale, z=zscale) ))
      }
    }
  }
  return(p)
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
#' @param title title printed over top center of diagram
#' @param palette palette to use in graphic (\code{"plasma"} or \code{"magma"}
#'    or \code{"inferno"} or \code{"viridis"})
#' @param naPlotColor name of color to use displaying NAs and zeros
#' @param verticalMultiplier default vertical exaggeration factor.  Default
#'    ranges from 25 to 60 depending on the length being plotted
#' @param ppm override calculated default number of points per mile
#' @param elevationShape shape to use for drawing the elevation plot
#' @param speedDistance display speed data scaled by distance if available
#' @param hrDistance display HR data scaled by distance if available
#' @param cadDistance display cadence data scaled by distance if available
#' @param powerDistance display power data scaled by distance if available
#' @param speedTime display speed data scaled by time if available
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
#' @param cadTarget target cadence range minimum
#' @param cadLow lower cadence limit for continuous color, all
#'    lower cadences are displayed as same color
#' @param cadHigh upper cadence limit for continuous color, all
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
                         naPlotColor="gray88",
                         verticalMultiplier=NA,ppm=NA,
                         elevationShape=46,
                         speedDistance=TRUE,hrDistance=TRUE,
                         cadDistance=TRUE,powerDistance=TRUE,
                         speedTime=TRUE,hrTime=TRUE,cadTime=TRUE,powerTime=TRUE,
                         showTime=TRUE,showSummary=TRUE,
                         showStops=TRUE,
                         hrLow=100,hrHigh=170,
                         hrColorLow=11,hrColorHigh=26,
                         cadTarget=88,
                         cadCont=TRUE,cadLow=50,cadHigh=120,
                         cadColorLow=4,cadColorMid=10,cadColorHigh=15,
                         powerLow=75,powerHigh=400,
                         powerColorLow=9,powerColorHigh=21,
                         hrSmoothBW=6,hrSmoothNN=6,
                         cadSmoothBW=10,cadSmoothNN=10,
                         powerSmoothBW=10,powerSmoothNN=10,
                         elevSmoothBWMeters=15,
                         stopToleranceMeters=20,
                         minSecsRolling=10,
                         minNumPoints=3000,
                         imperial=TRUE) {

  ##  what will we add below the profile
  cadDistance <- cadDistance & any(!is.na(track$cadence.rpm))
  hrDistance <- hrDistance & any(!is.na(track$heart_rate.bpm))
  powerDistance <- powerDistance & any(!is.na(track$power.watts))
  cadTime <- cadTime & any(!is.na(track$cadence.rpm))
  hrTime <- hrTime & any(!is.na(track$heart_rate.bpm))
  powerTime <- powerTime & any(!is.na(track$power.watts))
  speedTime <- speedTime & showTime
  cadTime <- cadTime & showTime
  hrTime <- hrTime & showTime
  powerTime <- powerTime & showTime
  showHr <- hrDistance | hrTime
  showCad <- cadDistance | cadTime
  showPower <- powerDistance | powerTime
  showSpeed <- speedDistance | speedTime
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
                               subsegment=track$subsegment,
                               stopDistTolerance=stopDistLim,
                               stopRunLength=minSecsRolling)

  #  adjust elevation if track is a loop
  if (!is.na(summary$ride.loop) & summary$ride.loop) {
    if (abs(summary$deltaElev) > 1) {
      track$altitude.m <- track$altitude.m + seq(0,-summary$deltaElev,length.out=nrow(track))
    }
  }

  #  note that may be multiple records at same distance.  smoothing
  #    algorithm will weight equally.
  elevsm <- smoothData(yvec=track$altitude.m,xvar=distance,
                       bw=elevsmbw,nneighbors=18,kernel="epanechnikov",
                       replaceNAs=TRUE)
  speedsm <- track$speed.m.s
  speedsm[speedsm==0] <- NA  #  don't display or average stops
  speedsm <- smoothDataSegments(yvec=track$speed.m.s,xvar=walltime,
                                segment=track$segment,
                                bw=4,nneighbors=4,
                                kernel="epanechnikov",
                                replaceNAs=FALSE)
  if (showCad) {
    cadzero <- track$cadence.rpm == 0
    cadencetemp <- track$cadence.rpm
    cadencetemp[cadencetemp==0] <- NA
    cadencesm <- smoothDataSegments(yvec=cadencetemp,xvar=walltime,
                                    segment=track$segment,
                                    bw=cadSmoothBW,nneighbors=cadSmoothNN,
                                    kernel="triangular",
                                    replaceNAs=FALSE)
    cadencesm[cadzero] <- 0.0
  } else {
    cadencesm <- rep(NA,length(walltime))
  }

  if (showHr) {
    hrsm <- smoothDataSegments(yvec=track$heart_rate.bpm,xvar=walltime,
                               segment=track$segment,
                               bw=hrSmoothBW,nneighbors=hrSmoothNN,
                               kernel="epanechnikov",
                               replaceNAs=FALSE)
  } else {
    hrsm <- rep(NA,length(walltime))
  }
  if (showPower) {
    powertemp <- track$power.watts
    powertemp[powertemp==0] <- NA
    powersm <- smoothDataSegments(yvec=powertemp,xvar=walltime,
                                  segment=track$segment,
                                  bw=powerSmoothBW,nneighbors=powerSmoothNN,
                                  kernel="epanechnikov",
                                  replaceNAs=FALSE)
  } else {
    powersm <- rep(NA,length(walltime))
  }
  if (imperial) {
    elevsm <- feetFromMeters(elevsm)
    speedsm <- milesFromMeters(speedsm)*3600
    speedLegendText <- "Speed (mph)"
  }
  else {
    speedsm <- kmFromMeters(speedsm)*3600
    speedLegendText <- "Speed (kph)"
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
                        distPerPoint=distPerPoint,
                        palette=palette,
                        naPlotColor=naPlotColor,
                        vertMult=verticalMultiplier,
                        npoints=npoints,
                        minNumPoints=minNumPoints,
                        elevationShape=elevationShape,
                        imperial=imperial,
                        speedDistance=speedDistance,hrDistance=hrDistance,
                        cadDistance=cadDistance,powerDistance=powerDistance,
                        speedTime=speedTime,hrTime=hrTime,
                        cadTime=cadTime,powerTime=powerTime,showTime=showTime)

  if (showSummary & !missing(summary))
    grlist <- drawSummary(ggp=grlist,summary=summary,title=title)

  if (!showTime) {
    grlist <- drawXAxis(ggp=grlist,distance=distance,
                        startsAndStops=startsAndStops,
                        showStops=showStops,
                        distPerPoint=distPerPoint,
                        imperial=imperial,underLine=TRUE,
                        lineAtZero=TRUE)
  }
  ###  draw legends
  if (showSpeed) grlist <- drawLegend(ggp=grlist,dvar=speedsm,
                                      xvar=distance,
                                      legendtext=speedLegendText,
                                      segment=track$segment,
                                      toofar=0,
                                      dLow=ifelse(imperial,3,5),
                                      dHigh=ifelse(imperial,40,67),
                                      dColorLow=0,
                                      dColorHigh=40,
                                      minNumPoints=minNumPoints)
  if (showPower) grlist <- drawLegend(ggp=grlist,dvar=powersm,
                                      xvar=distance,
                                      legendtext="Power (watts)",
                                      segment=track$segment,
                                      toofar=0,
                                      dLow=powerLow,
                                      dHigh=powerHigh,
                                      dColorLow=powerColorLow,
                                      dColorHigh=powerColorHigh,
                                      minNumPoints=minNumPoints)
  if (showHr) grlist <- drawLegend(ggp=grlist,dvar=hrsm,
                                   xvar=distance,
                                   legendtext="Heart Rate (bpm)",
                                   segment=track$segment,
                                   toofar=0,
                                   dLow=hrLow,
                                   dHigh=hrHigh,
                                   dColorLow=hrColorLow,
                                   dColorHigh=hrColorHigh,
                                   minNumPoints=minNumPoints)
  if (showCad) grlist <- drawLegend(ggp=grlist,dvar=powersm,
                                    xvar=distance,
                                    legendtext="Cadence (rpm)",
                                    segment=track$segment,
                                    toofar=0,
                                    dLowD=cadLow,
                                    dTarget=cadTarget,
                                    dCont=cadCont,
                                    dLow=cadLow,
                                    dHigh=cadHigh,
                                    dColorLow=cadColorLow,
                                    dColorMid=cadColorMid,
                                    dColorHigh=cadColorHigh,
                                    minNumPoints=minNumPoints)

  grlist <- addGap(ggp=grlist,nrep=1)

  if (speedDistance) grlist <- drawBar(ggp=grlist,dvar=speedsm,
                                       xvar=distance,
                                       segment=track$segment,
                                       toofar=0,
                                       dLow=ifelse(imperial,3,5),
                                       dHigh=ifelse(imperial,40,67),
                                       dColorLow=0,
                                       dColorHigh=40,
                                       minNumPoints=minNumPoints)
  if (powerDistance) grlist <- drawBar(ggp=grlist,dvar=powersm,
                                       xvar=distance,
                                       segment=track$segment,
                                       toofar=0,
                                       dLow=powerLow,
                                       dHigh=powerHigh,
                                       dColorLow=powerColorLow,
                                       dColorHigh=powerColorHigh,
                                       minNumPoints=minNumPoints)
  if (hrDistance) grlist <- drawBar(ggp=grlist,dvar=hrsm,
                                    xvar=distance,
                                    segment=track$segment,
                                    toofar=0,
                                    dLow=hrLow,
                                    dHigh=hrHigh,
                                    dColorLow=hrColorLow,
                                    dColorHigh=hrColorHigh,
                                    minNumPoints=minNumPoints)
  if (cadDistance) grlist <- drawBar(ggp=grlist,dvar=cadencesm,
                                     xvar=distance,
                                     segment=track$segment,
                                     toofar=0,
                                     dTarget=cadTarget,
                                     dCont=cadCont,
                                     dLow=cadLow,
                                     dHigh=cadHigh,
                                     dColorLow=cadColorLow,
                                     dColorMid=cadColorMid,
                                     dColorHigh=cadColorHigh,
                                     minNumPoints=minNumPoints)

  if (showTime) {
    #  draw opaque white background where faint gridlines aren't helpful
    grlist[["g"]] <- grlist[["g"]] +
      ggplot2::geom_rect(xmin=0,xmax=grlist[["xmax"]],
                         ymax=grlist[["ymin"]],
                         ymin=grlist[["ybottom"]],
                         color="white",fill="white")

    grlist <- drawXAxis(grlist,distance,startsAndStops=startsAndStops,
                        showStops=FALSE,distPerPoint,imperial=imperial,
                        underLine=FALSE,
                        lineAtZero=!(cadDistance|hrDistance|powerDistance))
    grlist <- drawXTConnect(grlist,distance,walltime,
                            startsAndStops=startsAndStops,
                            distPerPoint,hoursPerPoint)
    grlist <- drawTAxis(grlist,walltime,
                        startsAndStops=startsAndStops,
                        distPerPoint,hoursPerPoint)
  }

  if (speedTime) grlist <-drawBar(ggp=grlist,dvar=speedsm,
                                  xvar=walltime,
                                  segment=track$segment,
                                  toofar=0,
                                  dLow=ifelse(imperial,3,5),
                                  dHigh=ifelse(imperial,40,67),
                                  dColorLow=0,
                                  dColorHigh=40,
                                  minNumPoints=minNumPoints)
  if (powerTime) grlist <-drawBar(ggp=grlist,dvar=powersm,
                                  xvar=walltime,
                                  segment=track$segment,
                                  toofar=0,
                                  dLow=powerLow,
                                  dHigh=powerHigh,
                                  dColorLow=powerColorLow,
                                  dColorHigh=powerColorHigh,
                                  minNumPoints=minNumPoints)
  if (hrTime) grlist <- drawBar(ggp=grlist,dvar=hrsm,
                                xvar=walltime,
                                segment=track$segment,
                                 toofar=0,
                                 dLow=hrLow,
                                 dHigh=hrHigh,
                                 dColorLow=hrColorLow,
                                 dColorHigh=hrColorHigh,
                                 minNumPoints=minNumPoints)
  if (cadTime) grlist <- drawBar(ggp=grlist,dvar=cadencesm,
                                 xvar=walltime,
                                 segment=track$segment,
                                 toofar=0,
                                 dTarget=cadTarget,
                                 dCont=cadCont,
                                 dLow=cadLow,
                                 dHigh=cadHigh,
                                 dColorLow=cadColorLow,
                                 dColorMid=cadColorMid,
                                 dColorHigh=cadColorHigh,
                                 minNumPoints=minNumPoints)

#  this is key to having text being kept at an appropriate size.
  ymax <- grlist[["ymax"]]
  ymin <- grlist[["ymin"]]
  vertmult <- grlist[["vertmult"]]
  ngraphpoints <- grlist[["ngraphpoints"]]
  g <- grlist[["g"]]

  plot.width <- 0.5*(ngraphpoints/600)+0.75
  plot.height <- 0.5*(((ymax-ymin)*vertmult/(5280*distPerPoint))/600)*1.1 + 2.5

  if (!missing(savefn))
    ggsave(savefn,width=plot.width,height=plot.height,
           units="in",dpi=600,scale=1.3,limitsize=FALSE)
  return(g)
}

