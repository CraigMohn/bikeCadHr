#' plot ride track data on a map
#'
#' \code{map_rides} Plot lat/long data on maps, optionally using colored
#'   dots to indicate speed
#'
#' Create a map displaying the tracks specified.  Tracks may be drawn in a single
#'     specified color, with each separate track drawn in a different color from
#'     a specified palette, or as a series of closely spaced dots (which may
#'     be prodded into appearing as a line through judicious choice of point
#'     size and alpha) with the color varying based on speed and the chosen
#'     palette.
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
#'       \code{"esri"} or \code{"osm-public-transport"} or \code{"opencyclemap"} or
#'       \code{"apple-iphoto"} or \code{"skobbler"})
#' @param minTiles minimum number of tiles fetched for map, larger is slower
#'    but better quality
#' @param mapsize pixel size of map created
#' @param fine.map if true, use c(7680,4800) for map size
#' @param margin.factor percentage to expand map area beyond limits of
#'    the track in each direction, when autodetermining region to plot
#' @param draw.speed if true, draw track(s) as a series of points whose color
#'    indicates travel speed
#' @param line.color the color to draw the lines of the tracks,
#'    if a palette is specified (\code{"plasma"} or \code{"viridis"} or \code{"rainbow"}
#'     or \code{"heat"} or \code{"red-blue"})
#'    is specified, each track supplied will be assigned a color from that palette
#' @param line.width the width of the line for the tracks
#' @param line.alpha the opacity of the line
#' @param speed.color palette to use to represent speed on plot (\code{"speedcolors"} or
#'     \code{"red-blue-green"} or \code{"rainbow"} or \code{"plasma"} or \code{"magma"} or
#'     \code{"heat"} )
#' @param speed.alpha opacity of the speed line if \eqn{speed.color} specified
#' @param speed.ptsize size for the symbols used to plot the speed line, doubled
#'    if \eqn{fine.map} is set true.
#' @param speed.pch the character to use plotting the points of the speed line
#' @param jpeg.quality the "quality" of the JPEG image, as a percentage. Smaller
#'    values will give more compression but also more degradation of the image
#'
#' @return NULL
#'
#' @export
map_rides <- function(geodf,outfile,maptitle,definedmaps,usemap,maptype="maptoolkit-topo",minTiles=50,
                      mapsize=c(1600,1200),fine.map=FALSE,margin.factor=0.08,
                      draw.speed=FALSE,
                      line.color="magenta",line.width=3,line.alpha=0.8,
                      speed.color="speedcolors",speed.alpha=0.7,speed.ptsize=6,speed.pch=19,
                      jpeg.quality=90) {

  ##  geodf: a tibble or dataframe containing at least: position_lat.dd,position_lon.dd,(or lat,lon)(both numeric),
                                                    #     startbutton.time(int),segment(numeric)
  start.hour <- start.time <- startbutton.date <- stoplabels <- timestamp.s <- NULL
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
    geodf$start.time <- dateTimeStr(geodf$startbutton.date,geodf$startbutton.time)
    lat.max <- max(geodf$lat)
    lat.min <- min(geodf$lat)
    lon.max <- max(geodf$lon)
    lon.min <- min(geodf$lon)
    marginfactor <- margin.factor     #   expand the map area beyond the limits of the tracks
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
                        c(map.lat.min.dd, map.lon.max.dd), type =maptype,minNumTiles=minTiles)
      #  native mercator - for longlat add:  map <- openproj(map, projection = "+proj=longlat")
    } else {
     cat("\n",outfile," not created, map too small")
        return(NULL)
    }
    map.lat.min <- map$bbox[["p2"]][2] * (180 / (2 ^ 31))
    map.lat.max <- map$bbox[["p1"]][2] * (180 / (2 ^ 31))
    map.lon.min <- map$bbox[["p1"]][1] * (180 / (2 ^ 31))
    map.lon.max <- map$bbox[["p2"]][1] * (180 / (2 ^ 31))
    aspectcorrect <- 1  #  using mercator, better than latlon at northern latitudes
    aspectratio <- (map.lon.max - map.lon.min)/(map.lat.max-map.lat.min)
    if (aspectratio > mapsize[1]/mapsize[2]) {
      mapwidth <- mapsize[1]
      mapheight <- mapsize[1]/aspectratio
    } else {
      mapwidth <- mapsize[2]*aspectratio
      mapheight <- mapsize[2]
    }
    #  expand limits so any segments that have their middle cut out will be wrongly rendered, but off-map
    #    (except perhaps near corners, not worth fixing at this time, maybe never)
    mapdf <- geodf[geodf$lon>=map.lon.min.dd-.01 & geodf$lon<=map.lon.max.dd+.01 &
                   geodf$lat>=map.lat.min.dd-.01 & geodf$lat<=map.lat.max.dd+.01,]
    trackstarts <- unique(mapdf$start.time)
    cat("\noutfile=",outfile)
    if (outfiletype=="jpeg") {
      jpeg(outfile, width = mapwidth, height = mapheight, quality = jpeg.quality)
    } else if (outfiletype=="tiff") {
      tiff(outfile, width = mapwidth, height = mapheight,type="cairo",compression="zip+p")
    }
    par(mar = rep(0,4))
    plot(map)
    if (!missing(maptitle)) title(main=as.character(maptitle),cex.main=2*mapheight/800,col="gray57",line=-3*(mapheight/800))
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
               (lead_one(mapdf$segment)==mapdf$segment | lag_one(mapdf$segment)==mapdf$segment) # can't plot single point lines
          if(sum(use)>0) {
            temp <- OpenStreetMap::projectMercator(mapdf$lat[use], mapdf$lon[use])
            lines(temp[,1], temp[,2],type = "l", col = scales::alpha(draw.color, line.alpha), lwd = line.width)
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
        spdcolors <- colorRampPalette(c("red","orange","cornflowerblue","dodgerblue","blue","darkorchid","purple","magenta"))(101)
      }
      speed <- mapdf$speed.m.s*2.23694
      speed[speed>40] <- 40
      speed[speed<3] <- 3
      colorvec <- spdcolors[floor(100*(speed - 3)/37) + 1]
      temp <- OpenStreetMap::projectMercator(mapdf$lat, mapdf$lon)
      points(temp[,1], temp[,2], pch=speed.pch, col=alpha(colorvec,speed.alpha), lwd=speed.ptsize)
    }
    dev.off()
  }
  return(NULL)
}
#' plot heartrate, cadence, speed and more using color-driven
#'   data summaries to convey performance patterns
#'
#'
#' \code{plot_elev_profile_plus} creates a plot which uses the elevation profile
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
#' @param palette palette to use in graphic (\code{"plasma"} or \code{"magma"} or
#'       \code{"inferno"} or \code{"viridis"})
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
#'    colors are from same palette as speeds, number is the speed corresponding
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
plot_elev_profile_plus <- function(track,summary,savefn,title="Ride",palette="plasma",
          vertical.multiplier=NA,ppm=NA,elevation.shape=46,
          show.stops=TRUE,show.time=TRUE,show.summary=TRUE,cad.cont=TRUE,
          hr.low=100,hr.high=170,hr.color.low=9,hr.color.high=31,
          cad.low=65,cad.target=88,cad.cont.low=60,cad.cont.high=100,
          cad.color.low=3,cad.color.mid=9,cad.color.high=15,
          hr.smooth.bw.meters=6,hr.smooth.nn=5,cad.smooth.bw.meters=20,cad.smooth.nn=15,
          min.numpoints=1000) {
  ###  make the R checker happy with utterly irrelevant initializations of variables used with ggplot
  alphachar <- alphahour <- distlegend <- prtchar <- prthour <- NULL
  start.hour <- start.time <- startbutton.date <- stoplabels <- timestamp.s <- NULL
  x <- xtext.stop <- y <- NULL


  miles <- track$distance.m[nrow(track)]/1609.34
  elevsm <- smoothTriangular(track$distance.m,track$altitude.m,nneighbors=2,bw=5)
  track.elapsed <- as.numeric(difftime(track$timestamp.s,track$timestamp.s[1],units="secs"))
  #  set points in plot and points.per.mile
  if (!missing(ppm)&ppm>=10) {
    npoints <- ceiling(ppm*miles)
  } else {
    distbends <- c(0,10,35,85,200,100000000)       # begin at 0, end at max distance imaginable
    pointsbends <- c(0,2000,4000,6500,10000,10000) # begin at 0, end at maximum number of points
    npoints <- ceiling(pointsbends[which(distbends>miles)[1]-1] +
                       ( (pointsbends[which(distbends>miles)[1]]-
                          pointsbends[which(distbends>miles)[1]-1])/
                         (distbends[which(distbends>miles)[1]]-
                          distbends[which(distbends>miles)[1]-1]) )*
                          (miles-distbends[which(distbends>miles)[1]-1]))
    ppm <- npoints/miles
  }
  if (missing(vertical.multiplier)|vertical.multiplier<=1) {
    distbends <- c(0,10,35,85,200,100000000)       # begin at 0, end at max distance imaginable
    vertbends <- c(25,25,35,45,55,60) # begin at 0, end at maximum number of points
    vertical.multiplier <- ceiling(vertbends[which(distbends>miles)[1]-1] +
                                   ( (vertbends[which(distbends>miles)[1]]-
                                      vertbends[which(distbends>miles)[1]-1])/
                                     (distbends[which(distbends>miles)[1]]-
                                      distbends[which(distbends>miles)[1]-1]) )*
                                      (miles-distbends[which(distbends>miles)[1]-1]))
  }
  miles.per.point <- 1.0/ppm
  ngraphpoints <- max(min.numpoints,npoints)
  distancegraph <- seq(1,ngraphpoints)*miles.per.point
  #  use equally spaced grid for plotting elevation and smoothed quantities
  profile.pts <- stats::approx(track$distance.m,elevsm,n=npoints)
  distance <- profile.pts[[1]]
  elevation <- profile.pts[[2]]
  time <- stats::approx(track$distance.m,track.elapsed,xout=distance,n=npoints)[[2]]
  segment <- round(stats::approx(track$distance.m,track$segment,xout=distance,n=npoints)[[2]])
  speed <- (lead_one(lead_one(distance))-lag_one(lag_one(distance)))/(lead_one(lead_one(time))-lag_one(lag_one(time)))
  grade <- smoothEpanechnikov(distance,(lead_one(elevation)-lag_one(elevation))/(lead_one(distance)-lag_one(distance)),nneighbors=10,bw=20)

  elevprtchar <- rep("|",length(distance))
  elevalpha <- 0.4 + 0.6*(ifelse(speed > 15,ifelse(speed<40,(speed-15)/25,1),0))

  #  now convert units
  elevation <- elevation*3.28084 #m to ft
  distance <- distance/1609.34   #m to mi
  trackdistance <- track$distance.m/1609.34  #  need distance at trackpoints for smoothing
  speed <- speed*2.23694         #m/s to mi/hr
  cad.smooth.bw <- cad.smooth.bw.meters/1609.34
  hr.smooth.bw <- hr.smooth.bw.meters/1609.34

  # set limits for color plots
  elev.min <- min(elevation)
  elev.min.int <- 500*floor(elev.min/500)
  elev.max <- max(elevation)
  elev.max.int <- 200*ceiling(elev.max/200)

  ybottom <- elev.min.int
  if (show.time) {
    y.clock <- ybottom - 100
    ybottom <- y.clock - 50
  }
  if (any(!is.na(track$cadence.rpm))) {
    y.cadence.band <- ybottom - 300
    ybottom <- y.cadence.band

  }
  if (any(!is.na(track$heart_rate.bpm))) {
    y.hr.band <- ybottom - 300
    ybottom <- y.hr.band
  }
  if (show.stops) {
    y.stops <- ybottom - 240
    ybottom <- y.stops
  }
  ymin <- ybottom-130
  ymax <- elev.max.int+700
  xmin <- 0
  xmax <- miles.per.point*ngraphpoints

  hr.band.height <- 120
  hr.label.height <- 100

  cad.band.height <- 120
  cad.label.height <- 100

  aspect.ratio <- vertical.multiplier*(ymax-ymin)/(5280*(npoints)*miles.per.point)

  hrcadlegendwidth <- miles.per.point*min(npoints,2*min.numpoints)/(13*2) #column width vectors sum to 13

  speed[speed<3] <- 3
  speed[speed>40] <- 40
  major.breaks <- ifelse(miles>100,10,ifelse(miles>10,5,1))
  plotdata <- data.frame(distance,elevation,time,speed,elevprtchar,elevalpha)

  g <- ggplot(plotdata,aes(x=distance,y=elevation)) +
        scale_y_continuous(limits=c(ymin,ymax),expand=c(0,0),
                           breaks=seq(from=0,to=ymax,by=500),
                           minor_breaks=seq(0,ymax,100)) +
        scale_x_continuous(limits=c(xmin,xmax),expand=c(0,0),
                           breaks=seq(from=0,to=xmax,by=major.breaks),
                           minor_breaks=seq(0,xmax,1)) +
        theme(aspect.ratio=aspect.ratio) +
        theme(legend.title=element_text(colour="steelblue",size=7)) +
        theme(legend.text=element_text(colour="lightsteelblue",size=6)) +
        theme(legend.key.size=unit(10,"points")) +
        theme(legend.justification="top") +
        scale_color_viridis(option=palette,limits=c(0,40),na.value="white",direction=-1) +
        scale_fill_viridis(option=palette,limits=c(0,40),na.value="white",direction=-1) +
        geom_rect(aes(xmin=xmin,xmax=xmax,ymin=ymin,ymax=0),fill="white",color="white") +
        geom_area(color="white",fill="white") +
        geom_point(aes(color=speed),shape=elevation.shape,size=1,position=position_nudge(y=1.2))

  if (any(!is.na(track$cadence.rpm))) {
    temp <- track$cadence.rpm
    temp[is.na(temp)] <- 0  # treat NAs like 0 since they will be graphed that way, otherwise they are lost
    #  do this once to properly flag spots with 0 or NAs, set them back after smooth
    cadence <- stats::approx(trackdistance,temp,xout=distance,n=npoints)[[2]]
    cadencenas <- cadence == 0.0  #  treat zero like missing for graphing purposes
    cadence <- stats::approx(trackdistance,track$cadence.rpm,xout=distance,n=npoints)[[2]]
    #  now smooth only over positive values...
    cadence[cadencenas] <- NA
    cadence <- smoothTriangular(distance,cadence,segment,nneighbors=cad.smooth.nn,bw=cad.smooth.bw)
    cadence[cadencenas] <- NA
    if (cad.cont) {
      g <- continuous_bar(g,legendtext="Cadence",xvar=distance,vals=cadence,
                          lowval=cad.cont.low,hival=cad.cont.high,
                          lowcolor=cad.color.low,hicolor=cad.color.high,
                          legendwidth=hrcadlegendwidth,y.band=y.cadence.band,
                          band.height=cad.band.height,label.height=cad.label.height)
    } else {
      g <- discrete_bar(g,legendtext="Cadence",xvar=distance,vals=cadence,
                        lowval=cad.low,hival=cad.target,
                        lowcolor=cad.color.low,midcolor=cad.color.mid,hicolor=cad.color.high,
                        legendwidth=hrcadlegendwidth,y.band=y.cadence.band,
                        band.height=cad.band.height,label.height=cad.label.height)
    }
  }
  if (any(!is.na(track$heart_rate.bpm))) {
    #  assume if any heartrate that smoothing is okay,  this eliminates missing value information....
    heartrate <- stats::approx(trackdistance,track$heart_rate.bpm,xout=distance,n=npoints)[[2]]
    heartrate <- smoothEpanechnikov(distance,heartrate,segment,nneighbors=hr.smooth.nn,bw=hr.smooth.bw)
    g <- continuous_bar(g,legendtext="Heart Rate",xvar=distance,vals=heartrate,
                        lowval=hr.low,hival=hr.high,
                        lowcolor=hr.color.low,hicolor=hr.color.high,
                        legendwidth=hrcadlegendwidth,y.band=y.hr.band,
                        band.height=hr.band.height,label.height=hr.label.height)
  }
  if (show.summary & !missing(summary)) {
    header.width <- round(min(npoints,2*min.numpoints))*miles.per.point
    ycenter.sum <- c(ymax-140,ymax-320,ymax-500)
    xcenter.sum <- c(header.width*0.10,header.width*0.60)
    xpos.sum <- c(rep(xcenter.sum[1],3),rep(xcenter.sum[2],3))
    ypos.sum <- c(ycenter.sum,ycenter.sum)
    summarylabels <- c(paste0(round(miles,digits=2)," miles"),
                       paste0(round(3.28084*summary$ascent[1],digits=0)," ft climbing"),
                       paste0(round(1/60*summary$rolling.time[1],digits=0)," minutes rolling"),
                       paste0(round(2.23694*summary$speed.rolling.m.s[1],digits=2)," mph avg"),
                       paste0(round(summary$avgcadence.withzeros[1],digits=1)," cad avg(incl/zeros)"),
                       paste0(round(summary$session.total.calories[1],digits=0)," kcal burned"))
    summaryTextFrame <- data.frame(xpos.sum,ypos.sum,summarylabels)
    if ((summary$pct.trkpts.hr < .98)|(is.na(summary$session.total.calories))) summaryTextFrame <- summaryTextFrame[-6,]
    if (summary$pct.trkpts.cad < .7) summaryTextFrame <- summaryTextFrame[-5,]
    g <- g +
      ggtitle(paste0(title,"  ",summary$start.time[1])) +
      geom_text(data=summaryTextFrame,
                aes(x=xpos.sum,y=ypos.sum,label=summarylabels),
                size=3,hjust=0,alpha=1,color="slategray4") +
      theme(plot.title = element_text(color="Black",face="bold",size=15,hjust=0.5))
  }
  if (show.stops) {
    #  create data frames for stops/breaks
    #  classify segment breaks as short stops or long breaks - work in grid coords
    first.seg <- segment != lag_one(segment)  #  don't include first egment start
    pause.len <- first.seg*(time-lag_one(time))
    pause.size <- rep(0,npoints)
    pause.alpha <- rep(0,npoints)
    pause.short <- pause.len < 300 & pause.len > 0
    pause.long <- pause.len >= 300
    if (sum(pause.short) > 0) {
      pause.size[pause.short] <- 4
      pause.alpha[pause.short] <- 0.6
    }
    if (sum(pause.long) > 0) {
      pause.size[pause.long] <- 4 + 4*log(pause.len[pause.long]/300)
      pause.alpha[pause.long] <- 0.3
    }
    stopdata <- data.frame(distance,time,pause.long,pause.short,pause.size,pause.alpha)
    stopdata.short <- stopdata[stopdata$pause.short,]
    stopdata.long <- stopdata[stopdata$pause.long,]
    stopTextFrame <- data.frame(xtext.stop=0,stoplabels="Stops")
    g <- g +
      geom_text(data=stopTextFrame,aes(x=xtext.stop,label=stoplabels),y=y.stops+80,
                hjust=0,alpha=1,size=2,color="black",fontface="italic",show.legend = FALSE)
    if (nrow(stopdata.short)>0) {
      g <- g +
        geom_point(data=stopdata.short,aes(y=y.stops,alpha=pause.alpha,size=pause.size),color="red3",shape=124,show.legend=FALSE)
    }
    if (nrow(stopdata.long)>0) {
      g <- g +
        geom_point(data=stopdata.long,aes(y=y.stops,alpha=pause.alpha,size=pause.size),color="green",shape=124,show.legend=FALSE)
    }
  }
  if (show.time){
    #  create the ride time data frame
    hours <- seq(0,track.elapsed[length(track.elapsed)],3600)[-1]
    quarterhours <- setdiff(seq(0,track.elapsed[length(track.elapsed)],900),hours)[-1]
    walltimedist <- NULL
    if (length(quarterhours)>0) {
      quarterhouridx <- vapply(quarterhours, function(x) which.max(time >= x),FUN.VALUE=0L)
      walltimequarters <- plotdata[quarterhouridx,c("distance","time")]
      walltimequarters$prtchar <- 124
      walltimequarters$prthour <- 32
      walltimequarters$alphachar <- 0.5
      walltimequarters$alphahour <- 0
      walltimedist <- walltimequarters
    }
    if (length(hours)>0) {
      houridx <- vapply(hours, function(x) which.max(time >= x),FUN.VALUE=0L)
      walltimehours <-  plotdata[houridx,c("distance","time"),]
      walltimehours$prtchar <- 43
      walltimehours$prthour <- 48+mod(round(walltimehours$time/3600),10)  #  this uses the last digit as the symbol
      walltimehours$alphachar <- 1
      walltimehours$alphahour <- 1
      walltimedist <- dplyr::arrange(rbind(walltimedist,walltimehours),time)
      walltimedist <- walltimedist[walltimedist$time>0,]
    }
    if (length(walltimedist) > 0) {
      walltimeTextFrame <- data.frame(xtext.stop=0,stoplabels="Clock")
      g <- g +
        geom_text(data=walltimeTextFrame,aes(x=xtext.stop,label=stoplabels),y=y.clock,
                  hjust=0,alpha=1,size=2,color="black",fontface="italic",show.legend = FALSE)
      g <- g +
        scale_shape_identity() +
        geom_point(data=walltimedist,aes(y=y.clock-50,x=distance,shape=prtchar,alpha=alphachar),
                   size=1.5,color="black",position = position_jitter(width=0,height=0),show.legend = FALSE) +
        geom_point(data=walltimedist,aes(y=y.clock,x=distance,shape=prthour,alpha=alphahour),
                   size=1.5,color="grey13",position = position_jitter(width=0,height=0),show.legend = FALSE)
    }
  }
  xAxisData <- data.frame(x=distancegraph,y=0)
  if (elev.min.int >= 0) g <- g + geom_line(data=xAxisData,aes(x=x,y=y),alpha=0.1)
  #  this is key to having text being kept at an appropriate size.  trial-and-error tuning
  plot.width <- (ngraphpoints+800)/600
  plot.height <- plot.width*((ymax-ymin+400)*vertical.multiplier/5280)/((ngraphpoints+800)*miles.per.point)
  plot.height <- plot.width*aspect.ratio
  if (!missing(savefn))  ggsave(savefn,width=plot.width,height=plot.height,units="in",dpi=600,limitsize=FALSE)
  return(g)
}

continuous_bar <- function(g,legendtext,xvar,vals,lowval,hival,lowcolor,hicolor,
                           legendwidth,y.band,band.height,label.height) {
  prtvalue <- lowcolor + (hicolor-lowcolor)*(vals-lowval)/(hival-lowval) #  map interval to color interval
  prtvalue[!is.na(vals) & vals<lowcolor] <- lowcolor
  prtvalue[!is.na(vals) & vals<hicolor] <- hicolor
  prtalpha <- (prtvalue-lowcolor)/(hicolor-lowcolor)  #higher values more intense
  prtalpha[prtalpha<0.3] <- 0.3

  width.legend <- c(4,2,5,2) #  columns forlegend - description,lowvalue,colorbar,hivalue
  column.legend <- c(0,cumsum(width.legend)[-4])*legendwidth
  xtext.legend <- c(0,column.legend[3],column.legend[3],column.legend[4])
  ytext.legend <- rep(y.band+1*band.height,4)
  alpha.legend <- c(1,1,0,1)
  hjust.legend <- c(0,1,0,0)
  legendlabels <- c(legendtext,paste0(lowval," "),"",paste0("  ",hival))
  legendpos <- xvar>=column.legend[3] & xvar<=column.legend[4]
  prtlegend <- seq(lowcolor,hicolor,length.out=sum(legendpos))
  x.legend <- xvar[legendpos]
  y.legend <- rep(ytext.legend[1],length(x.legend))
  valsDataFrame <- data.frame(xvar,prtvalue,prtalpha,vals,y.band,band.height)
  valsLegendFrame <- data.frame(x.legend,y.legend,prtlegend,label.height)
  valsTextFrame <- data.frame(xtext.legend,ytext.legend,width.legend,
                              legendlabels,alpha.legend,hjust.legend)
  gnew <- g +
    geom_tile(data=valsDataFrame,
              aes(y=y.band,x=xvar,fill=prtvalue,color=prtvalue,
                  alpha=prtalpha),
              height=band.height,show.legend = FALSE) +
    geom_tile(data=valsLegendFrame,
              aes(y=y.legend,x=x.legend,fill=prtlegend,
                  color=prtlegend),
              height=label.height,alpha=1,show.legend = FALSE) +
    geom_text(data=valsTextFrame,
              aes(x=xtext.legend,y=ytext.legend,label=legendlabels,
                  hjust=hjust.legend,alpha=alpha.legend),
              size=2,color="black",fontface="italic",show.legend = FALSE)
  return(gnew)
}
discrete_bar <- function(g,legendtext,xvar,vals,lowval,hival,lowcolor,midcolor,hicolor,
                         legendwidth,y.band,band.height,label.height) {
  legendlabels <- c(legendtext,
                    paste0("   < ",lowval,"  "),
                    paste0("   ",lowval,"-",hival," "),
                    paste0("   >= ",hival," "))
  legendcolors <- c(NA,lowcolor,midcolor,hicolor)
  prtvalue  <-  rep(NA,length(vals))
  prtvalue[!is.na(vals) & vals>0 & vals<lowval] <- legendcolors[2]
  prtvalue[!is.na(vals) & vals>=lowval & vals<hival] <- legendcolors[3]
  prtvalue[!is.na(vals) & vals>=hival] <- legendcolors[4]

  width.legend <- c(4,3,3,3)
  x1.legend <- c(0,cumsum(width.legend[1:3]))*legendwidth
  x2.legend <- cumsum(width.legend)*legendwidth
  y1.legend <- y.band+label.height-10
  y2.legend <- y1.legend+label.height-10
  xtext.legend <- x1.legend
  ytext.legend <- y1.legend+(y2.legend-y1.legend)/2
  alpha.legend <- c(0,1,1,1)
  hjust.legend <- c(0,0,0,0)
  valsDataFrame <- data.frame(xvar,y.band,vals,prtvalue,band.height)
  valsTextFrame <- data.frame(x1.legend,x2.legend,y1.legend,y2.legend,
                              xtext.legend,ytext.legend,legendlabels,
                              legendcolors,alpha.legend,hjust.legend)
  gnew <- g +
    geom_tile(data=valsDataFrame,
              aes(y=y.band,x=xvar,fill=prtvalue,color=prtvalue),
              height=band.height,show.legend=FALSE) +
    geom_rect(data=valsTextFrame,
              aes(xmin=x1.legend,xmax=x2.legend,fill=legendcolors,alpha=alpha.legend),
              ymin=y1.legend,ymax=y2.legend,inherit.aes=FALSE,show.legend=FALSE) +
    geom_text(data=valsTextFrame,
              aes(x=xtext.legend,y=ytext.legend,label=legendlabels,hjust=hjust.legend),
              size=2,fontface="italic")
  return(gnew)
}
