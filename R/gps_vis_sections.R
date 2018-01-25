#  create the basic plot, with reserved room below
#  elevation and speed are smoothed already if that's what is wanted
drawProfile <- function(distancevec,elevationvec,speedvec,
                        distPerPoint,palette,naPlotColor,
                        vertMult,npoints,minNumPoints,
                        elevationShape,imperial,
                        hrDistance,cadDistance,powerDistance,
                        hrTime,cadTime,powerTime,showTime) {

  ngraphpoints <- max(minNumPoints,npoints)
  dist <- distancevec[length(distancevec)]
  if (is.na(vertMult)|vertMult<=1)
    vertMult <- verticalMult(dist,imperial)
  heightFactor=vertMult/50
  heightBelow <- heightWith(hrDistance,cadDistance,powerDistance,
                             hrTime,cadTime,powerTime,
                             showTime,totalCall=TRUE,
                             scale=heightFactor)

  #  use equally spaced grid for plotting
  eProfilePts <- stats::approx(distancevec,elevationvec,n=npoints)
  sProfilePts <- stats::approx(distancevec,speedvec,n=npoints)
  distance <- eProfilePts[[1]]
  elevation <- eProfilePts[[2]]
  speed <- sProfilePts[[2]]
  #  need to do color scaling
  if (imperial) {
    speed[speed<3] <- 3
    speed[speed>40] <- 40
    elevalpha <-
      0.4 + 0.6*(ifelse(speed > 15,ifelse(speed<40,(speed-15)/25,1),0))
    elevround <- 200
  }
  else {
    speed[speed<5] <- 5
    speed[speed>67] <- 67
    elevalpha <-
      0.4 + 0.6*(ifelse(speed > 25,ifelse(speed<67,(speed-12)/25,1),0))
    elevround <- 100
  }
  elevprtchar <- rep("|",length(distance))

# set limits for plots
  elevMin <- min(elevation)
  if (elevMin < 0) {
    elevMinInt <- 100*floor(elevround*floor(elevMin/elevround)/100)
  } else {
    elevMinInt <- 500*floor(elevround*floor(elevMin/elevround)/500)
  }
  elevMinShade <- max(0,elevMinInt)
  elevMinShade <- elevMinInt
  ymin <- elevMinInt - heightBelow - 50
  ybottom <- ymin

  elevMax <- max(elevation)
  elevMaxInt <- elevround*ceiling(elevMax/elevround)
  ymax <- 500*ceiling((elevMaxInt + height("summary",heightFactor))/500)

  xmin <- 0
  xmax <- distPerPoint*ngraphpoints

  plotdata <- data.frame(distance,elevation,speed,elevprtchar,elevalpha)
  major.breaks <- ifelse(dist>100,10,ifelse(dist>10,5,1))
 #  aspect.ratio <- vertMult*(ymax-ymin)/(5280*(ngraphpoints)*distPerPoint)

  g <- ggplot2::ggplot(plotdata,aes(x=distance,y=elevation)) +
    ggplot2::scale_y_continuous(limits=c(ymin,ymax),expand=c(0,0),
                                breaks=seq(from=0,to=ymax,by=500),
                                minor_breaks=seq(0,ymax,100)) +
    ggplot2::scale_x_continuous(limits=c(xmin,xmax),expand=c(0,0),
                                breaks=seq(from=0,to=xmax,by=major.breaks),
                                minor_breaks=seq(0,xmax,1)) +
 #  ggplot2::theme(aspect.ratio=aspect.ratio) +
    ggplot2::theme(legend.title=
                     ggplot2::element_text(colour="steelblue",size=7)) +
    ggplot2::theme(legend.text=
                     ggplot2::element_text(colour="lightsteelblue",size=6)) +
    ggplot2::theme(legend.key.size=unit(10,"points")) +
    ggplot2::theme(legend.justification="top") +
 #  suppress x axis, add it manually
    ggplot2::theme(axis.title.x=ggplot2::element_blank(),
                   axis.text.x=ggplot2::element_blank(),
                   axis.ticks.x=ggplot2::element_blank(),
                   axis.line.x=ggplot2::element_blank(),
                   axis.line.y=ggplot2::element_blank(),
                   panel.background=ggplot2::element_rect(fill="lightblue1",
                                                  colour="lightblue1",
                                                  size=0.5,linetype="solid"),
                   panel.grid.major=ggplot2::element_line(size=0.3,
                                                  linetype='solid',
                                                  colour="steelblue1"),
                   panel.grid.minor=ggplot2::element_line(size=0.15,
                                                  linetype='solid',
                                                  colour="steelblue1")) +
    ggplot2::labs(y=paste0("Elevation (",
                           ifelse(imperial,"ft)","m)"))) +
    ggplot2::theme(axis.title.y=element_text(hjust=0.8)) +
    viridis::scale_color_viridis(option=palette,limits=c(0,40),
                                 na.value=naPlotColor,direction=-1) +
    viridis::scale_fill_viridis(option=palette,limits=c(0,40),
                                na.value=naPlotColor,direction=-1) +
    ggplot2::geom_ribbon(ggplot2::aes(ymax=elevation,ymin=elevMinShade),
                         color="lightgreen",fill="lightgreen",
                         alpha=0.7) +
#   lay down a light background that lets some of the grid show through
    ggplot2::geom_ribbon(ggplot2::aes(ymax=elevMinShade,ymin=ymin),
                         color="white",fill="white",alpha=0.7) +
#    ggplot2::geom_ribbon(ggplot2::aes(ymax=elevation+5,ymin=elevation-5,
#                         color=speed,fill=speed))
    ggplot2::geom_line(ggplot2::aes(color=speed))
#    ggplot2::geom_point(ggplot2::aes(color=speed),shape=elevationShape,
#                      size=1.25*heightFactor,alpha=0.6,
#                      position=ggplot2::position_nudge(y=1.2))
    if (npoints < ngraphpoints) {
      g <- g +
        ggplot2::geom_rect(xmin=npoints*distPerPoint,xmax=xmax,
                           ymin=ymin,ymax=ymax,color="white",fill="white")
    }

    return(list(g=g,xmin=xmin,xmax=xmax,xlast=distPerPoint*npoints,
              ymin=elevMinInt,ymax=ymax,ybottom=ybottom,vertmult=vertMult,
              distPerPoint=distPerPoint,heightFactor=heightFactor,
              npoints=npoints,ngraphpoints=ngraphpoints))
}
drawSummary <- function(ggp,summary,title){

  ggpreturn <- ggp
  ymax <- ggp[["ymax"]]
  xmax <- ggp[["xmax"]]
  xlast <- ggp[["xlast"]]
  g <- ggp[["g"]]
  heightFactor=ggp[["heightFactor"]]
  headerWidth <- min(xmax,25)
  ycenterSum <- seq(1,4) #rows
  ycenterSum <- ymax - (50/heightFactor)*ycenterSum
  xcenterSum <- c(headerWidth*0.05,headerWidth*0.55)
  xposSum <- c(rep(xcenterSum[1],4),rep(xcenterSum[2],4))
  yposSum <- c(ycenterSum,ycenterSum)
  summarylabels <-
    c(paste0(round(xlast,digits=2)," miles"),
      paste0(round(3.28084*summary$ascent[1],digits=0)," ft climbing"),
      paste0(round(1/60*summary$rolling.time[1],digits=0)," minutes rolling"),
      paste0(round(1/60*summary$total.time[1],digits=0)," minutes total"),
      paste0(round(2.23694*summary$speed.rolling.m.s[1],digits=2)," mph avg"),
      paste0(round(summary$avgcadence.withzeros[1],digits=1),
             " cad avg(incl/zeros)"),
      paste0(round(summary$session.total.calories[1],digits=0)," kCal burned"),
      paste0(round(summary$avgpower.postcal.nozeros[1],digits=0)," watts post-cal(excl/zeros)"))
  summaryTextFrame <- data.frame(xposSum,yposSum,summarylabels)
  if ((summary$pct.trkpts.cad < .95)|(is.na(summary$avgpower.postcal.nozeros)))
    summaryTextFrame <- summaryTextFrame[-8,]
  if ((summary$pct.trkpts.hr < .95)|(is.na(summary$session.total.calories)))
    summaryTextFrame <- summaryTextFrame[-7,]
  if (summary$pct.trkpts.cad < .7)
    summaryTextFrame <- summaryTextFrame[-6,]
  g <- g +
    ggplot2::ggtitle(paste0(title,"  ",summary$start.time[1])) +
    ggplot2::geom_text(data=summaryTextFrame,
                       aes(x=xposSum,y=yposSum,label=summarylabels),
                       size=3,hjust=0,alpha=1,color="midnightblue") +
    ggplot2::theme(plot.title=element_text(color="Black",face="bold",
                                           size=13,hjust=0.5))
  ggpreturn[["g"]] <- g
  return(ggpreturn)

}
#  draw cadence bar graph
drawCadence <- function(ggp,cadence,xvar,segment,
                        cadLow,cadTarget,
                        cadCont,cadContLow,cadContHigh,
                        cadColorLow,cadColorMid,cadColorHigh,
                        minNumPoints,showlegend=TRUE) {
  ggpreturn <- ggp
  xend <- ggp[["xlast"]]
  g <- ggp[["g"]]
  npoints <- ggp[["npoints"]]
  heightFactor=ggp[["heightFactor"]]
  distPerPoint=ggp[["distPerPoint"]]
  yCadTop <- ggp[["ymin"]]
  ymin <- yCadTop - heightItem(scale=heightFactor)
  # column width vectors sum to 13 in bar functionsd
  cadLegendWidth <- hrCadLegendWidth(npoints,distPerPoint,minNumPoints)

  drawpts <- approxSegments(xvar=xvar,yvar=cadence,
                            segment=segment,npoints=npoints)
  xvardraw <- drawpts[[1]]*(xend/max(drawpts[[1]]))
  xvardraw[xvardraw>distPerPoint*npoints] <- distPerPoint*npoints
  xvardraw[xvardraw<0] <- 0
  caddraw <- drawpts[[2]]
  if (cadCont) {
    g <- continuous_bar(g,legendtext="Cadence",xvar=xvardraw,vals=caddraw,
                        lowval=cadContLow,hival=cadContHigh,
                        lowcolor=cadColorLow,hicolor=cadColorHigh,
                        legendwidth=cadLegendWidth,y.bottom=ymin,
                        band.height=height("band",heightFactor),
                        label.height=height("label",heightFactor),
                        gap.height=height("gap",heightFactor),showlegend)
    } else {
    g <- discrete_bar(g,legendtext="Cadence",xvar=xvardraw,vals=caddraw,
                      lowval=cadLow,hival=cadTarget,
                      lowcolor=cadColorLow,midcolor=cadColorMid,
                      hicolor=cadColorHigh,
                      legendwidth=cadLegendWidth,y.bottom=ymin,
                      band.height=height("band",heightFactor),
                      label.height=height("label",heightFactor),
                      gap.height=height("gap",heightFactor),showlegend)
  }
  ggpreturn[["g"]] <- g
  ggpreturn[["ymin"]] <- ymin
  return(ggpreturn)
}
drawHr <- function(ggp,hr,xvar,segment,
                   hrLow,hrHigh,
                   hrColorLow,hrColorHigh,
                   minNumPoints,showlegend=TRUE) {

  ggpreturn <- ggp
  xend <- ggp[["xlast"]]
  g <- ggp[["g"]]
  npoints <- ggp[["npoints"]]
  heightFactor=ggp[["heightFactor"]]
  distPerPoint=ggp[["distPerPoint"]]
  yHrTop <- ggp[["ymin"]]
  ymin <- yHrTop - heightItem(scale=heightFactor)
  # column width vectors sum to 13 in bar functionsd
  hrLegendWidth <- hrCadLegendWidth(npoints,distPerPoint,minNumPoints)

  drawpts <- approxSegments(xvar=xvar,yvar=hr,
                            segment=segment,npoints=npoints)
  xvardraw <- drawpts[[1]]*(xend/max(drawpts[[1]]))
  xvardraw[xvardraw>distPerPoint*npoints] <- distPerPoint*npoints
  xvardraw[xvardraw<0] <- 0
  hrdraw <- drawpts[[2]]
  g <- g +
    ggplot2::geom_blank()
  g <- continuous_bar(g,legendtext="Heart Rate",xvar=xvardraw,vals=hrdraw,
                      lowval=hrLow,hival=hrHigh,
                      lowcolor=hrColorLow,hicolor=hrColorHigh,
                      legendwidth=hrLegendWidth,y.bottom=ymin,
                      band.height=height("band",heightFactor),
                      label.height=height("label",heightFactor),
                      gap.height=height("gap",heightFactor),showlegend)
  ggpreturn[["g"]] <- g
  ggpreturn[["ymin"]] <- ymin
  return(ggpreturn)
}
drawPower <- function(ggp,power,xvar,segment,
                      powerLow,powerHigh,
                      powerColorLow,powerColorHigh,
                       minNumPoints,showlegend=TRUE) {

  ggpreturn <- ggp
  xend <- ggp[["xlast"]]
  g <- ggp[["g"]]
  npoints <- ggp[["npoints"]]
  heightFactor=ggp[["heightFactor"]]
  distPerPoint=ggp[["distPerPoint"]]
  yPowerTop <- ggp[["ymin"]]
  ymin <- yPowerTop - heightItem(scale=heightFactor)
  # column width vectors sum to 13 in bar functionsd
  powerLegendWidth <- hrCadLegendWidth(npoints,distPerPoint,minNumPoints)

  drawpts <- approxSegments(xvar=xvar,yvar=power,
                            segment=segment,npoints=npoints)
  xvardraw <- drawpts[[1]]*(xend/max(drawpts[[1]]))
  xvardraw[xvardraw>distPerPoint*npoints] <- distPerPoint*npoints
  xvardraw[xvardraw<0] <- 0
  powerdraw <- drawpts[[2]]
  powerdraw[!is.na(powerdraw) &
              powerdraw<powerLow &
              powerdraw>0.0] <- powerLow
  g <- g +
    ggplot2::geom_blank()
  g <- continuous_bar(g,legendtext="Watts",xvar=xvardraw,vals=powerdraw,
                      lowval=powerLow,hival=powerHigh,
                      lowcolor=powerColorLow,hicolor=powerColorHigh,
                      legendwidth=powerLegendWidth,y.bottom=ymin,
                      band.height=height("band",heightFactor),
                      label.height=height("label",heightFactor),
                      gap.height=height("gap",heightFactor),showlegend)
  ggpreturn[["g"]] <- g
  ggpreturn[["ymin"]] <- ymin
  return(ggpreturn)
}
drawXAxis <- function(ggp,distance,startsAndStops,
                      showStops,distPerPoint,imperial=TRUE,
                      underLine=FALSE,lineAtZero=FALSE) {
  ggpreturn <- ggp
  g <- ggp[["g"]]
  xmax <- ggp[["xmax"]]
  ngraphpoints <- ggp[["ngraphpoints"]]
  npoints <- ggp[["npoints"]]
  heightFactor=ggp[["heightFactor"]]
  yXAxis <- ggp[["ymin"]]

  if (underLine) {
    if (lineAtZero) {
      yDistAxis <- yXAxis
      yDistScale <- yDistAxis - height("axisToLegend",heightFactor)
      yDistLabel <- yDistScale - height("axisLabel",heightFactor)
      ymin <- yDistLabel - 2*height("gap",heightFactor)
    } else {
      yDistAxis <- yXAxis - height("gap",heightFactor)
      yDistScale <- yDistAxis - height("axisToLegend",heightFactor)
      yDistLabel <- yDistScale - height("axisLabel",heightFactor)
      ymin <- yDistLabel - height("gap",heightFactor)
    }
  } else {
    yDistLabel <- yXAxis - 2*height("gap",heightFactor) -
               height("axisLabel",heightFactor)
    yDistAxis <- yDistLabel - height("axisToLegend",heightFactor)
    yDistScale <- yDistAxis
    ymin <- yDistAxis
  }
  if (xmax < 2) {
    xincrement <- 0.5
    mincrement <- 0.5
  } else if (xmax < 5) {
    xincrement <- 1
    mincrement <- 0.25
  } else if (xmax < 10) {
    xincrement <- 2
    mincrement <- 0.5
  } else if (xmax < 50) {
    xincrement <- 5
    mincrement <- 1
  } else if (xmax < 100) {
    xincrement <- 10
    mincrement <- 5
  } else {
    xincrement <- 20
    mincrement <- 5
  }
  #  axis line
  distancegraphends <- c(0,npoints*distPerPoint)
  xAxisData <- data.frame(x=distancegraphends,y=yDistAxis)
  g <- g + ggplot2::geom_line(data=xAxisData,aes(x=x,y=y),alpha=1)

  stops <- startsAndStops[["stopSumFrame"]]
  if (showStops) {
    #  create data frames for stops/breaks
    #  classify seg breaks as short stops or long breaks, work in grid coords
    stopdata <-
      data.frame(distance=stops$locEnd,yDistAxis,lenStop=stops$lenStop)
    stopdata$pauseSize <- ifelse(stopdata$lenStop<300,
                                 1,
                                 1+log(stopdata$lenStop/300))
    stopdataShort <- stopdata[stopdata$lenStop<300,]
    stopdataLong <- stopdata[stopdata$lenStop>=300,]
    if (nrow(stopdataShort)>0) {
      g <- g +
        ggplot2::geom_point(data=stopdataShort,
                            aes(y=yDistAxis,alpha=0.8,size=pauseSize),
                            color="red1",shape=124,show.legend=FALSE)
    }
    if (nrow(stopdataLong)>0) {
      g <- g +
        ggplot2::geom_point(data=stopdataLong,
                            aes(y=yDistAxis,alpha=0.8,size=pauseSize),
                            color="purple3",shape=124,show.legend=FALSE)
    }
  }
  # axis labels
  axischarsize <- 2.75
  x <- seq(0,xmax,xincrement)
  xtext <- as.character(x)
  xhjust <- c(0,rep(0.5,length(x)-1))
  axisdata <- data.frame(x=x,y=yDistScale,label=xtext,hjust=xhjust)
  g <- g +
    ggplot2::geom_text(data=axisdata,aes(x=x,y=y,label=label,hjust=hjust,vjust=-0.2),
                       size=axischarsize)
  # axis ticks
  x <- seq(0,xmax,mincrement)
  axisdata2 <- data.frame(x=x,y=yDistAxis)
  g <- g +
    ggplot2::geom_point(data=axisdata2,aes(x=x,y=y),size=1.1,color="black",
                        shape=124,show.legend=FALSE)
  # axis title
  xAxisTextFrame <- data.frame(x=xmax/2,
                               y=yDistLabel,
                               label=ifelse(imperial,
                                            "Distance (mi)",
                                            "Distance (km)"))
  g <- g +
    ggplot2::geom_text(data=xAxisTextFrame,aes(x=x,y=y,label=label),
                       size=1.10*axischarsize,hjust=0.5,vjust=0,
                       color="black",show.legend = FALSE)

  ggpreturn[["g"]] <- g
  ggpreturn[["ymin"]] <- ymin
  return(ggpreturn)
}
drawXTConnect <- function(ggp,distance,walltime,startsAndStops,
                          distPerPoint,hoursPerPoint) {
  ggpreturn <- ggp
  g <- ggp[["g"]]
  xmax <- ggp[["xmax"]]
  ngraphpoints <- ggp[["ngraphpoints"]]
  npoints <- ggp[["npoints"]]
  timelast <- walltime[length(walltime)]
  yXTConnect <- ggp[["ymin"]]
  heightFactor=ggp[["heightFactor"]]
  ymin <- yXTConnect - height("connector",heightFactor)
  yCenter <- (yXTConnect + ymin)/2
  xscale <- distPerPoint/(hoursPerPoint*3600)

  yTimeAxis <- ymin
  yDistAxis <- yXTConnect

  stops <- startsAndStops[["stopSumFrame"]]

   #stop begin time,stop end times, stop location
  pointsStopBeg <- data.frame(group=stops$stopNum,
                              x=stops$locBeg,
                              y=yDistAxis)
  pointsStopEnd <- data.frame(group=stops$stopNum,
                              x=stops$locEnd,
                              y=yDistAxis)
  pointsTimeBeg <- data.frame(group=stops$stopNum,
                              x=xscale*stops$timeBeg,
                              y=yTimeAxis)
  pointsTimeEnd <- data.frame(group=stops$stopNum,
                              x=xscale*stops$timeEnd,
                              y=yTimeAxis)
  stopData <- rbind(pointsStopBeg,pointsStopEnd,pointsTimeBeg,pointsTimeEnd)
  g <- g +
     ggplot2::geom_polygon(data=stopData,
                           aes(x=x,y=y,group=group),fill="red3",alpha=0.5,
                           show.legend=FALSE)
   XTConnTextFrame <- data.frame(x=xmax/2,y=yCenter,label="Stops")
   g <- g +
       ggplot2::geom_text(data=XTConnTextFrame,aes(x=x,y=y,label=label),
                          size=3.3,hjust=0.5,vjust=0.5,
                          color="red",show.legend = FALSE)

  hourtimes <- seq(0,timelast,3600)
  hourcolors <- rep(40,length(hourtimes))
  houralphas <- rep(0.5,length(hourtimes))
  if (timelast <= 7200 & timelast >= 900) {
    quartertimes <- seq(900,timelast,3600)
    if (timelast >= 1800)
      quartertimes <- c(quartertimes,seq(1800,timelast,3600))
    if (timelast >= 2700)
      quartertimes <- c(quartertimes,seq(2700,timelast,3600))
    quartercolors <- rep(35,length(quartertimes))
    quarteralphas <- rep(0.25,length(quartertimes))
    hourtimes <- c(hourtimes,quartertimes)
    hourcolors <- c(hourcolors,quartercolors)
    houralphas <- c(houralphas,quarteralphas)
  }
  hourplaces <- approx(x=walltime,y=distance,hourtimes)[[2]]

  XTConnectData <- data.frame(x=c(hourplaces),
                              xend=c(hourtimes),
                              color=c(hourcolors),
                              alpha=c(houralphas),
                              y=yDistAxis,
                              yend=yTimeAxis,
                              stringsAsFactors=FALSE)
  XTConnectData$xend <- xscale*XTConnectData$xend
  g <- g +
    ggplot2::geom_segment(data=XTConnectData,
                          aes(x=x,y=y,xend=xend,yend=yend,color=color,alpha=alpha),
                          show.legend = FALSE)

  ggpreturn[["g"]] <- g
  ggpreturn[["ymin"]] <- ymin
  return(ggpreturn)
}
drawTAxis <- function(ggp,walltime,startsAndStops,distPerPoint,hoursPerPoint) {
  ggpreturn <- ggp
  g <- ggp[["g"]]
  xmax <- ggp[["xmax"]]
  ngraphpoints <- ggp[["ngraphpoints"]]
  npoints <- ggp[["npoints"]]
  heightFactor=ggp[["heightFactor"]]
  yTAxis <- ggp[["ymin"]]
  ymin <- yTAxis - heightTAxis(heightFactor)

  stops <- startsAndStops[["stopSumFrame"]]
  segs <- startsAndStops[["segSumFrame"]]

  #  axis line - color coded for stops
  xscale <- distPerPoint/hoursPerPoint
  tAxisSegData <- data.frame(x=segs$timeBeg,xend=segs$timeEnd,xcol=40,y=yTAxis)
  tAxisStopData <- data.frame(x=stops$timeBeg,xend=stops$timeEnd,xcol=15,y=yTAxis)
  tAxisData <- rbind(tAxisSegData,tAxisStopData)
  tAxisData$x <- (xscale/3600)*tAxisData$x
  tAxisData$xend <- (xscale/3600)*tAxisData$xend
  g <- g +
    geom_segment(data=tAxisData,
                 aes(x=x,y=y,xend=xend,yend=y,color=xcol))
  tmax <- (xmax/xscale)
  tincr <- round(exp(log(10)*floor(log10(tmax))))

  # axis numbers for hours and fractions
  axischarsize <- 2.75
  if (tincr > 0) {
    t <- seq(0,tmax,tincr)
    ttext <- as.character(t)
    t <- 3600*t
    tAxisLabels <- data.frame(x=t,y=yTAxis,ttext=ttext)
  } else {
    tAxisLabels <- data.frame(x=0,y=yTAxis,ttext="0")
  }
  tAxisLabels$x <- (xscale/3600)*tAxisLabels$x
  tAxisLabels$y <- yTAxis - height("axisToLegend",heightFactor)
  if (nrow(tAxisLabels) >= 2) {
    tAxisLabels$hjust <- c(0,rep(0.5,nrow(tAxisLabels)-2),1)
  } else {
    tAxisLabels$hjust <- c(0,rep(0,nrow(tAxisLabels)-1))
  }
  g <- g +
    geom_text(data=tAxisLabels,aes(x=x,y=y,label=ttext,hjust=hjust),vjust=0,
              size=axischarsize,show.legend = FALSE)
  if (tmax <3 & tmax >= 0.25) {
    df15 <- data.frame(x=seq(900,tmax*3600,3600),y=yTAxis,ttext="1/4")
    if (tmax >= 0.5)
      df15 <- rbind(df15,
                    data.frame(x=seq(1800,tmax*3600,3600),y=yTAxis,ttext="1/2"))
    if (tmax >= 0.75)
      df15 <- rbind(df15,
                    data.frame(x=seq(2700,tmax*3600,3600),y=yTAxis,ttext="3/4"))
    df15$x <- (xscale/3600)*df15$x
    df15$y <- yTAxis - .8*height("axisToLegend",heightFactor)
    g <- g +
      geom_text(data=df15,aes(x=x,y=y,label=ttext),hjust=0.5,vjust=0,
                size=0.75*axischarsize,show.legend = FALSE)
  }
  #  axis ticks
  incr <- ifelse(tmax > 6,xscale,xscale/4)
  t <- seq(0,xscale*tmax,incr)
  axisdata2 <- data.frame(x=t,y=yTAxis)
  g <- g +
    ggplot2::geom_point(data=axisdata2,aes(x=x,y=y),size=1.2,color="black",
                        shape=124,show.legend=FALSE)
  # axis label
  ylabel <- yTAxis-height("axisToLegend",heightFactor) -
    height("axisLabel",heightFactor)
  tAxisTextFrame <- data.frame(x=xmax/2,
                               y=ylabel,
                               label="Time (hrs)")
  g <- g +
    ggplot2::geom_text(data=tAxisTextFrame,aes(x=x,y=y,label=label),
                       size=1.10*axischarsize,hjust=0.5,vjust=0,
                       color="black",show.legend = FALSE)

  ggpreturn[["g"]] <- g
  ggpreturn[["ymin"]] <- ymin
  return(ggpreturn)
}

