#  create the basic plot, with reserved room below
#  elevation and speed are smoothed already if that's what is wanted
drawProfile <- function(distancevec,elevationvec,speedvec,
                        distPerPoint,palette,
                        vertMult,npoints,minNumPoints,
                        elevationShape,imperial,
                        hrDistance,cadDistance,
                        hrTime,cadTime,showTime) {

  ngraphpoints <- max(minNumPoints,npoints)
  dist <- distancevec[length(distancevec)]
  if (is.na(vertMult)|vertMult<=1)
    vertMult <- verticalMult(dist,imperial)
  heightFactor=vertMult/50
  heightBelow <- heightWith(hrDistance,cadDistance,
                             hrTime,cadTime,
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
    elevround <- 200
  }
  elevprtchar <- rep("|",length(distance))

# set limits for plots
  elevMin <- min(elevation)
  elevMinInt <- min(0,elevround*floor(elevMin/elevround))
  elevMax <- max(elevation)
  elevMaxInt <- elevround*ceiling(elevMax/elevround)
  ymin <- elevMinInt - heightBelow - 100
  ymax <- elevMaxInt + height("summary",heightFactor)
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
    ggplot2::theme(axis.title.x=element_blank(),
                   axis.text.x=element_blank(),
                   axis.ticks.x=element_blank()) +
    ggplot2::labs(y=paste0("Elevation (",
                           ifelse(imperial,"ft)","m)"))) +
    ggplot2::theme(axis.title.y=element_text(hjust=0.8)) +
    viridis::scale_color_viridis(option=palette,limits=c(0,40),
                                 na.value="white",direction=-1) +
    viridis::scale_fill_viridis(option=palette,limits=c(0,40),
                                na.value="white",direction=-1) +
    ggplot2::geom_rect(ggplot2::aes(xmin=xmin,xmax=xmax,ymin=ymin,ymax=0),
                       fill="white",color="white") +
    ggplot2::geom_area(color="white",fill="white") +
    ggplot2::geom_point(ggplot2::aes(color=speed),shape=elevationShape,
                        size=1.25*heightFactor,
                        position=ggplot2::position_nudge(y=1.2))

    return(list(g=g,xmin=xmin,xmax=xmax,xlast=distPerPoint*npoints,
              ymin=elevMinInt,ymax=ymax,vertmult=vertMult,
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
  ycenterSum <- seq(1,3) #rows
  ycenterSum <- ymax - (60/heightFactor)*ycenterSum
  xcenterSum <- c(headerWidth*0.05,headerWidth*0.55)
  xposSum <- c(rep(xcenterSum[1],3),rep(xcenterSum[2],3))
  yposSum <- c(ycenterSum,ycenterSum)
  summarylabels <-
    c(paste0(round(xlast,digits=2)," miles"),
      paste0(round(3.28084*summary$ascent[1],digits=0)," ft climbing"),
      paste0(round(1/60*summary$rolling.time[1],digits=0)," minutes rolling"),
      paste0(round(2.23694*summary$speed.rolling.m.s[1],digits=2)," mph avg"),
      paste0(round(summary$avgcadence.withzeros[1],digits=1),
             " cad avg(incl/zeros)"),
      paste0(round(summary$session.total.calories[1],digits=0)," kCal burned"))
  summaryTextFrame <- data.frame(xposSum,yposSum,summarylabels)
  if ((summary$pct.trkpts.hr < .98)|(is.na(summary$session.total.calories)))
    summaryTextFrame <- summaryTextFrame[-6,]
  if (summary$pct.trkpts.cad < .7)
    summaryTextFrame <- summaryTextFrame[-5,]
  g <- g +
    ggtitle(paste0(title,"  ",summary$start.time[1])) +
    geom_text(data=summaryTextFrame,
              aes(x=xposSum,y=yposSum,label=summarylabels),
              size=3,hjust=0,alpha=1,color="slategray4") +
    theme(plot.title=element_text(color="Black",face="bold",size=15,hjust=0.5))
  ggpreturn[["g"]] <- g
  return(ggpreturn)

}
#  draw cadence bar graph
drawCadence <- function(ggp,cadence,xvar,
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
  drawpts <- stats::approx(xvar,cadence,n=npoints)
  xvardraw <- drawpts[[1]]*(xend/max(drawpts[[1]]))
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
drawHr <- function(ggp,hr,xvar,
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
  drawpts <- stats::approx(xvar,hr,n=npoints)
  xvardraw <- drawpts[[1]]*(xend/max(drawpts[[1]]))
  hrdraw <- drawpts[[2]]
  g <- g +
    geom_blank()
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
drawTAxis <- function(ggp,segment,walltime,distPerPoint,hoursPerPoint) {
  ggpreturn <- ggp
  g <- ggp[["g"]]
  xmax <- ggp[["xmax"]]
  ngraphpoints <- ggp[["ngraphpoints"]]
  npoints <- ggp[["npoints"]]
  heightFactor=ggp[["heightFactor"]]
  yTAxis <- ggp[["ymin"]]
  ymin <- yTAxis - height("axis",heightFactor)
  yCenter <- (yTAxis+ymin)/2

  newseg <- segment != lag_one(segment)
  newseg[1] <- TRUE
  endseg <- segment != lead_one(segment)
  endseg[length(endseg)] <- TRUE
  segbegs <- walltime[newseg]
  segends <- walltime[endseg]
  stopbegs <- segends[-length(segbegs)]
  stopends <- segbegs[-1]

  #  axis line - color coded for stops
  xscale <- distPerPoint/hoursPerPoint
  tAxisSegData <- data.frame(x=segbegs,xend=segends,xcol=40,y=yCenter)
  tAxisStopData <- data.frame(x=stopbegs,xend=stopends,xcol=15,y=yCenter)
  tAxisData <- rbind(tAxisSegData,tAxisStopData)
  tAxisData$x <- (xscale/3600)*tAxisData$x
  tAxisData$xend <- (xscale/3600)*tAxisData$xend
  g <- g +
    geom_segment(data=tAxisData,
                 aes(x=x,y=y,xend=xend,yend=y,color=xcol))
  tmax <- (xmax/xscale)
  tincr <- round(exp(log(10)*floor(log10(tmax))))

  # axis numbers for hours and fractions
  axischarsize <- 3
  if (tincr > 0) {
    t <- seq(0,tmax,tincr)
    ttext <- as.character(t)
    t <- 3600*t
    tAxisLabels <- data.frame(x=t,y=yCenter,ttext=ttext)
  } else {
    tAxisLabels <- data.frame(x=0,y=yCenter,ttext="0")
  }
  tAxisLabels$x <- (xscale/3600)*tAxisLabels$x
  tAxisLabels$y <- yCenter - 0.8*height("label",heightFactor)
  if (nrow(tAxisLabels) >= 2) {
    tAxisLabels$hjust <- c(0,rep(0.5,nrow(tAxisLabels)-2),1)
  } else {
    tAxisLabels$hjust <- c(0,rep(0,nrow(tAxisLabels)-1))
  }
  g <- g +
    geom_text(data=tAxisLabels,aes(x=x,y=y,label=ttext,hjust=hjust),
              size=axischarsize,show.legend = FALSE)
  if (tmax <3 & tmax >= 0.25) {
    df15 <- data.frame(x=seq(900,tmax*3600,3600),y=yCenter,ttext="1/4")
    if (tmax >= 0.5)
      df15 <- rbind(df15,
               data.frame(x=seq(1800,tmax*3600,3600),y=yCenter,ttext="1/2"))
     if (tmax >= 0.75)
      df15 <- rbind(df15,
                    data.frame(x=seq(2700,tmax*3600,3600),y=yCenter,ttext="3/4"))
    df15$x <- (xscale/3600)*df15$x
    df15$y <- yCenter - .75*height("label",heightFactor)
    g <- g +
      geom_text(data=df15,aes(x=x,y=y,label=ttext),hjust=0.5,
                size=0.75*axischarsize,show.legend = FALSE)
  }
  #  axis ticks
  incr <- ifelse(tmax > 6,xscale,xscale/4)
  t <- seq(0,xscale*tmax,incr)
  axisdata2 <- data.frame(x=t,y=yCenter)
  g <- g +
    geom_point(data=axisdata2,aes(x=x,y=y),size=1.2,color="black",
               shape=124,show.legend=FALSE)
  # axis label
  tAxisTextFrame <- data.frame(x=xmax/2,
                               y=yCenter-1.6*height("label",heightFactor),
                               label="Time (hrs)")
  g <- g +
    geom_text(data=tAxisTextFrame,aes(x=x,y=y,label=label),
              size=1.10*axischarsize,hjust=0.5,
              color="black",show.legend = FALSE)

  ggpreturn[["g"]] <- g
  ggpreturn[["ymin"]] <- ymin
  return(ggpreturn)
}
drawXAxis <- function(ggp,segment,walltime,distance,
                      showStops,distPerPoint,imperial=TRUE,
                      underLine=FALSE,lineAtZero=FALSE) {
  ggpreturn <- ggp
  g <- ggp[["g"]]
  xmax <- ggp[["xmax"]]
  ngraphpoints <- ggp[["ngraphpoints"]]
  npoints <- ggp[["npoints"]]
  heightFactor=ggp[["heightFactor"]]
  yXAxis <- ggp[["ymin"]]
  if (!underLine & ! lineAtZero)
    yXAxis <- yXAxis - height("gap",heightFactor)
  if(underLine | lineAtZero) {
    ymin <- yXAxis - 0.5*height("axis",heightFactor)
    yCenter <- yXAxis
  } else {
    ymin <- yXAxis - height("axis",heightFactor)
    yCenter <- (yXAxis+ymin)/2
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
  xAxisData <- data.frame(x=distancegraphends,y=yCenter)
  g <- g + geom_line(data=xAxisData,aes(x=x,y=y),alpha=1)

  if (showStops) {
    #  create data frames for stops/breaks
    #  classify seg breaks as short stops or long breaks, work in grid coords
    first.seg <- segment != lag_one(segment) # don't include first egment start
    pause.len <- first.seg*(walltime-lag_one(walltime))
    pause.size <- rep(0,npoints)
    pause.alpha <- rep(0,npoints)
    pause.short <- pause.len < 300 & pause.len > 0
    pause.long <- pause.len >= 300
    if (sum(pause.short) > 0) {
      pause.size[pause.short] <- 1
      pause.alpha[pause.short] <- 0.6
    }
    if (sum(pause.long) > 0) {
      pause.size[pause.long] <- 1.5 + 1.5*log(pause.len[pause.long]/300)
      pause.alpha[pause.long] <- 0.5
    }
    stopdata <-
      data.frame(distance,yCenter,pause.long,pause.short,pause.size,pause.alpha)
    stopdata.short <- stopdata[stopdata$pause.short,]
    stopdata.long <- stopdata[stopdata$pause.long,]
    if (nrow(stopdata.short)>0) {
      g <- g +
        geom_point(data=stopdata.short,
                   aes(y=yCenter,alpha=pause.alpha,size=pause.size),
                   color="red3",shape=124,show.legend=FALSE)
    }
    if (nrow(stopdata.long)>0) {
      g <- g +
        geom_point(data=stopdata.long,
                   aes(y=yCenter,alpha=pause.alpha,size=pause.size),
                   color="green",shape=124,show.legend=FALSE)
    }
  }
  # axis labels
  axischarsize <- 3
  x <- seq(0,xmax,xincrement)
  xtext <- as.character(x)
  xhjust <- c(0,rep(0.5,length(x)-1))
  y <- yCenter + ifelse(underLine,
                        -0.8*height("label",heightFactor),
                        +0.8*height("label",heightFactor))
  axisdata <- data.frame(x=x,y=y,label=xtext,hjust=xhjust)
  g <- g +
    geom_text(data=axisdata,aes(x=x,y=y,label=label,hjust=hjust),
              size=axischarsize)
  # axis ticks
  x <- seq(0,xmax,mincrement)
  axisdata2 <- data.frame(x=x,y=yCenter)
  g <- g +
    geom_point(data=axisdata2,aes(x=x,y=y),size=1.2,color="black",
               shape=124,show.legend=FALSE)
  # axis title
  xAxisTextFrame <- data.frame(x=xmax/2,
                               y=yCenter +
                                 ifelse(underLine,
                                        -1.65*height("label",heightFactor),
                                        +1.65*height("label",heightFactor)),
                               label=ifelse(imperial,
                                            "Distance (mi)",
                                            "Distance (km)"))
  g <- g +
    geom_text(data=xAxisTextFrame,aes(x=x,y=y,label=label),
              size=1.10*axischarsize,hjust=0.5,
              color="black",show.legend = FALSE)

  ggpreturn[["g"]] <- g
  ggpreturn[["ymin"]] <- ymin
  return(ggpreturn)
}
drawXTConnect <- function(ggp,distance,walltime,segment,
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

  #  stops longer than 30 seconds
  stopConnectMin <- 30
  newseg <- segment != lag_one(segment)
  newseg[1] <- TRUE
  endseg <- segment != lead_one(segment)
  endseg[length(endseg)] <- TRUE
  segbegs <- walltime[newseg]
  segbegspot <- distance[newseg]
  segends <- walltime[endseg]
  segendspot <- distance[endseg]

  stopbegs <- segends[-length(segends)]
  stopends <- segbegs[-1]
  stopspots <- segbegspot[-1]
  lenstop <- stopends - stopbegs
  #stop begin time,stop end times, stop location
  if (sum(lenstop>stopConnectMin)>0) {
    starttimes <- stopends[lenstop>stopConnectMin]
    startplaces <- stopspots[lenstop>stopConnectMin]
    stoptimes <- c(stopbegs[lenstop>stopConnectMin],walltime[length(walltime)])
    stopplaces <- c(startplaces,distance[length(distance)])
    startcolors <- rep(20,length(starttimes))
    stopcolors <- rep(20,length(stoptimes))
    startalphas <- rep(1,length(starttimes))
    stopalphas <- rep(1,length(stoptimes))
    pointsStop <- data.frame(group=seq(1,length(stopplaces)),
                              x=stopplaces,
                              y=yXTConnect+(height("axis",heightFactor)/2))
    pointsTimeBeg <- data.frame(group=seq(1,length(stoptimes)),
                                x=xscale*stoptimes,
                                y=ymin-(height("axis",heightFactor)/2))
    pointsTimeEnd <- data.frame(group=seq(1,length(starttimes)),
                                x=xscale*starttimes,
                                y=ymin-(height("axis",heightFactor)/2))
    stopData <- rbind(pointsStop,pointsTimeBeg,pointsTimeEnd)
     g <- g +
      geom_polygon(data=stopData,
                   aes(x=x,y=y,group=group),fill="red3",alpha=0.3,
                   show.legend=FALSE)
     XTConnTextFrame <- data.frame(x=xmax/2,y=yCenter,label="Stops")
     g <- g +
       geom_text(data=XTConnTextFrame,aes(x=x,y=y,label=label),
                 size=3.3,hjust=0.5,vjust=0.5,
                 color="red",show.legend = FALSE)
  }
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
                              y=yXTConnect+(height("axis",heightFactor)/2),
                              yend=ymin-(height("axis",heightFactor)/2),
                              stringsAsFactors=FALSE)
  XTConnectData$xend <- xscale*XTConnectData$xend
  g <- g +
    geom_segment(data=XTConnectData,
                 aes(x=x,y=y,xend=xend,yend=yend,color=color,alpha=alpha),
                 show.legend = FALSE)

  ggpreturn[["g"]] <- g
  ggpreturn[["ymin"]] <- ymin
  return(ggpreturn)
}

