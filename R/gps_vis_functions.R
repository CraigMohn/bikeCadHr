continuous_bar <- function(g,legendtext,xvar,vals,lowval,hival,
                           lowcolor,hicolor,
                           legendwidth,y.bottom,
                           band.height,label.height,gap.height,
                           showlegend=TRUE) {

  #  map interval to color interval
  prtvalue <- lowcolor + (hicolor-lowcolor)*(vals-lowval)/(hival-lowval)
  prtvalue[!is.na(vals) & vals<lowval] <- lowcolor
  prtvalue[!is.na(vals) & vals>hival] <- hicolor
  prtvalue[vals==0] <- NA
  prtalpha <- (prtvalue-lowcolor)/(hicolor-lowcolor) #higher vals more intense
  prtalpha[prtalpha<0.3] <- 0.3

  y.band <- y.bottom + gap.height + (band.height/2)
  #columns fo rlegend - description,lowvalue,colorbar,hivalue
  width.legend <- c(5,2,4,2)
  column.legend <- c(0,cumsum(width.legend)[-4])*legendwidth
  if (showlegend) {
    xtext.legend <- c(0,column.legend[3],column.legend[3],column.legend[4])
    ytext.legend <- rep(y.band+(band.height/2)+gap.height+(label.height/2),4)
    alpha.legend <- c(1,1,0,1)
    hjust.legend <- c(0,1,0,0)
    legendlabels <- c(legendtext,paste0(lowval," "),"",paste0("  ",hival))
  } else {
    xtext.legend <- 0
    ytext.legend <- y.band+(band.height/2)+gap.height+(label.height/2)
    legendlabels <- legendtext
    alpha.legend <- 1
    hjust.legend <- 0
  }
  legendpos <- xvar>=column.legend[3] & xvar<=column.legend[4]
  prtlegend <- seq(lowcolor,hicolor,length.out=sum(legendpos))
  x.legend <- xvar[legendpos]
  y.legend <- rep(ytext.legend[1],length(x.legend))

  y.band <- rep(y.band,length(xvar))
  valsDataFrame <- data.frame(xvar,prtvalue,prtalpha,vals,y.band,band.height)
  valsLegendFrame <- data.frame(x.legend,y.legend,prtlegend,label.height)
  valsTextFrame <- data.frame(xtext.legend,ytext.legend,
                              legendlabels,alpha.legend,hjust.legend)
  if (showlegend) {
    g <- g +
      ggplot2::geom_tile(data=valsLegendFrame,
                         aes(y=y.legend,x=x.legend,fill=prtlegend,
                             color=prtlegend),
                         height=label.height,alpha=1,show.legend = FALSE)
  }
  g <- g +
#    ggplot2::geom_rect(xmin=0,xmax=max(xvar),
#                       ymin=y.bottom+gap.height,
#                       ymax=y.bottom+gap.height+band.height,fill="gray69") +
    ggplot2::geom_tile(data=valsDataFrame,
                       aes(y=y.band,x=xvar,fill=prtvalue,color=prtvalue),
                       alpha=0.6,
                       height=band.height,
                       show.legend = FALSE) +
    ggplot2::geom_text(data=valsTextFrame,
                       aes(x=xtext.legend,y=ytext.legend,label=legendlabels,
                           hjust=hjust.legend,alpha=alpha.legend),
                       size=2,color="black",fontface="italic",show.legend = FALSE)
  return(g)
}
discrete_bar <- function(g,legendtext,xvar,vals,lowval,hival,
                         lowcolor,midcolor,hicolor,
                         legendwidth,y.bottom,
                         band.height,label.height,gap.height,
                         showlegend=TRUE) {
  if (showlegend) {
    legendlabels <- c(legendtext,
                      paste0(" < ",lowval,"  "),
                      paste0(" ",lowval,"-",hival," "),
                      paste0(" >= ",hival," "))
    legendcolors <- c(NA,lowcolor,midcolor,hicolor)
  } else {
    legendlabels <- c(legendtext)
    legendcolors <- NA
  }
  prtvalue  <-  rep(NA,length(vals))
  prtvalue[!is.na(vals) & vals>0 & vals<lowval] <- lowcolor
  prtvalue[!is.na(vals) & vals>=lowval & vals<hival] <- midcolor
  prtvalue[!is.na(vals) & vals>=hival] <- hicolor


  y.band <- y.bottom + gap.height + (band.height/2)
  width.legend <- c(4,3,3,3)
  x1.legend <- c(0,cumsum(width.legend[1:3]))*legendwidth
  x2.legend <- cumsum(width.legend)*legendwidth
  y1.legend <- y.band+(band.height/2)+gap.height
  y2.legend <- y1.legend+label.height

  if (showlegend) {
    xtext.legend <- c(0,(x1.legend[2:4]+x2.legend[2:4])/2)
    ytext.legend <- rep(y.band+(band.height/2)+gap.height+(label.height/2),4)
    alpha.legend <- c(0,1,1,1)
    hjust.legend <- c(0,0.5,0.5,0.5)
  } else {
    xtext.legend <- 0
    ytext.legend <- y.band+(band.height/2)+gap.height+(label.height/2)
    legendlabels <- legendtext
    alpha.legend <- 1
    hjust.legend <- 0
  }
  valsDataFrame <- data.frame(xvar,y.band,vals,prtvalue,band.height)
  valsTextFrame <- data.frame(x1.legend,x2.legend,y1.legend,y2.legend,
                              xtext.legend,ytext.legend,legendlabels,
                              legendcolors,alpha.legend,hjust.legend)
  if (showlegend) {
      g <- g +
      ggplot2::geom_rect(data=valsTextFrame,
                         aes(xmin=x1.legend,xmax=x2.legend,fill=legendcolors,
                             alpha=alpha.legend),
                         ymin=y1.legend,ymax=y2.legend,inherit.aes=FALSE,
                         show.legend=FALSE)
  }
  g <- g +
    ggplot2::geom_tile(data=valsDataFrame,
                       aes(y=y.band,x=xvar,fill=prtvalue,color=prtvalue),
                       height=band.height,show.legend=FALSE)  +
    ggplot2::geom_text(data=valsTextFrame,
                       aes(x=xtext.legend,y=ytext.legend,label=legendlabels,
                       hjust=hjust.legend),size=2,fontface="italic")
  return(g)
}
# build tibble with segment data
#  segnum, begtime, endtime, stoptime, begdist, enddist
segSummary <- function(time,dist,segment,stopped,
                       stopDistTolerance,
                       stopRunLength,...) {

  if ( !is.numeric(segment) | !all(diff(segment)>=0) )
    stop("segment must be nondecreasing integers")
  if ( !is.numeric(dist) | !all(diff(dist)>=0) )
    stop("dist must be nondecreasing numeric")
  if ( !is.numeric(time) | !all(diff(time)>=0) )
    stop("time must be nondecreasing numeric")
  newseg <- c(TRUE,(diff(segment)>0))
  endseg <- c((diff(segment)>0),TRUE)
  stopdata <- tibble::as_tibble(list(time=time,
                                      dist=dist,
                                      segment=segment,
                                      stopped=stopped)) %>%
              dplyr::group_by(segment) %>%
              dplyr::mutate(maxdist=max(dist),
                            segbegtime=min(time),
                            segendtime=max(time)) %>%
              dplyr::mutate(timelaststop=lag_one(cumsum(stopped*time))) %>%
              dplyr::mutate(movingrun=((time-timelaststop) > stopRunLength) |
                                      ((time-segbegtime)  <= stopRunLength )) %>%
              dplyr::mutate(startofstop=!movingrun &
                                        lag_one(movingrun) &
                                        (maxdist-dist<stopDistTolerance) ) %>%
              dplyr::mutate(sosNA=ifelse(startofstop,1,NA)) %>%
              dplyr::summarize(timeStop=ifelse(sum(startofstop)==0,
                                               segendtime,
                                               min(sosNA*time,na.rm=TRUE)),
                               distStop=ifelse(sum(startofstop)==0,
                                               max(dist),
                                               min(sosNA*dist,na.rm=TRUE)))
  segSumFrame <- tibble::as_tibble(list(segment=segment[newseg],
                                        locBeg=dist[newseg],
                                        locEnd=stopdata$distStop,
                                        timeBeg=time[newseg],
                                        timeEnd=stopdata$timeStop))
  stopSumFrame <- tibble::as_tibble(list(stopNum=seq(1,nrow(segSumFrame)),
                                       locBeg=stopdata$distStop,
                                       locEnd=dist[endseg],
                                       timeBeg=stopdata$timeStop,
                                       timeEnd=c(segSumFrame$timeBeg[-1],max(time))))
  stopSumFrame <- stopSumFrame %>%
    dplyr::mutate(lenStop=timeEnd-timeBeg)
  return(list(segSumFrame=segSumFrame,stopSumFrame=stopSumFrame))
}

# wrapper for approx to leave between segment data missing after rescaling
approxSegments <- function(xvar,yvar,segment,npoints) {
  if (!is.vector(xvar) | !is.vector(yvar) | !is.vector(segment))
    stop("approxSegments needs 3 vectors")
  if (length(xvar) != length(yvar))
    stop("approxSegments needs equal length xvar and yvar")
  if (length(xvar) != length(segment))
    stop("approxSegments needs a segment for every x,y pair")

  xout <- seq(from=xvar[1], to=xvar[length(xvar)], length.out=npoints)
  yout <- rep(NA,npoints)

  #  xvar is the independent variable, and is increasing (as is segment)
  #  ignore middle points in sequence of xvar with the same value
  #  average the non missing values y at these points
  xdrop <- (xvar==lag_n(xvar,1)) & (xvar==lead_n(xvar,1))
  xvar <- xvar[!xdrop]
  yvar <- yvar[!xdrop]
  segment <- segment[!xdrop]

  #  how far apart are the x points
  xincr <- (xout[2] - xout[1])/2
  for (seg in unique(segment)) {
    if (sum(!is.na(yvar[segment==seg])) >= 2) {
      #  add repetitions of first/last values so y for those extremes will be
      #     interpolated near ends of segment
      pxv <- xvar[segment==seg]
      pyv <- yvar[segment==seg]
      pxv <- c( min(pxv)-xincr , pxv , max(pxv)+xincr )
      pyv <- c( pyv[1] , pyv , pyv[length(pyv)])
      yseg <- stats::approx(x=pxv,y=pyv,
                            xout=xout,method="linear",rule=1)[[2]]
      yout <- rowMeans(cbind(yseg,yout),na.rm=TRUE)
    }
  }
  return(list("xout"=xout,"yout"=yout))
}
#  return hr/cad legend width
hrCadLegendWidth <- function(npoints,distPerPoint,minNumPoints) {
  return( distPerPoint*min(npoints,2000)/13 )
}
# return the number of points on the x-axis for data
numPointsXAxis <- function(dist,ppm,imperial) {
  miles <- ifelse(imperial,dist,milesFromMeters(1000*dist))
  if (!is.na(ppm)&ppm>=10) {
    return(ceiling(ppm*miles))
  } else {
    distbends <- c(0,5,10,35,85,200,Inf)       # begin at 0, end at max distance
    pointsbends <- c(0,2200,3300,4500,6500,10000,10000) # begin at 0, end at max
    pointsbends <- c(0,800,1600,5600,13600,15000,15000) # begin at 0, end at max
    pointsbends <- c(0,2000,3000,6000,15000,18000,18000) # begin at 0, end at max
    return(ceiling(pointsbends[which(distbends>miles)[1]-1] +
                     ( (pointsbends[which(distbends>miles)[1]]-
                          pointsbends[which(distbends>miles)[1]-1])/
                         (distbends[which(distbends>miles)[1]]-
                            distbends[which(distbends>miles)[1]-1]) )*
                     (miles-distbends[which(distbends>miles)[1]-1])))
  }
}
#  return vertical scaling factor for profile
verticalMult <- function(dist,imperial) {
  miles <- ifelse(imperial,dist,milesFromMeters(1000*dist))
  distbends <- c(0,10,35,85,200,Inf) # begin at 0, end max distance
  vertbends <- c(15,18,24,32,40,50)  # begin at 25, end 50
  vm <-
    ceiling(vertbends[which(distbends>miles)[1]-1] +
                        ( (vertbends[which(distbends>miles)[1]]-
                           vertbends[which(distbends>miles)[1]-1]) /
                          (distbends[which(distbends>miles)[1]]-
                           distbends[which(distbends>miles)[1]-1]) ) *
                        (miles-distbends[which(distbends>miles)[1]-1]) )
  return(vm)
}
heightWith <- function(hrDistance,cadDistance,powerDistance,
                       hrTime,cadTime,powerTime,
                       headerTime,totalCall=FALSE,scale) {
  itemH <- heightItem(scale)
  headerH <- 2*height("axis",scale) + height("connector",scale)
  axisH <- height("axis",scale)
  return( ifelse((!headerTime)&totalCall,axisH,0) +
            ifelse(hrDistance,itemH,0) +
            ifelse(cadDistance,itemH,0) +
            ifelse(powerDistance,itemH,0) +
            ifelse(headerTime,headerH,0) +
            ifelse(hrTime,itemH,0) +
            ifelse(cadTime,itemH,0) +
            ifelse(powerTime,itemH,0)
  )
}
heightItem <- function(scale) {
  topGaps <- 1
  return(height("gap",scale) + height("band",scale) +
         height("gap",scale) +
         height("label",scale) + topGaps*height("gap",scale))
}
height <- function(what,scale) {
  if (what=="label") return(25/scale)
  else if (what=="band") return(45/scale)
  else if (what=="gap") return(3/scale)
  else if (what=="axis") return(190/scale)
  else if (what=="connector") return(1/scale)
  else if (what=="summary") return(180/scale)
  else return(NA)
}
milesFromMeters <- function(meters) {
  return(meters/1609.34)
}
kmFromMeters <- function(meters) {
  return(meters/1000)
}
feetFromMeters <- function(meters) {
  return(meters*3.28084)
}
