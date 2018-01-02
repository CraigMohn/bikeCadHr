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
      geom_tile(data=valsLegendFrame,
                aes(y=y.legend,x=x.legend,fill=prtlegend,
                    color=prtlegend),
                height=label.height,alpha=1,show.legend = FALSE)
  }
  g <- g +
    geom_tile(data=valsDataFrame,
              aes(y=y.band,x=xvar,fill=prtvalue,color=prtvalue,
                  alpha=prtalpha),
              height=band.height,show.legend = FALSE) +
    geom_text(data=valsTextFrame,
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
      geom_rect(data=valsTextFrame,
                aes(xmin=x1.legend,xmax=x2.legend,fill=legendcolors,
                    alpha=alpha.legend),
                ymin=y1.legend,ymax=y2.legend,inherit.aes=FALSE,
                show.legend=FALSE)
  }
  g <- g +
    geom_tile(data=valsDataFrame,
              aes(y=y.band,x=xvar,fill=prtvalue,color=prtvalue),
              height=band.height,show.legend=FALSE)  +
    geom_text(data=valsTextFrame,
              aes(x=xtext.legend,y=ytext.legend,label=legendlabels,
              hjust=hjust.legend),size=2,fontface="italic")
  return(g)
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
    pointsbends <- c(0,2000,3000,4500,6500,10000,10000) # begin at 0, end at max
    pointsbends <- c(0,800,1600,5600,13600,15000,15000) # begin at 0, end at max
    pointsbends <- c(0,2000,3000,6000,15000,15000,15000) # begin at 0, end at max
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
  vertbends <- c(25,25,30,40,40,40)  # begin at 25, end 50
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
  if (what=="label") return(40/scale)
  else if (what=="band") return(75/scale)
  else if (what=="gap") return(9/scale)
  else if (what=="axis") return(180/scale)
  else if (what=="connector") return(10/scale)
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
