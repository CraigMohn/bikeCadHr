read_gpxtrack <- function(gpxfile)  {

  gpxtrack <- readGPXhrcad(paste0(gpxfile))

  ntrackpoints <- 0
  for (i in 1:length(gpxtrack)) {
      ntrackpoints <- ntrackpoints + sum(sapply(gpxtrack[[i]],nrow))
  }

  #  distance and speed are calculated from gps positions
  segment <- vector("numeric",ntrackpoints)
  timestamp.s <- vector("character",ntrackpoints)
  position_lat.dd <- vector("numeric",ntrackpoints)
  position_lon.dd <- vector("numeric",ntrackpoints)
  altitude.m <- vector("numeric",ntrackpoints)
  heart_rate.bpm <- vector("numeric",ntrackpoints)
  cadence.rpm <- vector("numeric",ntrackpoints)
  distance.m <- vector("numeric",ntrackpoints)
  speed.m.s <- vector("numeric",ntrackpoints)
  power.watts <- vector("numeric",ntrackpoints)
  power.watts <- NA

  nsegments <- 0
  distcum <- 0
  segbeg <- 1
  for (i in 1:length(gpxtrack)) {
    for (subtrack in gpxtrack[[i]]) {
      nwaypoints <- nrow(subtrack)
      segend <- segbeg + nwaypoints - 1
      nsegments <- nsegments + 1
    	tempdtime <- as.numeric(difftime(lubridate::ymd_hms(gsub("T"," ",subtrack$time)),
      	                               lag_one(lubridate::ymd_hms(gsub("T"," ",subtrack$time))),
      	                               units="secs"))
     	tempdspace <- raster::pointDistance(cbind(subtrack$lon,subtrack$lat),
      	                                  cbind(lag_one(subtrack$lon),
      	                                        lag_one(subtrack$lat)),lonlat=TRUE)
    	tempspeed <- (tempdspace/tempdtime)
      tempspeed[tempdtime<=0] <- tempdtime[tempdtime<=0]

      segment[segbeg:segend] <- rep(nsegments,nwaypoints)
      timestamp.s[segbeg:segend] <- subtrack$time
      position_lat.dd[segbeg:segend] <- subtrack$lat
      position_lon.dd[segbeg:segend] <- subtrack$lon
      altitude.m[segbeg:segend] <- as.numeric(subtrack$ele)
      cadence.rpm[segbeg:segend] <- subtrack$cad
      heart_rate.bpm[segbeg:segend] <- subtrack$hr
      distance.m[segbeg:segend] <- distcum + cumsum(tempdspace)
      speed.m.s[segbeg:segend] <- tempspeed
      # power.watts already initialized to NA

      distcum <- distance.m[segend]
      segbeg <- segend + 1
    }
  }
  timestamp.s <- as.POSIXct(strptime(timestamp.s,"%Y-%m-%dT%H:%M:%SZ",tz="UTC"))
    #arrange by timestamp
    track <- data_frame(segment,timestamp.s,
                        position_lat.dd,position_lon.dd,altitude.m,
                        cadence.rpm,heart_rate.bpm,distance.m,speed.m.s,power.watts)
    return(list(track=track,recovery_hr=NULL,session=NULL))
}

readGPXhrcad <- function(gpx.file) {
    #   modified version of the readGPX routine from package plotKML, written
    #        and maintained by Tomislav Hengl, et al available on CRAN
    #
    #        This modification gathers hr and cadence values from XML tree instead
    #        collecting all extension values into a single string.  My experience
    #        with XML is near-zero, and the code is very slow, because I could
    #        not find vectorized XML functions which return NA when a leaf
    #        is not present, rather than just returning a shortened list.  gpx XML
    #        is not a natural format for recording multiple simultaneous data streams
    #        with intermittent data loss such as loss of GPS signal, or loss of contact
    #        with the heartrate or cadence transmitters
    #
    #        for readability, xml:: prefix before functions from package xml
    #        are omitted
    element <- "trk"
    ## gives a internal C-level document pointer - class=XMLInternalDocument
    trackret <- xmlTreeParse(gpx.file, useInternalNodes = TRUE)
    # top structure ##R# as a XMLNode object:
    top <- xmlRoot(trackret)

    # check if there is any content: ##R# XML tag name of each of the sub nodes
    # of a given XMLNode object
    trackret <- NULL
    if(any(grep(element, names(top)))) {
      if(element=="trk"){
        trackret <- NULL
        nu <- which(names(top) %in% element)
        for(c in seq_along(nu)){
          lst <- which(names(top[[nu[c]]]) %in% "trkseg")
          nm <- NULL
          for(i in seq_along(lst)) {
            nm <- union(nm,names(top[[nu[c]]][[lst[i]]][[1]]))
          }
          #nm <- names(top[[nu[c]]][[lst[1]]][[1]])
          trackret[[c]] <- list(NULL)
          for(i in seq_along(lst)) {
            trkpt <- top[[nu[c]]][[lst[i]]]

           if (FALSE) {
            trn <- newXMLDoc()
            addChildren(trn, newXMLNode("data"))
            for(x in getNodeSet(trkpt, "//*[contains(name(),'trkpt')]")) {
              row<-newXMLNode("row")
              for( z in getNodeSet(x, ".//*[not(*)]")) {
                li <- newXMLNode(xmlGetAttr(z, "hr", xmlName(z)))
                addChildren(li, newXMLTextNode(xmlGetAttr(z, "value",NA)))
                addChildren(row, li)
                li <- newXMLNode(xmlGetAttr(z, "cad", xmlName(z)))
                addChildren(li, newXMLTextNode(xmlGetAttr(z, "value",NA)))
                addChildren(row, li)
              }
              addChildren(xmlRoot(trn), row)
            }

            trackret[[c]][[i]] <- xmlToDataFrame(trn)
          } else {
            trackret[[c]][[i]] <- data.frame(NULL)
            ## get columns (http://www.topografix.com/GPX/1/1/#type_wptType)
            lon <- as.numeric(xmlSApply(trkpt, xmlGetAttr, "lon"))
            lat <- as.numeric(xmlSApply(trkpt, xmlGetAttr, "lat"))
            trackret[[c]][[i]][1:length(lon),"lon"] <- lon
            trackret[[c]][[i]][1:length(lat),"lat"] <- lat
            trackret[[c]][[i]][1:length(lat),"hr"] <- as.numeric(NA)
            trackret[[c]][[i]][1:length(lat),"cad"] <- as.numeric(NA)
            if(!nm[[1]]=="NULL"){
              for(j in 1:length(nm)){
                xm <- as.character(sapply(sapply(xmlChildren(trkpt), function(x) x[[nm[[j]]]]), xmlValue))
                trackret[[c]][[i]][1:length(xm), nm[[j]]] <- xm
                if (nm[[j]]=="extensions"){
                  #hr <- lat
                  #hr[] <- NA
                  #cad <- hr
                  #hrleaf <- sapply(seq(1:length(lat)), function(x) xmlElementsByTagName(trkpt[[x]],"hr",recursive=TRUE)[[1]] )
                  #hrok <- sapply(seq(1:length(lat)), function(x) length(hrleaf[[x]])>0)
                  #  the following recursive indexig fails
                  #hr[hrok] <- as.integer(xmlGetAttr(hrleaf[[hrok]],default=NA))
                  #cadleaf <- sapply(seq(1:length(lat)), function(x) xmlElementsByTagName(trkpt[[x]],"cad",recursive=TRUE)[[1]] )
                  #cadok <- sapply(seq(1:length(lat)), function(x) length(cadleaf[[x]])>0)
                  #cad[cadok] <- as.integer(xmlValue(cadleaf[[cadok]]))
                  #cad[cadok] <- as.integer(xmlGetAttr(cadleaf[[cadok]],default=NA))
                  for (tpt in 1:length(lat)) {
                    hr <-xmlElementsByTagName(trkpt[[tpt]],"hr",recursive=TRUE)
                    if (length(hr)>0) trackret[[c]][[i]][tpt,"hr"]  <- as.integer(xmlValue(hr[[1]]))
                    cad <- xmlElementsByTagName(trkpt[[tpt]],"cad",recursive=TRUE)
                    if (length(cad)>0) trackret[[c]][[i]][tpt,"cad"]<- as.integer(xmlValue(cad[[1]]))
                  }
                }
              }
            }
            names(trackret[[c]]) <- xmlValue(top[[nu[c]]][["name"]])
           }
          }
        }
      }
    }
    return(trackret)
}
