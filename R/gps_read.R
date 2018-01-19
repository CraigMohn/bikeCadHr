#' read multiple gps data files
#'
#' \code{read_ridefiles} reads a vector of gps .fit and/or .gpx track files
#'  and put into summary and detail data tibbles for modeling or graphical
#'  summary.  A wrapper for \code{\link{read_ride}}.
#'
#'
#' @param ridefilevec a vector of filenames to process
#' @param cores number of cores (default is #CPUs - 1)
#' @param loud display summary of re/segmenting actions
#' @param ... parameters passed for track cleaning
#'
#' @return a list of two data tibbles:  \eqn{summaries} and \eqn{tracks}
#'    These are linked by integer fields \eqn{startbutton.date}
#'    and \eqn{startbutton.time}
#'
#' @seealso \code{\link{read_ride}}
#'
#' @export
read_ridefiles <- function(ridefilevec,cores=4,loud=loud,...)  {

  nfiles <- length(ridefilevec)
  if(missing(cores)) cores <- parallel::detectCores()
  if ((nfiles > 10) & !is.na(cores) & (cores>1)) {
    doParallel::registerDoParallel(cores)
    `%dopar%` <- foreach::`%dopar%`
    cfun <- function(a,b) list(summary=dplyr::bind_rows(a[["summary"]],b[["summary"]]),
                               trackpoints=dplyr::bind_rows(a[["trackpoints"]],b[["trackpoints"]]))
    ridelist <- foreach (x = ridefilevec,.combine=`cfun`,
                        .packages=c("bikeCadHr")) %dopar% {
      read_ride(x,loud=loud,...)
    }
    doParallel::stopImplicitCluster()
    return(list(summaries=dplyr::arrange(ridelist[["summary"]],date,start.hour),
                tracks=dplyr::arrange(ridelist[["trackpoints"]],startbutton.date,timestamp.s)))
  } else {
    outdf <- NULL
    outtracks <- NULL
    for (x in ridefilevec) {
      ride <- read_ride(x,loud=loud,...)
      obsdf <- ride[["summary"]]
      obstrack <- ride[["trackpoints"]]
      if (is.null(outdf)) {
        outdf <- obsdf
      } else {
        outdf <- dplyr::bind_rows(outdf,obsdf)
      }
      if (is.null(outtracks)) {
        outtracks <- obstrack
      } else {
        outtracks <- dplyr::bind_rows(outtracks,obstrack)
      }
    }
    return(list(summaries=dplyr::arrange(outdf,date,start.hour),
                tracks=dplyr::arrange(outtracks,startbutton.date,timestamp.s)))
  }
}
#' read and clean a gps data file
#'
#' \code{read_ride}  processes a gps .fit and/or .gpx track file to create
#'   summary and detail data tibbles for modeling or graphical summary.  Track
#'   segments are adjusted to remove many false autostop-start and startline
#'   delay sequences.  Several measures of cadence are calculated (including
#'   zeros, excluding zeros, and midsegment),spurious and missing cadence values
#'   are repaired, and summary stats are calculated for analysis.
#'
#' @param ridefile a filenames (.fit or .gpx extension) to process
#' @param tz string contaiing timezone for converting from UTC
#' @param fixDistance repair nonmonotonicities in distance which are on
#'    segment breaks - this occurs when power is lost or on some device lockups
#' @param loud print information about re/segmenting track data
#' @param ... parameters for \code{\link{processSegments}},
#'    \code{\link{statsCadence}},
#'    \code{\link{statsPower}},
#'    \code{\link{statsHeartRate}},
#'    \code{\link{statsGearing}},
#'    \code{\link{statsGrade}},
#'    \code{\link{statsSession}},
#'    \code{\link{statsStops}}
#'
#' @return a list of three data tibbles:  \eqn{summary} and \eqn{trackpoints}
#'    and \eqn{session}
#'    These are linked by integer fields \eqn{startbutton.date}
#'    and \eqn{startbutton.time}
#'
#' @seealso \code{\link{read_ridefiles}},
#'    \code{\link{processSegments}},
#'    \code{\link{statsCadence}},
#'    \code{\link{statsPower}},
#'    \code{\link{statsHeartRate}},
#'    \code{\link{statsGearing}},
#'    \code{\link{statsGrade}},
#'    \code{\link{statsSession}},
#'    \code{\link{statsStops}}
#'
#' @export
read_ride <- function(ridefile,tz="America/Los_Angeles",
                      fixDistance=FALSE,loud=FALSE,...)  {

  cat("reading: ",ridefile,"\n")
  if (missing(tz)) {
    tz <- Sys.timezone()
  }
  if (substr(ridefile,nchar(ridefile)-3,nchar(ridefile))==".fit") {
    temp <- read_fittrack(ridefile)
    time.fn.string <- basename(ridefile)
    fit.fn.time.parse <- getOption("bCadHr.fit.fn.time.parse")
    fit.fn.lead <- getOption("bCadHr.fit.fn.lead")
    fit.fn.trail <- getOption("bCadHr.fit.fn.trail")
    if (nchar(fit.fn.lead)>0) time.fn.string <-
      substr(time.fn.string,
             regexpr(fit.fn.lead,time.fn.string)+nchar(fit.fn.lead)+1,1000000)
    if (nchar(fit.fn.trail)>0) time.fn.string <-
      substr(time.fn.string,1,regexpr(fit.fn.trail,time.fn.string))
    time.turned.on <- strptime(time.fn.string,fit.fn.time.parse,tz=tz)
  } else if (substr(ridefile,nchar(ridefile)-3,nchar(ridefile))==".gpx") {
    temp <- read_gpxtrack(ridefile)
    time.fn.string <- basename(ridefile)
    gpx.fn.time.parse <- getOption("bCadHr.gpx.fn.time.parse")
    gpx.fn.lead <- getOption("bCadHr.gpx.fn.lead")
    gpx.fn.trail <- getOption("bCadHr.gpx.fn.trail")
    if (nchar(gpx.fn.lead)>0) time.fn.string <-
      substr(time.fn.string,
             regexpr(gpx.fn.lead,time.fn.string)+nchar(fit.fn.lead)+1,1000000)
    if (nchar(gpx.fn.trail)>0) time.fn.string <-
      substr(time.fn.string,1,regexpr(gpx.fn.trail,time.fn.string))
    time.turned.on <- strptime(time.fn.string,gpx.fn.time.parse,tz=tz)
  } else {
    stop("unknown file extension")
  }
  trackdata <- temp[["track"]]
  recovery_hr <- temp[["recovery_hr"]]
  session <- temp[["session"]]

  attr(trackdata$timestamp.s,"tzone") <- tz
  if (is.na(time.turned.on)){
    #  if filename not successfully turned into a start-button time, use 1st
    #   timestamp value recorded in the track.  Likely but not certain to match
    #   between different versions of the same ride
    time.turned.on <- trackdata$timestamp.s[1]
  }
  startbuttonDate <- as.integer(lubridate::mday(time.turned.on)+
                                   100*lubridate::month(time.turned.on)+
                                   10000*lubridate::year(time.turned.on))
  startbuttonTime <- as.integer(lubridate::second(time.turned.on)+
                                   100*lubridate::minute(time.turned.on)+
                                   10000*lubridate::hour(time.turned.on))
  trackdata$startbutton.date <- startbuttonDate
  trackdata$startbutton.time <- startbuttonTime
  ###   minimal data validity checks after dumping unusable obs
  trackdata <- stripDupTrackRecords(trackdata,fixDistance=fixDistance)
  if (is.unsorted(trackdata$segment,strictly=FALSE))
    stop(paste0(ridefile," yields segment ids that are not nondecreasing!"))
  if (is.unsorted(trackdata$distance.m,strictly=FALSE))
    stop(paste0(ridefile," yields distances that are not nondecreasing!"))
  if (is.unsorted(trackdata$timestamp.s,strictly=TRUE))
    stop(paste0(ridefile," yields timestamps that are
                            not strictly increasing!"))

  #  split/revise track into segments separated by non-negligile stops
  trackdata <- processSegments(trackdf=trackdata,loud=loud,...)
  cadStats <- statsCadence(trackdf=trackdata,...)
  powStats <- statsPower(trackdf=trackdata,...)
  hrStats <- statsHeartRate(trackdf=trackdata,recovery_hr=recovery_hr,...)
  gearStats <- statsGearing(trackdf=trackdata,...)
  climbStats <- statsGrade(trackdf=trackdata,...)
  sessionStats <- statsSession(session)
  stopsStats <- statsStops(trackdf=trackdata,...)

  if (!is.na(trackdata$position_lon.dd[1])){
    begEndGap <- raster::pointDistance(cbind(trackdata$position_lon.dd[1],
                                             trackdata$position_lat.dd[1]),
                                       cbind(trackdata$position_lon.dd[nrow(trackdata)],
                                             trackdata$position_lat.dd[nrow(trackdata)]),
                                       lonlat=TRUE)
    if (begEndGap>100) {
      cat("  *non-loop - distance between start and end = ",begEndGap,"m \n")
      cat("   start = ",trackdata$position_lon.dd[1],"  ",
          trackdata$position_lat.dd[1],"  \n")
      cat("   stop  = ",trackdata$position_lon.dd[nrow(trackdata)],"  ",
          trackdata$position_lat.dd[nrow(trackdata)],"\n")
      rideLoop <- FALSE
    } else {
      rideLoop <- TRUE
    }
  } else {
    begEndGap <- NA
    rideLoop <- NA
  }

  rideDate <- as.Date(format(trackdata$timestamp.s[1],tz=tz))
  startTime <- as.character(trackdata$timestamp.s[1])
  startHour <- as.numeric(lubridate::hour(format(time.turned.on,tz=tz))
           + 0.25*(round((60*lubridate::minute(format(time.turned.on,tz=tz))+
                             lubridate::second(format(time.turned.on,tz=tz)))/
                           (60*15))))
   track.cleaned <-
    tibble::data_frame(date = rideDate,
                       start.time = startTime,
                       start.hour = startHour,
                       nwaypoints = nrow(trackdata),
                       numsegs = max(trackdata$segment),
                       pct.trkpts.hr = sum(!is.na(trackdata$heart_rate.bpm))/
                                         nrow(trackdata),
                       pct.trkpts.cad = sum(!is.na(trackdata$cadence.rpm))/
                                          nrow(trackdata),
                       begEndGap = begEndGap,
                       deltaElev = trackdata$altitude.m[nrow(trackdata)] -
                                   trackdata$altitude.m[1],
                       ride.loop = rideLoop,
                       distance = trackdata$distance.m[nrow(trackdata)],
                       total.time = totalTime(trackdata),
                       rolling.time = rollingTime(trackdata),
                       pedal.time = pedalTime(trackdata),
                       speed.rolling.m.s = trackdata$distance.m[nrow(trackdata)]/
                                              rollingTime(trackdata),
                       speed.all.m.s = trackdata$distance.m[nrow(trackdata)]/
                                              totalTime(trackdata),
                       speed.max.m.s = max(trackdata$speed.m.s,na.rm=TRUE),
                       avgcadence.nozeros = cadStats[["avgcadenceNoZeros"]],
                       avgcadence.withzeros = cadStats[["avgcadenceWithZeros"]],
                       avgcadence.midsegment = cadStats[["avgcadenceMidsegment"]],
                       avgpower.nozeros=powStats[["avgpowerNoZeros"]],
                       avgpower.withzeros=powStats[["avgpowerWithZeros"]],
                       avgpower.postcal.nozeros=powStats[["avgpowerPostCalNoZeros"]],
                       avgpower.postcal.rolling=powStats[["avgpowerPostCalRolling"]],
                       ascent = climbStats[["ascent"]],
                       descent = climbStats[["descent"]],
                       distance.ascending = climbStats[["distanceAscending"]],
                       distance.descending = climbStats[["distanceDescending"]],
                       pct.low.gear = gearStats[["pctLowGear1"]],
                       low.gear = gearStats[["lowGear1"]],
                       pct.low.gear2 = gearStats[["pctLowGear2"]],
                       low.gear2 = gearStats[["lowGear2"]],
                       hr.at.stop = hrStats[["hrAtStop"]],
                       hr.recovery = hrStats[["hrRecovery"]],
                       startline.time = stopsStats[["startlineTime"]],
                       stops.subminute = stopsStats[["stopsSubMinute"]],
                       stops.1to10minutes = stopsStats[["stops1to10Minutes"]],
                       stops.10to30minutes = stopsStats[["stops10to30Minutes"]],
                       stops.long = stopsStats[["stopsLong"]],
                       session.distance = sessionStats[["sessionDistance"]],
                       session.elapsed.time = sessionStats[["sessionElapsedTime"]],
                       session.timer.time = sessionStats[["sessionTimerTime"]],
                       session.pedal.strokes = sessionStats[["sessionPedalStrokes"]],
                       session.total.calories = sessionStats[["sessionTotalCalories"]],
                       session.avg.speed = sessionStats[["sessionAvgSpeed"]],
                       session.max.speed = sessionStats[["sessionMaxSpeed"]],
                       session.total.ascent = sessionStats[["sessionTotalAscent"]],
                       session.total.descent = sessionStats[["sessionTotalDescent"]],
                       session.avg.cadence = sessionStats[["sessionAvgCadence"]],
                       session.avg.hr = sessionStats[["sessionAvgHr"]],
                       session.max.hr = sessionStats[["sessionMaxHr"]],
                       session.avg.power = sessionStats[["sessionAvgPower"]],
                       session.max.power = sessionStats[["sessionMaxPower"]],
                       sourcefile = basename(ridefile),
                       processed.time = Sys.time(),
                       startbutton.date=startbuttonDate,
                       startbutton.time=startbuttonTime)
  return(list(summary=track.cleaned,trackpoints=trackdata,session=session))
}
#' clean up and add start/stop segments to a track tibble, add flag for stopped
#'
#' \code{processSegments}  processes a gps track file to correct or add
#'    start/stop segment information like that generated by gps autopause.
#'    The sequence of processing is: eliminate stops that are too short,
#'    split segments where time between observations is too large, split
#'    segments where stopped for longer than threshold, then merge segments
#'    which are too small.  Finally, all segment breaks too close to the
#'    start line are removed
#'
#' @param trackdf data frame or tibble with gps track data
#' @param segBreakTimeMin (seconds) segments separated by less time
#'    than this are joined
#' @param nonsegTimeGapMax (seconds) segemnts are split between datapoints
#'    separated by more time than this
#' @param segSplitTimeStop segments are split at points where
#' @param segMinObs segments smaller than this are joined to closest
#'    neighboring segment
#' @param segMinMeters segments shorter than this are joined with predecessor
#' @param segMinSecs (seconds) segments shorter than this are joined
#' @param segInitIdleAggSecs stops/restarts before this time has
#'    elapsed are discarded and the ride starts at the last start prior to this
#' @param segInitIdleAggMeters stops/restarts within this distance
#'    of the initial start are discarded as above
#' @param ignoreSegInfo wipe existing segment information and use
#'    \code{nonsegTimeGapMax} and \code{segSplitTimeStop} to
#'    reconstruct segments before merging short ones
#' @param loud display summary of actions
#' @param ... parameters for
#'    \code{\link{statsCadence}},
#'    \code{\link{statsPower}},
#'    \code{\link{statsHeartRate}},
#'    \code{\link{statsGearing}},
#'    \code{\link{statsGrade}},
#'    \code{\link{statsSession}},
#'    \code{\link{statsStops}}
#'
#' @return a tibble containing track data with start/stop segment data
#'
#' @seealso \code{\link{read_ride}},
#'    \code{\link{statsCadence}},
#'    \code{\link{statsPower}},
#'    \code{\link{statsHeartRate}},
#'    \code{\link{statsGearing}},
#'    \code{\link{statsGrade}},
#'    \code{\link{statsSession}},
#'    \code{\link{statsStops}}
#'
#' @export
processSegments <- function(trackdf,
                            segInitIdleAggSecs=60,
                            segInitIdleAggMeters=20,
                            segBreakTimeMin=3,
                            nonsegTimeGapMax=30,
                            segSplitTimeStop=3,
                            segMinObs=4,segMinMeters=10,segMinSecs=10,
                            ignoreSegInfo=FALSE,loud=FALSE,...) {

  #  demand some small amount of consistency
  segSplitTimeStop <- max(segSplitTimeStop,segBreakTimeMin)

  #   no observations will be deleted or added, so set up some useful stuff
  #
  deltatime <- as.numeric(difftime(trackdf$timestamp.s,
                                   lag_one(trackdf$timestamp.s),units="secs"),
                          units="secs")
  timesecs <- cumsum(deltatime)
  stopped <- trackdf$speed.m.s==0
  stopped[length(stopped)] <-TRUE
  starting <- !stopped & lag_one(stopped)
  starting[1] <- TRUE

  ###   now process segment and stop data
  #  wipe existing segment data from gps (auto)start/stop if requested
  #  otherwise make it a sequence of consecutive nondecreasing integers
  if (loud) {
    newseg <- trackdf$segment != lag_one(trackdf$segment)
    newseg[1] <- TRUE
    cat("starting out with ",1+sum(trackdf$segment!=lag_one(trackdf$segment)),
        " segments\n")
  }
  if (ignoreSegInfo) {
    if (loud) cat("wiping out existing segment data\n")
    trackdf$segment <- 1
  }
  newseg <- trackdf$segment != lag_one(trackdf$segment)
  newseg[1] <- TRUE
  # split really, really long intervals between points
  needsplit <- (deltatime > nonsegTimeGapMax) & !newseg
  if (sum(needsplit) > 0) {
    if (loud) {
      cat("splitting ",sum(needsplit)," long intervals in same segment\n")
      # print(trackdf$timestamp.s[needsplit])
    }
    newseg <- newseg | needsplit
  }
  # join segment breaks which are too short
  needjoin <- (deltatime < segBreakTimeMin) & newseg
  needjoin[1] <- FALSE
  if (sum(needjoin) > 0) {
    if (loud) {
      cat("joining ",sum(needjoin)," short breaks between segments\n")
      #print(trackdf$timestamp.s[needjoin])
    }
    newseg <- newseg & !needjoin
  }
  # split if time since last motion is too large
  lastmovedtime <- cummax(ifelse(stopped&!newseg,
                                 0,
                                 timesecs))
  stoptoolong <- c(FALSE,diff(lastmovedtime) >= segSplitTimeStop) &
                 !stopped &
                 lag_one(stopped)

  needsplit <- stoptoolong & !newseg
  if (sum(needsplit) > 0) {
    if (loud) {
      cat("splitting ",sum(needsplit)," segments with long stops\n")
      #print(trackdf$timestamp.s[needsplit])
    }
    newseg <- newseg | needsplit
  }

  # collapse too-short segments together  join them with previous segment
  #     (stops and start/stops occur at end of segment)
  tempdf <-  trackdf %>% dplyr::select(timestamp.s,distance.m)
  newseg[1] <- TRUE  # do this before regenerate segment variable
  tempdf$segment <- cumsum(newseg)
  #  stack the last obs on bottom of newseg for diff'ing
  segobs <- newseg
  segobs[length(segobs)] <- TRUE
  segdf <- tempdf[segobs,]
  segmeters <- diff(segdf$distance.m)
  segsecs <- difftime(segdf$timestamp.s,
                      lag_one(segdf$timestamp.s),units="secs")[-1]
  segdf <- segdf[-nrow(segdf),]
  segdf$segmeters <- segmeters
  segdf$segsecs <- segsecs

  tempdf <- tempdf %>%
    dplyr::group_by(segment) %>%
    dplyr::mutate(nobsinseg=n()) %>%
    dplyr::left_join(segdf,by="timestamp.s")
  tempdf[!newseg,c("segmeters","segsecs")] <- 0
  needjoin <- (tempdf$nobsinseg < segMinObs) & newseg
  needjoin[1] <- FALSE
  if (sum(needjoin) > 0) {
    if (loud) {
      cat("joining ",sum(needjoin)," segments with fewer than ",
          segMinObs," obs\n")
      #print(trackdf$timestamp.s[needjoin])
    }
    newseg <- newseg & !needjoin
  }
  needjoin <- (tempdf$segmeters < segMinMeters) & newseg
  needjoin[1] <- FALSE
  if (sum(needjoin) > 0) {
    if (loud) {
      cat("joining ",sum(needjoin)," segments shorter than ",
          segMinMeters," meters\n")
      #print(trackdf$timestamp.s[needjoin])
    }
    newseg <- newseg & !needjoin
  }
  needjoin <- (tempdf$segsecs < segMinSecs) & newseg
  needjoin[1] <- FALSE
  if (sum(needjoin) > 0) {
    if (loud) {
      cat("joining ",sum(needjoin)," segments shorter than ",
          segMinSecs," seconds\n")
      #print(trackdf$timestamp.s[needjoin])
    }
    newseg <- newseg & !needjoin
  }

  ###  now allow the first segment to include starts/stop issues at beginning
  insidefirstseg <- (cumsum(deltatime) <= segInitIdleAggSecs)  |
    (trackdf$distance.m <= segInitIdleAggMeters)
  needjoin <- insidefirstseg & newseg
  needjoin[1] <- FALSE
  if (sum(needjoin) > 0) {
    if (loud) {
      cat("joining ",sum(needjoin)," segments at start\n")
      #print(trackdf$timestamp.s[needjoin])
    }
    newseg <- newseg & !needjoin
  }

  #  these are only modifications to the track data tibble
  newseg[1] <- TRUE
  trackdf$segment <- cumsum(newseg)
  trackdf$stopped <- stopped
  trackdf$deltatime <- deltatime

  return(trackdf)
}
#' clean up and summarize cadence statistics for a track
#'
#' \code{statsCadence}  processes a gps track file to correct and
#'   summarize cadence data
#'
#'
#' @param trackdf data frame or tibble with gps track data
#' @param cadTrimBegSecs number of seconds after the beginning of a
#'    track segment that are ignored in calculating midsegment avg cadence
#' @param cadTrimEndSecs number of seconds before the end of a
#'    track segment that are ignored in calculating midsegment avg cadence
#' @param cadTrimBegMeters number of meters after the beginning of a
#'    track segment that are ignored in calculating midsegment avg cadence
#' @param cadTrimEndMeters number of meters before the end of a
#'    track segment that are ignored in calculating midsegment avg cadence
#' @param cadMax max credible cadence value, larger values are errors
#' @param cadCorrectTooHigh repair excessive cadence values
#'    using triangular-kernel-weighted average of the nearest nonmissing values
#'    in the same segment
#' @param cadCorrectNA repair cadence missing values using
#'    triangular-kernel-weighted average of the nearest nonmissing values in
#'    the same segment
#' @param cadCorrectWindowSec window size for kernel smoothing of cadence
#' @param ... parameters for \code{\link{processSegments}},
#'    \code{\link{statsPower}},
#'    \code{\link{statsHeartRate}},
#'    \code{\link{statsGearing}},
#'    \code{\link{statsGrade}},
#'    \code{\link{statsSession}},
#'    \code{\link{statsStops}}
#'
#' @return a list containing summary data
#'
#' @seealso \code{\link{read_ride}},\code{\link{processSegments}},
#'    \code{\link{statsPower}},
#'    \code{\link{statsHeartRate}},
#'    \code{\link{statsGearing}},
#'    \code{\link{statsGrade}},
#'    \code{\link{statsSession}},
#'    \code{\link{statsStops}}
#'
#' @export
statsCadence <- function(trackdf,
                         cadTrimBegSecs=15,cadTrimBegMeters=10,
                         cadTrimEndSecs=20,cadTrimEndMeters=15,
                         cadMax=160,cadCorrectTooHigh=TRUE,
                         cadCorrectNA=FALSE,
                         cadCorrectWindowSec=7,...) {

  ## patch cadence errors and then NAs
  cadenceTooHigh <- trackdf$cadence.rpm > cadMax
  cadenceTooHigh[is.na(cadenceTooHigh)] <- FALSE
  if (sum(cadenceTooHigh) > 0) {
    if (cadCorrectTooHigh) {
      trackdf$cadence.rpm[cadenceTooHigh] <- NA
      cadenceSmoothed <- smoothDataSegments(yvec=trackdf$cadence.rpm,
                                           xvar=cumsum(trackdf$deltatime),
                                           segment=trackdf$segment,
                                           bw=cadCorrectWindowSec,
                                           nneighbors=cadCorrectWindowSec,
                                           kernel="triangular")
      trackdf$cadence.rpm[cadCorrectTooHigh] <-
        cadenceSmoothed[cadCorrectTooHigh]
    } else {
      trackdf$cadence.rpm[cadCorrectTooHigh] <- NA
    }
  }
  if (cadCorrectNA) {
    cadenceNA <- is.na(trackdf$cadence.rpm)
    if (sum(cadenceNA) > 0) {
      cadenceSmoothed <- smoothDataSegments(yvec=trackdf$cadence.rpm,
                                            xvar=cumsum(trackdf$deltatime),
                                            segment=trackdf$segment,
                                            bw=cadCorrectWindowSec,
                                            nneighbors=cadCorrectWindowSec,
                                            kernel="triangular")
      trackdf$cadence.rpm[cadenceNA] <- cadenceSmoothed[cadenceNA]
    }
  }
  pedaling <- !is.na(trackdf$cadence.rpm) & trackdf$cadence.rpm > 0
  segtimes <- tibble::data_frame(timestamp=cumsum(trackdf$deltatime),
                                 distance.m=trackdf$distance.m,
                                 segment=trackdf$segment)    %>%
    dplyr::group_by(segment) %>%
    dplyr::mutate(segbegtime=min(timestamp),
                  segendtime=max(timestamp),
                  segbegdist=min(distance.m),
                  segenddist=max(distance.m)) %>%
    dplyr::arrange(timestamp)

  innersegment <- segtimes$timestamp >= segtimes$segbegtime+cadTrimBegSecs &
    segtimes$timestamp <= segtimes$segendtime-cadTrimEndSecs &
    segtimes$distance.m >= segtimes$segbegdist+cadTrimBegMeters &
    segtimes$distance.m <= segtimes$segenddist-cadTrimBegMeters

  cadenceNumerator <- sum(trackdf$deltatime[pedaling]*
                            trackdf$cadence.rpm[pedaling])
  cadenceNumeratorInner <- sum(trackdf$deltatime[pedaling&innersegment]*
                                 trackdf$cadence.rpm[pedaling&innersegment])

  avgcadenceNoZeros <- cadenceNumerator / pedalTime(trackdf)
  avgcadenceWithZeros <- cadenceNumerator / rollingTime(trackdf)
  avgcadenceMidsegment <- cadenceNumeratorInner /
    sum(trackdf$deltatime[pedaling & innersegment])

  return(list(avgcadenceNoZeros=avgcadenceNoZeros,
              avgcadenceWithZeros=avgcadenceWithZeros,
              avgcadenceMidsegment=avgcadenceMidsegment))
}
#' generate power statistics for a track
#'
#' \code{statsPower}  processes a gps track file to summarize the power data
#'
#' @param trackdf data frame or tibble with gps track data
#' @param powerCalibrateTime number of seconds to ignore in avgpowerPostCal
#' @param ... parameters for \code{\link{processSegments}},
#'    \code{\link{statsCadence}},
#'    \code{\link{statsHeartRate}},
#'    \code{\link{statsGearing}},
#'    \code{\link{statsGrade}},
#'    \code{\link{statsSession}},
#'    \code{\link{statsStops}}
#'
#' @return a list containing summary data
#'
#' @seealso \code{\link{read_ride}},\code{\link{processSegments}},
#'    \code{\link{statsCadence}},
#'    \code{\link{statsHeartRate}},
#'    \code{\link{statsGearing}},
#'    \code{\link{statsGrade}},
#'    \code{\link{statsSession}},
#'    \code{\link{statsStops}}
#'
#' @export
statsPower <- function(trackdf,powerCalibrateTime=600,...) {
  powerpos <- !is.na(trackdf$power.watts) & trackdf$power.watts > 0
  powerNum <- sum(trackdf$power.watts[powerpos]*
                    trackdf$deltatime[powerpos]  )
  if (sum(powerpos)>0) {
    avgpowerNoZeros <- powerNum / totalTime(trackdf,powerpos)
    avgpowerWithZeros <- powerNum / rollingTime(trackdf)

    calibrate <- cumsum(trackdf$deltatime)<powerCalibrateTime
    if (sum(!calibrate)>sum(calibrate)) {
      powerNumpost <- sum(trackdf$power.watts[powerpos & !calibrate]*
                          trackdf$deltatime[powerpos & !calibrate]  )
      avgpowerPostCalNoZeros <-powerNumpost / totalTime(trackdf,powerpos & !calibrate)
      avgpowerPostCalRolling <-powerNumpost / rollingTime(trackdf,!calibrate)
    } else {
      avgpowerPostCalNoZeros <- NA
      avgpowerPostCalRolling <- NA
    }
  } else {
    avgpowerNoZeros <- NA
    avgpowerWithZeros <- NA
    avgpowerPostCalNoZeros <- NA
    avgpowerPostCalRolling <- NA
  }
  return(list(avgpowerNoZeros=avgpowerNoZeros,
              avgpowerWithZeros=avgpowerWithZeros,
              avgpowerPostCalNoZeros=avgpowerPostCalNoZeros,
              avgpowerPostCalRolling=avgpowerPostCalRolling))
}
#' generate heartrate statistics for a track
#'
#' \code{statsHeartRate}  processes a gps track file to summarize the hr data
#'
#' @param trackdf data frame or tibble with gps track data
#' @param recovery_hr data frame or tibble with gps recovery hr data
#' @param ... parameters for \code{\link{processSegments}},
#'    \code{\link{statsCadence}},
#'    \code{\link{statsPower}},
#'    \code{\link{statsGearing}},
#'    \code{\link{statsGrade}},
#'    \code{\link{statsSession}},
#'    \code{\link{statsStops}}
#'
#' @return a list containing summary data
#'
#' @seealso \code{\link{read_ride}},\code{\link{processSegments}},
#'    \code{\link{statsCadence}},
#'    \code{\link{statsPower}},
#'    \code{\link{statsGearing}},
#'    \code{\link{statsGrade}},
#'    \code{\link{statsSession}},
#'    \code{\link{statsStops}}
#'
#' @export
statsHeartRate <- function(trackdf,recovery_hr,...) {
  if (any(!is.na(trackdf$heart_rate.bpm))) {
    lastHr <- max(which(!is.na(trackdf$heart_rate.bpm)))
    if (as.numeric(difftime(trackdf$timestamp.s[nrow(trackdf)],
                            trackdf$timestamp.s[lastHr],units="secs")) < 10) {
      hrAtStop <- as.numeric(trackdf$heart_rate.bpm[lastHr])
    }
    else {
      hrAtStop <- NA
    }
  } else {
    hrAtStop <- NA
  }
  if (!is.null(nrow(recovery_hr))) {
    if (nrow(recovery_hr)>0) {
      hrRecovery <- as.numeric(recovery_hr[1,"heart_rate.postride"])
    } else {
      hrRecovery <- NA
    }
  } else {
    hrRecovery <- NA
  }
  return(list(hrAtStop=hrAtStop,
              hrRecovery=hrRecovery))
}
#' generate gear ratio statistics for a track
#'
#' \code{statsGearing}  processes a gps track file to summarize the gear ratio
#'   implied by the ratio of cadence to speed
#'
#' @param trackdf data frame or tibble with gps track data
#' @param grLow1 lowest gear speed/cadence upper bound
#' @param grLow2 alternate lowest gear speed/cadence upper bound
#' @param ... parameters for \code{\link{processSegments}},
#'    \code{\link{statsCadence}},
#'    \code{\link{statsPower}},
#'    \code{\link{statsHeartRate}},
#'    \code{\link{statsGrade}},
#'    \code{\link{statsSession}},
#'    \code{\link{statsStops}}
#'
#' @return a list containing summary data
#'
#' @seealso \code{\link{read_ride}},\code{\link{processSegments}},
#'    \code{\link{statsCadence}},
#'    \code{\link{statsPower}},
#'    \code{\link{statsHeartRate}},
#'    \code{\link{statsGrade}},
#'    \code{\link{statsSession}},
#'    \code{\link{statsStops}}
#'
#' @export
statsGearing <- function(trackdf,
                         grLow1=0.044,grLow2,...) {
  ## ratio of speed to cadence
  pctLowGear1 <- NA
  pctLowGear2 <- NA
  pedaling <- (!is.na(trackdf$cadence.rpm)) & (trackdf$cadence.rpm > 0)
  if (sum(pedaling)>100) {
    gearratio <- trackdf$speed.m.s/trackdf$cadence.rpm
    gearratio[is.na(gearratio)|is.infinite(gearratio)] <- 10000
    inlowgear <- ( gearratio < grLow1 )
    pctLowGear1 <- sum(trackdf$deltatime[pedaling & inlowgear]*
                       gearratio[pedaling & inlowgear]) /
                             pedalTime(trackdf)
    if (!missing(grLow2)) {
      inlowgear <- ( gearratio < grLow2 )
      pctLowGear2 <- sum(trackdf$deltatime[pedaling & inlowgear]*
                         gearratio[pedaling & inlowgear]) /
                             pedalTime(trackdf)
    } else {
      grLow2 <- NA
    }
  }
  return(list(lowGear1=grLow1,
              lowGear2=grLow2,
              pctLowGear1=pctLowGear1,
              pctLowGear2=pctLowGear2))

}
#' generate grade statistics for a track
#'
#' \code{statsGrade}  processes a gps track file to summarize grade data
#'
#' @param trackdf data frame or tibble with gps track data
#' @param gradeSmoothWindowNN number of observations on each side used for
#'    calculating kernel smoothed elevation for the grade calculation
#' @param gradeSmoothWindowMeters bandwidth (meters) for kernel smoothing of
#'    elevation in the grade calculation
#' @param ... parameters for \code{\link{processSegments}},
#'    \code{\link{statsCadence}},
#'    \code{\link{statsPower}},
#'    \code{\link{statsHeartRate}},
#'    \code{\link{statsGearing}},
#'    \code{\link{statsSession}},
#'    \code{\link{statsStops}}
#'
#' @return a list containing summary data
#'
#' @seealso \code{\link{read_ride}},\code{\link{processSegments}},
#'    \code{\link{statsCadence}},
#'    \code{\link{statsPower}},
#'    \code{\link{statsHeartRate}},
#'    \code{\link{statsGearing}},
#'    \code{\link{statsSession}},
#'    \code{\link{statsStops}}
#'
#' @export
statsGrade <- function(trackdf,
                       gradeSmoothWindowMeters=7,
                       gradeSmoothWindowNN=10,...) {

  deltaelev <- trackdf$altitude.m - lag_one(trackdf$altitude.m)
  elevSmoothed <- smoothDataSegments(yvec=trackdf$altitude.m,
                                     xvar=trackdf$distance.m,
                                     segment=trackdf$segment,
                                     bw=gradeSmoothWindowMeters,
                                     nneighbors=gradeSmoothWindowNN,
                                     kernel="epanechnikov")
  deltaelevSmoothed <- elevSmoothed - lag_one(elevSmoothed)
  trackdeltadistance <- trackdf$distance.m - lag_one(trackdf$distance.m)

  trackgrade <- deltaelevSmoothed/trackdeltadistance
  trackgrade[trackdeltadistance<1] <- NA

  ascent  <- sum(deltaelevSmoothed[deltaelevSmoothed>0])
  descent <- sum(deltaelevSmoothed[deltaelevSmoothed<0])
  distanceAscending  <- sum(trackdeltadistance[deltaelev >  0.25])
  distanceDescending <- sum(trackdeltadistance[deltaelev < -0.25])

  return(list(ascent=ascent,descent=descent,
              distanceAscending=distanceAscending,
              distanceDescending=distanceDescending))
}
#' collect gps session statistics for a track
#'
#' \code{statsSession}  processes a gps session file
#'
#' @param session data frame or tibble with gps session data
#'
#' @return a list containing summary data
#'
#' @seealso \code{\link{read_ride}},\code{\link{processSegments}},
#'    \code{\link{statsCadence}},
#'    \code{\link{statsPower}},
#'    \code{\link{statsHeartRate}},
#'    \code{\link{statsGearing}},
#'    \code{\link{statsGrade}},
#'    \code{\link{statsStops}}
#'
#' @export
statsSession <- function(session) {
  if (is.null(session)) {
    sessionDistance <- NA
    sessionElapsedTime <- NA
    sessionTimerTime <- NA
    sessionPedalStrokes <- NA
    sessionTotalCalories <- NA
    sessionAvgSpeed <- NA
    sessionMaxSpeed <- NA
    sessionTotalAscent <- NA
    sessionTotalDescent <- NA
    sessionAvgCadence <- NA
    sessionAvgHr <- NA
    sessionMaxHr <- NA
    sessionAvgPower <- NA
    sessionMaxPower <- NA
  } else {
    sessionDistance <- ifelse("total_distance" %in% colnames(session),
                              session$total_distance[1],NA)
    sessionElapsedTime <- ifelse("total_elapsed_time" %in% colnames(session),
                                 session$total_elapsed_time[1],NA)
    sessionTimerTime <- ifelse("total_timer_time" %in% colnames(session),
                               session$total_timer_time[1],NA)
    sessionPedalStrokes <- ifelse("total_cycles" %in% colnames(session),
                                  session$total_cycles[1],NA)
    sessionTotalCalories <- ifelse("total_calories" %in% colnames(session),
                                   session$total_calories[1],NA)
    sessionAvgSpeed <- ifelse("avg_speed" %in% colnames(session),
                              session$avg_speed[1],NA)
    sessionMaxSpeed <- ifelse("max_speed" %in% colnames(session),
                              session$max_speed[1],NA)
    sessionTotalAscent <- ifelse("total_ascent" %in% colnames(session),
                                 session$total_ascent[1],NA)
    sessionTotalDescent <- ifelse("total_descent" %in% colnames(session),
                                  session$total_descent[1],NA)
    sessionAvgCadence <- ifelse("avg_cadence" %in% colnames(session),
                                session$avg_cadence[1],NA)
    sessionAvgHr <- ifelse("avg_heart_rate" %in% colnames(session),
                           session$avg_heart_rate[1],NA)
    sessionMaxHr <- ifelse("max_heart_rate" %in% colnames(session),
                           session$max_heart_rate[1],NA)
    sessionAvgPower <- ifelse("avg_power" %in% colnames(session),
                              session$avg_power[1],NA)
    sessionMaxPower <- ifelse("max_power" %in% colnames(session),
                              session$max_power[1],NA)
  }
  return(list(
    sessionDistance = sessionDistance,
    sessionElapsedTime = sessionElapsedTime,
    sessionTimerTime = sessionTimerTime,
    sessionPedalStrokes = sessionPedalStrokes,
    sessionTotalCalories = sessionTotalCalories,
    sessionAvgSpeed = sessionAvgSpeed,
    sessionMaxSpeed = sessionMaxSpeed,
    sessionTotalAscent = sessionTotalAscent,
    sessionTotalDescent = sessionTotalDescent,
    sessionAvgCadence = sessionAvgCadence,
    sessionAvgHr = sessionAvgHr,
    sessionMaxHr = sessionMaxHr,
    sessionAvgPower = sessionAvgPower,
    sessionMaxPower = sessionMaxPower))
}
#' generate stops statistics for a track
#'
#' \code{statsStops}  processes a gps track file to summarize grade data
#'
#' @param trackdf data frame or tibble with gps track data
#' @param segInitIdleAggSecs stops/restarts before this time has
#'    elapsed are discarded and the ride starts at the last start prior to this
#' @param segInitIdleAggMeters stops/restarts within this distance
#'    of the initial start are discarded as above
#' @param ... parameters for \code{\link{processSegments}},
#'    \code{\link{statsCadence}},
#'    \code{\link{statsPower}},
#'    \code{\link{statsHeartRate}},
#'    \code{\link{statsGearing}},
#'    \code{\link{statsGrade}},
#'    \code{\link{statsSession}}
#'
#' @return a list containing summary data
#'
#' @seealso \code{\link{read_ride}},
#'    \code{\link{processSegments}},
#'    \code{\link{statsCadence}},
#'    \code{\link{statsPower}},
#'    \code{\link{statsHeartRate}},
#'    \code{\link{statsGearing}},
#'    \code{\link{statsGrade}},
#'    \code{\link{statsSession}}
#'
#' @export
statsStops <- function(trackdf,
                       segInitIdleAggSecs=60,
                       segInitIdleAggMeters=20,
                       ...) {

  segstart <- trackdf$segment != lag_one(trackdf$segment)
  segstart[1] <- TRUE

  timesecs <- cumsum(trackdf$deltatime)
#  moving <- trackdf$speed.m.s > 0
  moving <- !trackdf$stopped
  moving[!((lag_one(lag_one(moving))&lag_one(moving))|
           (lead_one(lead_one(moving))&lead_one(moving)))] <- FALSE
  movingtime <- timesecs*as.numeric(moving)
  lastmovetime <- lag_one(cummax(movingtime))
  stoplength <- (timesecs[segstart] - lastmovetime[segstart])[-1]

  startlinewait <- timesecs <= segInitIdleAggSecs &
                   trackdf$distance.m <= segInitIdleAggMeters &
                   trackdf$speed.m.s == 0
  startlineTime <- sum(trackdf$deltatime[startlinewait])
  stopsSubMinute <- sum(stoplength > 0 & stoplength < 60)
  stops1to10Minutes <- sum(stoplength >= 60 & stoplength < 600)
  stops10to30Minutes <- sum(stoplength >= 600 & stoplength < 1800)
  stopsLong <- sum(stoplength >= 1800)
  return(list(startlineTime=startlineTime,
              stopsSubMinute=stopsSubMinute,
              stops1to10Minutes=stops1to10Minutes,
              stops10to30Minutes=stops10to30Minutes,
              stopsLong=stopsLong))
}
totalTime <- function(trackdf,include) {
  if (missing(include)) include <- TRUE
  return(sum(trackdf$deltatime[include]))
}
rollingTime <- function(trackdf,include) {
  if (missing(include)) include <- TRUE
  return(sum(trackdf$deltatime[include & trackdf$speed.m.s > 0]))
}
pedalTime <- function(trackdf,include) {
  if (missing(include)) include <- TRUE
  pedaling <- !is.na(trackdf$cadence.rpm) & trackdf$cadence.rpm > 0
  return(sum(trackdf$deltatime[include & pedaling]))
}

