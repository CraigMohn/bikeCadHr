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
#' @param usefitdc use package fitdc to read fit files instead of fitparse
#' @param ... parameters passed for track cleaning
#'
#' @return a list of two data tibbles:  \eqn{summaries} and \eqn{tracks}
#'    These are linked by integer fields \eqn{startbutton.date}
#'    and \eqn{startbutton.time}
#'
#' @seealso \code{\link{read_ride}}
#'
#' @export
read_ridefiles <- function(ridefilevec,cores=4,
                           usefitdc=FALSE,loud=loud,...)  {

  nfiles <- length(ridefilevec)
  if(missing(cores)) cores <- parallel::detectCores()
  if ((nfiles > 10) & !is.na(cores) & (cores>1)) {
    doParallel::registerDoParallel(cores)
    `%dopar%` <- foreach::`%dopar%`
    cfun <- function(a,b) list(summary=dplyr::bind_rows(a[["summary"]],b[["summary"]]),
                               trackpoints=dplyr::bind_rows(a[["trackpoints"]],b[["trackpoints"]]))
    ridelist <- foreach (x = ridefilevec,.combine=`cfun`,
                        .packages=c("bikeCadHr")) %dopar% {
      read_ride(x,loud=loud,usefitdc=usefitdc,...)
    }
    doParallel::stopImplicitCluster()
    return(list(summaries=dplyr::arrange(ridelist[["summary"]],date,start.hour),
                tracks=dplyr::arrange(ridelist[["trackpoints"]],startbutton.date,timestamp.s)))
  } else {
    outdf <- NULL
    outtracks <- NULL
    for (x in ridefilevec) {
      ride <- read_ride(x,loud=loud,usefitdc=usefitdc,...)
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
#' @param stopSpeed - speed in m/s below which the bike is considered stopped
#'    for time and cadence calculations
#' @param loud print information about hr/cadence data issues/fixes
#' @param loudSegment print information about re/segmenting track data
#' @param usefitdc use package fitdc to read fit files instead of fitparse
#' @param ... parameters for \code{\link{processSegments}},
#'    \code{\link{repairHR}},
#'    \code{\link{repairCadence}},
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
#'    \code{\link{repairHR}},
#'    \code{\link{repairCadence}},
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
                      stopSpeed=0.0,
                      fixDistance=FALSE,loud=FALSE,loudSegment=FALSE,
                      usefitdc=FALSE,...)  {

  cat("\nreading: ",ridefile,"\n")
  if (missing(tz)) {
    tz <- Sys.timezone()
  }
  if (substr(ridefile,nchar(ridefile)-3,nchar(ridefile))==".fit") {
    temp <- read_fittrack(ridefile,usefitdc=usefitdc)
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

  trackdata$deltatime <- as.numeric(difftime(trackdata$timestamp.s,
                                             lag_one(trackdata$timestamp.s),units="secs"),
                                    units="secs")

  #  repair problems with data
  trackdata <- repairSensorDropOut(trackdata,loud=loud,...)
  repHR <- repairHR(trackdata,loud=loud,...)
  trackdata <- repHR[["trackdf"]]
  repCad <- repairCadence(trackdata,loud=loud,...)
  trackdata <- repCad[["trackdf"]]
  repPower <- repairPower(trackdata,loud=loud,...)
  trackdata <- repPower[["trackdf"]]
  #  split/revise track into segments separated by non-negligile stops
  trackdata <- processSegments(trackdf=trackdata,loud=loudSegment,...)

  sessionStats <- statsSession(session)
  cadStats <- statsCadence(trackdf=trackdata,
                sessionpedalstrokes=sessionStats[["sessionPedalStrokes"]],
                loud=loud,...)
  powStats <- statsPower(trackdf=trackdata,...)
  hrStats <- statsHeartRate(trackdf=trackdata,recovery_hr=recovery_hr,...)
  gearStats <- statsGearing(trackdf=trackdata,...)
  climbStats <- statsGrade(trackdf=trackdata,...)
  stopsStats <- statsStops(trackdf=trackdata,...)

  if (!is.na(trackdata$position_lon.dd[1])){
    begEndGap <-
      raster::pointDistance(cbind(trackdata$position_lon.dd[1],
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
                       nHRTooHigh = repHR[["nHRTooHigh"]],
                       pct.trkpts.cad = sum(!is.na(trackdata$cadence.rpm))/
                                          nrow(trackdata),
                       nCadTooHigh = repCad[["nCadTooHigh"]],
                       nCadTooLow = repCad[["nCadTooLow"]],
                       nCadStoppedPos = repCad[["nCadStoppedPos"]],
                       nCadStuck = repCad[["nCadStuck"]],
                       begEndGap = begEndGap,
                       deltaElev = trackdata$altitude.m[nrow(trackdata)] -
                                   trackdata$altitude.m[1],
                       ride.loop = rideLoop,
                       distance = trackdata$distance.m[nrow(trackdata)],
                       total.time = totalTime(trackdata),
                       rolling.time = rollingTime(trackdata,
                                                  stopSpeed=stopSpeed),
                       pedal.time = pedalTime(trackdata,
                                              stopSpeed=stopSpeed,...),
                       speed.rolling.m.s = trackdata$distance.m[nrow(trackdata)]/
                                              rollingTime(trackdata,
                                                          stopSpeed=stopSpeed),
                       speed.all.m.s = trackdata$distance.m[nrow(trackdata)]/
                                              totalTime(trackdata),
                       speed.max.m.s = max(trackdata$speed.m.s,na.rm=TRUE),
                       avgcadence.nozeros.session = cadStats[["avgcadenceNoZerosSession"]],
                       avgcadence.withzeros.session = cadStats[["avgcadenceWithZerosSession"]],
                       avgcadence.nozeros = cadStats[["avgcadenceNoZerosSum"]],
                       avgcadence.withzeros = cadStats[["avgcadenceWithZerosSum"]],
                       avgcadence.midsegment = cadStats[["avgcadenceMidsegment"]],
                       summed.pedal.strokes = cadStats[["summedPedalStrokes"]],
                       avgpower.nozeros=powStats[["avgpowerNoZeros"]],
                       avgpower.withzeros=powStats[["avgpowerWithZeros"]],
                       power.calibrations.detected=repPower[["nCalibrateSequences"]],
                       ascent = climbStats[["ascent"]],
                       descent = climbStats[["descent"]],
                       distance.ascending = climbStats[["distanceAscending"]],
                       distance.descending = climbStats[["distanceDescending"]],
                       pct.low.gear = gearStats[["pctLowGear1"]],
                       low.gear = gearStats[["lowGear1"]],
                       pct.low.gear2 = gearStats[["pctLowGear2"]],
                       low.gear2 = gearStats[["lowGear2"]],
                       hr.max = hrStats[["hrMax"]],
                       hr.min = hrStats[["hrMin"]],
                       hr.mean = hrStats[["hrMean"]],
                       hr.m2 = hrStats[["hrMoment2"]],
                       hr.at.start = hrStats[["hrAtStart"]],
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

