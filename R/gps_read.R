#' read multiple gps data files
#'
#' \code{read_ridefiles} reads a vector of gps .fit and/or .gpx track files
#'  and put into summary and detail data tibbles for modeling or graphical
#'  summary.  A wrapper for \code{\link{read_ride}}.
#'
#'
#' @param ridefilevec a vector of filenames to process
#' @param ... parameters passed for track cleaning
#'
#' @return a list of two data tibbles:  \eqn{summaries} and \eqn{tracks}
#'    These are linked by integer fields \eqn{startbutton.date}
#'    and \eqn{startbutton.time}
#'
#' @seealso \code{\link{read_ride}}
#'
#' @export
read_ridefiles <- function(ridefilevec,...)  {
  ###  make the R checker happy with utterly irrelevant initializations of variables used by dplyr
  start.time <- startbutton.date <- timestamp.s <- start.hour <- NULL
  outdf <- NULL
  outtracks <- NULL
  for (x in ridefilevec) {
    ride <- read_ride(x,...)
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
#' @param subtrack.initial.idle.agg.secs stops/restarts before this time has
#'    elapsed are discarded and the ride starts at the last start prior to this
#' @param subtrack.initial.idle.agg.meters stops/restarts within this distance
#'    of the initial start are discarded as above
#' @param min.segment.break.time (seconds) segments separated by less time
#'    than this are joined
#' @param max.interval.time (seconds) tracks are split between datapoints
#'    separated by more time than this
#' @param min.segment.obs segments smaller than this are joined to closest
#'    neighboring segment
#' @param min.segment.dist (meters) segments shorter than this are joined
#' @param min.segment.time (seconds) segments shorter than this are joined
#' @param short.segment.merge.timegap.max (seconds) short segments are joined
#'    if interval between closest is no longer than this time
#' @param short.seg.delete delete short segments if no other segment
#'    within the max tim gap above
#' @param short.seg.cadence.delete.na.threshold delete short segments even
#'    if \eqn{short.seg.delete}=FALSE if share of cadence NAs in segment exceeds this
#' @param cadence.trim.beg.secs number of seconds after the beginning of a
#'    track segment that are ignored in calculating midsegment avg cadence
#' @param cadence.trim.end.secs number of seconds before the end of a
#'    track segment that are ignored in calculating midsegment avg cadence
#' @param cadence.max max credible cadence value, larger values are errors
#' @param cadence.correct.errors repair excessive cadence values
#'    using triangular-kernel-weighted average of the nearest nonmissing values
#'    in the same segment
#' @param cadence.correct.nas repair cadence missing values using
#'    triangular-kernel-weighted average of the nearest nonmissing values in
#'    the same segment
#' @param cadence.correct.window window size for kernel smoothing of cadence
#' @param grade.smooth.window number of observations on each side used for
#'    calculating kernel smoothed elevation for the grade calculation
#' @param grade.smooth.bw bandwidth for kernel smoothing of elevation
#'    in the grade calculation
#' @param grade.smooth.percentiles percentiles of elevations to use
#'    as steepest grades
#' @param cadence.time.shift.start do not use, alter assumptions about gps sample recording
#' @param last.interval.end.notrigger do not use, alter assumptions about gps sample recording
#'
#' @return a list of two data tibbles:  \eqn{summary} and \eqn{trackpoints}
#'    These are linked by integer fields \eqn{startbutton.date}
#'    and \eqn{startbutton.time}
#'
#' @seealso \code{\link{read_ridefiles}}
#'
#' @export
read_ride <- function(ridefile,tz="America/Los_Angeles",
          subtrack.initial.idle.agg.secs=20,subtrack.initial.idle.agg.meters=15,
          min.segment.break.time=3,max.interval.time=30,
          min.segment.obs=2,min.segment.dist=10,min.segment.time=5,
          short.segment.merge.timegap.max=4,
          short.seg.delete=FALSE,short.seg.cadence.delete.na.threshold=0.5,
          cadence.time.shift.start=0,last.interval.end.notrigger=FALSE,
          cadence.trim.beg.secs=30,cadence.trim.end.secs=30,
          cadence.max=160,cadence.correct.window=7,
          cadence.correct.errors=TRUE,cadence.correct.nas=FALSE,
          grade.smooth.window=7,grade.smooth.bw=3,
          grade.smooth.percentiles=c(0.005,.98))  {

  cat(paste0("\nreading: ",ridefile))
  if (missing(tz)) {
    tz <- Sys.timezone()
  }
  if (substr(ridefile,nchar(ridefile)-3,nchar(ridefile))==".fit") {
    temp <- read_fittrack(ridefile)
#fit.track <<- temp[["track"]]
    time.fn.string <- basename(ridefile)
    fit.fn.time.parse <- getOption("bCadHr.fit.fn.time.parse")
    fit.fn.lead <- getOption("bCadHr.fit.fn.lead")
    fit.fn.trail <- getOption("bCadHr.fit.fn.trail")
    if (nchar(fit.fn.lead)>0) time.fn.string <-
        substr(time.fn.string,regexpr(fit.fn.lead,time.fn.string)+nchar(fit.fn.lead)+1,1000000)
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
      substr(time.fn.string,regexpr(gpx.fn.lead,time.fn.string)+nchar(fit.fn.lead)+1,1000000)
    if (nchar(gpx.fn.trail)>0) time.fn.string <-
      substr(time.fn.string,1,regexpr(gpx.fn.trail,time.fn.string))
    time.turned.on <- strptime(time.fn.string,gpx.fn.time.parse,tz=tz)
  } else {
    stop("unknown file extension")
  }
  trackdata <- temp[["track"]]
  recovery_hr <- temp[["recovery_hr"]]
  session <- temp[["session"]]
  #  trackdata$timestamp.s <- format(trackdata$timestamp.s,tz=tz)
  attr(trackdata$timestamp.s,"tzone") <- tz
  if (is.na(time.turned.on)){
    #  if filename was not successfully turned into a start-button time, use first
    #   timestamp value recorded in the track.  Likely but not certain to match between
    #   different versions of the same ride
    time.turned.on <- trackdata$timestamp.s[1]
  }
  startbutton.date <- as.integer(lubridate::mday(time.turned.on)+
                             100*lubridate::month(time.turned.on)+
                           10000*lubridate::year(time.turned.on))
  startbutton.time <- as.integer(lubridate::second(time.turned.on)+
                             100*lubridate::minute(time.turned.on)+
                           10000*lubridate::hour(time.turned.on))
  trackdata$startbutton.date <- startbutton.date
  trackdata$startbutton.time <- startbutton.time


  ##  if no distance in an observation, then no cadence sensor and no gps signal to give location, waypoints contain no info, so delete
  if (length(which(is.na(trackdata$distance.m)))>0) {
    first.move <- min(which(!is.na(trackdata$distance.m)))
    if (first.move > 1 ) {
      trackdata <- trackdata[first.move:nrow(trackdata),]
    }
  }
  if (any(is.na(trackdata$distance.m))) {
    no.dist.segnums <- unique(trackdata$segment[is.na(trackdata$distance.m)])
    segs.dropped <- setdiff(no.dist.segnums,unique(trackdata$segment[!is.na(trackdata$distance.m)]))
    if (length(segs.dropped)>0){
      first.seg <- trackdata$segment != lag_one(trackdata$segment)
      first.nodist <- first.seg & trackdata$segment %in% segs.dropped
      newsegnum <- trackdata$segment - cumsum(first.nodist)
      trackdata$segement <- newsegnum
    }
    trackdata <- trackdata[!is.na(trackdata$distance.m),]
  }

  # remove really, really long intervals between points
  first.seg <- trackdata$segment != lag_one(trackdata$segment)
  long.interval <-  as.numeric(difftime(trackdata$timestamp.s,lag_one(trackdata$timestamp.s),units="secs")) > max.interval.time &
                             !first.seg
  if (sum(long.interval)>0) {
    trackdata$segment <- trackdata$segment + cumsum(long.interval)
  }

  #  summary stats based on all usable nontrivial data
  ride.date <- as.Date(format(trackdata$timestamp.s[1],tz=tz))
  start.time <- as.character(trackdata$timestamp.s[1])
  start.hour <- as.numeric(lubridate::hour(format(time.turned.on,tz=tz))
           + 0.25*(round((60*lubridate::minute(format(time.turned.on,tz=tz))+
                          lubridate::second(format(time.turned.on,tz=tz)))/(60*15))))
  nwaypoints <- nrow(trackdata)  #  this will be recalculated, but is appropriate for these summary stats
  pct.trkpts.hr <- sum(!is.na(trackdata$heart_rate.bpm))/nwaypoints
  pct.trkpts.cad <- sum(!is.na(trackdata$cadence.rpm))/nwaypoints

  ##   collapse (usually spurious) initial start/stops before ride really begins, and accumulate startline time and startline distance offset
  track.start <- (as.numeric(difftime(trackdata$timestamp.s,trackdata$timestamp.s[1],units="secs")) < subtrack.initial.idle.agg.secs) |
     		 (trackdata$distance.m < subtrack.initial.idle.agg.meters)
  startline.time <- 0.0
  startline.distance <- 0.0
  if (sum(track.start) > 0) {
    drop.obs <- trackdata$segment < max(trackdata$segment[track.start])
    if (sum(drop.obs) > 0) {
      first.seg <- trackdata$segment != lag_one(trackdata$segment)
      first.seg[1] <- TRUE  #  there should be exactly one obs in the "startline set" of close points which is both not dropped and first-in-seg
      startline.time <- as.numeric(difftime(trackdata$timestamp.s[first.seg&track.start&!drop.obs],trackdata$timestamp.s[1],units="secs"))
      startline.distance <- trackdata$distance.m[first.seg&track.start&!drop.obs]
      trackdata$segment[!track.start] <- trackdata$segment[!track.start] + 1 - max(trackdata$segment[track.start])
      trackdata$segment[track.start] <- 1
      trackdata <- trackdata[!drop.obs,]
    }
  }
  ##   join segments where separating breaks are less than minimum time
  trackdeltatime <- as.numeric(difftime(trackdata$timestamp.s,lag_one(trackdata$timestamp.s),units="secs"))
  trackdeltanexttime <- as.numeric(difftime(trackdata$timestamp.s,lead_one(trackdata$timestamp.s),units="secs"))
  trackdeltadist <- as.numeric(trackdata$distance.m,lag_one(trackdata$distance.m))
  first.seg <- trackdata$segment != lag_one(trackdata$segment)   # this does not flag the first start, which is fine
  first.nomin <- first.seg & (trackdata$segment != 1) & (trackdeltatime < min.segment.break.time)
  if (sum(first.nomin) > 0) {
    newsegnum <- trackdata$segment - cumsum(first.nomin)
    trackdata$segment <- newsegnum
    #  patch missing cadence in midblock later
  }
  ##  segments with too few samples, too short in time or distance
  ##
  first.seg <- trackdata$segment != lag_one(trackdata$segment)   # this does not flag the first start
  first.seg[1] <- TRUE
  last.seg <- trackdata$segment != lead_one(trackdata$segment)   # this does not flag the last stop
  last.seg[nrow(trackdata)] <- TRUE
  first.short <- first.seg & (trackdata$segment != lead_n(trackdata$segment,min.segment.obs))
  first.short[is.na(first.short)] <- FALSE
  seg.start.m <- trackdata$distance.m[first.seg]
  seg.stop.m <- trackdata$distance.m[last.seg]
  seg.length.m <- seg.stop.m - seg.start.m
  seg.start.s <- trackdata$timestamp.s[first.seg]
  seg.stop.s <- trackdata$timestamp.s[last.seg]
  seg.length.s <- as.numeric(difftime(seg.stop.s,seg.start.s,units="secs"))
  gap.prior <- trackdeltatime[first.seg]
  gap.prior[1] <- Inf
  gap.post <- trackdeltanexttime[last.seg]
  gap.post[1] <- Inf
  segs.na.share <- sapply(1:length(seg.start.m),
                          function(x) sum(is.na(trackdata$cadence.rpm)&trackdata$segment==x)/sum(trackdata$segment==x))
  segs.too.short <- seg.length.m < min.segment.dist | seg.length.s < min.segment.time | first.short[first.seg]
  segs.merge.prior  <- segs.too.short & (gap.prior < gap.post) & (gap.prior < short.segment.merge.timegap.max)
  segs.merge.post   <- segs.too.short & (gap.prior >= gap.post) & (gap.post  < short.segment.merge.timegap.max)
  segs.merge.delete <- segs.too.short & !(segs.merge.prior | segs.merge.post) &
                            (short.seg.delete | segs.na.share > short.seg.cadence.delete.na.threshold)

  if (length(c(segs.merge.prior,segs.merge.post,segs.merge.delete))>0) {
     first.merged.dropped <- first.seg & trackdata$segment %in% c(segs.merge.prior,segs.merge.post,segs.merge.delete)
     newsegnum <- trackdata$segment - cumsum(first.merged.dropped) + as.numeric(trackdata$segment %in% segs.merge.post)
     dropobs <- trackdata$segment %in% segs.merge.delete
     trackdata$segment <- newsegnum
     trackdata <- trackdata[!dropobs,]

  }

  ### set first,last and middle flags after all segment clean-up is done, and set up final quantities for calulations ######
  nwaypoints <- nrow(trackdata)
  nsegments <- max(trackdata$segment)
  first.seg <- trackdata$segment != lag_one(trackdata$segment)
  first.seg[1] <- TRUE
  last.seg <- trackdata$segment != lead_one(trackdata$segment)
  last.seg[nwaypoints] <- TRUE
  middle.of.seg <- !(first.seg | last.seg)
  start.times <- trackdata$timestamp.s[first.seg]
  stop.times <- trackdata$timestamp.s[last.seg]
  trackdeltatime <- as.numeric(difftime(trackdata$timestamp.s,lag_one(trackdata$timestamp.s),units="secs"))
  trackdeltaelev <- trackdata$altitude.m - lag_one(trackdata$altitude.m)
  trackdeltadistance <- trackdata$distance.m - lag_one(trackdata$distance.m)
  time.from.start <- cumsum(trackdeltatime)
  elev.smoothed <- smoothTriangular(time.from.start,trackdata$altitude.m,segment=trackdata$segment,nneighbors=grade.smooth.window,bw=grade.smooth.bw)
  deltaelev.smoothed <- elev.smoothed - lag_one(elev.smoothed)
  trackgrade <- deltaelev.smoothed/trackdeltadistance
  trackgrade[trackdeltadistance<1] <- NA
  trackgrade.tails <- stats::quantile(trackgrade,probs=grade.smooth.percentiles,na.rm=TRUE)

  ## patch cadence errors and then NAs
  cadence.error <- trackdata$cadence.rpm > cadence.max
  cadence.error[is.na(cadence.error)] <- FALSE
  if (sum(cadence.error,na.rm=TRUE) > 0) {
    if (cadence.correct.errors) {
      trackdata$cadence.rpm[cadence.error] <- NA
      cadence.smoothed <- smoothTriangular(time.from.start,trackdata$cadence.rpm,segment=trackdata$segment,nneighbors=cadence.correct.window,bw=cadence.correct.window)
      trackdata$cadence.rpm[cadence.error] <- cadence.smoothed[cadence.error]
    } else {
      trackdata$cadence.rpm[cadence.error] <- NA
    }
  }
  if (cadence.correct.nas) {
    cadence.na <- is.na(trackdata$cadence.rpm)
    if (sum(cadence.na,na.rm=TRUE) > 0) {
      cadence.smoothed <- smoothTriangular(time.from.start,trackdata$cadence.rpm,segment=trackdata$segment,nneighbors=cadence.correct.window,bw=cadence.correct.window)
      trackdata$cadence.rpm[cadence.na] <- cadence.smoothed[cadence.na]
    }
  }
  cadence.pos.index <- !is.na(trackdata$cadence.rpm) & trackdata$cadence.rpm>0
  trimmed.cadence <- rep(NA,nwaypoints)
  if (sum(cadence.pos.index) > 0) {
    for (segment in 1:nsegments) {
      innersegment <- (trackdata$timestamp.s >= start.times[segment]+lubridate::seconds(cadence.trim.beg.secs)) &
        (trackdata$timestamp.s <   stop.times[segment]-lubridate::seconds(cadence.trim.end.secs))
      if (sum(innersegment)>0) trimmed.cadence[innersegment] <- trackdata$cadence.rpm[innersegment]
    }
  }

  ## times for constructing weights on cadences and summarizing the ride
  idletime <- rep(0,nwaypoints)
  idletime[first.seg] <- trackdeltatime[first.seg] - cadence.time.shift.start
  idletime[last.seg] <- cadence.time.shift.start

  rollingtime <- rep(0,nwaypoints)
  rollingtime[first.seg] <- cadence.time.shift.start
  rollingtime[middle.of.seg] <- trackdeltatime[middle.of.seg]
  #  use last interval if recording is triggered by stop, half if stop recorded at scheduled time
  if (last.interval.end.notrigger) {
    rollingtime[last.seg] <- trackdeltatime[last.seg]/2
  } else {
    rollingtime[last.seg] <- trackdeltatime[last.seg] - cadence.time.shift.start
  }

  pedaltime <- rep(0,nwaypoints)
  pedaltime[first.seg&cadence.pos.index] <- cadence.time.shift.start
  pedaltime[middle.of.seg&cadence.pos.index] <- trackdeltatime[middle.of.seg&cadence.pos.index]
  #  use last interval if recording is triggered by stop, half if stop recorded at scheduled time
  if (last.interval.end.notrigger) {
    pedaltime[last.seg&cadence.pos.index] <- trackdeltatime[last.seg&cadence.pos.index]/2
  } else {
    pedaltime[last.seg&cadence.pos.index] <- trackdeltatime[last.seg&cadence.pos.index] - cadence.time.shift.start
  }
  pedaltime[pedaltime<0.0] <- 0.0

  trackpedalstrokes <- (pedaltime/60)*trackdata$cadence.rpm
  trackpedalstrokes[pedaltime==0] <- 0.0

  nwaypoints <- nwaypoints
  numsegs <- nsegments
  startline.time <- startline.time
  total.time <- as.numeric(difftime(trackdata$timestamp.s[nwaypoints],trackdata$timestamp.s[1],units="secs"))
  rolling.time <- sum(rollingtime)
  pedal.time <- sum(pedaltime)
  ped.strks <- sum(trackpedalstrokes[!is.na(trackdata$cadence.rpm)])
  ped.strks.na <- ped.strks
  ped.strks.na[ped.strks.na==0] <- NA
  pedal.strokes <- ped.strks
  distance <- trackdata$distance.m[nwaypoints]-startline.distance
  avgcadence.nozeros <- 60*ped.strks.na/sum(pedaltime)
  avgcadence.withzeros <- 60*ped.strks.na/sum(rollingtime[!is.na(trackdata$cadence.rpm)])
  avgcadence.midsegment <- mean(trimmed.cadence,na.rm=TRUE)
  hr.at.stop <- NA
  if (any(!is.na(trackdata$heart_rate.bpm))) {
    last.hr <- max(which(!is.na(trackdata$heart_rate.bpm)))
    if (as.numeric(difftime(trackdata$timestamp.s[nwaypoints],trackdata$timestamp.s[last.hr],units="secs")) < 10) {
      hr.at.stop <- as.numeric(trackdata$heart_rate.bpm[last.hr])
    }
  }
  if (!is.null(nrow(recovery_hr))) {
    if (nrow(recovery_hr)>0) {
      hr.recovery <- as.numeric(recovery_hr[1,"heart_rate.postride"])
    } else {
      hr.recovery <- NA
    }
  } else {
    hr.recovery <- NA
  }
  speed.rolling.m.s <- distance/rolling.time
  speed.all.m.s <- distance/total.time
  speed.max.m.s <- max(trackdata$speed.m.s,na.rm=TRUE)
  ascent  <- sum(trackdeltaelev[trackdeltaelev>0])
  descent <- sum(trackdeltaelev[trackdeltaelev<0])
  distance.ascending  <- sum(trackdeltadistance[trackdeltaelev >  0.25])
  distance.descending <- sum(trackdeltadistance[trackdeltaelev < -0.25])
  grade.ascending.steepest <- stats::median(trackgrade[!is.na(trackgrade)&(trackgrade>=trackgrade.tails[2])])
  grade.descending.steepest <- stats::median(trackgrade[!is.na(trackgrade)&(trackgrade<=trackgrade.tails[1])])

  break.time <- trackdeltatime[first.seg]
  stops.subminute <- length(break.time[break.time<=60 & break.time>2])
  stops.1to10minutes <- length(break.time[break.time<=600 & break.time>60])
  stops.10to30minutes <- length(break.time[break.time<=1800 & break.time>600])
  stops.long <- length(break.time[break.time>1800])

  session.distance <- ifelse("total_distance" %in% colnames(session),session$total_distance[1],NA)
  session.elapsed.time <- ifelse("total_elapsed_time" %in% colnames(session),session$total_elapsed_time[1],NA)
  session.timer.time <- ifelse("total_timer_time" %in% colnames(session),session$total_timer_time[1],NA)
  session.pedal.strokes <- ifelse("total_cycles" %in% colnames(session),session$total_cycles[1],NA)
  session.total.calories <- ifelse("total_calories" %in% colnames(session),session$total_calories[1],NA)
  session.avg.speed <- ifelse("avg_speed" %in% colnames(session),session$avg_speed[1],NA)
  session.max.speed <- ifelse("max_speed" %in% colnames(session),session$max_speed[1],NA)
  session.total.ascent <- ifelse("total_ascent" %in% colnames(session),session$total_ascent[1],NA)
  session.total.descent <- ifelse("total_descent" %in% colnames(session),session$total_descent[1],NA)
  session.avg.cadence <- ifelse("avg_cadence" %in% colnames(session),session$avg_cadence[1],NA)
  session.avg.hr <- ifelse("avg_heart_rate" %in% colnames(session),session$avg_heart_rate[1],NA)
  session.max.hr <- ifelse("max_heart_rate" %in% colnames(session),session$max_heart_rate[1],NA)

  sourcefile <- basename(ridefile)
  processed.time <- Sys.time()

  if (!is.na(trackdata$position_lon.dd[1])){
    beg.end.gap <- raster::pointDistance(cbind(trackdata$position_lon.dd[1],trackdata$position_lat.dd[1]),
                                 cbind(trackdata$position_lon.dd[nrow(trackdata)],trackdata$position_lat.dd[nrow(trackdata)]),
                                 lonlat=TRUE)
    if (beg.end.gap>133) {
      print(paste0("non-loop - distance between start and end = ",beg.end.gap))
      print(paste0("start = ",trackdata$position_lon.dd[1],"  ",trackdata$position_lat.dd[1]))
      print(paste0("stop  = ",trackdata$position_lon.dd[nrow(trackdata)],"  ",trackdata$position_lat.dd[nrow(trackdata)]))
      ride.loop <- FALSE
    } else {
      ride.loop <- TRUE
    }
  } else {
    ride.loop <- NA
  }
  track.cleaned <- data_frame(date=ride.date,start.time,start.hour,nwaypoints,numsegs,
          distance,total.time,rolling.time,pedal.time,startline.time,pedal.strokes,
          avgcadence.nozeros,avgcadence.withzeros,avgcadence.midsegment,
          hr.at.stop,hr.recovery,speed.rolling.m.s,speed.all.m.s,speed.max.m.s,
          ascent,descent,distance.ascending,distance.descending,
          grade.ascending.steepest,grade.descending.steepest,
          pct.trkpts.hr,pct.trkpts.cad,ride.loop,
          stops.subminute,stops.1to10minutes,stops.10to30minutes,stops.long,
          session.distance,session.elapsed.time,session.timer.time,session.pedal.strokes,
          session.total.calories,session.avg.speed,session.max.speed,
          session.total.ascent,session.total.descent,
          session.avg.cadence,session.avg.hr,session.max.hr,
          sourcefile,processed.time,startbutton.date,startbutton.time)
  return(list(summary=track.cleaned,trackpoints=trackdata))
}

