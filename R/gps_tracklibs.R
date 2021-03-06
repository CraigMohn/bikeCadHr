#' add/replace ride data in all tbl pairs
#'
#' \code{update_gps_variables} Add/replace ride data in .fit, .gpx and combined
#'  tbl pairs (summaries,tracks)
#'
#' Update existing (summaries,tracks) pairs for .fit and .gpx files structured
#'   with new files at the top level, and files already included in the
#'   pairs of tbls summarizing the rides are in subdirectories below them.
#'   there are 3 file pairs created:
#'     (fitsummary,fittracks), (gpxsummary,gpxtracks) and (gpssummary,gpstracks)
#'     the last pair contains data from both joined, with preference given to
#'     .fit-sourced data
#'
#'
#' @param outdir string containing location of output data pair files
#' @param fitrootdir string containing location of root directory for gps .fit files
#' @param gpxrootdir string containing location of root directory for gps .gpx files
#' @param merge.files list of vectors (order matters within vectors) of .fit
#'    and .gpx files at the top level to be joined
#' @param fitexcludes list of substrings within a file name which
#'    will cause the file to be ignored.  occasionally .fit files will be unreadable
#'    by these routines and excluding them will be necessary
#' @param gpxexcludes list of substrings within a file
#'    name which will cause the file to be ignored.  occasionally .fit files will
#'    be unreadable by the routines in this package
#' @param rebuild.all.fit if TRUE, will read all files in subdirectories
#'    of \eqn{fitrootdir}, but not in the directory itself
#' @param rebuild.all.gpx if TRUE, will read all files in subdirectories
#'    of \eqn{gpxrootdir}, but not in the directory itself
#' @param prefer.gpx vector of strings identifying times to use a .gpx file
#'    rather than the corresponding .fit file
#' @param drawprofile create a .png file in the output directory profiling
#'    the ride
#' @param drawprofile.both create a .png file in the output directory profiling
#'    the ride for both the .fit files and .gpx files
#' @param rgl if TRUE use rgl to draw 3D track in viewer
#' @param drawmap create a .tiff file in the output directory mapping
#'    the ride with speed-varying color
#' @param maptype map to use as background (\code{"maptoolkit-topo"} or
#'       \code{"bing"} or \code{"osm"} or \code{"stamen-terrain"} or
#'       \code{"esri-topo"}  or \code{"stamen-watercolor"} or \code{"mapbox"} or
#'       \code{"esri"} or \code{"osm-public-transport"} or \code{"opencyclemap"}
#'       or \code{"apple-iphoto"} or \code{"skobbler"})
#' @param drawmap.both create a .tiff file in the output directory mapping
#'    the ride with speed-varying color for both the .fit files and the .gox files
#' @param cores numer of cores (default is #CPUs - 1)
#' @param loud display summary of re/segmenting
#' @param usefitdc use package fitdc to read fit files instead of fitparse
#' @param ... parameters passed for track cleaning, output production
#'
#' @return a tbl contining the combined summaries and tracks, with preference
#'   given to data from .fit files over the corresponding .gpx file, with specified
#'   exceptions
#'
#' @seealso \code{\link{read_ride}}
#'
#' @export
update_gps_variables <- function(outdir,fitrootdir,gpxrootdir=NA,merge.files=list(c(NA,NA)),
                  fitexcludes=c("bad","short"),gpxexcludes=c("bad","short","nosegs"),prefer.gpx=c(""),
                  rebuild.all.fit=FALSE,rebuild.all.gpx=FALSE,
                  drawprofile=TRUE,drawprofile.both=FALSE,
                  rgl=FALSE,
                  maptype="bing",
                  drawmap=TRUE,drawmap.both=FALSE,
                  cores=4,loud=FALSE,usefitdc=FALSE,...) {

  nomap <- maptype=="none"
  num_drawn <- 0
  num_mapped <- 0
  old_wd <- getwd()
  on.exit(setwd(old_wd))
  setwd(fitrootdir)
  if(missing(cores)) cores <- parallel::detectCores()
  if (is.na(cores)) cores <- 1

  if (rebuild.all.fit) {
    sink(file=paste0(outdir,"/gps_readfitfiles.txt"),split=TRUE)
    fl <- list.files(pattern=".fit",recursive=TRUE)
    if (!(fitexcludes[1]=="")) {
      for (x in fitexcludes) {
        fl <- fl[grep(x,fl,invert=TRUE)]
      }
    }
    fitlist <- rideReadGPS::read_ridefiles(fl,cores=cores,loud=loud,usefitdc=usefitdc,...)
    fitsummary <- fitlist[["summaries"]]
    fittracks <- dplyr::arrange(fitlist[["tracks"]],startbutton.date,timestamp.s)
    newfitfiles <- basename(fl)
    if (!missing(merge.files)&!is.na(merge.files[[1]][1])) {
      temp <- join_ridefiles(merge.files,fitsummary,fittracks)
      if (temp[[1]]) {
        fitsummary <- temp[[3]]
        fittracks <- temp[[4]]
        newfitfiles <- setdiff(newfitfiles,temp[[2]])
      }
    }
    sink()
  } else {
    newfitfiles <- list.files(pattern=".fit",recursive=FALSE)
    load(paste0(outdir,"/fitsummary.rda"))
    load(paste0(outdir,"/fittracks.rda"))
    libtz <- lubridate::tz(fittracks$timestamp.s)
    if (!(fitexcludes[1]=="")) {
      for (x in fitexcludes) {
        newfitfiles <- newfitfiles[grep(x,newfitfiles,invert=TRUE)]
      }
    }
    if (length(newfitfiles)>0) {
      newfitlist <- rideReadGPS::read_ridefiles(newfitfiles,cores=cores,
                                   loud=loud,usefitdc=usefitdc,tz=libtz,...)
      newfitresults <- newfitlist[["summaries"]]
      newfittracks <- newfitlist[["tracks"]]
      newfiles <- newfitresults$sourcefile
      if (!missing(merge.files)&!is.na(merge.files[[1]][1])) {
        temp <- join_ridefiles(merge.files,newfitresults,newfittracks)
        if (temp[[1]]) {
          newfitresults <- temp[[3]]
          newfittracks <- temp[[4]]
          newfiles <- setdiff(newfiles,temp[[2]])
        }
      }
      fitpair <- combine_track_stores(primary_sums=newfitresults,
                                      primary_tracks=newfittracks,
                                      secondary_sums=fitsummary,
                                      secondary_tracks=fittracks)
      fitsummary <- fitpair[["summaries"]]
      fittracks <- fitpair[["tracks"]]

      for (ridefn in basename(newfiles)) {
        idate <- fitsummary[fitsummary$sourcefile==ridefn,]$startbutton.date
        itime <- fitsummary[fitsummary$sourcefile==ridefn,]$startbutton.time
        if (drawprofile) {
          rideProfile::rideProfile(fittracks[fittracks$startbutton.date==idate&
                                             fittracks$startbutton.time==itime,],
                                   fitsummary[fitsummary$sourcefile==ridefn,],
                                   savefn=paste0(outdir,"/",ridefn,"profile.pdf"),
                                   ...)
          num_drawn <- 1
        }
        if (drawmap) {
          if (nomap) {
            outfile <- "none"
          } else {
            outfile <- paste0(outdir,"/",ridefn,"map.jpg")
          }
          map_rides(fittracks[fittracks$startbutton.date==idate&
                              fittracks$startbutton.time==itime,],
                    draw.speed=TRUE,minTiles=200,
                    outfile=outfile,mapsize=c(3840,2400),
                    rgl=rgl,
                    speed.color="speedcolors",maptype=maptype,
                    ...)
           num_mapped <- 1
        }
      }
    }
  }
  if (length(newfitfiles)>0) save(fitsummary,file=paste0(outdir,"/fitsummary.rda"))
  if (length(newfitfiles)>0) save(fittracks,file=paste0(outdir,"/fittracks.rda"))

  if (!is.na(gpxrootdir)) {
    setwd(gpxrootdir)
    if (rebuild.all.gpx) {
      sink(file=paste0(outdir,"/gps_readgpxfiles.txt"),split=TRUE)
      fl <- list.files(pattern=".gpx",recursive=TRUE)
      if (!(gpxexcludes[1]=="")) {
        for (x in gpxexcludes) {
          fl <- fl[grep(x,fl,invert=TRUE)]
        }
      }
      gpxlist <- rideReadGPS::read_ridefiles(fl,cores=cores,loud=loud,usefitdc=usefitdc,...)
      gpxsummary <- gpxlist[["summaries"]]
      gpxtracks <- dplyr::arrange(gpxlist[["tracks"]],timestamp.s)
      newgpxfiles <- basename(fl)
      if (!missing(merge.files)&!is.na(merge.files[[1]][1])) {
        temp <- join_ridefiles(merge.files,gpxsummary,gpxtracks)
        if (temp[[1]]) {
          gpxsummary <- temp[[3]]
          gpxtracks <- temp[[4]]
          newgpxfiles <- setdiff(newgpxfiles,temp[[2]])
        }
      }
      sink()
    } else {
      newgpxfiles <- list.files(pattern=".gpx",recursive=FALSE)
      load(paste0(outdir,"/gpxsummary.rda"))
      load(paste0(outdir,"/gpxtracks.rda"))
      libtz <- lubridate::tz(gpxtracks$timestamp.s)
      if (!(gpxexcludes[1]=="")) {
        for (x in gpxexcludes) {
        newgpxfiles <- newgpxfiles[grep(x,newgpxfiles,invert=TRUE)]
         }
      }
      if (length(newgpxfiles)>0) {
        newgpxlist <- rideReadGPS::read_ridefiles(newgpxfiles,cores=cores,
                                     loud=loud,usefitdc=usefitdc,tz=libtz,...)
        newgpxresults <- newgpxlist[["summaries"]]
        newgpxtracks <- newgpxlist[["tracks"]]
        newfiles <- newgpxresults$sourcefile
        if (!missing(merge.files)&!is.na(merge.files[[1]][1])) {
          temp <- join_ridefiles(merge.files,newgpxresults,newgpxtracks)
          if (temp[[1]]) {
            newgpxresults <- temp[[3]]
            newgpxtracks <- temp[[4]]
            newfiles <- setdiff(newfiles,temp[[2]])
          }
        }
        gpxpair <- combine_track_stores(primary_sums=newgpxresults,
                                        primary_tracks=newgpxtracks,
                                        secondary_sums=gpxsummary,
                                        secondary_tracks=gpxtracks)
        gpxsummary <- gpxpair[["summaries"]]
        gpxtracks <- gpxpair[["tracks"]]
        for (ridefn in basename(newfiles)) {
          idate <- gpxsummary[gpxsummary$sourcefile==ridefn,]$startbutton.date
          itime <- gpxsummary[gpxsummary$sourcefile==ridefn,]$startbutton.time
          if (drawprofile &(num_drawn==0 | drawprofile.both))
            rideProfile::rideProfile(gpxtracks[gpxtracks$startbutton.date==idate&
                                               gpxtracks$startbutton.time==itime,],
                                     gpxsummary[gpxsummary$sourcefile==ridefn,],
                                     savefn=paste0(outdir,"/",ridefn,"profile.png"),
                                     ...)
          if ((drawmap) &(num_mapped==0 | drawmap.both)) {
            if (nomap) {
              outfile <- "none"
            } else {
              outfile <- paste0(outdir,"/",ridefn,"map.jpg")
            }
            p<-map_rides(gpxtracks[gpxtracks$startbutton.date==idate&
                                   gpxtracks$startbutton.time==itime,],
                         outfile=outfile,mapsize=c(1920,1200),maptype=maptype,
                         rgl=rgl,
                         draw.speed=TRUE,speed.color="magma",
                         ...)
          }
        }
      }
    }
    if (length(newgpxfiles)>0) save(gpxsummary,file=paste0(outdir,"/gpxsummary.rda"))
    if (length(newgpxfiles)>0) save(gpxtracks,file=paste0(outdir,"/gpxtracks.rda"))

    if (length(newgpxfiles)+length(newfitfiles)> 0) {
      if (length(newfitfiles)==0) load(paste0(outdir,"/fitsummary.rda"))
      if (length(newfitfiles)==0) load(paste0(outdir,"/fittracks.rda"))
      if (length(newgpxfiles)==0) load(paste0(outdir,"/gpxsummary.rda"))
      if (length(newgpxfiles)==0) load(paste0(outdir,"/gpxtracks.rda"))
      mergeddata <- combine_track_stores(primary_sums=fitsummary,primary_tracks=fittracks,
                                         secondary_sums=gpxsummary,secondary_tracks=gpxtracks,
                                         prefer_secondary=prefer.gpx)
      gpssummary <- mergeddata[[1]]
      gpstracks <- mergeddata[[2]]
      save(gpssummary,file=paste0(outdir,"/gpssummary.rda"))
      save(gpstracks,file=paste0(outdir,"/gpstracks.rda"))
    } else {
      load(paste0(outdir,"/gpssummary.rda"))
    }
  } else {
    gpssummary <- fitsummary
  }
  return(gpssummary)
}
#' join multiple ride segments which come from separate files
#'
#' \code{join_ridefiles} Join two or more ride segments which come from
#'   separate files, since binary .fit files are not amenable to easy
#'   editing.  The merged data is in the updated entries in the
#'   (summary,track) data pair.  The joined data cannot be
#'   exported as a fit file, sorry.
#'
#' @param joinlist list of vectors (order matters within vectors) of .fit
#'    or .gpx filenames, each vector a part of a track to be joined in order
#' @param summaries tbl of ride summaries created from gpx and/or fit files
#' @param tracks tbl of ride tracks created from gpx and/or fit files
#'
#' @return a list of two tbls containing the summaries and tracks with the
#'    specified sets of tracks merged
#'
#' @seealso \code{\link{read_ride}}
#'
#' @export
join_ridefiles <- function(joinlist,summaries,tracks) {
  #  joinlist - a list of vectors of sourcefile names to join
  #  summaries - tbl of ridesummaries created from gpx and/or fit files
  #  tracks - tbl of tracks consisting of timestamp.s,position_lat.dd,position_lon.dd,starttime,segment plus anything else
  values.from.first <- c("date","start.time","start.hour","sourcefile","processed.time",
                         "startbutton.date","startbutton.time","low.gear","low.gear2")
  values.to.add <- c("nwaypoints","numsegs","distance","total.time","rolling.time","pedal.time",
                     "ascent","descent","distance.ascending","distance.descending","stops.subminute","stops.1to10minutes",
                     "stops.10to30minutes","stops.long","session.distance","session.elapsed.time","session.timer.time",
                     "session.pedal.strokes","session.total.calories","session.total.ascent","session.total.descent",
                     "session.distance")
  values.special <- c("startline.time","avgcadence.nozeros","avgcadence.withzeros",
                      "speed.rolling.m.s","speed.all.m.s","speed.max.m.s",
                      "session.max.speed","session.max.hr","session.max.power","ride.loop",
                      "deltaElev","begEndGap")
  values.weighted.mean.time <- c("avgcadence.midsegment","pct.trkpts.hr","pct.trkpts.cad",
                                 "session.avg.speed","session.avg.cadence",
                                 "session.avg.hr", "session.avg.power","pct.low.gear",
                                 "pct.low.gear2")
  values.from.last <- c("hr.at.stop","hr.recovery")

  dropped.files <- "zzz"
  changed <- FALSE
  for (ridevec in joinlist) {
    if (length(ridevec) < 2) stop("cannot join fewer than 2 tracks")
    rides <- dplyr::arrange(summaries[summaries$sourcefile %in% ridevec,],start.time)
    if (nrow(rides)>0) {
      changed <- TRUE
      cat("joining ride files: \n")
      cat(ridevec,"\n")
      dropped.files <- c(dropped.files,ridevec[-1])
      summary.joined <- rides[1,]
      for (varname in values.from.last) {
        summary.joined[1,varname] <- rides[nrow(rides),varname]
      }
      for (varname in values.to.add){
        summary.joined[1,varname] <- sum(rides[,varname],na.rm=T)
      }
      for (varname in values.weighted.mean.time){
        if (any(!is.na(rides[,varname]))) {
          summary.joined[1,varname] <-
                stats::weighted.mean(unlist(rides[,varname]),
                                     unlist(rides[,"rolling.time"]),na.rm=TRUE)
        } else {
          summary.joined[1,varname] <- NA
        }
      }
      #startline time comes from first (already put there by copy), others are added into breaks
      summary.joined[1,"stops.subminute"] <- summary.joined[1,"stops.subminute"] +
            sum( rides[-1,"startline.time"] < 60 )
      summary.joined[1,"stops.1to10minutes"] <- summary.joined[1,"stops.1to10minutes"] +
            sum( rides[-1,"startline.time"] >= 60 & rides[-1,"startline.time"] < 600 )
      summary.joined[1,"stops.10to30minutes"] <- summary.joined[1,"stops.10to30minutes"] +
            sum( rides[-1,"startline.time"] >= 600 & rides[-1,"startline.time"] < 1800 )
      summary.joined[1,"stops.long"] <- summary.joined[1,"stops.1to10minutes"] +
            sum( rides[-1,"startline.time"] >= 1800 )
      pedal.strokes <- sum(rides$avgcadence.nozeros*rides$pedal.time/60)
      summary.joined$avgcadence.nozeros <- 60*pedal.strokes/summary.joined$pedal.time
      summary.joined$avgcadence.withzeros <- 60*pedal.strokes/summary.joined$rolling.time
      summary.joined$speed.rolling.m.s <- summary.joined$distance/summary.joined$rolling.time
      summary.joined$speed.all.m.s <- summary.joined$distance/summary.joined$total.time
      summary.joined$speed.max.m.s <- max(rides$speed.max.m.s,na.rm=T)
      summary.joined$session.max.speed <- max(rides$session.max.speed,na.rm=T)
      if (any(is.na(rides$session.max.hr))) {
        summary.joined$session.max.hr <- max(rides$session.max.hr,na.rm=T)
      } else {
        summary.joined$session.max.hr <- NA
      }
      if (!all(is.na(rides$session.max.power))) {
        summary.joined$session.max.power <- max(rides$session.max.power,na.rm=T)
      } else {
        summary.joined$session.max.power <- NA
      }
      datetimestr.sum <- dateTimeStr(rides$startbutton.date,rides$startbutton.time)
      track <- dplyr::arrange(tracks[dateTimeStr(tracks$startbutton.date,tracks$startbutton.time) %in% datetimestr.sum,],
                        startbutton.date,timestamp.s)
      newfile <- track$startbutton.time != lag_one(track$startbutton.time) | track$startbutton.date != lag_one(track$startbutton.date)
      newseg <- (track$segment != lag_one(track$segment)) | newfile
      priordistance <- rep(0,nrow(track))
      priordistance[newfile] <- lag_one(track$distance.m)[newfile]
      track$segment <- 1 + cumsum(newseg)
      track$distance.m <- track$distance.m + cumsum(priordistance)
      if (summary.joined$numsegs[1] != max(track$segment)) {
        print(max(track$segment))
        print(summary.joined)
        stop("incorrect segment count after join")
      }
      if (!is.na(track$position_lon.dd[1])){
        begEndGap <- raster::pointDistance(cbind(track$position_lon.dd[1],
                                                 track$position_lat.dd[1]),
                                           cbind(track$position_lon.dd[nrow(track)],
                                                 track$position_lat.dd[nrow(track)]),
                                           lonlat=TRUE)
        if (begEndGap>100) {
          cat("  *non-loop - distance between start and end = ",begEndGap,"m \n")
          cat("   start = ",track$position_lon.dd[1],"  ",
              track$position_lat.dd[1],"  \n")
          cat("   stop  = ",track$position_lon.dd[nrow(track)],"  ",
              track$position_lat.dd[nrow(track)],"\n")
          rideLoop <- FALSE
        } else {
          rideLoop <- TRUE
        }
      } else {
        begEndGap <- NA
        rideLoop <- NA
      }
      summary.joined$ride.loop <- rideLoop
      summary.joined$begEndGap <- begEndGap
      summary.joined$deltaElev <- track$altitude.m[nrow(track)] -
                                  track$altitude.m[1]
      track$startbutton.date <- track$startbutton.date[1]
      track$startbutton.time <- track$startbutton.time[1]
      tracks <- dplyr::bind_rows(tracks[!(dateTimeStr(tracks$startbutton.date,tracks$startbutton.time) %in% datetimestr.sum),],
                                 track) %>%
        dplyr::arrange(startbutton.date,timestamp.s)
      summaries <- dplyr::bind_rows(summaries[!summaries$sourcefile %in% ridevec,],summary.joined[1,]) %>%
        dplyr::arrange(start.time)
    }
  }
  return(list(changed,dropped.files[-1],summaries,tracks))
}
#' combine two (summary,track) tbl pairs
#'
#' \code{combine_track_stores} Combine two (summary,track) data pairs.  When both
#'   sets have data for the same date and time, the first is used unless overidden.
#'
#' Combine two (summary,track) tbl pairs,using data
#'   the first of the pairs for rides starting at the same time on the same day,
#'   with exceptions specifiable.  Typically this is to prefer data from a .fit
#'   file over data from a .gpx file corresponding to the same ride.
#'
#' @param primary_sums  tbl of ride summaries created from gpx and/or fit files
#' @param primary_tracks  tbl of ride tracks created from gpx and/or fit files
#' @param secondary_sums  tbl of ride summaries created from gpx and/or fit files
#' @param secondary_tracks  tbl of ride tracks created from gpx and/or fit files
#' @param prefer_secondary vector of strings specifying times to use data from
#'    secondary pair even if present in primary.  format is "YYYYMMDDHHMMSS",
#'    where YYY is the 4 digit year, MM is the 2 digit month, DD is the 2 digit
#'    day of month, HH is the 2 digit (0-23) hour of day, MM is 2 digit minutes
#'    and SS is 2 digit seconds
#'
#' @return a list of two tbls containing the combined summaries and tracks
#'
#' @seealso \code{\link{read_ride}}
#'
#' @export
combine_track_stores <- function(primary_sums,primary_tracks,
                                 secondary_sums,secondary_tracks,
                                   prefer_secondary=""){
  tzpri <- lubridate::tz(primary_tracks$timestamp.s)
  tzsec <- lubridate::tz(secondary_tracks$timestamp.s)
  if (tzpri != tzsec)
    stop("cannot combine tracks and summaries with different timezones")

  #every entry in track tbls should have a corresponding summary entry
  pri.on.times <- unique(dateTimeStr(primary_sums$startbutton.date,
                                     primary_sums$startbutton.time))
  sec.on.times <- unique(dateTimeStr(secondary_sums$startbutton.date,
                                     secondary_sums$startbutton.time))
  #  specify rides to keep based on time startbutton turned on
  #     (from filename so gpx and fit should match)
  if (nrow(primary_tracks[!dateTimeStr(primary_tracks$startbutton.date,
                                       primary_tracks$startbutton.time) %in%
                           pri.on.times,1])>0) {
    cat("** track dataframe track not in primary summary, starting time \n",
        setdiff(unique(dateTimeStr(primary_tracks$startbutton.date,
                                   primary_tracks$startbutton.time)),
                pri.on.times))
  }
  if (nrow(secondary_tracks[!dateTimeStr(secondary_tracks$startbutton.date,
                                         secondary_tracks$startbutton.time) %in%
                            sec.on.times,1])>0) {
    cat("** track dataframe track not in secondary summary, start time ",
        setdiff(unique(dateTimeStr(secondary_tracks$startbutton.date,
                                   secondary_tracks$startbutton.time)),
                sec.on.times),"\n")
  }
  frompri <- pri.on.times
  if (!missing(prefer_secondary))
    frompri <- setdiff(frompri,intersect(sec.on.times,prefer_secondary))
  fromsec <- setdiff(sec.on.times,frompri)
  summaries <-
    dplyr::arrange(
      dplyr::bind_rows(
        primary_sums[dateTimeStr(primary_sums$startbutton.date,
                                 primary_sums$startbutton.time) %in%
                     frompri,],
        secondary_sums[dateTimeStr(secondary_sums$startbutton.date,
                                   secondary_sums$startbutton.time) %in%
                     fromsec,]),
      start.time)
  sourcefiles <- unique(summaries$sourcefile)
#temppt <<- primary_tracks[dateTimeStr(primary_tracks$startbutton.date,
#                                  primary_tracks$startbutton.time) %in%
#                          frompri,]
#tempst <<- secondary_tracks[dateTimeStr(secondary_tracks$startbutton.date,
#                                    secondary_tracks$startbutton.time) %in%
#                          fromsec,]
  excludenames <- c("left_right_balance.")
  ptracks <- primary_tracks[dateTimeStr(primary_tracks$startbutton.date,
                                        primary_tracks$startbutton.time) %in%
                              frompri,]
  stracks <- secondary_tracks[dateTimeStr(secondary_tracks$startbutton.date,
                                          secondary_tracks$startbutton.time) %in%
                              fromsec,]
  if (any(names(ptracks) %in% excludenames))
    ptracks <- ptracks[,-which(names(ptracks) %in% excludenames)]
  if (any(names(stracks) %in% excludenames))
    stracks <- stracks[,-which(names(stracks) %in% excludenames)]
  tracks <- dplyr::arrange(dplyr::bind_rows(ptracks,stracks),
                           startbutton.date,timestamp.s)
return(list(summaries=summaries,tracks=tracks))
}
