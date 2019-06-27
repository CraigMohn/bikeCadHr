read_fittrack <- function(fitfile,usefitdc,createSegs=FALSE) {

  requiredVars <- c("altitude.m", "distance.m", "heart_rate.bpm", "speed.m.s",
                    "timestamp.s", "cadence.rpm", "power.watts")
  if (usefitdc) {
    dflist <- read_fitdc(fitfile, requiredVars=requiredVars)
  } else {
    dflist <- read_fitPython(fitfile, requiredVars=requiredVars)
  }
  records <- dflist[["records"]]
  session <- dflist[["session"]]
  events <- dflist[["events"]]
  records$timestamp.s <- as.POSIXct(records$timestamp.s,tz="UTC",origin='1989-12-31')
  events$timestamp.s <- as.POSIXct(events$timestamp.s,tz="UTC",origin='1989-12-31')


  #  drop records with no distance measure, they are beyond salvage
  records <- records[!is.na(records$distance.m),]
  #  put in check that assumption of 1 record per timestamp holds...
  #  add auto start/stop if GPS not set to auto on/off, but only if lat/lon
  if ((length(which(events$timer_trigger == "auto"))==0) & createSegs &
      ("position_lat.semicircles" %in% colnames(records))){

    seg.start <- (records$distance.m == lag_one(records$distance.m)) &
                 (records$distance.m != lead_one(records$distance.m))
    seg.stop <- c(seg.start[-1],TRUE)

    timestamp.s <- records$timestamp.s[seg.start]
    event <- rep("timer",length(timestamp.s))
    event_type <- rep("start",length(timestamp.s))
    timer_trigger <- rep("auto",length(timestamp.s))
    event_group <- rep(0,length(timestamp.s))
    data <- rep(NA,length(timestamp.s))
    new.starts <- data.frame(timestamp.s,timer_trigger,event,event_type,event_group,data)
    timestamp.s <- records$timestamp.s[seg.stop]
    event <- rep("timer",length(timestamp.s))
    event_type <- rep("stop_all",length(timestamp.s))
    timer_trigger <- rep("auto",length(timestamp.s))
    event_group <- rep(0,length(timestamp.s))
    data <- rep(NA,length(timestamp.s))
    new.stops <- data.frame(timestamp.s,timer_trigger,event,event_type,event_group,data)

    events <- dplyr::arrange(rbind(events,rbind(new.starts,new.stops)),timestamp.s)
  }
  recovery_hr <- events[events$event == "recovery_hr",]
  recovery_hr <- recovery_hr[,c("timestamp.s","data")]

  #############################################################################
  # clean up events file to handle unusual power-on/off sequences
  #   delete/change affected events data
  #   generally do not touch records data except for too-early and too-late obs
  events <- events[events$event %in% c("timer","power_down","power_up"),]
  events <- dplyr::arrange(left_join(events,records,by="timestamp.s"),
                           timestamp.s,event,event_type)
  # drop events and records before any early (< 10m,<5Min) power-off-power-on pairs
  power.on.event <- events$event == "power_up" &
                    (lag_n(events$event,1) == "power_down" |
                     lag_n(events$event,2) == "power_down") &
                    cumsum(ifelse(is.na(events$distance.m),
                                  0,events$distance.m)) < 10 &
                    difftime(as.POSIXct(events$timestamp.s,tz="UTC",origin='1989-12-31'),
                             as.POSIXct(events$timestamp.s[1],tz="UTC",origin='1989-12-31'),
                             units="secs") < 300

  if (sum(power.on.event,na.rm=TRUE)>0){
    last.power.on <- max(which(power.on.event))
    first.time <- events$timestamp.s[last.power.on]
    events <- events[events$timestamp.s>first.time,]
    records <- records[records$timestamp.s>first.time,]
  }
  #  remove manual stops and starts in track
  #  assume auto stop without speed is an event immediately after
  #       manual start during pause, drop it
  astop.nospeed <- (events$timer_trigger == "auto" &
                    !is.na(events$timer_trigger))   &
                   events$event_type == "stop_all"  &
                   is.na(events$speed.m.s)
  events <- events[!astop.nospeed,]
  # remove second and third of power_down=manual.stop_all,power_up,manual.start
  event.seq.beg <-  events$event=="power_down" &
                    !is.na(lead_one(events$timer_trigger)) &
                    lead_one(events$timer_trigger)=="manual" &
                    lead_one(events$event_type)=="stop_all" &
                    !is.na(lead_n(events$event,2)) &
                    lead_n(events$event,2)=="power_up" &
                    lead_n(events$event_type,3)=="start" &
                    events$timestamp.s==lead_one(events$timestamp.s)
  drop.powerdown <- lag_one(event.seq.beg)
  drop.powerup <- lag_one(drop.powerdown)
  events <- events[!(drop.powerdown|drop.powerup),]
  # remove first,second and third of manual.stop_all,power_down,power_up,
  #             manual.start if preceding event was a stop
  event.seq.beg <-  !is.na(events$timer_trigger) &
                    events$timer_trigger=="manual" &
                    events$event_type == "stop_all" &
                    lead_one(events$event)=="power_down" &
                    lead_n(events$event,2)=="power_up" &
                    lead_n(events$event_type,3)=="start" &
                    lag_one(events$event_type)=="stop_all"
  drop.powerdown <- lag_one(event.seq.beg)
  drop.powerup <- lag_one(drop.powerdown)
  events <- events[!(event.seq.beg|drop.powerdown|drop.powerup),]
  # if timer_trigger is missing and power_up+stop_all is followed by
  #   power_down+stop_all then power_up+stop_all, delete last pair
  # manual stop which immediately follows: a stop or
  #      precedes a stop with the same timestamp and follows a start
  mstop.delete <- events$timer_trigger == "manual" &
                  events$event_type == "stop_all" &
                  (lag_one(events$event_type) == "stop_all" |
                   (lead_one(events$event_type) == "stop_all" &
                    lead_one(events$timestamp.s) == events$timestamp.s &
                    !is.na(lead_n(events$timestamp.s,1)) &
                    lag_one(events$event_type) == "start"))
  ## manual start which precedes an auto start a stop with the same timestamp
  mstart.delete <-  events$timer_trigger == "manual" &
                    events$event_type == "start" &
                    (lead_one(events$event_type) == "start" |
                     (lag_one(events$event_type) == "stop_all" &
                      lag_one(events$timestamp.s) == events$timestamp.s))
  events <- events[!(mstop.delete | mstart.delete),]

  last.start <- max(which(events$event_type == "start"))
  if (length(events$event_type) > last.start) {
    if (events$event_type[last.start+1] != "stop_all")
      stop("fitfile problem - event after last start not a stop")
    events <- events[(1:(last.start+1)),]
  }
  #   drop events where distance is missing (those before and after
  #          collected location data) keep last start if needed
  drop.events <- is.na(events$distance.m)
  drop.events <- drop.events &
                 !(events$timer_trigger == "manual" &
                   events$event_type == "start" &
                   !lead_one(drop.events) &
                   lead_one(events$event_type) == "stop_all") &
                 !(events$event=="power_down" | events$event=="power_up")
  events <- events[!drop.events,]

  segment.start.times <- events$timestamp.s[events$event_type == "start"]
  segment.end.times <- events$timestamp.s[events$event_type == "stop_all"]
  nsegments <- length(segment.start.times)

  records$segment <- NA
  for(seg in 1:nsegments) {
    records$segment[(records$timestamp.s>=segment.start.times[seg])&(records$timestamp.s<=segment.end.times[seg])] <- seg
  }
  ## stick everything after the end in last seg, dump everything before beginning
  records$segment[(records$timestamp.s>segment.end.times[nsegments])] <- nsegments
  records <- records[records$timestamp.s>=segment.start.times[1],]
  ##  snip off any short very delayed final records
  if (nsegments > 1) {
    if (sum(records$segment==nsegments)<3 &
       (as.numeric(segment.start.times[nsegments])-as.numeric(segment.end.times[nsegments-1])>240)){
      records <- records[records$segment!=nsegments,]
      nsegments <- nsegments-1
      segment.start.times <- segment.start.times[1:nsegments]
      segment.end.times <- segment.end.times[1:nsegments]
    }
  }
  ## and remove any empty segments (can arise from unit lockups, power cycling, sensor failure)
  ## note that nsegments is not updated, since it isn't used again.  be forwarned...
  emptyseg <- NULL
  for(seg in 1:nsegments) {
    if (sum(records$segment==seg)==0) emptyseg <- c(seg,emptyseg) # reversed order is important
  }
  if (length(emptyseg) > 0) {
    for(seg in emptyseg) {
      records$segment[records$segment>seg] <- records$segment[records$segment>seg]-1
    }
  }

#records$timestamp.s <- as.POSIXct(records$timestamp.s,tz="UTC",origin='1989-12-31')
  records <- dplyr::arrange(records[!is.na(records$segment),],timestamp.s)
  if ("position_lat.semicircles" %in% colnames(records)) {
    records$position_lat.dd <- records$position_lat.semicircles*( 180 / 2^31 )
    records$position_lon.dd <- records$position_long.semicircles*( 180 / 2^31 )
  } else {
    records$position_lat.dd <- NA
    records$position_lon.dd <- NA
  }

  records <- records[,!(names(records) %in% c("position_lat.semicircles","position_long.semicircles"))]
  if (nrow(recovery_hr)>0) {
    recovery_hr$heart_rate.at.stop <- records$heart_rate.bpm[nrow(records)]
    names(recovery_hr) <- gsub("data","heart_rate.postride",names(recovery_hr))
    hrdrop <- recovery_hr$heart_rate.at.stop - recovery_hr$heart_rate.postride
    cat("  ** hr at stop = ",recovery_hr$heart_rate.at.stop,
        "   hr after 2 min = ",recovery_hr$heart_rate.postride,
        "   change = ",hrdrop,"\n")
  }
  return(list(track=records,recovery_hr=recovery_hr,session=session))
}
merge_lists <- function(ls_part, ls_full) {
  extra <- setdiff(names(ls_full), names(ls_part))
  as_data_frame(append(ls_part, ls_full[extra])[names(ls_full)])  # order as well
}
format_record <- function(record) {
  out <- record$fields
  units <- record$units
  # get rid of the units if hr or cadence aren't there
  if (!"cadence"%in%names(out)) {
    if (length(which(units=="rpm"))>0) units <- units[-which(units=="rpm")]
  }
  if (!"heart_rate"%in%names(out)) {
    if (length(which(units=="bpm"))>0) units <- units[-which(units=="bpm")]
  }
  names(out) <- paste(names(out), units, sep = ".")
  out
}
format_event <- function(event) {
  out <- event$fields
  if (!"event_group"%in%names(out)) {
    #  this is a cheap and dirty fix, we won't use the variable but it screws up processing
    #  could use fitfilerepair utility, but why bother.
    if (out$event == "recovery_hr") {
      #cat("  *****recovery_hr timer event in fit file. HR=",out$data,"\n")
    } else if (out$event == "hr_high_alert") {
      cat("  *****hr high alert event in fit file. HR=",out$data,"\n")
    } else if (out$event == "course_point") {
      #cat("*****course point event in fit file.\n")
    } else {
      cat(" *****missing event_group in fit file!\n")
      print(event)
    }
    out$event_group <- 0
  }
  if (!"event_group"%in%names(out)) {
    out$data <- 0
  }
  names(out) <- paste(names(out), event$units, sep = ".")
  return(out)
}
format_session <- function(session) {
  out <- session$fields
  #  units should be obvious, don't append them
  return(out)
}

read_fitdc <- function(fitfile,requiredVars) {
  #  this code is a modified and repurposed version of the scripts posted in
  #  various websites and in the examples with the fitdc package, which does the
  #  actual hard work of reading the binary fit file.
  data_mesgs <- fitdc::read_fit(fitfile)

  ## msg types: "file_id","file_creator","event","device_info","unknown","record","lap","session","activity"
  ## Filter out the record and event messages, the session summary:
  is_record <- function(mesg) mesg$name == "record"
  is_event <- function(mesg) (mesg$name == "event")
  is_session <- function(mesg) (mesg$name == "session")

  records <- Filter(is_record, data_mesgs)
  records <- lapply(records, format_record)
  ## Some records have missing fields:
  colnames_full <- names(records[[which.max(lengths(records))]])
  empty <- stats::setNames(as.list(rep(NA, length(colnames_full))),colnames_full)
  records <- dplyr::bind_rows(lapply(records, merge_lists, empty))
  if (!"cadence.rpm" %in% colnames(records)) records$cadence.rpm <- NA
  if (!"cadence.rpm" %in% colnames(records)) records$cadence.rpm <- NA
  if (!"heart_rate.bpm" %in% colnames(records)) records$heart_rate.bpm <- NA
  if (!"power.watts" %in% colnames(records)) records$power.watts <- NA
  colnames(records) <- gsub("m/s","m.s",colnames(records))

  events <- Filter(is_event, data_mesgs)
  events <- lapply(events, format_event)
  ## Some records have missing fields:
  #colnames_full <- names(events[[which.max(lengths(events))]])
  #  try this since few relatively few events compared to records, slow but avoids warnings...
  colnames_full <- unique(unlist(lapply(events,names)))
  empty <- stats::setNames(as.list(rep(NA, length(colnames_full))),colnames_full)
  if (!"data." %in% colnames(empty)) empty$data. <- NA
  events <- dplyr::bind_rows(lapply(events, merge_lists, empty))
  names(events) <- gsub("[.]","",names(events))
  names(events) <- gsub("timestamps","timestamp.s",names(events))

  session <- Filter(is_session, data_mesgs)
  session <- lapply(session, format_session)
  if (length(session)==1) {
    session <- as_data_frame(session[[1]])
  } else {
    print(paste0("file has ",length(session)," session records, returning NULL for session variables"))
    session <- NULL
  }
  records <- addVars(records,varvec=requiredVars)
  return(list(session=session,records=records,events=events))

}
read_fitPython <- function(fitfile,requiredVars) {

  return(fitparseR::get_fit_dfs(fitfile,checkconda=FALSE,
                                requiredVars=requiredVars))

}
addVars <- function(df,varvec)  {
  if (length(varvec) > 0)
  for (v in varvec) {
    if (! v %in% names(df)) {
      print(paste0("adding ",v," to records dataframe"))
      df[[v]] <- NA
    }
  }
  return(df)
}

