#' Apply a speed filter to each deployment in the Argos dataset
#'
#' Apply a speed filter to each deployment in the Argos dataset
#'
#' @param tag.data data frame; the raw(ish) data from the ARGOS download
#' @param ptt.table data frame;
#'   file specified by \code{log.file} in \code{\link{import_argos}}
#' @param deployments character; the unique deployment codes
#'
#' @details
#' This function uses the R package \code{argosfilter} to filter the speeds for each tag
#' \code{\link[argosfilter]{sdafilter}} removes Z codes and keeps all other positions that meet a
#' threshold speed criteria set in the function call,
#' but note that it requires 5 or more points to run.
#' Tracks with fewer than five points will be set to NA.
#'
#' Note that this function will mark points as 'to remove' if
#' \code{\link[argosfilter]{sdafilter}} returns 'end_location'.
#' See the Value section of \code{\link[argosfilter]{sdafilter}} for more details.
#'
#' @return
#' A data frame without the filtered Argos data
#'
#' @export
speed_filter<-function(tag.data, ptt.table, deployments){
  # library(argosfilter)
  #
  #
  # Create a vector of unique deployments for each tag based on Tag, Spp, Study, Site, and FieldYearEnd (ie.e, 2012 for the 2011-2012 field season).
  #deployment<-paste(tag.data$Tag, tag.data$Spp, tag.data$Study, tag.data$Stage, tag.data$Deploy, tag.data$Site, tag.data$FieldYearEnd, sep="|")
  #tag.data$Deployment<-deployment
  #deployments<-unique(deployment)
  n.deployments<-length(deployments)
  #
  # Apply the speed filter only to deployments that have new data based on the
  #
  # use R package argosfilter to filter the speeds for each tag.
  # this package removes Z codes, and keeps all other positions that meet a threshold speed criteria set in the function call
  out<-list(n.deployments)
  ticker<-0
  excluded<-numeric()
  for(i in 1:n.deployments){
    # print(deployments[i])
    dp.data<-tag.data[tag.data$Deployment==deployments[i],]
    #
    # check to see if only one VMAX is specified for each deployment
    #
    if(length(unique(dp.data$Vmax))>1){
      stop("There are multiple VMAX values associated with the following deployment: ",
           deployments[i])
    } else {
      VMAX<-dp.data$Vmax[1]
      if(is.na(VMAX)){
        VMAX<-2.5
        warning("MISSING DEPLOYMENT INFO FOR ", deployments[i],
                ". ASSUMING 2.5m/s VMAX for the time being.",
                immediate. = TRUE)
        # assign the Vmax value for each deployment if there is only one value for this deployment
      }
    }
    #
    # remove any records for which there is an NA due to subscripting done above
    dp.data<-dp.data[!is.na(dp.data$Date),]
    if(dim(dp.data)[1]>4){
      # the speed filter requires 5 or more points to run because the function can't be applied to the first and last 2 points.
      dp.filter<-argosfilter::sdafilter(
        lat=dp.data$Latitude, lon=dp.data$Longitude, dtime=dp.data$Date,
        lc=dp.data$Loc.Qual, vmax=VMAX, ang=c(15,25), distlim=c(2500,5000)
      )
      #dp.filter<-vmask(lat=dp.data$Latitude, lon=dp.data$Longitude, dtime=dp.data$Date, vmax=VMAX)  # alternative filter in argosfilter library
      dp.data$Keep<-dp.filter=="not"
      out[[i]]<-dp.data
    } else {
      warning("Too few records for ", deployments[i], " to be filtered. ",
              "N must be >4. Keep set to NA",
              immediate. = TRUE)
      ticker<-ticker+1
      excluded[ticker]<-deployments[i]
      dp.data$Keep<-rep(NA, dim(dp.data)[1])
      out[[i]]<-dp.data
    }
  }
  filtered.tracks<-do.call("rbind", out)
  message("Exlcuded deployments: ", excluded)

  # return the filtered ARGOS data to R for further evaluation
  filtered.tracks
}

