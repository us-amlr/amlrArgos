speed_filter<-function(tag.data=imported.data, ptt.table, deployments){
  library(argosfilter)
  #
  # Function to apply a speed filter to each deployment in the dataset imported from import.argos()
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
    print(deployments[i])
    dp.data<-tag.data[tag.data$Deployment==deployments[i],]
    #
    # check to see if only one VMAX is specified for each deployment
    #
    if(length(unique(dp.data$Vmax))>1){
      print(paste("There are multiple VMAX values associated with the following deployment: ", deployments[i], sep=""))
      stop()
    } else {
      VMAX<-dp.data$Vmax[1]
      if(is.na(VMAX)){
        VMAX<-2.5
        print(paste("MISSING DEPLOYMENT INFO FOR ",deployments[i],". ASSUMING 2.5m/s VMAX for the time being.", sep=""))
        # assign the Vmax value for each deployment if there is only one value for this deployment
      }
    }
    #
    # remove any records for which there is an NA due to subscripting done above
    dp.data<-dp.data[!is.na(dp.data$Date),]
    if(dim(dp.data)[1]>4){
      # the speed filter requires 5 or more points to run because the function can't be applied to the first and last 2 points. 
      dp.filter<-sdafilter(lat=dp.data$Latitude, lon=dp.data$Longitude, dtime=dp.data$Date, lc=dp.data$Loc.Qual, vmax=VMAX, ang=c(15,25), distlim=c(2500,5000))
      #dp.filter<-vmask(lat=dp.data$Latitude, lon=dp.data$Longitude, dtime=dp.data$Date, vmax=VMAX)  # alternative filter in argosfilter library
      dp.data$Keep<-dp.filter=="not"
      out[[i]]<-dp.data
    } else { 
      print(paste("To few records for ", deployments[i], " to be filtered. N must be >4. Keep set to NA", sep=""))
      ticker<-ticker+1
      excluded[ticker]<-deployments[i]
      dp.data$Keep<-rep(NA, dim(dp.data)[1])
      out[[i]]<-dp.data         
    }
  }
  filtered.tracks<-do.call("rbind", out)
  print(excluded)
  #
  # return the filtered ARGOS data to R for further evaluation
  filtered.tracks
} 

