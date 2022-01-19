assign_info<-function(tt.dat, tt.deployed, ITER){
  #
  # This ugly pile of braces, brackets, fors, ifs, and elses will assign Spp, Site, Study, etc. designations to each record in the file for which deployment information is available.
  # tt.dat is the raw data from the ARGOS downlowd. tt.deployed is the log containing deployment information for each tag
  #It goes row by row through the giant data set, so expect it to take awhle...
  # 
  # Ready
  #
  # Set
  #
  # Go
  #
  print("Assigning info")
  # Remove any lines of data that are missing a Date
  #
  #
  #tt.deployed<-na.omit(tt.deployed)
  n.records<-dim(tt.dat)[1]
  out<-matrix(NA, ncol=9, nrow=n.records)
  #
  # Proceed according to each tag number in the data
  for(i in 1:n.records){
    deployed.tag<-tt.deployed[tt.deployed$PTT==tt.dat$Tag[i],]
    #
    # find out if the tag in the data has a deployment record
    if(dim(deployed.tag)[1]==0 |is.na(tt.dat$Date[i])){
      print(paste("Tag ", tt.dat$Tag[i], " was either not found in deployment log or the date <", tt.dat$Date[i], "> associated with a particular Argos location is invalid. Values set to NA",sep=""))
      if(dim(deployed.tag)[1]==0) print("No data")
      if(is.na(tt.dat$Date[i])) print("Invalid date")
      out[i,1:9]<-rep(NA,9)
    } else {
      #
      # If found in the log, find the appropriate deployment record from all possibilities for this tag in the tagging log
      n.deployed<-dim(deployed.tag)[1]
      index<-numeric()
      if(n.deployed>1){
        # if statement to deal with tags with multiple deployments
        #choose which deployment houses the correct info
        tt.err<-0
        #n.matches<-numeric(n.deployed) # catch-all index if all tests come up empty, then fill record with NAs
        for(j in 1:n.deployed){
          #
          # check to see if the deployment includes an overwinter deployment or missing retrieval data and whether there are later deployments that mean the earlier deployment is not the one you want
          if(is.na(deployed.tag$retUTC[j])){
            max.deploy<-max(deployed.tag$depUTC, na.rm=TRUE) # the maximum deploy date (i.e. most recent)
            if(deployed.tag$depUTC[j]==max.deploy){
              tt.match<-tt.dat$Date[i]>deployed.tag$depUTC[j] # implies that there are no other deployments of this tag,so see if the deployment in question is after the most recent deployemnt
              #n.matches[j]<-tt.match
              if(tt.match){
                out[i,1]<-deployed.tag$Species[j]
                out[i,2]<-deployed.tag$Site[j]
                out[i,3]<-deployed.tag$Study[j]
                out[i,4]<-deployed.tag$Taxa[j]
                out[i,5]<-deployed.tag$Stage[j] 
                out[i,6]<-deployed.tag$FieldYear[j]
                out[i,7]<-deployed.tag$Vmax[j]
                out[i,8]<-deployed.tag$Sex[j] 
                out[i,9]<-deployed.tag$Deploy[j]
                
              }
            } else {
              # what to do if a record is prior to the real deployment, but there are legitiate deployments later, so the record with the missing recovery should be ignored....
              tt.match<-deployed.tag$depUTC[j]!=max.deploy & tt.dat$Date[i]<deployed.tag$depUTC[j+1]
              #n.matches[j]<-tt.match
              if(tt.match){
                out[i,1]<-deployed.tag$Species[j]
                out[i,2]<-deployed.tag$Site[j]
                out[i,3]<-deployed.tag$Study[j]
                out[i,4]<-deployed.tag$Taxa[j]
                out[i,5]<-deployed.tag$Stage[j] 
                out[i,6]<-deployed.tag$FieldYear[j]
                out[i,7]<-deployed.tag$Vmax[j]
                out[i,8]<-deployed.tag$Sex[j]
                out[i,9]<-deployed.tag$Deploy[j]
              }
              # if deployment is not the correct deployment, skip to next deployment
              #print(paste("skipping deployment ", j, " for tag ", tt.dat$Tag[i], sep="")) 
            }
          } else {
            # if there is no missing recovery information, ensure the record in question falls between deployment and recovery dates in the log
            tt.match<-tt.dat$Date[i]>deployed.tag$depUTC[j] & tt.dat$Date[i]<deployed.tag$retUTC[j]
            #n.matches[j]<-tt.match
            if(tt.match){
              tt.err<-tt.err+1
              if(tt.err>1){
                # a Trap to snare potential problems in the log
                print(paste("More than two records meet date matching criteria for Tag ", tt.dat$Tag[i], " on ", tt.dat$Date[i], ". Default to NA until confirmation.", sep=""))
                out[i,1:9]<-rep(NA, 9)
              } else {
                out[i,1]<-deployed.tag$Species[j]
                out[i,2]<-deployed.tag$Site[j]
                out[i,3]<-deployed.tag$Study[j]
                out[i,4]<-deployed.tag$Taxa[j]
                out[i,5]<-deployed.tag$Stage[j] 
                out[i,6]<-deployed.tag$FieldYear[j]
                out[i,7]<-deployed.tag$Vmax[j]
                out[i,8]<-deployed.tag$Sex[j]
                out[i,9]<-deployed.tag$Deploy[j]
              }
            }
          }
        }
      } else {
        #
        # else loop for tags with only one recorded deployment in the log
        #
        # ensure the date in the data is correctly coded - this problem occurs very infrequently, but for now requires the data be ignored
        if(is.na(tt.dat$Date[i])){
          print("Bad Date found")
          out[i,1:9]<-rep(NA,9)
        } else {
          #
          # if the tag was not recovered (i.e. an over winter tag or no associated recovery date, know that
          if(is.na(deployed.tag$retUTC)){
            tt.match<-tt.dat$Date[i]>deployed.tag$depUTC
          } else {
            # ensure the only available deployment covers the available ARGOS record
            tt.match<-tt.dat$Date[i]>deployed.tag$depUTC & tt.dat$Date[i]<deployed.tag$retUTC
          }
          if(tt.match){
            out[i,1]<-deployed.tag$Species
            out[i,2]<-deployed.tag$Site
            out[i,3]<-deployed.tag$Study
            out[i,4]<-deployed.tag$Taxa
            out[i,5]<-deployed.tag$Stage 
            out[i,6]<-deployed.tag$FieldYear
            out[i,7]<-deployed.tag$Vmax
            out[i,8]<-deployed.tag$Sex
            out[i,9]<-deployed.tag$Deploy
          } else {
            out[i,1:9]<-rep(NA, 9)
          }
        }
      }
    }
  }
  out<-cbind(tt.dat, out)
  out[,6]<-as.character(out[,6])
  out[,7]<-as.character(out[,7])
  out[,8]<-as.character(out[,8])
  out[,9]<-as.character(out[,9])
  out[,10]<-as.character(out[,10])
  out[,11]<-as.numeric(as.character(out[,11]))
  out[,12]<-as.numeric(as.character(out[,12]))
  out[,13]<-as.numeric(as.character(out[,13]))
  out[,14]<-as.numeric(as.character(out[,14]))
  names(out)[6]<-"Spp"
  names(out)[7]<-"Site"
  names(out)[8]<-"Study"
  names(out)[9]<-"Taxa"
  names(out)[10]<-"Stage"
  names(out)[11]<-"FieldYearEnd"
  names(out)[12]<-"Vmax"
  names(out)[13]<-"Sex"
  names(out)[14]<-"Deploy"
  #print(str(out))
  # pass the formated data back to import.argos() for exports
  out
}