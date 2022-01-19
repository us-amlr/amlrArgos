import_argos<-function(UPLOAD=FALSE, FORMAT.ARGOS=TRUE, SPEED.FILTER=TRUE){
  #.
  # function to import data from ARGOS downloads housed in the directory specified in the path call
  # if UPLOAD is TRUE, then an existing, formated file housed at the path specified (line 13) is already available. Only the newest files will be appendeded to the existing data set.
  # if UPLOAD is FALSE, the analyst must specify a date (lines 43-46, below) and all downloads from the ARGOS server that occurred after the specified date will be imported and formatted
  # The final full data set will be exported to a local directory specified below (Line).
  #
  # 
  # set the path to the ARGOS data
  directory<-"//superba/Overwinter_foraging/Argos_csv"
  #directory<-"c:/users/jefferson.hinke/desktop/argos/data/hope bay/Argos_csv"
  #
  # set the path for where the data to import is housed
  data.path<-"c:/users/jefferson.hinke/desktop/argos/trackdata.csv"
  print(data.path)
  #data.path<-"c:/users/jefferson.hinke/desktop/argos/data/hope bay/hopebaydata.csv"
  
  #
  #set the path for exporting the final data
  export.path<-"c:/users/jefferson.hinke/desktop/argos/trackdata_updated.csv"
  #export.path<-"c:/users/jefferson.hinke/desktop/argos/data/hope bay/hopebaydata.csv"
  
  #
  # set the path to where the PTT log and deployment information is housed.
  log.path<-"c:/users/jefferson.hinke/desktop/argos/PTTlog.csv"
  #log.path<-"c:/users/jefferson.hinke/desktop/argos/data/hope bay/PTTlog_HopeBay.csv"
  #
  # import the most recent full data set if specified and ensure proper formating of time variable
  if(UPLOAD){
    last.upload<-read.table(data.path, sep=",", header=TRUE, stringsAsFactors=FALSE)
    #
    # try a number of conversion formats on the Date --- one should work
    tt.date<-as.POSIXct(last.upload$Date, "%m/%d/%Y %H:%M", tz="GMT")
    if(all(is.na(tt.date))){
      tt.date<-as.POSIXct(last.upload$Date, "%Y/%m/%d %H:%M", tz="GMT")
    }
    if(all(is.na(tt.date))){
      tt.date<-as.POSIXct(last.upload$Date, "%Y-%m-%d %H:%M", tz="GMT")
    }
    last.upload$Date<-tt.date
    last.date<-max(last.upload$Date, na.rm=TRUE)
    # lose the first column and any rows filled with NA
    n1<-names(last.upload)
    if(n1[1]!="Tag"){
      last.upload<-last.upload[,-1]  # lose the first column if a row name vector
    }
    last.upload<-last.upload[!is.na(last.upload$Tag),] # lose records where Tag is listed as NA
  } else {
    #
    #this can either be pulled as the max date from an existing dataframe or set in the code
    print("USING USER DEFINED DATE TO INITIATE DATA UPLOAD AND FORMAT")
    last.upload.yr<-1980
    last.upload.m<-1
    last.upload.d<-1
    last.date<-ISOdate(last.upload.yr, last.upload.m, last.upload.d, tz="GMT")
    print(paste("THE USER SPECIFIED DATE IS ", last.date, ". SET NEW DATES IF THIS DATE IS INCORRECT (LINES 43-46 IN CODE).", sep=""))
  }
  #
  # figure out which files in the ARGOS directory are new
  tt.files<-list.files(path=directory)
  tt.files1<-unlist(strsplit(tt.files, split=".csv"))
  tt.files1<-strsplit(tt.files1, split="_")
  files.yr<-unlist(lapply(tt.files1, function(x){as.numeric(x[2])}))
  files.m<-unlist(lapply(tt.files1, function(x){as.numeric(x[3])}))
  files.d<-unlist(lapply(tt.files1, function(x){as.numeric(x[4])}))
  #
  # now compare available files with the most recent date to only import more recent files
  available.dates<-ISOdate(files.yr, files.m, files.d, tz="GMT")
  tt.available<-available.dates>last.date
  available.dates<-na.omit(available.dates[tt.available])
  #
  # the dformat indicates a date when the format of the argos csv files changed. 
  # further below, when 'scan' is called, there is a different ordering of elements to ensure the correct import of data pre and post the dformat date.
  #
  dformat<-ISOdate(2016, 12, 1)
  available.files<-na.omit(tt.files[tt.available])
  n.files<-length(available.files)
  print(paste(n.files, " FILES AVAILABLE FOR UPLOAD BASED ON LAST UPLOAD DATE (", last.date,") PROVIDED", sep=""))
  if(n.files==0){
    print("Upload terminated")
    if (UPLOAD){
      last.upload$FieldYearEnd<-as.numeric(last.upload$FieldYearEnd)
      last.upload$Vmax<-as.numeric(last.upload$Vmax)
      return(last.upload)
    }
    stop()
  }
  if(FORMAT.ARGOS){
    # this section will prepare the deployment log for appenidng meta data to the raw track data
    ptt.table<-read.csv(file=log.path,header=TRUE,sep=",",stringsAsFactors=FALSE, na.strings="NA")
    # 
    # provide a default deploy time for any record with a missing deployment time
    new.time<-ifelse(is.na(ptt.table$DeployTime), "12:00:00 AM", ptt.table$DeployTime)
    ptt.table$DeployTime<-new.time
    #
    # exclude any ptt records that do not have a deployment date recorded
    exclude.missing.dates<-!is.na(ptt.table$DeployDate)
    ptt.table<-ptt.table[exclude.missing.dates,]
    #
    # convert separate date and time columns to a single date format suitable for comparisons
    depUTC<-paste(ptt.table$DeployDate,ptt.table$DeployTime,sep=" ")
    retUTC<-paste(ptt.table$RetrievalDate,ptt.table$RetrievalTime,sep=" ")
    #
    # set time (UTC) using POSIX standards
    ptt.table$depUTC<-as.POSIXct(depUTC,format="%m/%d/%Y %H:%M",tz="GMT")
    ptt.table$retUTC<-as.POSIXct(retUTC,format="%m/%d/%Y %H:%M",tz="GMT")
    # 
  }
  #
  # import each available file into a list - to be appended to existing data frame after all available files are imported and formatted.
  imports<-list(n.files)
  for(ii in 1:n.files){
    print(ii)
    #
    # import a dataset
    import.path<-paste(directory,"/", available.files[ii], sep="")
    #print(import.path)
    if(available.dates[ii]<dformat){
      tt.data<-scan(import.path, skip=1,sep=",", nlines=0, what=list("numeric", "character", "character", "numeric","numeric", "character", "character", "character", "character"), flush=TRUE)
      tag.data<-data.frame(Tag=as.numeric(unlist(tt.data[[1]])), Latitude=as.numeric(unlist(tt.data[[4]])), Longitude=as.numeric(unlist(tt.data[[5]])), Loc.Qual=unlist(tt.data[[8]]), Date=unlist(tt.data[[9]]))
    } else {
      # ARGOS downloads after Dec 1 2016 use a new format that requires a slightly different specification of column identifiers
      tt.data<-scan(import.path, skip=1,sep=",", nlines=0, what=list("numeric", "character", "character", "character", "character", "character", "character", "character", "character", "numeric","numeric", "character", "character", "character", "character"), flush=TRUE)
      tag.data<-data.frame(Tag=as.numeric(unlist(tt.data[[1]])), Latitude=as.numeric(unlist(tt.data[[11]])), Longitude=as.numeric(unlist(tt.data[[10]])), Loc.Qual=unlist(tt.data[[15]]), Date=unlist(tt.data[[9]]))
      #print(summary(tag.data))
    }
    #tag.data<-data.frame(Tag=as.numeric(unlist(tt.data[[1]])), Latitude=as.numeric(unlist(tt.data[[4]])), Longitude=as.numeric(unlist(tt.data[[5]])), Loc.Qual=unlist(tt.data[[8]]), Date=unlist(tt.data[[9]]))
    if(dim(na.omit(tag.data))[1]>0){
      #
      # remove any records without a Date record now
      date.missing<-tag.data$Date!=""
      tag.data<-tag.data[date.missing,]
      #
      # remove any records without a Latitude record now
      lat.missing<-tag.data$Latitude!=""
      tag.data<-tag.data[lat.missing,]
      # remove any records without a Longitude record now
      long.missing<-tag.data$Longitude!=""
      tag.data<-tag.data[long.missing,]
      #
      # remove NA reows
      tag.data<-na.omit(tag.data)
      #
      # remove duplicated records
      tt.pasted<-paste(tag.data[,1],tag.data[,2],tag.data[,3],tag.data[,4],tag.data[,5],sep="|")  # create a character vector with tag, lat, long,loc.quality, and time-stamp
      tag.data<-tag.data[!duplicated(tt.pasted),]   # keep only rows that are not duplicates
      #
      # Return the Date field to a time 
      tag.data$Date<-as.character(tag.data$Date)
      tt.Date<-as.POSIXct(tag.data$Date,format="%Y/%m/%d %H:%M",tz="GMT")
      if(is.na(tt.Date[1])){
        rm(tt.Date)
        tt.Date<-as.POSIXct(tag.data$Date, format="%m/%d/%Y %H:%M", tz="GMT") # alternative time format if default format does not work
      }
      if(is.na(tt.Date[1])){
        rm(tt.Date)
        tt.Date<-as.POSIXct(tag.data$Date, format="%Y-%m-%d %H:%M", tz="GMT") # alternative time format if default format does not work
      }
      tag.data$Date<-tt.Date
      
      #
      # optionally add identifying information to each record for SPP, SITE, and STUDY (ie. Brood, Creche, Overwinter, etc, as from PTT logs using the format.argos() function
      if(FORMAT.ARGOS){
        #
        #
        formated.data<-format_argos(tt=tag.data, ptt.table, iter=ii)
        # add deployment identifier
        deployment<-paste(formated.data$Tag, formated.data$Spp, formated.data$Study, formated.data$Stage, formated.data$Deploy, formated.data$Site, formated.data$FieldYearEnd, sep="|")
        formated.data$Deployment<-deployment
        #deployments<-unique(deployment)
        # check to make sure all dates in the resulting import are OK.
        # if not, identify the file which leads to bad dates
        date.check<-as.numeric(strftime(formated.data$Date, format="%Y")) # pulls out the year
        print(unique(date.check))
        print(available.files[ii])
        imports[[ii]]<-formated.data
      } else {
        #
        # return the tag data without formating
        imports[[ii]]<-tag.data
      }
    } else {
      #
      # if there is no data in the imported file, note that here.
      print(paste("No data in the file: ", import.path, sep=""))
      #tag.data<-rep(NA, 5) # for any data set that had 0 records in the AROGS_csv directory
    }
  }
  #
  # rbind each list component
  imports<-do.call("rbind", imports)
  imports$Keep<-NA
  print(summary(imports))
  deployments<-unique(imports$Deployment)
  print(deployments)
  #
  # append imported data to existing data set
  if(UPLOAD){
    print("Speed filter will be run only on deployments with new data based on this upload cycle.")
    #last.two<-c(dim(last.upload)[2]-1, dim(last.upload)[2]) 
    #imports<-rbind(last.upload[,-last.two], imports)
    imports<-rbind(last.upload, imports)
  }
  # 
  # remove any duplicated records resulting from the newest imports - for some reason there's a couple minute difference in some records
  # Columns used :Tag, Lat, Long, Date
  tt.dupes<-paste(imports[,1],imports[,2],imports[,3],imports[,5],sep="|")
  imports<-imports[!duplicated(tt.dupes),]
  print("here")
  write.csv(imports, export.path)
  #
  # as a last step, sort the data to be ordered by Tag and Date
  imports<-imports[order(imports$Tag, imports$Date),]
  #
  if(SPEED.FILTER){
    require(argosfilter)
    print("Running the speed filter. This takes awhile. Be patient. Go get a delicious beverage.")
    if(UPLOAD){
      # create an index to separate full deployments from those with new data based on latest upload
      all.dep.keep<-imports$Deployment%in%deployments
      # 
      # now pull out all deployments with new data 
      new.imports<-imports[all.dep.keep,]
      full.imports<-imports[!all.dep.keep,]
      # send deployments with new data to the speed filter if required
      new.imports<-speed_filter(tag.data=new.imports, ptt.table, deployments=deployments)
      #combine full and new imports again
      imports<-rbind(full.imports, new.imports)
    } else {
      # run speed filter on all entire data set
      imports<-speed_filter(tag.data=imports, ptt.table, deployments=deployments)
    }
  }
  
  imports<-imports[order(imports$Tag, imports$Date),]
  #
  # export the new dataset for use later- this will over write any existing data inthe data.path
  write.csv(imports, export.path)
  #
  # return the imported and appended data set to R for further evaluation
  imports$FieldYearEnd<-as.numeric(imports$FieldYearEnd)
  imports$Vmax<-as.numeric(imports$Vmax) 
  imports
  # end of file
}





