#' Import, process, and export data from Argos downloads
#'
#' Import, process, and export data from Argos downloads
#'
#' @param UPLOAD logical; if TRUE, then an existing, formatted file
#'   housed at the path specified (line 13) is already available.
#'   Only the newest files will be appended to the existing data set.
#'   If FALSE, the analyst must specify a date (lines 43-46, below) and all
#'   downloads from the ARGOS server that occurred after the specified date
#'   will be imported and formatted
#' @param FORMAT.ARGOS logical
#' @param SPEED.FILTER logical
#' @param argos.csv.path character; the path to the Argos data
#' @param argos.processed.file character; the file that contains the data to import
#'   TODO clarify this
#' @param export.file character; the file to which to export the data
#' @param log.file character; the PTT log file
#' @param tz character; time zone to pass to datetime functions. Default is 'GMT'
#'
#' @details
#' todo
#'
#' @return
#' A data frame with the processed Argos data
#'
#' @export
import_argos <- function(UPLOAD=FALSE, FORMAT.ARGOS=TRUE, SPEED.FILTER=TRUE,
                         argos.csv.path, argos.processed.file, export.file, log.file,
                         tz = "GMT") {
  # #.
  # # function to import data from ARGOS downloads housed in the directory specified in the path call
  # # if UPLOAD is TRUE, then an existing, formated file housed at the path specified (line 13) is already available. Only the newest files will be appendeded to the existing data set.
  # # if UPLOAD is FALSE, the analyst must specify a date (lines 43-46, below) and all downloads from the ARGOS server that occurred after the specified date will be imported and formatted
  # # The final full data set will be exported to a local directory specified below (Line).
  # #
  # #
  # # set the path to the ARGOS data
  # directory<-"//superba/Overwinter_foraging/Argos_csv"
  # #directory<-"c:/users/jefferson.hinke/desktop/argos/data/hope bay/Argos_csv"
  # #
  # # set the path for where the data to import is housed
  # data.path<-"c:/users/jefferson.hinke/desktop/argos/trackdata.csv"
  # print(data.path)
  # #data.path<-"c:/users/jefferson.hinke/desktop/argos/data/hope bay/hopebaydata.csv"
  #
  # #
  # #set the path for exporting the final data
  # export.path<-"c:/users/jefferson.hinke/desktop/argos/trackdata_updated.csv"
  # #export.path<-"c:/users/jefferson.hinke/desktop/argos/data/hope bay/hopebaydata.csv"
  #
  # #
  # # set the path to where the PTT log and deployment information is housed.
  # log.path<-"c:/users/jefferson.hinke/desktop/argos/PTTlog.csv"
  # #log.path<-"c:/users/jefferson.hinke/desktop/argos/data/hope bay/PTTlog_HopeBay.csv"


  # temporary solution
  directory <- argos.csv.path
  data.path <- argos.processed.file
  export.path <- export.file
  log.path <- log.file

  # import the most recent full data set if specified and ensure proper formating of time variable
  if(UPLOAD){
    last.upload<-read.table(data.path, sep=",", header=TRUE, stringsAsFactors=FALSE)
    #
    # try a number of conversion formats on the Date --- one should work
    tt.date<-as.POSIXct(last.upload$Date, "%m/%d/%Y %H:%M", tz = tz)
    if(all(is.na(tt.date))){
      tt.date<-as.POSIXct(last.upload$Date, "%Y/%m/%d %H:%M", tz = tz)
    }
    if(all(is.na(tt.date))){
      tt.date<-as.POSIXct(last.upload$Date, "%Y-%m-%d %H:%M", tz = tz)
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
    last.upload.yr<-1980
    last.upload.m<-1
    last.upload.d<-1
    last.date<-ISOdate(last.upload.yr, last.upload.m, last.upload.d, tz = tz)
    message("USING USER DEFINED DATE TO INITIATE DATA UPLOAD AND FORMAT.",
            "\nTHE USER SPECIFIED DATE IS: ", last.date, " ", tz,
            ".\nCONTACT PACKAGE AUTHORS IF THIS DATE IS INCORRECT.\n")
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
  available.dates<-ISOdate(files.yr, files.m, files.d, tz = tz)
  tt.available<-available.dates>last.date
  available.dates<-na.omit(available.dates[tt.available])
  #
  # the dformat indicates a date when the format of the argos csv files changed.
  # further below, when 'scan' is called, there is a different ordering of elements to ensure the correct import of data pre and post the dformat date.
  #
  dformat<-ISOdate(2016, 12, 1)
  available.files<-na.omit(tt.files[tt.available])
  n.files<-length(available.files)
  message(n.files, " FILE(S) AVAILABLE FOR UPLOAD BASED ON LAST UPLOAD DATE (",
          last.date,") PROVIDED\n")

  if(n.files==0){
    if (UPLOAD){
      last.upload$FieldYearEnd<-as.numeric(last.upload$FieldYearEnd)
      last.upload$Vmax<-as.numeric(last.upload$Vmax)
      return(last.upload)
    }
    stop("No files, upload and processing terminated")
  }

  if(FORMAT.ARGOS){
    # this section will prepare the deployment log for appending meta data to the raw track data
    ptt.table <- read.csv(file=log.path,header=TRUE,stringsAsFactors=FALSE, na.strings="NA") %>%
      rename(Deployment_orig = Deployment) %>%
      filter(!is.na(DeployDate)) %>%
      mutate(DeployTime = if_else(is.na(DeployTime), "00:00:00", DeployTime),
             depUTC = as.POSIXct(paste(DeployDate, DeployTime),
                                 format="%m/%d/%Y %H:%M", tz = tz),
             retUTC = as.POSIXct(paste(RetrievalDate, RetrievalTime),
                                 format="%m/%d/%Y %H:%M", tz = tz),
             Deployment = paste(PTT, Species, Study, Stage, Deploy, Site, FieldYear,
                                sep = "|"))

    # Check for mismatches
    dep.mis <- ptt.table %>% filter(Deployment != Deployment_orig)
    if (nrow(dep.mis) > 0) {
      warning("The following 'Deployment' values appear to have been ",
              "created incorrectly in the CSV file ",
              "(note these are the original values in the CSV file):\n",
              paste(dep.mis$Deployment_orig, collapse = "\n"),
              "\n", immediate. = TRUE)
    }
    rm(dep.mis)

    # ptt.table<-read.csv(file=log.path,header=TRUE,stringsAsFactors=FALSE, na.strings="NA")
    #
    # # provide a default deploy time for any record with a missing deployment time
    # new.time<-ifelse(is.na(ptt.table$DeployTime), "12:00:00 AM", ptt.table$DeployTime)
    # ptt.table$DeployTime<-new.time
    # #
    # # exclude any ptt records that do not have a deployment date recorded
    # exclude.missing.dates<-!is.na(ptt.table$DeployDate)
    # ptt.table<-ptt.table[exclude.missing.dates,]
    # #
    # # convert separate date and time columns to a single date format suitable for comparisons
    # depUTC<-paste(ptt.table$DeployDate,ptt.table$DeployTime,sep=" ")
    # retUTC<-paste(ptt.table$RetrievalDate,ptt.table$RetrievalTime,sep=" ")
    # #
    # # set time (UTC) using POSIX standards
    # ptt.table$depUTC<-as.POSIXct(depUTC,format="%m/%d/%Y %H:%M",tz = tz)
    # ptt.table$retUTC<-as.POSIXct(retUTC,format="%m/%d/%Y %H:%M",tz = tz)
  }

  # import each available file into a list - to be appended to existing data frame after all available files are imported and formatted.
  imports<-list(n.files)
  for(ii in 1:n.files){
    # message(ii)
    #
    # import a dataset
    import.path<-paste(directory,"/", available.files[ii], sep="")
    #message(import.path)
    if(available.dates[ii]<dformat){
      tt.data<-scan(import.path, skip=1,sep=",", nlines=0,
                    what=list("numeric", "character", "character", "numeric","numeric", "character", "character", "character", "character"), flush=TRUE)
      tag.data<-data.frame(
        Tag=as.numeric(unlist(tt.data[[1]])),
        Latitude=as.numeric(unlist(tt.data[[4]])), Longitude=as.numeric(unlist(tt.data[[5]])),
        Loc.Qual=unlist(tt.data[[8]]), Date=unlist(tt.data[[9]])
      )
    } else {
      # ARGOS downloads after Dec 1 2016 use a new format that requires a slightly different specification of column identifiers
      tt.data<-scan(import.path, skip=1,sep=",", nlines=0,
                    what=list("numeric", "character", "character", "character", "character", "character", "character", "character", "character", "numeric","numeric", "character", "character", "character", "character"), flush=TRUE)
      tag.data<-data.frame(
        Tag=as.numeric(unlist(tt.data[[1]])),
        Latitude=as.numeric(unlist(tt.data[[11]])), Longitude=as.numeric(unlist(tt.data[[10]])),
        Loc.Qual=unlist(tt.data[[15]]), Date=unlist(tt.data[[9]])
      )
      #message(summary(tag.data))
    }

    #tag.data<-data.frame(Tag=as.numeric(unlist(tt.data[[1]])), Latitude=as.numeric(unlist(tt.data[[4]])), Longitude=as.numeric(unlist(tt.data[[5]])), Loc.Qual=unlist(tt.data[[8]]), Date=unlist(tt.data[[9]]))
    if(dim(na.omit(tag.data))[1]>0) {
      # remove any records without a Date, Latitude, or Longitude record
      # remove duplicate rows
      tag.data <- tag.data %>%
        filter(Date != "", Latitude != "", Longitude != "") %>%
        group_by(Tag, Latitude, Longitude, Loc.Qual, Date) %>%
        distinct() %>%
        ungroup()


      # Return the Date field to a time
      tag.data$Date<-as.character(tag.data$Date)
      tt.Date<-as.POSIXct(tag.data$Date,format="%Y/%m/%d %H:%M",tz = tz)
      if(is.na(tt.Date[1])){
        rm(tt.Date)
        tt.Date<-as.POSIXct(tag.data$Date, format="%m/%d/%Y %H:%M", tz = tz) # alternative time format if default format does not work
      }
      if(is.na(tt.Date[1])){
        rm(tt.Date)
        tt.Date<-as.POSIXct(tag.data$Date, format="%Y-%m-%d %H:%M", tz = tz) # alternative time format if default format does not work
      }
      tag.data$Date<-tt.Date


      # Verbosely remove any Tag numbers not in the PTT log
      tag.data.notag <- tag.data %>% filter(!(Tag %in% ptt.table$PTT))

      if (nrow(tag.data.notag) > 0) {
        warning(nrow(tag.data.notag), " data points for the following ",
                "Tag number(s) from the raw Argos data file '",
                basename(import.path),
                "' have been removed because these tag number(s) ",
                "are not present in the PTT log:\n",
                paste(sort(unique(tag.data.notag$Tag)), collapse = ", "), "\n",
                immediate. = TRUE)

        tag.data <- tag.data %>% filter(Tag %in% ptt.table$PTT)
      }


      # optionally add identifying information to each record for SPP, SITE, and STUDY (ie. Brood, Creche, Overwinter, etc, as from PTT logs using the format.argos() function
      if(FORMAT.ARGOS){
        # format data, and add deployment identifier
        formated.data <- format_argos(tt=tag.data, ptt.table, iter=ii) %>%
          mutate(Deployment = paste(Tag, Spp, Study, Stage, Deploy, Site, FieldYearEnd,
                                    sep = "|"))

        # check to make sure all dates in the resulting import are OK.
        # if not, identify the file which leads to bad dates
        date.check<-as.numeric(strftime(formated.data$Date, format="%Y")) # pulls out the year
        message("Current file:\n", available.files[ii])
        # message("Unique dates:\n", unique(date.check))
        imports[[ii]]<-formated.data
      } else {
        #
        # return the tag data without formating
        imports[[ii]]<-tag.data
      }
    } else {
      #
      # if there is no data in the imported file, note that here.
      warning("No data in the file: ", import.path, immediate. = TRUE)
      #tag.data<-rep(NA, 5) # for any data set that had 0 records in the AROGS_csv directory
    }
  }
  #
  # rbind each list component
  imports<-do.call("rbind", imports)
  imports$Keep<-NA
  # message("Summary of processed data:")
  # print(summary(imports))
  deployments <- unique(imports$Deployment)
  message("Deployments:\n", paste(sort(deployments), collapse = "\n"))
  #
  # append imported data to existing data set
  if(UPLOAD){
    message("Speed filter will be run only on deployments with new data based on this upload cycle.")
    #last.two<-c(dim(last.upload)[2]-1, dim(last.upload)[2])
    #imports<-rbind(last.upload[,-last.two], imports)
    imports<-rbind(last.upload, imports)
  }
  #
  # remove any duplicated records resulting from the newest imports - for some reason there's a couple minute difference in some records
  # Columns used :Tag, Lat, Long, Date
  tt.dupes<-paste(imports[,1],imports[,2],imports[,3],imports[,5],sep="|")
  imports<-imports[!duplicated(tt.dupes),]
  # print("here")
  # write.csv(imports, export.path)
  #
  # as a last step, sort the data to be ordered by Tag and Date
  imports<-imports[order(imports$Tag, imports$Date),]
  #
  if(SPEED.FILTER){
    # require(argosfilter)
    message("Running the speed filter. This takes awhile. Be patient. Go get a delicious beverage.")
    if(UPLOAD){
      # create an index to separate full deployments from those with new data based on latest upload
      all.dep.keep<-imports$Deployment %in% deployments
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





