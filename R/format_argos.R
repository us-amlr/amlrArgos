format_argos<-function(tt=tag.data, ptt.table, iter){
  #
  # function to add specific information to the ARGOS data set produced by the import.argos() function. 
  # This will only append information to records in the individual files that are newly imported by import.argos (i.e. not any data already imported or stored locally).
  #
  # pull out the deployment data for the tags that are in the tag data and pass into the assign.info() function.
  deployed<-ptt.table[!is.na(match(ptt.table$PTT, unique(tt$Tag))),]
  #
  # run the code to process the data
  new.out<-assign_info(tt.dat=tt, tt.deployed=deployed, ITER=iter)
  #
  # pass the formated data back to import.argos() for final export
  new.out
}