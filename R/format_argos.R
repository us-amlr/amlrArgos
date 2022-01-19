#' Import, process, and export data from Argos downloads
#'
#' Add specific information to the ARGOS data set produced by \code{\link{import_argos}}
#'
#' @param tt data frame; processed tag data from Argos CSV files
#' @param ptt.table data frame;
#'   file specified by \code{log.file} in \code{\link{import_argos}}
#' @param iter integer; the index of the file being processed
#'
#' @details
#' This will only append information to records in the individual files
#' that are newly imported by import_argos
#' (i.e. not any data already imported or stored locally).
#' This function pulls out the deployment data for the tags that are in the tag data,
#' and passes everything to \code{\link{assign_info}}.
#'
#' @return
#' A data frame with the formatted Argos data
#'
#' @export
format_argos<-function(tt, ptt.table, iter){
  deployed<-ptt.table[!is.na(match(ptt.table$PTT, unique(tt$Tag))),]

  # run the code to process the data
  new.out<-assign_info(tt.dat=tt, tt.deployed=deployed, iter=iter)

  new.out
}
