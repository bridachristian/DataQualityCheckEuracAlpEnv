#' This function delete duplcated data in a data.frame
#'
#' @param DATA data.frame
#' @param DATETIME_HEADER header corresponding to datetime

#'
#' @return A list containing a data.frame without dates already analyzed rows and a dataframe with dates already analyzed
#' @export
#' @examples
#' deletes_old_datetime(DATA = mydata, DATETIME_HEADER = "TIMESTAMP")
#' deletes_old_datetime(DATA = your data.frame, DATETIME_HEADER = "Your datetime header")
#'


deletes_old_datetime = function(DATA, DATETIME_HEADER = "TIMESTAMP"){
  start_date = DATA[1, which(colnames(DATA) == DATETIME_HEADER)]
  
  # w = which(DATA[,which(colnames(DATA) == DATETIME_HEADER)]< start_date)
  
  DATA <- DATA[-which(DATA[,which(colnames(DATA) == DATETIME_HEADER)]< start_date),]
  
  df_already_analyzed  = DATA[which(DATA[,which(colnames(DATA) == DATETIME_HEADER)]< start_date),]
  
  return(list(DATA,df_already_analyzed))
  
}
  
