#' This function detect missing record in a data.frame timeserie and fill with rows containing NAs
#'
#'  @param DATA data.frame having a column defined as POSIXct (datetime)
#'  @param DATETIME_HEADER header corresponding to datetime
#'  @param RECORD_HEADER header corresponding to record
#'  @param DATETIME_SAMPLING time sampling (e.g. "15 min", "hour". See seq.POSIXt {base} on website)
#'
#'  @return a data.frame which contains a continuos timeseries
#'
#'  @export
#'
#'  @examples
#'  missing_record(DATA = mydata ,DATETIME_HEADER = "TIMESTAMP", RECORD_HEADER = "Record", DATETIME_SAMPLING = "15 min")
#'  missing_record(DATA = your data.frame ,DATETIME_HEADER = "your datetime header", RECORD_HEADER = "your datetime record", DATETIME_SAMPLING = "your datetime sampling")

# TO COMPLETE!
missing_record = function(DATA ,DATETIME_HEADER = DATETIME_HEADER, RECORD_HEADER = RECORD_HEADER, DATETIME_SAMPLING = DATETIME_SAMPLING){
  
  data = DATA
 
  data_timestamp = data[,which(colnames(data) == DATETIME_HEADER)]
  data_record = data[,which(colnames(data) == RECORD_HEADER)]
  data_new =  data[,which(colnames(data) %in% c(RECORD_HEADER, DATETIME_HEADER))]
  data_new = cbind(data_new,c(1,diff(data_record)))
  
  w = which(diff(data_record) != 1)
  data_new[w,1:2]
  return(list(df_merge,missing))
}
