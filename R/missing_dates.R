#' This function detect missing dates in a data.frame timeserie and fill with rows containing NAs
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
#'  missing_dates(DATA = mydata ,DATETIME_HEADER = "TIMESTAMP", RECORD_HEADER = "Record", DATETIME_SAMPLING = "15 min")
#'  missing_dates(DATA = your data.frame ,DATETIME_HEADER = "your datetime header", RECORD_HEADER = "your datetime record", DATETIME_SAMPLING = "your datetime sampling")


missing_dates = function(DATA ,DATETIME_HEADER = DATETIME_HEADER, RECORD_HEADER = RECORD_HEADER, DATETIME_SAMPLING = DATETIME_SAMPLING){
  
  start_date = DATA[1,which(colnames(DATA) == DATETIME_HEADER)]
  end_date = DATA[nrow(DATA),which(colnames(DATA) == DATETIME_HEADER)]
  
  all_dates = seq(from = start_date, to = end_date, by = DATETIME_SAMPLING) # create sequence of datetime from star to end every time sampling (15 min)
  
  all_dates_df =  data.frame(matrix(nrow =length(all_dates), ncol = ncol(DATA)))
  colnames(all_dates_df) = colnames(DATA)
  
  time = DATA[,which(colnames(DATA) == DATETIME_HEADER)]
  time_new = all_dates
  time_added = time_new[which(!(time_new %in% time))]
  
  zoo_DATA = zoo(DATA,order.by = time)
  
  zoo_NEW = zoo(all_dates_df,order.by = time_new)
  
  merge = merge(zoo_DATA,zoo_NEW)
  options(stringsAsFactors = F)
  
  merge = merge[,1:ncol(DATA)]
  colnames(merge)=colnames(DATA)

  w2= which(time_new %in% time_added)
  merge[w2,which(colnames(merge)==RECORD_HEADER)] = -1
  
  df_merge = as.data.frame(merge)
  df_merge = df_merge[,-which(colnames(merge) == DATETIME_HEADER)]
  df_merge = cbind(time_new, df_merge)
  colnames(df_merge)[1] = DATETIME_HEADER
  
  df_merge = df_merge[colnames(DATA)]
  
  for(i in which(colnames(df_merge)!= DATETIME_HEADER)){
    df_merge[,i] = as.numeric(df_merge[,i])
  }
  
  missing = data.frame(w2,df_merge[w2,which(colnames(df_merge) == DATETIME_HEADER)])
  colnames(missing) = c("Index", "Date")
  return(list(df_merge,missing)
}
