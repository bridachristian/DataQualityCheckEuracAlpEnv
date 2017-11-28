# DATA -> is a dataframe having a column defined as Posixct
# DATETIME_HEADER -> is the header corresponding to datetime (defined as Posixct)
# RECORD_HEADER -> is the header corresponding to Record
# DATETIME_SAMPLING -> is the time sampling (e.g. "15 min", "hour". See seq.POSIXt {base} on website) 

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
  
  return(df_merge)
}
