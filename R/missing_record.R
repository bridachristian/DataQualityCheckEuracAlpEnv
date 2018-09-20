#' This function detect missing record in a data.frame timeserie and fill with rows containing NAs
#'
#'  @param DATA data.frame having a column defined as POSIXct (datetime)
#'  @param DATETIME_HEADER header corresponding to datetime
#'  @param RECORD_HEADER header corresponding to record
#'  @param DATETIME_SAMPLING time sampling (e.g. "15 min", "hour". See seq.POSIXt {base} on website)
#'  @param DATETIME_FORMAT datetime format ("%Y-%m-%d %h:%m")
#'  @return a data.frame which contains a continuos timeseries
#'
#'  @export
#'
#'  @examples
#'  missing_record(DATA = mydata ,DATETIME_HEADER = "TIMESTAMP", RECORD_HEADER = "Record", DATETIME_SAMPLING = "15 min")
#'  missing_record(DATA = your data.frame ,DATETIME_HEADER = "your datetime header", RECORD_HEADER = "your datetime record", DATETIME_SAMPLING = "your datetime sampling")

missing_record = function(DATA ,DATETIME_HEADER = DATETIME_HEADER, RECORD_HEADER = RECORD_HEADER, DATETIME_SAMPLING = DATETIME_SAMPLING, DATETIME_FORMAT = DATETIME_FORMAT){
  
  data = DATA
  
  data_timestamp = data[,which(colnames(data) == DATETIME_HEADER)]
  data_record = data[,which(colnames(data) == RECORD_HEADER)]
  data_new =  data[,which(colnames(data) %in% c(RECORD_HEADER, DATETIME_HEADER))]
  
  record_diff = c(1,diff(data_record))
  
  # missing record
  miss = which(sign(record_diff-1) > 0)
  
  dataframe_miss = matrix(nrow = length(miss),ncol = 6)
  colnames(dataframe_miss) = c("Datetime_From", "Datetime_To","Datetime_Missing","Record_From", "Record_To", "Record_Missing")
  dataframe_miss = as.data.frame(dataframe_miss)
  
  if(length(miss) != 0){
    
    miss_date_start = data_new[miss-1,which(colnames(data) == DATETIME_HEADER)]
    miss_date_end = data_new[miss,which(colnames(data) == DATETIME_HEADER)]
    miss_record_start= data_new[miss-1,which(colnames(data) == RECORD_HEADER)]
    miss_record_end = data_new[miss,which(colnames(data) == RECORD_HEADER)]
    
    for(i in 1:length(miss)){
      dataframe_miss[i,1] = format(miss_date_start[i],format = DATETIME_FORMAT)
      dataframe_miss[i,2] = format(miss_date_end[i],format = DATETIME_FORMAT)   
      dataframe_miss[i,3] = length(seq(from = miss_date_start[i],to = miss_date_end[i],by = DATETIME_SAMPLING))-2
      
      dataframe_miss[i,4] = format(miss_record_start[i])
      dataframe_miss[i,5] = format(miss_record_end[i])
      dataframe_miss[i,6] = data_new[miss[i],which(colnames(data) == RECORD_HEADER)]-data_new[miss[i]-1,which(colnames(data) == RECORD_HEADER)]-1
    }
    dataframe_miss = dataframe_miss[-(dataframe_miss$Record_From == -1),]
  }
  
  # restart record
  rest = which(sign(record_diff-1) < 0)
  
  dataframe_rest = matrix(nrow = length(rest),ncol = 6)
  colnames(dataframe_rest) = c("Datetime_From", "Datetime_To","Datetime_Missing","Record_From", "Record_To", "Record_Missing")
  dataframe_rest = as.data.frame(dataframe_rest)
  dataframe_rest_new = dataframe_rest
  
  
  if(length(rest) != 0){
    
    rest_date_start = data_new[rest-1,which(colnames(data) == DATETIME_HEADER)]
    rest_date_end = data_new[rest,which(colnames(data) == DATETIME_HEADER)]
    rest_record_start= data_new[rest-1,which(colnames(data) == RECORD_HEADER)]
    rest_record_end = data_new[rest,which(colnames(data) == RECORD_HEADER)]
    
    for(j in 1:length(rest)){
      dataframe_rest[j,1] = format(rest_date_start[j],format = DATETIME_FORMAT)
      dataframe_rest[j,2] = format(rest_date_end[j],format = DATETIME_FORMAT) 
      dataframe_rest[j,3] = length(seq(from = rest_date_start[j],to = rest_date_end[j],by = DATETIME_SAMPLING))-2
      
      dataframe_rest[j,4] = format(rest_record_start[j])
      dataframe_rest[j,5] = format(rest_record_end[j])
      dataframe_rest[j,6] = data_new[rest[j],which(colnames(data) == RECORD_HEADER)]-data_new[rest[j]-1,which(colnames(data) == RECORD_HEADER)]-1
    }
    
    w = which(dataframe_rest[,3] == 0)
    
    if(length(w) != 0){
      dataframe_rest_new = dataframe_rest[-w,]
    }else{
      dataframe_rest_new = dataframe_rest
    }
    
  }
  
  if(nrow(dataframe_miss) > 0 | nrow(dataframe_rest_new) > 0){
    flag_missing_records = 1
  }else{
    flag_missing_records = 0
  }
  
  l = list(flag_missing_records,dataframe_miss, dataframe_rest_new)
  names(l) = c("flag_missing_records", "data_table_missing_records","data_table_restarted_records")
  return(l)
} 

