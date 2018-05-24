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
  
  hhh = which(sign(record_diff-1) != 0)
  
  dd = matrix(nrow = length(hhh),ncol = 6)
  colnames(dd) = c("Datetime_From", "Datetime_To","Datetime_Missing","Record_From", "Record_To", "Record_Missing")
  dd = as.data.frame(dd)
  
  if(length(hhh) != 0){
    
    d_st = data_new[hhh-1,which(colnames(data) == DATETIME_HEADER)]
    d_en = data_new[hhh,which(colnames(data) == DATETIME_HEADER)]
    r_st = data_new[hhh-1,which(colnames(data) == RECORD_HEADER)]
    r_en = data_new[hhh,which(colnames(data) == RECORD_HEADER)]
    # dd = matrix(nrow = length(hhh),ncol = 6)
    # colnames(dd) = c("Datetime_From", "Datetime_To","Datetime_Missing","Record_From", "Record_To", "Record_Missing")
    # dd = as.data.frame(dd)
    
    for(i in 1:length(hhh)){
      dd[i,1] = format(d_st[i],format = DATETIME_FORMAT)
      dd[i,2] = format(d_en[i],format = DATETIME_FORMAT)   
      dd[i,3] = length(seq(from = d_st[i],to = d_en[i],by = DATETIME_SAMPLING))-1
      
      dd[i,4] = format(r_st[i])
      dd[i,5] = format(r_en[i])
      dd[i,6] = data_new[hhh[i],which(colnames(data) == RECORD_HEADER)]-data_new[hhh[i]-1,which(colnames(data) == RECORD_HEADER)]
    }
    flag_missing_records = 1
  } else{
    flag_missing_records = 0
  }
  
  # if(length(hhh) == 0){
  #   flag_missing_records = 0 
  # } else{
  #   flag_missing_records = 1
  # }
  # 
  
  l = list(flag_missing_records,dd)
  names(l) = c("flag_missing_records", "data_table_missing_records")
  return(l)
} 
