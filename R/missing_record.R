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

# TO COMPLETE!
missing_record = function(DATA ,DATETIME_HEADER = DATETIME_HEADER, RECORD_HEADER = RECORD_HEADER, DATETIME_SAMPLING = DATETIME_SAMPLING, DATETIME_FORMAT = DATETIME_FORMAT){
  
  data = DATA
  
  data_timestamp = data[,which(colnames(data) == DATETIME_HEADER)]
  data_record = data[,which(colnames(data) == RECORD_HEADER)]
  data_new =  data[,which(colnames(data) %in% c(RECORD_HEADER, DATETIME_HEADER))]
  
  record_diff = c(1,diff(data_record))
  
  hhh = which(sign(record_diff-1) != 0)
  
  d_st = data_new[hhh-1,which(colnames(data) == DATETIME_HEADER)]
  d_en = data_new[hhh,which(colnames(data) == DATETIME_HEADER)]
  r_st = data_new[hhh-1,which(colnames(data) == RECORD_HEADER)]
  r_en = data_new[hhh,which(colnames(data) == RECORD_HEADER)]
  dd = matrix(nrow = length(hhh),ncol = 6)
  colnames(dd) = c("Datetime_From", "Datetime_To","Datetime_Missing","Record_From", "Record_To", "Record_Missing")
  dd = as.data.frame(dd)
  
  for(i in 1:length(hhh)){
    dd[i,1] = format(d_st[i],format = DATETIME_FORMAT)
    dd[i,2] = format(d_en[i],format = DATETIME_FORMAT)   
    dd[i,3] = length(seq(from = d_st[i],to = d_en[i],by = DATETIME_SAMPLING))-1
    
    dd[i,4] = format(r_st[i])
    dd[i,5] = format(r_en[i])
    dd[i,6] = data_new[hhh[i],which(colnames(data) == RECORD_HEADER)]-data_new[hhh[i]-1,which(colnames(data) == RECORD_HEADER)]
  }
  
  dd
  
  if(length(hhh) == 0){
    flag_missing_records = 0 
  } else{
    flag_missing_records = 1
  }
  
  return(list(flag_missing_records,dd))
} 
# missed = which(sign(record_diff-1) > 0)
# 
# m_en = data_new[missed,which(colnames(data) == DATETIME_HEADER)]
# m_st = data_new[missed-1,which(colnames(data) == DATETIME_HEADER)]
# dm = matrix(nrow = length(missed),ncol = 4)
# colnames(dm) = c("From", "To", "N_datetime", "N_record")
# dm = as.data.frame(dm)
# 
# for(i in 1:length(missed)){
#   dm[i,1] = format(m_st[i])
#   dm[i,2] = format(m_en[i])
#   dm[i,3] = length(seq(from = m_st[i],to = m_en[i],by = DATETIME_SAMPLING))-1
#   dm[i,4] = data_new[missed[i],which(colnames(data) == RECORD_HEADER)]-data_new[missed[i]-1,which(colnames(data) == RECORD_HEADER)]
# }
# 
# dm
# 
# restarted = which(sign(record_diff-1) < 0)
# 
# r_en = data_new[restarted,which(colnames(data) == DATETIME_HEADER)]
# r_st = data_new[restarted-1,which(colnames(data) == DATETIME_HEADER)]
# dr = matrix(nrow = length(restarted),ncol = 4)
# colnames(dr) = c("From", "To", "N_datetime", "N_record")
# dr = as.data.frame(dr)
# 
# for(i in 1:length(restarted)){
#   dr[i,1] = format(r_st[i])
#   dr[i,2] = format(r_en[i])
#   dr[i,3] = length(seq(from = r_st[i],to = r_en[i],by = DATETIME_SAMPLING))-1
#   dr[i,4] = data_new[restarted[i],which(colnames(data) == RECORD_HEADER)]-data_new[restarted[i]-1,which(colnames(data) == RECORD_HEADER)]
# }
# 
# dm
# dr 
# 
# if(length(missed)!=0 | length(restarted) !=0){
#   
# }
# 
# data_timestamp[www]
# cc = sort(c(www-1,www))
# dd = sort(c(ppp-1,ppp))
# 
# data_new[cc,]
# data_new[dd,]
# 
# str(data_new)
# p=as.numeric(96*diff(data_new$TIMESTAMP[www]))
# data_record[cc]
# 
# plot(data_timestamp,sign(dif-1))
# data_new =  data[,which(colnames(data) %in% c(RECORD_HEADER, DATETIME_HEADER))]
# data_new = cbind(data_new,c(1,diff(data_record)))
# 
# w = which(diff(data_record) != 1)
# data_new[w,1:2]
# return(list(df_merge,missing))
# }
