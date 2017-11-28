detect_overlap = function(DATA, DATETIME_HEADER = "TIMESTAMP", RECORD_HEADER = "RECORD"){
  
  if(any(duplicated(DATA[,which(colnames(data) == DATETIME_HEADER)]))){
    
    w <- which(duplicated(DATA[,which(colnames(data) == DATETIME_HEADER)]))
    
    date_overlap = data[w,which(colnames(data) == DATETIME_HEADER)]
    index_overlap = which(data[,which(colnames(data) == DATETIME_HEADER)] %in% date_overlap)
    record_overlap = data[index_overlap,which(colnames(data) == RECORD_HEADER)]
    date_record = data[index_overlap,which(colnames(data) == DATETIME_HEADER)]
    df_overlap = data.frame(record_overlap,date_overlap)
    colnames(df_overlap)= c(RECORD_HEADER,DATETIME_HEADER )
    
  }else{
    df_overlap = NULL
  }
  return(df_overlap)
}


