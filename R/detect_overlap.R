#' This function detect overlapping dates in a data.frame
#'
#' @param DATA data.frame
#' @param DATETIME_HEADER header corresponding to datetime
#' @param RECORD_HEADER header corresponding to record
#'
#' @return A data.frame containing records and datetime where overlaps occour
#' @export
#' @examples
#' detect_overlap(mydata, "TIMESTAMP", "RECORD")
#' detect_overlap(DATA = mydata, DATETIME_HEADER = "your datetime header", RECORD_HEADER = "your record header")
#'

detect_overlap = function(DATA, DATETIME_HEADER = "TIMESTAMP", RECORD_HEADER = "RECORD"){
  
  if(any(duplicated(DATA[,which(colnames(data) == DATETIME_HEADER)]))){
    
    w <- which(duplicated(DATA[,which(colnames(data) == DATETIME_HEADER)]))
    
    date_overlap = data[w,which(colnames(data) == DATETIME_HEADER)]
    
    index_overlap = which(data[,which(colnames(data) == DATETIME_HEADER)] %in% date_overlap)
    
    record_overlap = data[index_overlap,which(colnames(data) == RECORD_HEADER)]
    
    date_record = data[index_overlap,which(colnames(data) == DATETIME_HEADER)]
    
    df_overlap = data.frame(index_overlap,record_overlap,date_record)
    colnames(df_overlap)= c("Index",RECORD_HEADER,DATETIME_HEADER )
    
  }else{
    df_overlap = NULL
  }
  return(df_overlap)
}


