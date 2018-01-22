#' This function reject all values greater than max and smaller than min in a data.frame
#'
#' @param DATA data.frame of timeseries
#' @param DATETIME_HEADER header of data corresponding to datetime
#' @param RANGE_DIR directory where support files are stored
#' @param RANGE_FILE name of filr where min/max thresholds are defined for each variable. Thi file is in RANGE_DIR
#'
#' @return A data.frame with all values in its physical range
#'
#' @export
#'
#' @examples
#' exclude_out_of_range(DATA = mydata, RANGE_DIR = RANGE_DIR, RANGE_FILE = "Range.csv")
#'

exclude_out_of_range = function(DATA,DATETIME_HEADER = "TIMESTAMP", RANGE_DIR, RANGE_FILE){
  options(scipen = 999)
  range = read.csv(paste(RANGE_DIR, RANGE_FILE,sep = ""),stringsAsFactors = FALSE)          # <- import table that contains for each variable the permissible range
  
  # to_set = range[which(range$min == "to set" | range$max == "to set"),]
  # 
  # if(nrow(to_set) != 0){
  # range = range[-which(range$Variable == to_set$Variable),]
  # }
  
  range$min = as.numeric(range$min)
  range$max = as.numeric(range$max)
  
  range = range[order(range$Variable),] # reorder range file based on variable
  
  new = DATA # define new dataframe called new that is a copy of DATA
  
  new_status = new # create a dataframe with the same structure that DATA. Inside there is only 0. When data are out of range 0 is subsitute wiht -1 or 1
  new_status[,-which(colnames(new_status) == DATETIME_HEADER )] = 0
  
  # This loop checks if variables in result are in the range list.
  # It could be a good index to see if there are issues in headers
  to_add = c()
  
  for(k in 1:ncol(new)){
    if(colnames(new)[k] %in% range$Variable){
      w = which(range$Variable == colnames(new)[k])
      
      range$Variable[w]
      lower_limit = range$min[w]
      upper_limit = range$max[w]
      
      if(!is.na(lower_limit) & !is.na(upper_limit)){          # Exclude data without a range set
        
        new_status[,k] = ifelse(new[,k] < lower_limit, -1, new_status[,k])
        new_status[,k] = ifelse(new[,k] > upper_limit, 1, new_status[,k])
        
        new_status[is.na(new_status[,k]),k] = 0
        
        new[,k] = ifelse(new[,k] < lower_limit, NA, new[,k])
        new[,k] = ifelse(new[,k] > upper_limit, NA, new[,k])
      }
      
    }else{
      to_add = c(to_add, colnames(new)[k])
    }
  }
  
  if(length(to_add) != 0){
    df_to_add = data.frame(to_add, rep(NA, times=length(to_add)),rep(NA, times=length(to_add)),rep(1, times=length(to_add)))
    colnames(df_to_add) = colnames(range)
    
    range = rbind(range,df_to_add)
  }

  range$min = as.character(range$min)
  range$max = as.character(range$max)
  write.csv(range,paste(RANGE_DIR, RANGE_FILE,sep = ""),quote = F,row.names = F, na = "")
  
  out = list(new, new_status, to_add)
  
  return(out)
}
