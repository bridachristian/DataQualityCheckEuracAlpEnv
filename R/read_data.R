#' This function read .csv and .dat file and adjust headers and time format
#'
#'  @param INPUT_DATA_DIR directory where input file is stored
#'  @param FILE_NAME name of file to read. Admitted files .csv and .dat
#'  @param DATETIME_HEADER header corresponding to datetime
#'  @param DATETIME_FORMAT format of datetime (E.g. "yyyy-mm-dd HH:MM")
#'  @param DATA_FROM_ROW the number of row of the first value
#'  @param HEADER_ROW_NUMBER the number of row of the header
#'
#'  @return a list containing a data.frame of header, a data.frame of column names, a data.frame of data
#'
#'  @export
#'
#'  @examples
#'  read_data(INPUT_DATA_DIR = "~/Data/Input/", FILE_NAME = "M4s.dat", DATETIME_HEADER = "TIMESTAMP" , DATETIME_FORMAT = "yyyy-mm-dd HH:MM", DATA_FROM_ROW = 5, HEADER_ROW_NUMBER = 2)
#'  read_data(INPUT_DATA_DIR = "Your input file storage", FILE_NAME = "Your data name", DATETIME_HEADER = "Your datetime headere" , DATETIME_FORMAT = "Your datetime format", DATA_FROM_ROW = "The row of your first data", HEADER_ROW_NUMBER = "The row of your data column names")

# DATETIME_FORMAT = "%Y-%m-d %H:%M"
read_data = function(INPUT_DATA_DIR, FILE_NAME, DATETIME_HEADER = "TIMESTAMP" , DATETIME_FORMAT = "yyyy-mm-dd HH:MM", DATA_FROM_ROW = 5, HEADER_ROW_NUMBER = 2){
  
  header <- read.csv(paste(INPUT_DATA_DIR, FILE_NAME,sep = ""), nrows = DATA_FROM_ROW - 1,header = F,stringsAsFactors = F,na.strings = c(NA, "NaN", "NAN"))
  header_colnames <- header[HEADER_ROW_NUMBER,]
  data <- read.csv(paste(INPUT_DATA_DIR, FILE_NAME,sep = ""), skip = DATA_FROM_ROW - 1,header = F,stringsAsFactors = F,na.strings = c(NA, "NaN", "NAN"))
  
  data_star <- read.csv(paste(INPUT_DATA_DIR, FILE_NAME,sep = ""), skip = HEADER_ROW_NUMBER - 1,header = F,stringsAsFactors = F)
  data_star = data_star[-c(1:(DATA_FROM_ROW-HEADER_ROW_NUMBER)),]
  
  if(any(data_star == "", na.rm = T)){
    flag_error_df = 2
    
    # w1 = as.data.frame(which(data_star == "",arr.ind = T))
    # names(w1) = c("row", "col")
    # df = data.frame(  data_star$V1[w1$row], as.character(header_colnames)[w1$col])
    
  }else{
    
    if(ncol(data) == ncol(header_colnames)){
      
      colnames(data) = header_colnames
      
      w <- which(colnames(data) == DATETIME_HEADER)
      
      date_chr = data[,w]
      
      time <- as.POSIXct(strptime(x = date_chr, format = DATETIME_FORMAT), tz = 'Etc/GMT-1') # Error in strptime(x = as.character(date_chr), format = DATETIME_FORMAT):input string is too long
      
      data[,w] <- time
      
      not_w <- which(colnames(data) != DATETIME_HEADER)
      
      for(i in not_w){
        data[,i] <- as.numeric(data[,i])
      }
      flag_error_df = 0
      
      matr_data = as.matrix(data)
      ind_NA = which(apply(matr_data,1, function(x) all(is.na(x))))
      
      if(length(ind_NA) != 0){
        data = data[-ind_NA,]
      }
      
    } else{
      
      
      if(ncol(data) < ncol(header_colnames)){
        
        df_NA = as.data.frame(matrix(data = NA, ncol = ncol(header_colnames)-ncol(data),nrow = nrow(data)))
        
        data = cbind(data,df_NA)
        colnames(data) = header_colnames
        
        w <- which(colnames(data) == DATETIME_HEADER)
        
        date_chr = data[,w]
        time <- as.POSIXct( strptime(x = date_chr, format = DATETIME_FORMAT), tz = 'Etc/GMT-1')
        time <- format(time,format = DATETIME_FORMAT)
        
        data[,w] <- time
        
        not_w <- which(colnames(data) != DATETIME_HEADER)
        
        for(i in not_w){
          data[,i] <- as.numeric(data[,i])
        }
        flag_error_df = -1
        
      } else {
        if(ncol(data) > ncol(header_colnames)){
          flag_error_df = 1
        }
      }
    }
  }
  gc(reset = T)
  
  return(list(header,header_colnames,data,flag_error_df,data_star))
  
}
