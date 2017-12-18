#' This function read .csv and .dat file and adjust headers and time format
#' 
#'  @param FILE_PATH directory where input file is stored
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
#'  read_data(FILE_PATH = "~/Data/Input/", FILE_NAME = "M4s.dat", DATETIME_HEADER = "TIMESTAMP" , DATETIME_FORMAT = "yyyy-mm-dd HH:MM", DATA_FROM_ROW = 5, HEADER_ROW_NUMBER = 2)
#'  read_data(FILE_PATH = "Your input file storage", FILE_NAME = "Your data name", DATETIME_HEADER = "Your datetime headere" , DATETIME_FORMAT = "Your datetime format", DATA_FROM_ROW = "The row of your first data", HEADER_ROW_NUMBER = "The row of your data column names")


read_data = function(FILE_PATH, FILE_NAME, DATETIME_HEADER = "TIMESTAMP" , DATETIME_FORMAT = "yyyy-mm-dd HH:MM", DATA_FROM_ROW = 5, HEADER_ROW_NUMBER = 2){
  
  header <- read.csv(paste(FILE_PATH, FILE_NAME,sep = ""), nrows = DATA_FROM_ROW - 1,header = F,stringsAsFactors = F)
  header_colnames <- header[HEADER_ROW_NUMBER,]
  data <- read.csv(paste(FILE_PATH, FILE_NAME,sep = ""), skip = DATA_FROM_ROW - 1,header = F,stringsAsFactors = F) 
  
  if(ncol(data) == ncol(header_colnames)){
    
    
    colnames(data) = header_colnames
    # data <- read.csv(paste(FILE_PATH, FILE_NAME,sep = ""), skip = 1,header = T,stringsAsFactors = F) 
    # data <- data[-c(1,2),]
    
    # colnames(data) = header
    
    w <- which(colnames(data) == DATETIME_HEADER)
    
    y <- c(min(gregexpr("y",DATETIME_FORMAT)[[1]]),max(gregexpr("y",DATETIME_FORMAT)[[1]]))
    m <- c(min(gregexpr("m",DATETIME_FORMAT)[[1]]),max(gregexpr("m",DATETIME_FORMAT)[[1]]))
    d <- c(min(gregexpr("d",DATETIME_FORMAT)[[1]]),max(gregexpr("d",DATETIME_FORMAT)[[1]]))
    H <- c(min(gregexpr("H",DATETIME_FORMAT)[[1]]),max(gregexpr("H",DATETIME_FORMAT)[[1]]))
    M <- c(min(gregexpr("M",DATETIME_FORMAT)[[1]]),max(gregexpr("M",DATETIME_FORMAT)[[1]]))
    
    year <- substring(data[,w],y[1],y[2])
    month <- substring(data[,w],m[1],m[2])
    day <- substring(data[,w],d[1],d[2])
    hour <- substring(data[,w],H[1],H[2])
    min <- substring(data[,w],M[1],M[2])
    
    if(length(gregexpr("y",DATETIME_FORMAT)[[1]])==4){
      
      date_chr <- paste(year, "-", month, "-", day, " ", hour, ":", min, ":00", sep="")
    } else{
      if(length(gregexpr("y",DATETIME_FORMAT)[[1]])==2){
        
        date_chr <- paste("20",year, "-", month, "-", day, " ", hour, ":", min, ":00", sep="")
      }
      
    }
    time <- as.POSIXct( strptime(x = date_chr, format = "%Y-%m-%d %H:%M:%S"), tz = 'Etc/GMT-1')
    
    data[,w] <- time
    
    not_w <- which(colnames(data) != DATETIME_HEADER)
    
    for(i in not_w){
      data[,i] <- as.numeric(data[,i])
    }
    flag_error_df = 0
  } else{
    if(ncol(data) < ncol(header_colnames)){
      
      df_NA = as.data.frame(matrix(data = NA, ncol = ncol(header_colnames)-ncol(data),nrow = nrow(data)))
      
      data = cbind(data,df_NA) 
      colnames(data) = header_colnames
      
      # data <- read.csv(paste(FILE_PATH, FILE_NAME,sep = ""), skip = 1,header = T,stringsAsFactors = F) 
      # data <- data[-c(1,2),]
      
      # colnames(data) = header
      
      w <- which(colnames(data) == DATETIME_HEADER)
      
      y <- c(min(gregexpr("y",DATETIME_FORMAT)[[1]]),max(gregexpr("y",DATETIME_FORMAT)[[1]]))
      m <- c(min(gregexpr("m",DATETIME_FORMAT)[[1]]),max(gregexpr("m",DATETIME_FORMAT)[[1]]))
      d <- c(min(gregexpr("d",DATETIME_FORMAT)[[1]]),max(gregexpr("d",DATETIME_FORMAT)[[1]]))
      H <- c(min(gregexpr("H",DATETIME_FORMAT)[[1]]),max(gregexpr("H",DATETIME_FORMAT)[[1]]))
      M <- c(min(gregexpr("M",DATETIME_FORMAT)[[1]]),max(gregexpr("M",DATETIME_FORMAT)[[1]]))
      
      year <- substring(data[,w],y[1],y[2])
      month <- substring(data[,w],m[1],m[2])
      day <- substring(data[,w],d[1],d[2])
      hour <- substring(data[,w],H[1],H[2])
      min <- substring(data[,w],M[1],M[2])
      
      if(length(gregexpr("y",DATETIME_FORMAT)[[1]])==4){
        
        date_chr <- paste(year, "-", month, "-", day, " ", hour, ":", min, ":00", sep="")
      } else{
        if(length(gregexpr("y",DATETIME_FORMAT)[[1]])==2){
          
          date_chr <- paste("20",year, "-", month, "-", day, " ", hour, ":", min, ":00", sep="")
        }
        
      }
      time <- as.POSIXct( strptime(x = date_chr, format = "%Y-%m-%d %H:%M:%S"), tz = 'Etc/GMT-1')
      
      data[,w] <- time
      
      not_w <- which(colnames(data) != DATETIME_HEADER)
      
      for(i in not_w){
        data[,i] <- as.numeric(data[,i])
      }
      flag_error_df = 0
    } 
  }else{
    if(ncol(data) > ncol(header_colnames)){
      flag_error_df = 1
    }
  }
}

return(list(header,header_colnames,data,flag_error_df))

}