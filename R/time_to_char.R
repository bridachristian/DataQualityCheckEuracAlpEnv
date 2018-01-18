#' This function convert POSIXct datetime in a data.frame to character
#' 
#'  @param DATA data.frame having a column defined as POSIXct (datetime)
#'  @param DATETIME_HEADER header corresponding to datetime
#'  @param DATETIME_FORMAT format of datetime (E.g. "yyyy-mm-dd HH:MM")
#'  
#'  @return a data.frame which contains character datetime
#'  
#'  @export
#'  
#'  @examples
#'  time_to_char = function(DATA = mydata, DATETIME_HEADER = "TIMESTAMP", DATETIME_FORMAT = "yyyy-mm-dd HH:MM")
#'  time_to_char = function(DATA = your data.frame, DATETIME_HEADER = "your datetime header", DATETIME_FORMAT = "your datetime format")
#'  

time_to_char = function(DATA, DATETIME_HEADER, DATETIME_FORMAT){

  w = which(colnames(DATA)== DATETIME_HEADER)

  # year = year(DATA[,w])
  # month = month(DATA[,w])
  # day = day(DATA[,w])
  # hour = hour(DATA[,w])
  # min = minute(DATA[,w])
  # sec = second(DATA[,w])
  
  # y <- c(min(gregexpr("y",DATETIME_FORMAT)[[1]]),max(gregexpr("y",DATETIME_FORMAT)[[1]]))
  # m <- c(min(gregexpr("m",DATETIME_FORMAT)[[1]]),max(gregexpr("m",DATETIME_FORMAT)[[1]]))
  # d <- c(min(gregexpr("d",DATETIME_FORMAT)[[1]]),max(gregexpr("d",DATETIME_FORMAT)[[1]]))
  # H <- c(min(gregexpr("H",DATETIME_FORMAT)[[1]]),max(gregexpr("H",DATETIME_FORMAT)[[1]]))
  # M <- c(min(gregexpr("M",DATETIME_FORMAT)[[1]]),max(gregexpr("M",DATETIME_FORMAT)[[1]]))


  
  DATA[,w] = format(DATA[,w],format = DATETIME_FORMAT)
  
  
  
  # DATA[,w] = as.character(DATA[,w])


  return(DATA)
}
