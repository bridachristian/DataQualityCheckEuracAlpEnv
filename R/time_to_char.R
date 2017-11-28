time_to_char = function(DATA, DATETIME_HEADER, DATETIME_FORMAT){

  w = which(colnames(DATA)== DATETIME_HEADER)

  # year = year(DATA[,w])
  # month = month(DATA[,w])
  # day = day(DATA[,w])
  # hour = hour(DATA[,w])
  # min = minute(DATA[,w])
  #
  # y <- c(min(gregexpr("y",DATETIME_FORMAT)[[1]]),max(gregexpr("y",DATETIME_FORMAT)[[1]]))
  # m <- c(min(gregexpr("m",DATETIME_FORMAT)[[1]]),max(gregexpr("m",DATETIME_FORMAT)[[1]]))
  # d <- c(min(gregexpr("d",DATETIME_FORMAT)[[1]]),max(gregexpr("d",DATETIME_FORMAT)[[1]]))
  # H <- c(min(gregexpr("H",DATETIME_FORMAT)[[1]]),max(gregexpr("H",DATETIME_FORMAT)[[1]]))
  # M <- c(min(gregexpr("M",DATETIME_FORMAT)[[1]]),max(gregexpr("M",DATETIME_FORMAT)[[1]]))
  #
  # if(diff(y)==1){
  #   year=substring(year,3,4)
  # }


  DATA[,w] = as.character(DATA[,w])

  return(DATA)
}
