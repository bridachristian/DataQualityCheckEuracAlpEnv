#' This function check if in input file there is empty rows and if a row is shifted in the previous line
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



structure_control = function(FILE_PATH, FILE_NAME, DATETIME_HEADER = "TIMESTAMP" , DATETIME_FORMAT = "yyyy-mm-dd HH:MM", DATA_FROM_ROW = 5, HEADER_ROW_NUMBER = 2){
  
  input_file = read.csv(paste(FILE_PATH,FILE_NAME,sep = ""),header = F,stringsAsFactors = F)
  input_data = input_file[-c(1:(DATA_FROM_ROW-1)),]
  
  # check if there are empty rows 
  
  a = apply(input_data,1, function(x) length(unique(x)))
  w = as.numeric(which(a == 1))
  row_number = w + DATA_FROM_ROW -1
  if(length(row_number) != 0){
    flag_empty_row = 1
  }
  
  # check if there is a shift on columns
  input_header = input_file[HEADER_ROW_NUMBER,]
  header_new = as.character(input_header)

  b = apply(input_header,1, function(x) count(which(x != "NA")))
  
  count(df = input_data,)
  column_strange = which(header_new == "NA")
  
  if(length(wh) != 0){
    flag_shift_column = 1
  }

  out = list(flag_empty_row = 1,row_number,flag_shift_column,column_strange)
  names(out) = c("Empty_rows", "Which_empty", "Column_shift","Which_shift")
}

data[!apply(data == "", 1, all),]

FILE_NAME  = "xM4s.dat"
