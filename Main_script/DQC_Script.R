#-------------------------------------------------------------------------------------------------------------------------------------------------------
# File Title:   DQC_Script.R
# TITLE:        Data quality check LTER on different files in scheduling folder
# Author:       Brida Christian, Genova Giulio, Zandonai Alessandro
#               Institute for Alpine Environment
# Data:         13/12/2017
# Version:      1.0
#
#------------------------------------------------------------------------------------------------------------------------------------------------------
remove(list=ls())
Sys.setenv(TZ='Etc/GMT-1') # sets the environment on italy?s time zone

# ..... Libraries .....................................................................................................................................

library(devtools)
install_github("bridachristian/DataQualityCheckEuracAlpEnv")
library("DataQualityCheckEuracAlpEnv")

library(zoo)
library(timeSeries)
library(knitr)
library(ggplot2)
library(dplyr)
library(plyr)
library(imputeTS)
library(reshape2)
library(kableExtra)

# ..... Input section .................................................................................................................................

## Description: 
## input_dir             # dir:  Directory where to put input files.
## output_dir_data       # dir:  Directory where to save output files
## output_dir_report     # dir:  Directory where to save output report
## project_dir           # dir:  Directory of DataQualityCheckEuracAlpEnv package. Inside there are R script, Rmarkdown document and support files.
## data_from_row         # num:  The row number of the first data row. (How many rows are dedicated to headers? + 1)
## header_row_number     # num:  The row number of the header names
## datetime_header       # chr:  The string to use to recognize datetime
## datetime_format       # chr:  The string to use to recognize datetime. Use only: y -> year, m -> month, d -> day, H -> hour, M -> minute
## datetime_sampling     # chr:  The string that define the time sampling (in POSIXct format)
## record_header         # chr:  The string to use to recognize record header
## range_file            # chr:  The string that indicate the name of range file saved in folder "project_dir/Data/Support_files/Range"
## write_output_files    # log:  Logical status (TRUE/FALSE) to decide if save output data or not
## write_output_report   # log:  Logical status (TRUE/FALSE) to decide if save output report or not
## file                  # chr:  The string that indicate the name of file to process
## start_date            # date: First date to consider for the analysis. Date before are not considered. No data is start_data <- NA
  
input_dir <- "H:/Projekte/Klimawandel/Experiment/data/2order/DQC_BrC_test_data/"
output_dir_data <- "H:/Projekte/Klimawandel/Experiment/data/2order/DQC_BrC_test_data/test_output/Out_Data/"
output_dir_report <- "H:/Projekte/Klimawandel/Experiment/data/2order/DQC_BrC_test_data/test_output/Out_Report/"
project_dir <- "H:/Projekte/Klimawandel/Experiment/data/2order/DataQualityCheckEuracAlpEnv/"
data_from_row =  5
header_row_number =  2
datetime_header =  "TIMESTAMP"
datetime_format =  "yyyy-mm-dd HH:MM"
datetime_sampling =  "15 min"
record_header =  "RECORD"
range_file =  "Range.csv"
write_output_files =  "FALSE"
write_output_report =  "FALSE"
file <- "M3.dat"
start_date <- "2017-11-23 05:45:00"

# ~~~ Default directory ~~~~

range_dir <- paste(project_dir, "Data/Support_files/Range/",sep = "")

# .....................................................................................................................................................

# ..... Body ..........................................................................................................................................


if(check_empty_file(INPUT_DATA_DIR = input_dir, FILE_NAME = file) == TRUE){
  # writeLines(paste(FILE,"WARNING: NO DATA FOUND!!!",sep = " "))
  flag_empty = 1
}else{
  
  flag_empty = 0
  
  data_import <- read_data(INPUT_DATA_DIR = input_dir, FILE_NAME = file, 
                           DATETIME_HEADER = datetime_header, DATETIME_FORMAT = datetime_format,
                           DATA_FROM_ROW = data_from_row, HEADER_ROW_NUMBER = header_row_number)  
  header <- data_import [[1]]
  header_colnames <- data_import [[2]]
  data <- data_import [[3]]
  flag_error_df <- data_import [[4]]
  
  if(flag_error_df == 0){
    time_data = data[,which(colnames(data)==datetime_header)]
    
    if(is.na(start_date)){
      
      original <- data
      mydata <- data    
      flag_date = 0
      
    }else{
      
      if(as.POSIXct(start_date) < time_data[length(time_data)]){
        original <- data[(which(time_data == as.POSIXct(start_date))+1):nrow(data),]      # possible issues in data subset!!! to check 
        mydata <- data[(which(time_data == as.POSIXct(start_date))+1):nrow(data),]
        
        flag_date = 0
        
      } else {
        
        flag_date = 1
      }
    }
    
    
    if(flag_date == 0){
      deletes_duplcated <- deletes_duplcated_data(DATA = mydata,DATETIME_HEADER = datetime_header)         # <- Deletes identical rows if found
      mydata <- deletes_duplcated [[1]]                                                                                                        
      duplicated_data <- deletes_duplcated [[2]]
      duplicated_data <- time_to_char(DATA = duplicated_data, DATETIME_HEADER = datetime_header, DATETIME_FORMAT = datetime_format)
      
      overlap <- detect_overlap(DATA = mydata,DATETIME_HEADER = datetime_header, RECORD_HEADER = record_header)          # <- Detect overlap
      
      if(length(overlap) != 0){
        
        flag_overlap = 1
        # stop(paste("Overlapping data in files:", FILE))
        overlap[,1]<- overlap[,1] + data_from_row - 1
        colnames(overlap)[1]= "File Row"
        
      }else{
        
        flag_overlap = 0
        
        missing  <- missing_dates(DATA = mydata, DATETIME_HEADER = datetime_header, RECORD_HEADER = record_header,DATETIME_SAMPLING = datetime_sampling)  # <- fill missing dates with NA
        mydata <- missing[[1]]
        missing_index_date <- missing[[2]]
        
        range <- exclude_out_of_range(DATA = mydata,DATETIME_HEADER = datetime_header, RANGE_DIR = range_dir, RANGE_FILE = range_file) # <- Substitute with NA data out of phisical range
        mydata <- range[[1]]
        check_out_of_range <- range[[2]]
        variable_not_in_range_file <-range[[3]]
        
        mydata <- time_to_char(DATA = mydata, DATETIME_HEADER = datetime_header, DATETIME_FORMAT = datetime_format)
        
      }
    }
  }
}


# ..... Output ..........................................................................................................................................

if(flag_empty == 0){
  if(flag_error_df == 0){
    if(flag_date == 0){
      if(flag_overlap == 0){
        if(write_output_files == TRUE){
          #~~~~~~~~~~
          colnames(header) = header[1,]
          
          out_my = mydata
          colnames(out_my) = colnames(header)
          
          out_mydata=rbind(header[-1,],out_my)
          
          out_filename = paste(substring(mydata[nrow(mydata),which(colnames(mydata) == datetime_header)],1,4),
                               substring(mydata[nrow(mydata),which(colnames(mydata) == datetime_header)],6,7),
                               substring(mydata[nrow(mydata),which(colnames(mydata) == datetime_header)],9,10),
                               # "_",
                               substring(mydata[nrow(mydata),which(colnames(mydata) == datetime_header)],12,13),
                               substring(mydata[nrow(mydata),which(colnames(mydata) == datetime_header)],15,16),
                               sep = "")
          
          
          
          write.csv(out_mydata,paste(output_dir_data,"DQCok_",substring(file,1,nchar(file)-4),"_",out_filename,".csv",sep = ""),quote = F,row.names = F)
          
          #~~~~~~~~~~
          colnames(duplicated_data) = colnames(header)
          
          out_duplicated_data=rbind(header[-1,],duplicated_data)
          write.csv(out_duplicated_data,paste(output_dir_data,"Duplicated_",substring(file,1,nchar(file)-4),"_",out_filename,".csv",sep = ""),quote = F,row.names = F)
        }
      }
    }
  }
}
