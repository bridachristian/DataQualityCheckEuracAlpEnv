#-------------------------------------------------------------------------------------------------------------------------------------------------------
# File Title:   DQC_Manual_Single_File.R
# TITLE:        Data quality check to run manual for a generic data file
# Author:       Brida Christian, Genova Giulio, Zandonai Alessandro
#               Institute for Alpine Environment
# Data:         21/11/2017
# Version:      2.0
#
#PORPUSE: Finds overlap or missing dates
#If overlap dates are found generates an error and tells you at which row you have the problem (fix it manually)
#If missing dates are found it automatically generates NAs
#Appllies thresholds to desired variables

#Support files:
#Download table = has memory of the last check done
#Rande settings= contains thresholds to bo applied to certain variables

#Required packages:zoo, timeSeries

#------------------------------------------------------------------------------------------------------------------------------------------------------

remove(list=ls())
Sys.setenv(TZ='Etc/GMT-1') # sets the environment on italy?s time zone

# ..... Libraries .....................................................................................................................................
library(devtools)
install_github("bridachristian/DataQualityCheckEuracAlpEnv")
library("DataQualityCheckEuracAlpEnv")

library(zoo)
library(timeSeries)
# .....................................................................................................................................................

# ..... Input section .................................................................................................................................

# ~~~ Folders ~~~~

# scheduling_dir <- "H:/Projekte/Klimawandel/Experiment/data/2order/scheduling/"                              # <-- schelduling directory: for files to be processed
scheduling_dir <- "H:/Projekte/Klimawandel/Experiment/data/2order/DataQualityCheckEuracAlpEnv//Data/Input/"                   # <-- schelduling directory: for files to be processed
# scheduling_dir <- report_dir                                                                                # <-- schelduling directory: for files that the script found whith overlap and you fixed manually

setwd(scheduling_dir)

report_dir <- "H:/Projekte/Klimawandel/Experiment/data/2order/DataQualityCheckEuracAlpEnv/Report/"                    # <-- report directory: where to put reports and files whith overlaps
output_dir <- "H:/Projekte/Klimawandel/Experiment/data/2order/DataQualityCheckEuracAlpEnv/Data/Output/"               # <-- output directory: where processed files go
support_dir <- "H:/Projekte/Klimawandel/Experiment/data/2order/DataQualityCheckEuracAlpEnv/Data/Support_files/"       # <-- support directory: where to read support files

write_output <- TRUE                                             # if write_output == TRUE => write csv is admitted, if == FALSE not!

# ~~~ Files ~~~~

files <- files_in_scheduling_dir(SCHEDULING_DIR = scheduling_dir)
cat("Which of this files you want analyze? \n ",files)         # <-- Here we show files available for data quality check

FILE <- c("M4s.dat")                                            # <-- Write here the file or the list of file  that you want to analyze!
cat("Selected files: \n", FILE)

RANGE_FILE = "Range.csv"

# ~~~ Datetime ~~~~
DATA_FROM_ROW <- 5                                             # <-- Row number of first data

HEADER_ROW_NUMBER <- 2                                         # <-- Row number of header

DATETIME_HEADER <- "TIMESTAMP"                                 # <-- header corresponding to TIMESTAMP
DATETIME_FORMAT <- "yyyy-mm-dd HH:MM"                          # <-- datetime format. Use only: y -> year, m -> month, d -> day, H -> hour, M -> minute
DATETIME_SAMPLING <- "15 min"

RECORD_HEADER <- "RECORD"                                      # <-- header corresponding to RECORD

# .....................................................................................................................................................

# ..... Body ..........................................................................................................................................

data_import <- read_data(FILE_PATH = scheduling_dir,FILE_NAME = FILE,DATETIME_HEADER = DATETIME_HEADER, DATETIME_FORMAT = DATETIME_FORMAT)
header <- data_import [[1]]
header_colnames <- data_import [[2]]
data <- data_import [[3]]

original <- data
mydata <- data

if(check_empty_files(SCHEDULING_DIR = scheduling_dir, FILE = FILE) == TRUE){
  # writeLines(paste(FILE,"WARNING: NO DATA FOUND!!!",sep = " "))
  flag_empty = 1
}else{
  mydata <- deletes_duplcated_data(DATA = mydata)                                                                                                         # <- Deletes identical rows if found

  overlap <- detect_overlap(DATA = mydata,DATETIME_HEADER = DATETIME_HEADER, RECORD_HEADER = RECORD_HEADER)                                               # <- Detect overlap

  if(length(overlap) != 0){
    stop(paste("Overlapping data in files:", FILE))
  }else{

    mydata  <- missing_dates(DATA = mydata, DATETIME_HEADER = DATETIME_HEADER, RECORD_HEADER = RECORD_HEADER,DATETIME_SAMPLING = DATETIME_SAMPLING)       # <- fill missing dates with NA

    mydata <- exclude_out_of_range(DATA = mydata, SUPPORT_DIR = support_dir, RANGE_FILE = RANGE_FILE)                     # <- Substitute with NA data out of phisical range

    mydata <- time_to_char(DATA = mydata, DATETIME_HEADER = DATETIME_HEADER, DATETIME_FORMAT = DATETIME_FORMAT)
    }
}

if(write_output == TRUE){

  colnames(header) = header[1,]
  colnames(mydata) = colnames(header)

  out_mydata=rbind(header[-1,],mydata)
  write.csv(out_mydata,paste(output_dir,"DQCok_",substring(FILE,1,nchar(FILE)-4),".csv",sep = ""),quote = F,row.names = F)
}
