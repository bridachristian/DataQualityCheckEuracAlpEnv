#-------------------------------------------------------------------------------------------------------------------------------------------------------
# File Title:   DQC_LTER_BrC_test.R
# TITLE:        Data quality check LTER
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
Rfunctions_dir <- "H:/Projekte/Klimawandel/Experiment/data/2order/DataQualityCheckEuracAlpEnv/R/"                     # <-- R functions directory: where functions are defined

# ~~~ Use download_table ~~~~

Use_dwnld_tbl <- FALSE                                                                                   # <-- Do you want analyze all file or only data recently downloaded?
# NOTE: if Use_dwnld_tbl == FALSE  => analyze all data file.
#  else if Use_dwnld_tbl == TRUE   => analyze only the last data download!

# ~~~ Functions ~~~~

# for(r in 1: length(dir(Rfunctions_dir))){
#   source(paste(Rfunctions_dir,dir(Rfunctions_dir)[r],sep = ""))
# }

# ~~~ Files ~~~~

files <- files_in_scheduling_dir(SCHEDULING_DIR = scheduling_dir)
cat("Which of this files you want analyze? \n ",files)         # <-- Here we show files available for data quality check

FILE <- c("M4s.dat")                                            # <-- Write here the file or the list of file  that you want to analyze!
# FILE <- c(files)
cat("Selected files: \n", FILE)

DOWNLOAD_TABLE = "download_table.csv"

RANGE_FILE = "Range.csv"

# ~~~ Datetime ~~~~
DATA_FROM_ROW <- 5                                             # <-- Row number of first data

HEADER_ROW_NUMBER <- 2                                         # <-- Row number of header

DATETIME_HEADER <- "TIMESTAMP"                                 # <-- header corresponding to TIMESTAMP
DATETIME_FORMAT <- "yyyy-mm-dd HH:MM"                          # <-- datetime format. Use only: y -> year, m -> month, d -> day, H -> hour, M -> minute
DATETIME_SAMPLING <- "15 min"

RECORD_HEADER <- "RECORD"                                      # <-- header corresponding to RECORD

# .....................................................................................................................................................

# ..... Report section ................................................................................................................................

# report=paste(report_dir,"report_",substring(output_dir,13,17),"_",substring(Sys.timeDate(FinCenter = "Berlin"),1,10),"_",
#            substring(Sys.timeDate(FinCenter = "Berlin"),12,13),"-",substring(Sys.timeDate(FinCenter = "Berlin"),15,16),".txt",sep="")
# sink(report,append =T,type = "output",split = T)
# writeLines("==============================================================================================")
# writeLines("R script: DQC_LTER.R           Authors: Brida Christian, Genova Giulio, Zandonai Alessandro")
# writeLines("Institute for Alpine Environment")
# writeLines(paste("start working date and time:",Sys.time()))
# writeLines("==============================================================================================")
# sink()
# .....................................................................................................................................................

# ..... Body ..........................................................................................................................................


# data=read.csv(FILE, stringsAsFactors = F,skip = 1); data=data[-c(1,2),]
data_import <- read_data(FILE_PATH = scheduling_dir,FILE_NAME = FILE,DATETIME_HEADER = DATETIME_HEADER, DATETIME_FORMAT = DATETIME_FORMAT)
header <- data_import [[1]]
header_colnames <- data_import [[2]]
data <- data_import [[3]]

original <- data
mydata <- data

if(check_empty_files(SCHEDULING_DIR = scheduling_dir, FILE = FILE) == TRUE){
  # writeLines(paste(FILE,"WARNING: NO DATA FOUND!!!",sep = " "))
}else{

  if(Use_dwnld_tbl == FALSE){

    mydata <- deletes_duplcated_data(DATA = mydata)                                                                                                         # <- Deletes identical rows if found

    overlap <- detect_overlap(DATA = mydata,DATETIME_HEADER = DATETIME_HEADER, RECORD_HEADER = RECORD_HEADER)                                               # <- Detect overlap

    if(length(overlap) != 0){
      stop(paste("Overlapping data in files:", FILE))
    }else{

      mydata  <- missing_dates(DATA = mydata, DATETIME_HEADER = DATETIME_HEADER, RECORD_HEADER = RECORD_HEADER,DATETIME_SAMPLING = DATETIME_SAMPLING)       # <- fill missing dates with NA

      mydata <- exclude_out_of_range(DATA = mydata, SUPPORT_DIR = support_dir, RANGE_FILE = RANGE_FILE)                     # <- Substitute with NA data out of phisical range
    }
  }

  if(Use_dwnld_tbl == TRUE){

    dwnld_tbl <- read.csv(paste(support_dir,"Download_table/",DOWNLOAD_TABLE,sep=""),stringsAsFactors = F)                  # <-- dwnld_tbl: the list of dates of last download data
    file.copy(from = paste(support_dir,"Download_table/",DOWNLOAD_TABLE,sep=""),to = paste(support_dir,"Download_table/",substring(DOWNLOAD_TABLE, 1,nchar(DOWNLOAD_TABLE)-4),"_old.csv",sep=""),overwrite = T)

    ##############################################   to complete
    ##############################################   to complete
    lst_dwnld_row<-grep(substring(FILE,1,nchar(FILE)-4),dwnld_tbl[,1],fixed = T )
    lst_dwnld_date<-as.POSIXct(paste(substring(dwnld_tbl[lst_dwnld_row,2],0),":00",sep = ""),tz = 'Etc/GMT-1' )
    #lst_dwnld_date<-as.POSIXct(paste("2016-11-24 06:00",":00",sep = ""),tz = 'Etc/GMT-1' )
    last_data_date<-as.POSIXct(paste(substring(data[nrow(data),1],1,16),":00",sep = ""),tz = 'Etc/GMT-1' )
  }
}

# .....................................................................................................................................................
