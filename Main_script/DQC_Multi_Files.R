#-------------------------------------------------------------------------------------------------------------------------------------------------------
# File Title:   DQC_Multi_Files.R
# TITLE:        Data quality check LTER on different files in scheduling folder
# Author:       Brida Christian, Genova Giulio, Zandonai Alessandro
#               Institute for Alpine Environment
# Data:         13/12/2017
# Version:      1.0
#
#------------------------------------------------------------------------------------------------------------------------------------------------------

rm(list = ls())

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
# .....................................................................................................................................................

# ..... Params section .....................................................................................................................................

# scheduling_dir =  "H:/Projekte/Klimawandel/Experiment/data/2order/DataQualityCheckEuracAlpEnv/Data/Input/"
scheduling_dir =  "H:/Projekte/Klimawandel/Experiment/data/2order/DQC_BrC_test_data/"
report_dir =  "H:/Projekte/Klimawandel/Experiment/data/2order/DataQualityCheckEuracAlpEnv/Report/"
# output_dir =  "H:/Projekte/Klimawandel/Experiment/data/2order/DataQualityCheckEuracAlpEnv/Data/Output/"
output_dir =  "H:/Projekte/Klimawandel/Experiment/data/2order/DQC_BrC_test_data/test_output/"
support_dir =  "H:/Projekte/Klimawandel/Experiment/data/2order/DataQualityCheckEuracAlpEnv/Data/Support_files/"
write_output =  "TRUE"
RANGE_FILE =  "Range.csv"
DATA_FROM_ROW =  5                                             # <-- Row number of first data
HEADER_ROW_NUMBER =  2                                         # <-- Row number of header
DATETIME_HEADER =  "TIMESTAMP"                                 # <-- header corresponding to TIMESTAMP
DATETIME_FORMAT =  "yyyy-mm-dd HH:MM"                          # <-- datetime format. Use only: y -> year, m -> month, d -> day, H -> hour, M -> minute
DATETIME_SAMPLING =  "15 min"
RECORD_HEADER =  "RECORD"

files = dir(scheduling_dir,pattern = ".dat")

# ..........................................................................................................................................................

# ..... download table section .....................................................................................................................................

download_table_file = paste(support_dir,"Download_table/download_table.csv",sep = "")

if(!file.exists(download_table_file)){
  first_download_table = data.frame(substring(files,1, nchar(files)-4), rep(NA, times = length(files)), rep(0,times = length(files)))
  colnames(first_download_table) = c("Station", "Last_date", "Stop_DQC")
  write.csv(first_download_table,download_table_file,quote = F,row.names = F)
}

file.copy(from = download_table_file, to = paste(substring(download_table_file,1,nchar(download_table_file)-4),"_old.csv",sep = ""),overwrite = TRUE)
download_table = read.csv(download_table_file,stringsAsFactors = F)



# ..........................................................................................................................................................



# ..... rmarkdown render section .....................................................................................................................................

for(i in 1: length(files)){
  rmarkdown::render(input = paste(report_dir,"DQC_Manual_Multi_Files.Rmd",sep = ""),
                    output_file = paste("DQC_Report_",substring(files[i],1,nchar(files[1])-4),".html",sep = ""),
                    output_dir = paste(report_dir,"/Output_report/",sep = ""),
                    params = list(file = files[i],
                                  scheduling_dir = scheduling_dir,
                                  report_dir = report_dir,
                                  output_dir = output_dir ,
                                  support_dir = support_dir,
                                  write_output = write_output,
                                  RANGE_FILE = RANGE_FILE,
                                  DATA_FROM_ROW = DATA_FROM_ROW,
                                  HEADER_ROW_NUMBER = HEADER_ROW_NUMBER,
                                  DATETIME_HEADER = DATETIME_HEADER,
                                  DATETIME_FORMAT = DATETIME_FORMAT,
                                  DATETIME_SAMPLING = DATETIME_SAMPLING,
                                  RECORD_HEADER = RECORD_HEADER ))


  rm(list=setdiff(ls(),c("scheduling_dir","report_dir","output_dir", "support_dir",
                         "write_output","RANGE_FILE","DATA_FROM_ROW", "HEADER_ROW_NUMBER",
                         "DATETIME_HEADER","DATETIME_FORMAT","DATETIME_SAMPLING", "RECORD_HEADER",
                         "files", "i")))
}

# i=1
# rmarkdown::render(input = paste(report_dir,"DQC_Manual_Multi_Files.Rmd",sep = ""),
#                   output_file = paste("DQC_Report_",substring(files[i],1,nchar(files[1])-4),".html",sep = ""),
#                   output_dir = paste(report_dir,"/Output_report/",sep = ""),
#                   params = "ask")
#
