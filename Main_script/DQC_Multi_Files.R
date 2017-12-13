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
  file.copy(from = download_table_file, to = paste(substring(download_table_file,1,nchar(download_table_file)-4),"_old.csv",sep = ""),overwrite = TRUE)

} else{
  file.copy(from = download_table_file, to = paste(substring(download_table_file,1,nchar(download_table_file)-4),"_old.csv",sep = ""),overwrite = TRUE)
  download_table = read.csv(download_table_file,stringsAsFactors = F)
  station_to_process = substring(files,1, nchar(files)-4)
  station_already_register = download_table$Station
  station_to_add = setdiff(station_to_process,station_already_register)

  df_to_add = data.frame(station_to_add, rep(NA, times = length(station_to_add)), rep(0,times = length(station_to_add)))
  colnames(df_to_add) = c("Station", "Last_date", "Stop_DQC")

  download_table = rbind(download_table, df_to_add )
}

start_date = download_table$Last_date
# file_to_analyze = download_table$Station[download_table$Stop_DQC == 0]
# setdiff(station_to_process,station_already_register)

# ..........................................................................................................................................................



# ..... rmarkdown render section .....................................................................................................................................

for(i in 1: length(files)){
  
  input = paste(report_dir,"DQC_Manual_Multi_Files.Rmd",sep = "")
  output_file = paste("DQC_Report_",substring(files[i],1,nchar(files[1])-4),".html",sep = "")
  output_dir = paste(report_dir,"Output_report/",sep = "")
  
  rmarkdown::render(input = input,
                    output_file = output_file,
                    output_dir = output_dir,
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
                                  RECORD_HEADER = RECORD_HEADER,
                                  start_date = start_date[i]))
  
  last_date = mydata[nrow(mydata),which(colnames(mydata)== DATETIME_HEADER)]
  download_table$Last_date[i] = last_date
  
  new_output_file = paste("DQC_Report_",substring(files[i],1,nchar(files[1])-4),"_",
                          substring(last_date,1,4),
                          substring(last_date,6,7),
                          substring(last_date,9,10),
                          substring(last_date,12,13),
                          substring(last_date,15,16),".html",sep = "")
  # if(!dir.exists(paste(output_dir,substring(files[i],1,nchar(files[1])-4),sep = ""))){
  #   dir.create(paste(output_dir,substring(files[i],1,nchar(files[1])-4),sep = ""))
  #   
  # }
  
  file.rename(from = paste(output_dir,output_file,sep = "") ,to = paste(output_dir,new_output_file,sep = "") )
  write.csv(download_table,download_table_file,quote = F,row.names = F)

}

#   rm(list=setdiff(ls(),c("scheduling_dir","report_dir","output_dir", "support_dir",
#                          "write_output","RANGE_FILE","DATA_FROM_ROW", "HEADER_ROW_NUMBER",
#                          "DATETIME_HEADER","DATETIME_FORMAT","DATETIME_SAMPLING", "RECORD_HEADER",
#                          "files", "i")))


# i=1
# rmarkdown::render(input = paste(report_dir,"DQC_Manual_Multi_Files.Rmd",sep = ""),
#                   output_file = paste("DQC_Report_",substring(files[i],1,nchar(files[1])-4),".html",sep = ""),
#                   output_dir = paste(report_dir,"/Output_report/",sep = ""),
#                   params = "ask")
#
