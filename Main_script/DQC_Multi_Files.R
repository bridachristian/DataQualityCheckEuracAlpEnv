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

# ~~~ Path ~~~

# input_dir <- "H:/Projekte/Klimawandel/Experiment/data/2order/DataQualityCheckEuracAlpEnv/Data/Input/"  # where input files are
input_dir <- "H:/Projekte/Klimawandel/Experiment/data/2order/DQC_BrC_test_data/"                # where input files are

# output_dir <- "H:/Projekte/Klimawandel/Experiment/data/2order/DataQualityCheckEuracAlpEnv/Data/Output/"  # where to put output files and reports
output_dir <- "H:/Projekte/Klimawandel/Experiment/data/2order/DQC_BrC_test_data/test_output/"   # where to put output files and reports

project_dir <- "H:/Projekte/Klimawandel/Experiment/data/2order/DataQualityCheckEuracAlpEnv/"  # where package is developed or cloned from github

# ~~~ Data file structure ~~~

DATA_FROM_ROW =  5                                             # <-- Row number of first data
HEADER_ROW_NUMBER =  2                                         # <-- Row number of header
DATETIME_HEADER =  "TIMESTAMP"                                 # <-- header corresponding to TIMESTAMP
DATETIME_FORMAT =  "yyyy-mm-dd HH:MM"                          # <-- datetime format. Use only: y -> year, m -> month, d -> day, H -> hour, M -> minute
DATETIME_SAMPLING =  "15 min"
RECORD_HEADER =  "RECORD"

# ~~~ Support files  ~~~

RANGE_FILE =  "Range.csv"

# ~~~ Output results  ~~~

write_output_files =  "TRUE"
write_output_reports =  "TRUE"

# ..........................................................................................................................................................


# # scheduling_dir =  "H:/Projekte/Klimawandel/Experiment/data/2order/DataQualityCheckEuracAlpEnv/Data/Input/"
# scheduling_dir =  "H:/Projekte/Klimawandel/Experiment/data/2order/DQC_BrC_test_data/"
# report_dir =  "H:/Projekte/Klimawandel/Experiment/data/2order/DataQualityCheckEuracAlpEnv/Report/"
# # output_dir =  "H:/Projekte/Klimawandel/Experiment/data/2order/DataQualityCheckEuracAlpEnv/Data/Output/"
# output_dir =  "H:/Projekte/Klimawandel/Experiment/data/2order/DQC_BrC_test_data/test_output/"
# support_dir =  "H:/Projekte/Klimawandel/Experiment/data/2order/DataQualityCheckEuracAlpEnv/Data/Support_files/"
# write_output =  "TRUE"


# ..... files selection .....................................................................................................................................

files_available = dir(input_dir,pattern = ".dat")                  # <-- Admitted pattern:  ".dat" or ".csv"

# ..........................................................................................................................................................

# ..... download table section .....................................................................................................................................

download_table_path = paste(project_dir,"Data/Support_files/Download_table/",sep = "")
download_table_file = paste(download_table_path,"download_table.csv",sep = "")

if(!file.exists(download_table_file)){
  
  first_download_table = data.frame(substring(files_available,1, nchar(files_available)-4), 
                                    rep(NA, times = length(files_available)),
                                    rep(0,times = length(files_available)),
                                    as.character(file.mtime(paste(input_dir,files_available,sep = ""))))
  colnames(first_download_table) = c("Station", "Last_date", "Stop_DQC", "Last_Modification")
  
  download_table = first_download_table
  
  write.csv(first_download_table,download_table_file,quote = F,row.names = F)
  file.copy(from = download_table_file, to = paste(substring(download_table_file,1,nchar(download_table_file)-4),"_old.csv",sep = ""),overwrite = TRUE)
  
} else{
  
  file.copy(from = download_table_file, to = paste(substring(download_table_file,1,nchar(download_table_file)-4),"_old.csv",sep = ""),overwrite = TRUE)
  
  download_table = read.csv(download_table_file,stringsAsFactors = F)
  
  station_to_process = substring(files_available,1, nchar(files_available)-4)
  station_already_register = download_table$Station
  station_to_add = setdiff(station_to_process,station_already_register)
  
  w = which(substring(files_available,1, nchar(files_available)-4) %in% station_to_add)
  
  # file.mtime(paste(input_dir,files_available[w],sep = ""))
  
  if(length(w) != 0){
    
    df_to_add = data.frame(station_to_add,
                           rep(NA, times = length(station_to_add)),
                           rep(0,times = length(station_to_add)),
                           as.character(file.mtime(paste(input_dir,files_available[w],sep = ""))))
    colnames(df_to_add) = c("Station", "Last_date", "Stop_DQC", "Last_Modification")
    
    download_table = rbind(download_table, df_to_add )
    
  }
}

start_date = download_table$Last_date
last_modification = download_table$Last_Modification
# [,c(1,4)]


# ..... files selection .....................................................................................................................................

files_to_avoid = download_table$Station[download_table$Stop_DQC == 1]

if(length(files_to_avoid) == 0){
  files = files_available
}else{
  files = files_available[-which(substring(files_available,1, nchar(files_available)-4) %in% files_to_avoid)]
  last_modification = last_modification [-which(substring(files_available,1, nchar(files_available)-4) %in% files_to_avoid)]
}

new_modification = as.character(file.mtime(paste(input_dir,files,sep = "")))

last_modification == new_modification

names(last_modification_new) = files
last_modification_new = file.mtime(paste(input_dir,files_available,sep = ""))
# ..........................................................................................................................................................
write.csv(download_table,download_table_file,quote = F,row.names = F)

# ..........................................................................................................................................................

i=1

# ..... rmarkdown render section .....................................................................................................................................

for(i in 1: length(files)){
  
  input = paste(report_dir,"DQC_Manual_Multi_Files.Rmd",sep = "")
  output_file = paste("DQC_Report_",substring(files[i],1,nchar(files[1])-4),".html",sep = "")
  output_dir_new = paste(output_dir,"Output_report/",sep = "")
  
  rmarkdown::render(input = input,
                    output_file = output_file,
                    output_dir = output_dir,
                    params = list(file = files[i],
                                  scheduling_dir = scheduling_dir,
                                  report_dir = report_dir,
                                  output_dir = output_dir_new ,
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
  
  file.rename(from = paste(output_dir_new,output_file,sep = "") ,to = paste(output_dir_new,new_output_file,sep = "") )
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
