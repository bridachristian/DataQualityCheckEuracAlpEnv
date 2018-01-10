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

input_dir <- "H:/Projekte/Klimawandel/Experiment/data/2order/DQC_BrC_test_data/"                # where input files are
# input_dir <- "H:/Projekte/Klimawandel/Experiment/data/2order/DataQualityCheckEuracAlpEnv/Data/Input/"                # where input files are
output_dir_data <- "H:/Projekte/Klimawandel/Experiment/data/2order/DQC_BrC_test_data/test_output/Out_Data/"   # where to put output files
# output_dir_data <- "H:/Projekte/Klimawandel/Experiment/data/2order/DataQualityCheckEuracAlpEnv/Data/Output/data/"   # where to put output files
output_dir_report <- "H:/Projekte/Klimawandel/Experiment/data/2order/DQC_BrC_test_data/test_output/Out_Report/"   # where to put output reports
# output_dir_report <- "H:/Projekte/Klimawandel/Experiment/data/2order/DataQualityCheckEuracAlpEnv/Data/Output/report/"   # where to put output reports
project_dir <- "H:/Projekte/Klimawandel/Experiment/data/2order/DataQualityCheckEuracAlpEnv/"  # where package is developed or cloned from github

data_from_row =  5                                             # <-- Row number of first data
header_row_number =  2                                         # <-- Row number of header
datetime_header =  "TIMESTAMP"                                 # <-- header corresponding to TIMESTAMP
datetime_format =  "yyyy-mm-dd HH:MM"                          # <-- datetime format. Use only: y -> year, m -> month, d -> day, H -> hour, M -> minute
datetime_sampling =  "15 min"
record_header =  "RECORD"
range_file =  "Range.csv"

write_output_files =  "TRUE"
write_output_report =  "TRUE"


# file <- "M4s.dat"
# start_date <- NA

# ~~~ Default directory ~~~~

range_dir <- paste(project_dir, "Data/Support_files/Range/",sep = "")
download_table_dir <- paste(project_dir, "Data/Support_files/Download_table/",sep = "")
Rmd_report_generator <- paste(project_dir, "Rmd/DQC_Report_Generator.Rmd",sep = "")

# ..........................................................................................................................................................

# ..... files selection .....................................................................................................................................

files_available = dir(input_dir,pattern = ".dat")                  # <-- Admitted pattern:  ".dat" or ".csv"

# ..........................................................................................................................................................

# ..... download table section .....................................................................................................................................

download_table_file = paste(download_table_dir,"download_table.csv",sep = "") 

if(!file.exists(download_table_file)){              # <- define or extact info from download table
  
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

start_date_available = start_date[which(download_table$Station %in% substring(files_available, 1, nchar(files_available)-4))]

# ..... files selection on flag Stop_DQC .....................................................................................................................................

files_to_avoid = download_table$Station[download_table$Stop_DQC == 1]  # exclude the file with flag Stop_DQC == 1 (flag to set manually)

if(length(files_to_avoid) == 0){
  files = files_available
  start = start_date_available
}else{
  files = files_available[-which(substring(files_available,1, nchar(files_available)-4) %in% files_to_avoid)]
  start = start_date_available[-which(substring(files_available,1, nchar(files_available)-4) %in% files_to_avoid)]
  # which(download_table$Station %in% substring(files, 1, nchar(files)-4))
}

last_modification = last_modification[which(download_table$Station %in% substring(files, 1, nchar(files)-4))]
new_modification = as.character(file.mtime(paste(input_dir,files,sep = ""))) 

# ..... files selection on file update .....................................................................................................................................

files_updated = files[last_modification != new_modification]
start_updated = start[last_modification != new_modification]

d = data.frame(files_updated,new_modification[last_modification != new_modification])
colnames(d) = c("file", "new_modification")
if(nrow(d) > 0){
  w_update_last_modification = which(download_table$Station %in% substring(d[,1],1, nchar(d[,1])-4))
  
  download_table$Last_Modification[w_update_last_modification] = d$new_modification
  
  write.csv(download_table,download_table_file,quote = F,row.names = F)
}
# names(last_modification_new) = files
# last_modification_new = file.mtime(paste(input_dir,files_available,sep = ""))
# ..........................................................................................................................................................

# ..........................................................................................................................................................


# ..... rmarkdown render section .....................................................................................................................................
# substitute files with files_updated 


# input_dir <- "H:/Projekte/Klimawandel/Experiment/data/2order/DQC_BrC_test_data/"                # where input files are
# output_dir_data <- "H:/Projekte/Klimawandel/Experiment/data/2order/DQC_BrC_test_data/test_output/Out_Data/"   # where to put output files
# output_dir_report <- "H:/Projekte/Klimawandel/Experiment/data/2order/DQC_BrC_test_data/test_output/Out_Report/"   # where to put output reports
# project_dir <- "H:/Projekte/Klimawandel/Experiment/data/2order/DataQualityCheckEuracAlpEnv/"  # where package is developed or cloned from github
# data_from_row =  5                                             # <-- Row number of first data
# header_row_number =  2                                         # <-- Row number of header
# datetime_header =  "TIMESTAMP"                                 # <-- header corresponding to TIMESTAMP
# datetime_format =  "yyyy-mm-dd HH:MM"                          # <-- datetime format. Use only: y -> year, m -> month, d -> day, H -> hour, M -> minute
# datetime_sampling =  "15 min"
# record_header =  "RECORD"
# range_file =  "Range.csv"
# write_output_files =  "TRUE"
# write_output_report =  "TRUE"
# file <- "M3.dat"
# start_date <- NA

file_to_process = files_available[8]                # WARNING:  the selection of files to process is to define better!!!!!
start_date_to_process  = NA         # WARNING: 

i=1

# test dqc function !!!!!!
input_dir = input_dir 
output_dir_data = output_dir_data
output_dir_report = output_dir_report
project_dir = project_dir
data_from_row = data_from_row
header_row_number = header_row_number
datetime_header = datetime_header
datetime_format = datetime_format
datetime_sampling = datetime_sampling
record_header = record_header
range_file = range_file
write_output_files = write_output_files
write_output_report = write_output_report

file = file_to_process[1]
start_date = start_date_to_process[1]


# dqc_funct = function(input_dir, 
#                      output_dir_data, 
#                      output_dir_report, 
#                      project_dir,
#                      data_from_row,
#                      header_row_number,
#                      datetime_header,
#                      datetime_format,
#                      datetime_sampling, 
#                      record_header,
#                      range_file,
#                      write_output_files,
#                      write_output_report ,
#                      file,
#                      start_date){
  
  output_file_report = paste("DQC_Report_",substring(file[i],1,nchar(file[i])-4),"_tmp.html",sep = "")
  
  
  rmarkdown::render(input = Rmd_report_generator ,
                    output_file = output_file_report,
                    output_dir = output_dir_report,
                    params = list(input_dir = input_dir , 
                                  output_dir_data = output_dir_data, 
                                  output_dir_report = output_dir_report, 
                                  project_dir = project_dir,
                                  data_from_row = data_from_row,
                                  header_row_number = header_row_number,
                                  datetime_header = datetime_header,
                                  datetime_format = datetime_format,
                                  datetime_sampling = datetime_sampling , 
                                  record_header = record_header,
                                  range_file = range_file,
                                  write_output_files = write_output_files,
                                  write_output_report = write_output_report,
                                  file = file,
                                  start_date = start_date))
  
  if(flag_date == 0){
  out_filename = paste(substring(mydata[nrow(mydata),which(colnames(mydata) == datetime_header)],1,4),
                       substring(mydata[nrow(mydata),which(colnames(mydata) == datetime_header)],6,7),
                       substring(mydata[nrow(mydata),which(colnames(mydata) == datetime_header)],9,10),
                       # "_",
                       substring(mydata[nrow(mydata),which(colnames(mydata) == datetime_header)],12,13),
                       substring(mydata[nrow(mydata),which(colnames(mydata) == datetime_header)],15,16),
                       sep = "")
  
  output_file_report = file.rename(from = paste(output_dir_report,output_file_report,sep = ""),
                                   to = paste(output_dir_report,"DQC_Report_",substring(file[i],1,nchar(file[i])-4),"_",out_filename,".html",sep = ""))
  
  
  last_date = mydata[nrow(mydata),which(colnames(mydata)== datetime_header)]
  
  download_table$Last_date[i] = as.character(last_date)
  write.csv(download_table,download_table_file,quote = F,row.names = F)
  } else {
    warning("File already process!")
  }
  
# }


# for(i in 1: length(file_to_process)){
# 
# output_file_report = paste("DQC_Report_",substring(files[i],1,nchar(files[1])-4),"_tmp.html",sep = "")
# 
# 
# rmarkdown::render(input = Rmd_report_generator ,
#                   output_file = output_file_report,
#                   output_dir = output_dir_report,
#                   params = list(input_dir = input_dir ,
#                                 output_dir_data = output_dir_data,
#                                 output_dir_report = output_dir_report,
#                                 project_dir = project_dir,
#                                 data_from_row = data_from_row,
#                                 header_row_number = header_row_number,
#                                 datetime_header = datetime_header,
#                                 datetime_format = datetime_format,
#                                 datetime_sampling = datetime_sampling,
#                                 record_header = record_header,
#                                 range_file = range_file,
#                                 write_output_files = write_output_files,
#                                 write_output_report = write_output_report,
#                                 file = file_to_process[i],
#                                 start_date = start_date_to_process[i] ))
# 
# 
# out_filename = paste(substring(mydata[nrow(mydata),which(colnames(mydata) == datetime_header)],1,4),
#                      substring(mydata[nrow(mydata),which(colnames(mydata) == datetime_header)],6,7),
#                      substring(mydata[nrow(mydata),which(colnames(mydata) == datetime_header)],9,10),
#                      # "_",
#                      substring(mydata[nrow(mydata),which(colnames(mydata) == datetime_header)],12,13),
#                      substring(mydata[nrow(mydata),which(colnames(mydata) == datetime_header)],15,16),
#                      sep = "")
# 
# output_file_report = file.rename(from = paste(output_dir_report,output_file_report,sep = ""),
#                                  to = paste(output_dir_report,"DQC_Report_",substring(files[i],1,nchar(files[1])-4),"_",out_filename,".html",sep = ""))
# 
# 
# last_date = mydata[nrow(mydata),which(colnames(mydata)== datetime_header)]
# 
# download_table$Last_date[i] = as.character(last_date)
# write.csv(download_table,download_table_file,quote = F,row.names = F)
# 
# }