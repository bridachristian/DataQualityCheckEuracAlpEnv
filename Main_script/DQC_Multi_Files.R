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
# datetime_format =  "yyyy-mm-dd HH:MM"                          # <-- datetime format. Use only: y -> year, m -> month, d -> day, H -> hour, M -> minute
datetime_format =  "%Y-%m-%d %H:%M"                          # <-- datetime format. Use only: Y -> year, m -> month, d -> day, H -> hour, M -> minute
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


download_table = read_and_update_download_table(DOWNLOAD_TABLE_DIR = download_table_dir, FILES_AVAILABLE = files_available)


############################################
j = 1

for(j in  1: length(files_available)){
  
  rm(list = setdiff(ls(),c("j","data_from_row","datetime_format","datetime_header","datetime_sampling","download_table","download_table_dir",
                           "files_available","header_row_number","input_dir","output_dir_data","output_dir_report","project_dir",
                           "range_dir","range_file","record_header","Rmd_report_generator","write_output_files","write_output_report" )))
  FILE = files_available[j]
  
  w_dwnl = which(download_table$Station == substring(FILE, 1, nchar(FILE) - 4))
  dwnl_info = download_table[w_dwnl,] 
  
  if(dwnl_info$Stop_DQC == 0){
    date_last_modif_file = as.character(file.mtime(paste(input_dir,FILE,sep = ""))) 
    
    if(date_last_modif_file != dwnl_info$Last_Modification | is.na(dwnl_info$Last_Modification)){
      
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
      
      file = FILE
      start_date = dwnl_info$Last_date
      
      output_file_report = paste("DQC_Report_",substring(FILE,1,nchar(FILE)-4),"_tmp.html",sep = "")
      
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
      
      
      
      out_filename_date = paste(substring(mydata[nrow(mydata),which(colnames(mydata) == datetime_header)],1,4),
                                substring(mydata[nrow(mydata),which(colnames(mydata) == datetime_header)],6,7),
                                substring(mydata[nrow(mydata),which(colnames(mydata) == datetime_header)],9,10),
                                # "_",
                                substring(mydata[nrow(mydata),which(colnames(mydata) == datetime_header)],12,13),
                                substring(mydata[nrow(mydata),which(colnames(mydata) == datetime_header)],15,16),
                                sep = "")
      
      out_filename_report = paste("DQC_Report_",substring(FILE,1,nchar(FILE)-4),"_",out_filename_date,".html",sep = "")
      
      if(file.exists(paste(output_dir_report,out_filename_report,sep = ""))){
        
        j=0
        repeat{
          j=j+1
          out_filename_report_new = paste(substring(out_filename_report,1, nchar(out_filename_report)-5),"_vers",j,".html",sep = "")
          if(!file.exists(paste(output_dir_report,out_filename_report_new,sep = ""))){
            break
          }
        }
      } else {
        out_filename_report_new = out_filename_report
      }
      
      out_filename_report = out_filename_report_new
      output_file_report = file.rename(from = paste(output_dir_report,output_file_report,sep = ""),
                                       to = paste(output_dir_report,out_filename_report,sep = ""))
      
      
      last_date = mydata[nrow(mydata),which(colnames(mydata)== datetime_header)]
      
      if(!is.na(flags_df$value[8])){
        download_table$Last_date[w_dwnl] = last_date
        download_table$Last_Modification[w_dwnl] = date_last_modif_file
        write.csv(download_table,paste(download_table_dir,"download_table.csv",sep = ""),quote = F,row.names = F)
      }
      
      # download_table$Last_date[w_dwnl] = as.character(last_date)
      # download_table$Last_Modification[w_dwnl] = as.character(last_date)
    } else {
      warning("File already process!")
    }
    
    
    
    # data_written = c(out_filename_data,out_filename_dupli)
    
    
    
  }
}

