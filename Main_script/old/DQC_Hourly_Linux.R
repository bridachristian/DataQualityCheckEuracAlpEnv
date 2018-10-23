#-------------------------------------------------------------------------------------------------------------------------------------------------------
# File Title:   DQC_Multi_Files.R
# TITLE:        Data quality check LTER on different files in scheduling folder
# Author:       Brida Christian, Genova Giulio, Zandonai Alessandro
#               Institute for Alpine Environment
# Data:         13/02/2018
# Version:      2.0
#
#------------------------------------------------------------------------------------------------------------------------------------------------------

rm(list = ls(all.names = TRUE))

print("--------------------------------------------------------------------------------------------------")
print(paste("Data Quality Check:",Sys.time()))

# ..... Libraries .....................................................................................................................................
library(devtools,lib.loc = '/home/cbrida/Libraries_DataQualityCheckEuracAlpEnv/') 
install_github("bridachristian/DataQualityCheckEuracAlpEnv")
library("DataQualityCheckEuracAlpEnv")
install_github("alexsanjoseph/compareDF")
library(compareDF)

library(zoo,lib.loc = '/home/cbrida/Libraries_DataQualityCheckEuracAlpEnv/')
library(knitr,lib.loc = '/home/cbrida/Libraries_DataQualityCheckEuracAlpEnv/')
library(ggplot2,lib.loc = '/home/cbrida/Libraries_DataQualityCheckEuracAlpEnv/')
library(reshape2,lib.loc = '/home/cbrida/Libraries_DataQualityCheckEuracAlpEnv/')
library(DT,lib.loc = '/home/cbrida/Libraries_DataQualityCheckEuracAlpEnv/')
library(htmltools,lib.loc = '/home/cbrida/Libraries_DataQualityCheckEuracAlpEnv/')
library(rmarkdown,lib.loc = '/home/cbrida/Libraries_DataQualityCheckEuracAlpEnv/')
library(yaml,lib.loc = '/home/cbrida/Libraries_DataQualityCheckEuracAlpEnv/')
library(highr,lib.loc = '/home/cbrida/Libraries_DataQualityCheckEuracAlpEnv/')

Sys.setenv(RSTUDIO_PANDOC = "/usr/lib/rstudio/bin/pandoc/")

# .....................................................................................................................................................

# ..... Params section .....................................................................................................................................
main_dir = "/shared/"

project_type = c("LTER","MONALISA")

# PROJECT = "LTER" # Possible project: "LTER"; "MONALISA";
# input_dir <- paste(main_dir,"/loggernet/scheduling_test/",sep = "")                    # where input files are

input_dir <- paste(main_dir,"Stations_Data/Data/LoggerNet_Raw_Data/Data/",sep = "")                    # where input files are

project_dir <- "/home/cbrida/DataQualityCheckEuracAlpEnv/"  # where package is developed or cloned from github

DQC_setting_dir <- paste(main_dir,"Stations_Data/DQC/",sep = "")

logger_info_file <- paste(DQC_setting_dir,"/Process/Logger_number_and_software.csv", sep = "")
range_dir <- paste(DQC_setting_dir,"/Process/", sep = "")
download_table_dir <- paste(DQC_setting_dir,"/Process/", sep = "")

file.create(paste(DQC_setting_dir,"lock_DQC.lock",sep = ""))

for(PROJECT in project_type){
  data_output_dir <- paste(main_dir,"Stations_Data/Data/DQC_Processed_Data/",PROJECT,"/Stations/",sep = "")  # where to put output files
  report_output_dir <- paste(main_dir,"Stations_Data/Data/DQC_Processed_Data/",PROJECT,"/DQC_Reports/",sep = "")  # where to put output reports
  database_file_dir <- paste(main_dir,"Stations_Data/Data/DQC_DB/",PROJECT,"/", sep = "")  # where to put output files (MODIFIED FOR DATABASE TESTING) -----> "Permission denied"
  
  data_from_row =  5                                             # <-- Row number of first data
  header_row_number =  2                                         # <-- Row number of header
  datetime_header =  "TIMESTAMP"                                 # <-- header corresponding to TIMESTAMP
  datetime_format =  "%Y-%m-%d %H:%M"                          # <-- datetime format. Use only: Y -> year, m -> month, d -> day, H -> hour, M -> minute
  datetime_sampling =  "15 min"
  record_header =  "RECORD"
  range_file =  "Range.csv"
  
  write_output_files =  "TRUE"
  write_output_report =  "FALSE"
  
  
  # file <- "M4s.dat"
  # start_date <- NA
  
  # ~~~ Default directory ~~~~
  
  
  
  if(write_output_report == TRUE){
    Rmd_report_generator <- paste(project_dir, "Rmd/DQC_Report_Generator.Rmd",sep = "")
  }else{
    Rmd_report_generator <- paste(project_dir, "Rmd/DQC_Calculator.Rmd",sep = "")
  }
  
  # ..........................................................................................................................................................
  
  # ..... files selection .....................................................................................................................................
  
  files_available = dir(input_dir,pattern = ".dat")                  # <-- Admitted pattern:  ".dat" or ".csv"
  
  files_available = files_available[!grepl(pattern = "backup",x = files_available)]          # REMOVE FILES WITH WRONG NAMES (.dat.backup not admitted) 
  files_available = files_available[!grepl(pattern = "LTER",x = files_available)]          # REMOVE FILES WITH WRONG NAMES (LTER_XXX.dat not admitted) 
  files_available = files_available[!grepl(pattern = "MONALISA",x = files_available)]      # REMOVE FILES WITH WRONG NAMES (MONALISA_XXX.dat not admitted) 
  
  # ..........................................................................................................................................................
  
  # ..... download table section .....................................................................................................................................
  
  
  download_table = read_and_update_download_table(DOWNLOAD_TABLE_DIR = download_table_dir, FILES_AVAILABLE = files_available, DATETIME_FORMAT = datetime_format)
  
  download_table_proj = download_table$Station[which(download_table$Project == PROJECT)]
  
  files_available_project = files_available[which(substring(files_available,1, nchar(files_available)-4) %in% download_table_proj)]
  
  ############################################
  t = 1
  
  final_dataframe = matrix(ncol = 19, nrow = length(files_available_project))
  
  colnames(final_dataframe) = c("Station", "Status",
                                "flag_empty","flag_logger_number", "flag_error_df","flag_date",
                                "flag_duplicates_rows","flag_overlap","flag_missing_dates",
                                "flag_range_variable_to_set","flag_range_variable_new","flag_out_of_range",
                                "flag_new_duplicates_rows","flag_new_overlap","flag_new_missing_dates", "flag_append_new",   
                                "Report_link", "Data_folder", "File_name")
  
  
  
  
  report_start = Sys.time()
  
  
  for(t in  1: length(files_available_project)){
    gc(reset = T)
    
    rm(list = setdiff(ls(all.names = TRUE),c("main_dir","DQC_setting_dir","t","data_from_row","datetime_format","datetime_header","datetime_sampling","download_table","download_table_dir",
                                             "files_available","files_available_project","header_row_number","input_dir","data_output_dir","output_dir_raw","report_output_dir","project_dir",
                                             "range_dir","range_file","record_header","Rmd_report_generator","write_output_files","write_output_report",
                                             "report_start", "final_dataframe","output_dir_report", "database_file_dir","logger_info_file")))
    
    
    FILE = files_available_project[t]
    
    w_dwnl = which(download_table$Station == substring(FILE, 1, nchar(FILE) - 4))
    dwnl_info = download_table[w_dwnl,]
    
    if(dir.exists(paste(data_output_dir,substring(FILE,1,nchar(FILE)-4),"/", sep = ""))){                # create subfolder to store data organized by station name
      if(dir.exists(paste(data_output_dir,substring(FILE,1,nchar(FILE)-4),"/Total/", sep = ""))){
        output_dir_data_new = paste(data_output_dir,substring(FILE,1,nchar(FILE)-4),"/Total/", sep = "")
        output_dir_raw_new = paste(data_output_dir,substring(FILE,1,nchar(FILE)-4),"/Raw/", sep = "")
      }else{
        dir.create(paste(data_output_dir,substring(FILE,1,nchar(FILE)-4),"/Reports/", sep = ""))
        dir.create(paste(data_output_dir,substring(FILE,1,nchar(FILE)-4),"/Raw/", sep = ""))
        dir.create(paste(data_output_dir,substring(FILE,1,nchar(FILE)-4),"/Total/", sep = ""))
        dir.create(paste(data_output_dir,substring(FILE,1,nchar(FILE)-4),"/Processed/", sep = ""))
        dir.create(paste(data_output_dir,substring(FILE,1,nchar(FILE)-4),"/Pics/", sep = ""))
        output_dir_data_new = paste(data_output_dir,substring(FILE,1,nchar(FILE)-4),"/Total/", sep = "")
        output_dir_raw_new = paste(data_output_dir,substring(FILE,1,nchar(FILE)-4),"/Raw/", sep = "")
        
      }
    }else{
      dir.create(paste(data_output_dir,substring(FILE,1,nchar(FILE)-4),"/", sep = ""))
      dir.create(paste(data_output_dir,substring(FILE,1,nchar(FILE)-4),"/Reports/", sep = ""))
      dir.create(paste(data_output_dir,substring(FILE,1,nchar(FILE)-4),"/Raw/", sep = ""))
      dir.create(paste(data_output_dir,substring(FILE,1,nchar(FILE)-4),"/Total/", sep = ""))
      dir.create(paste(data_output_dir,substring(FILE,1,nchar(FILE)-4),"/Processed/", sep = ""))
      dir.create(paste(data_output_dir,substring(FILE,1,nchar(FILE)-4),"/Pics/", sep = ""))
      output_dir_data_new = paste(data_output_dir,substring(FILE,1,nchar(FILE)-4),"/Total/", sep = "")
      output_dir_raw_new = paste(data_output_dir,substring(FILE,1,nchar(FILE)-4),"/Raw/", sep = "")
    }
    
    if(dir.exists(paste(database_file_dir,substring(FILE,1,nchar(FILE)-4),"/", sep = ""))){                # create subfolder to store mini files for database organized by station name 
      if(dir.exists(paste(database_file_dir,substring(FILE,1,nchar(FILE)-4),"/Data/", sep = ""))){ 
        database_file_dir_new = paste(database_file_dir,substring(FILE,1,nchar(FILE)-4),"/Data/", sep = "")
      }else{
        dir.create(paste(database_file_dir,substring(FILE,1,nchar(FILE)-4),"/Data/", sep = ""))
        dir.create(paste(database_file_dir,substring(FILE,1,nchar(FILE)-4),"/Pics/", sep = ""))
        database_file_dir_new = paste(database_file_dir,substring(FILE,1,nchar(FILE)-4),"/Data/", sep = "")
        
      }
    }else{
      dir.create(paste(database_file_dir,substring(FILE,1,nchar(FILE)-4),"/", sep = "")) 
      dir.create(paste(database_file_dir,substring(FILE,1,nchar(FILE)-4),"/Data/", sep = ""))
      dir.create(paste(database_file_dir,substring(FILE,1,nchar(FILE)-4),"/Pics/", sep = ""))
      database_file_dir_new = paste(database_file_dir,substring(FILE,1,nchar(FILE)-4),"/Data/", sep = "")
    }
    
    if(dwnl_info$Stop_DQC == 0){
      
      date_last_modif_file = as.character(format(file.mtime(paste(input_dir,FILE,sep = "")),format = datetime_format))
      
      if(date_last_modif_file != dwnl_info$Last_Modification | is.na(dwnl_info$Last_Modification)){
        
        input_dir = input_dir
        output_dir_data = output_dir_data_new
        output_dir_raw = output_dir_raw_new
        output_dir_report = report_output_dir
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
        database_dir = database_file_dir_new
        file = FILE
        start_date = dwnl_info$Last_date
        logger_info_file = logger_info_file
        record_check = dwnl_info$record_check
        
        output_file_report = paste("DQC_Report_",substring(FILE,1,nchar(FILE)-4),"_tmp.html",sep = "")
        
        rm(dwnl_info)
        
        rmarkdown::render(input = Rmd_report_generator ,
                          output_file = output_file_report,
                          output_dir = output_dir_report,
                          params = list(input_dir = input_dir ,
                                        output_dir_data = output_dir_data ,
                                        output_dir_raw = output_dir_raw,
                                        output_dir_report = output_dir_report ,
                                        project_dir = project_dir ,
                                        data_from_row = data_from_row ,
                                        header_row_number = header_row_number ,
                                        datetime_header = datetime_header ,
                                        datetime_format = datetime_format ,
                                        datetime_sampling = datetime_sampling ,
                                        record_header = record_header ,
                                        range_file = range_file ,
                                        write_output_files = write_output_files ,
                                        write_output_report = write_output_report ,
                                        database_dir = database_dir,
                                        file = file ,
                                        start_date = start_date,
                                        logger_info_file = logger_info_file,
                                        record_check = record_check))
        
        gc(reset = T)
        
        if(flag_empty == 0 & flag_logger_number == 0 & flag_error_df == 0 & flag_date == 0){
          out_filename_date = paste(substring(mydata[nrow(mydata),which(colnames(mydata) == datetime_header)],1,4),
                                    substring(mydata[nrow(mydata),which(colnames(mydata) == datetime_header)],6,7),
                                    substring(mydata[nrow(mydata),which(colnames(mydata) == datetime_header)],9,10),
                                    # "_",
                                    substring(mydata[nrow(mydata),which(colnames(mydata) == datetime_header)],12,13),
                                    substring(mydata[nrow(mydata),which(colnames(mydata) == datetime_header)],15,16),
                                    sep = "")
          
          last_date = mydata[nrow(mydata),which(colnames(mydata)== datetime_header)]
          
        } else {
          out_filename_date = "no_datetime"
        }
        
        
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
        
        if(write_output_report == TRUE){
          output_file_report = file.rename(from = paste(output_dir_report,output_file_report,sep = ""),
                                           to = paste(output_dir_report,out_filename_report,sep = ""))
        }else{
          file.remove(paste(output_dir_report,output_file_report,sep = ""))
        }
        
        
        
        if(!is.na(flag_missing_dates)){
          if(flag_new_overlap == 1){
            if(write_output_report == TRUE){
              final_info = c(substring(FILE,1,nchar(FILE)-4), "Analyzed and write output",
                             flags_df$value,
                             paste(output_dir_report,out_filename_report,sep = ""),
                             NA,
                             NA)
            }else{
              final_info = c(substring(FILE,1,nchar(FILE)-4), "Analyzed and write output",
                             flags_df$value,
                             NA,
                             NA,
                             NA)
            }
          }else{
            download_table$Last_date[w_dwnl] = last_date
            download_table$Last_Modification[w_dwnl] = date_last_modif_file
            write.csv(download_table,paste(download_table_dir,"download_table.csv",sep = ""),quote = F,row.names = F)
            
            if(write_output_report == TRUE){
              final_info = c(substring(FILE,1,nchar(FILE)-4), "Analyzed and write output",
                             flags_df$value,
                             paste(output_dir_report,out_filename_report,sep = ""),
                             paste(output_dir_data,sep = ""),
                             paste(file_names[length(file_names)],sep = ""))
            }else{
              final_info = c(substring(FILE,1,nchar(FILE)-4), "Analyzed and write output",
                             flags_df$value,
                             NA,
                             paste(output_dir_data_new,sep = ""),
                             paste(file_names[length(file_names)],sep = ""))
            }
          }
        }else{
          # file_stopped = c(file_stopped, FILE)
          if(write_output_report == TRUE){
            final_info = c(substring(FILE,1,nchar(FILE)-4), "Analyzed with errors",
                           flags_df$value,
                           paste(output_dir_report,out_filename_report,sep = ""),
                           NA, NA )
          }else{
            final_info = c(substring(FILE,1,nchar(FILE)-4), "Analyzed with errors",
                           flags_df$value,
                           NA,
                           NA, NA )
          }
          
          
        }
        
      } else {
        warning(paste("File",FILE, "already analyzed!"))
        # file_already_processed = c(file_already_processed,FILE)
        final_info = c(substring(FILE,1,nchar(FILE)-4), "Already analyzed",
                       NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA,
                       NA,
                       NA, NA)
        output_dir_report = report_output_dir
        
      }
      
    }else{
      final_info = c(substring(FILE,1,nchar(FILE)-4), "Not analyzed",
                     NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA,
                     NA,
                     NA, NA)
      output_dir_report = report_output_dir
      
    }
    
    # final_dataframe = rbind(final_dataframe,final_info)
    final_dataframe[t,] = final_info
    
    gc(reset = T)
  }
  
  # ..... Final Report .....................................................................................................................................
  
  
  input_final = paste(project_dir,"Rmd/DQC_Final_Report_Hourly.Rmd",sep = "")
  output_file_final =  paste("DQC_Report_",substring(report_start,1,4),
                             substring(report_start,6,7),
                             substring(report_start,9,10),
                             substring(report_start,12,13),
                             substring(report_start,15,16),".html", sep = "")
  output_dir_final = output_dir_report
  
  rmarkdown::render(input = input_final,
                    output_file = output_file_final ,
                    output_dir = output_dir_final,
                    params = list(report_start = report_start ,
                                  final_dataframe = final_dataframe))
  
  
  # ..... Data preparation for Database .....................................................................................................................................
  
  
  print("--------------------------------------------------------------------------------------------------")
  
}

file.remove(paste(DQC_setting_dir,"lock_DQC.lock",sep = ""))
