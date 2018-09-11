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

main_dir = "/shared/test_christian/"

project_type = c("LTER","MONALISA")

PROJECT = "LTER" # Possible project: "LTER"; "MONALISA";

# input_dir <- paste(main_dir,"/Stations_Data/Data/LoggerNet_Raw_Data/Data/",sep = "")                    # where input files are
input_dir <- paste("/shared","/Stations_Data/Data/LoggerNet_Raw_Data/Data/",sep = "")                    # where input files are

project_dir <- "/home/cbrida/DataQualityCheckEuracAlpEnv/"  # where package is developed or cloned from github

DQC_setting_dir <- paste(main_dir,"/Stations_Data/DQC/",sep = "")

logger_info_file <- paste(DQC_setting_dir,"/Process/Logger_number_and_software.csv", sep = "")
range_dir <- paste(DQC_setting_dir,"/Process/", sep = "")
download_table_dir <- paste(DQC_setting_dir,"/Process/", sep = "")
issue_counter_dir <- paste(DQC_setting_dir,"/Process/", sep = "")

# file.create(paste(DQC_setting_dir,"lock_DQC.lock",sep = ""))

for(PROJECT in project_type){
  data_output_dir <- paste(main_dir,"/Stations_Data/Data/DQC_Processed_Data/",PROJECT,"/Stations/",sep = "")  # where to put output files
  report_output_dir <- paste(main_dir,"/Stations_Data/Data/DQC_Processed_Data/",PROJECT,"/DQC_Reports/",sep = "")  # where to put output reports
  database_file_dir <- paste(main_dir,"/Stations_Data/Data/DQC_DB/",PROJECT,"/", sep = "")  # where to put output files (MODIFIED FOR DATABASE TESTING) -----> "Permission denied"
  
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
    Rmd_report_generator <- paste(project_dir, "Rmd/DQC_Calculator_2.Rmd",sep = "")
  }
  
  # ..........................................................................................................................................................
  
  # ..... files selection .....................................................................................................................................
  
  files_available_raw = dir(input_dir,pattern = ".dat")                  # <-- Admitted pattern:  ".dat" or ".csv"
  
  files_available_raw = files_available_raw[!grepl(pattern = "backup",x = files_available_raw)]          # REMOVE FILES WITH WRONG NAMES (.dat.backup not admitted) 
  
  if(PROJECT != "MONALISA"){
    files_available_raw = files_available_raw[!grepl(pattern = "IP",x = files_available_raw)]          # <- for MONALISA  IP in name is admitted 
  }
  
  files_available = files_available_raw[grepl(pattern = paste("^",PROJECT,sep = ""),x = files_available_raw)]          
  
  files_no_project = substring(files_available, nchar(PROJECT)+2, nchar(files_available)-4)
  
  if(length(files_no_project) > 0){
    u1 =c()
    logg_data_NAME = c()
    table_data_NAME = c()
    
    for(h in 1:length(files_no_project)){
      u1[h] = gregexpr(files_no_project,pattern = "_")[[h]][1]   # <- here we find the sencond "[[1]][2]" underscore!!!!!
      logg_data_NAME[h] = substring(text = files_no_project[h],first = 1,last = u1[h]-1)
      table_data_NAME[h] = substring(text = files_no_project[h],first = u1[h]+1,last = nchar(files_no_project[h]))
    }  
    df_files = data.frame(files_available, logg_data_NAME, table_data_NAME)
    colnames(df_files) = c("Files", "LoggerNet_name", "Datatable_name")
    
    if(PROJECT == "LTER"){                                                                        # <--Filter files based on Project (diffent if is MONALISA or LTER)
      files_available = df_files$Files[which(df_files$LoggerNet_name == df_files$Datatable_name)]
    }
    
    if(PROJECT == "MONALISA"){                                                                        # <--Filter files based on Project (diffent if is MONALISA or LTER)
      # files_available = df_files$Files
      files_available = df_files$Files[which(df_files$Datatable_name == "MeteoVal")]
      
    }
  } else{
    files_available = files_no_project
  }
  
  
  # ..........................................................................................................................................................
  
  # ..... download table section .....................................................................................................................................
  
  
  download_table = read_and_update_download_table(DOWNLOAD_TABLE_DIR = download_table_dir, FILES_AVAILABLE = files_available, DATETIME_FORMAT = datetime_format, PROJECT = PROJECT)
  issue_counter = read_and_update_issue_counter(ISSUE_COUNTER_DIR = issue_counter_dir, FILES_AVAILABLE = files_available, DATETIME_FORMAT = datetime_format, PROJECT = PROJECT)
  
  download_table_proj = download_table$Station[which(download_table$Project == PROJECT)]
  issue_counter_proj = issue_counter$Station[which(issue_counter$Project == PROJECT)]
  
  files_available_project = files_available[which(substring(files_available,1, nchar(files_available)-4) %in% download_table_proj)]
  
  ############################################
  t = 15
  
  final_dataframe = matrix(ncol = 20, nrow = length(files_available_project))
  
  colnames(final_dataframe) = c("Station", "Status",
                                "flag_empty","flag_logger_number", "flag_error_df","flag_date",
                                "flag_duplicates_rows","flag_overlap","flag_missing_records","flag_missing_dates",
                                "flag_range_variable_to_set","flag_range_variable_new","flag_out_of_range",
                                "flag_new_duplicates_rows","flag_new_overlap","flag_new_missing_dates", "flag_missing_records_new",   
                                "Report_link", "Data_folder", "File_name")
  
  
  
  
  report_start = Sys.time()
  
  
  for(t in  1: length(files_available_project)){
    gc(reset = T)
    
    rm(list = setdiff(ls(all.names = TRUE),c("main_dir","PROJECT","DQC_setting_dir","t","data_from_row","datetime_format","datetime_header","datetime_sampling","download_table","download_table_dir","issue_counter", "issue_counter_dir",
                                             "files_available","files_available_project","header_row_number","input_dir","data_output_dir","output_dir_raw","report_output_dir","project_dir",
                                             "range_dir","range_file","record_header","Rmd_report_generator","write_output_files","write_output_report","flag_names",
                                             "report_start", "final_dataframe","output_dir_report", "database_file_dir","logger_info_file")))
    
    
    FILE_NAME = files_available_project[t]
    
    u1 = gregexpr(FILE_NAME,pattern = "_")[[1]][1]      # <- here we find the first "[[1]][1]" underscore!!!!!
    u2 = gregexpr(FILE_NAME,pattern = "_")[[1]][2]      # <- here we find the first "[[1]][1]" underscore!!!!!
    
    if(PROJECT == "MONALISA"){
      STATION_NAME = substring(FILE_NAME,u1+1, u1+9)
    }else{
      STATION_NAME = substring(FILE_NAME,u1+1, u2-1)
    }
    
    w_dwnl = which(download_table$Station == substring(FILE_NAME, 1, nchar(FILE_NAME) - 4))
    dwnl_info = download_table[w_dwnl,]
    
    if(dir.exists(paste(data_output_dir,STATION_NAME,"/", sep = ""))){                # create subfolder to store data organized by station name
      if(dir.exists(paste(data_output_dir,STATION_NAME,"/Total/", sep = ""))){
        output_dir_data_new = paste(data_output_dir,STATION_NAME,"/Total/", sep = "")
        output_dir_raw_new = paste(data_output_dir,STATION_NAME,"/Raw/", sep = "")
      }else{
        dir.create(paste(data_output_dir,STATION_NAME,"/Reports/", sep = ""))
        dir.create(paste(data_output_dir,STATION_NAME,"/Raw/", sep = ""))
        dir.create(paste(data_output_dir,STATION_NAME,"/Total/", sep = ""))
        dir.create(paste(data_output_dir,STATION_NAME,"/Processed/", sep = ""))
        dir.create(paste(data_output_dir,STATION_NAME,"/Pics/", sep = ""))
        output_dir_data_new = paste(data_output_dir,STATION_NAME,"/Total/", sep = "")
        output_dir_raw_new = paste(data_output_dir,STATION_NAME,"/Raw/", sep = "")
        
      }
    }else{
      dir.create(paste(data_output_dir,STATION_NAME,"/", sep = ""))
      dir.create(paste(data_output_dir,STATION_NAME,"/Reports/", sep = ""))
      dir.create(paste(data_output_dir,STATION_NAME,"/Raw/", sep = ""))
      dir.create(paste(data_output_dir,STATION_NAME,"/Total/", sep = ""))
      dir.create(paste(data_output_dir,STATION_NAME,"/Processed/", sep = ""))
      dir.create(paste(data_output_dir,STATION_NAME,"/Pics/", sep = ""))
      output_dir_data_new = paste(data_output_dir,STATION_NAME,"/Total/", sep = "")
      output_dir_raw_new = paste(data_output_dir,STATION_NAME,"/Raw/", sep = "")
    }
    
    if(dir.exists(paste(database_file_dir,STATION_NAME,"/", sep = ""))){                # create subfolder to store mini files for database organized by station name 
      if(dir.exists(paste(database_file_dir,STATION_NAME,"/Data/", sep = ""))){ 
        database_file_dir_new = paste(database_file_dir,STATION_NAME,"/Data/", sep = "")
      }else{
        dir.create(paste(database_file_dir,STATION_NAME,"/Data/", sep = ""))
        dir.create(paste(database_file_dir,STATION_NAME,"/Pics/", sep = ""))
        database_file_dir_new = paste(database_file_dir,STATION_NAME,"/Data/", sep = "")
        
      }
    }else{
      dir.create(paste(database_file_dir,STATION_NAME,"/", sep = "")) 
      dir.create(paste(database_file_dir,STATION_NAME,"/Data/", sep = ""))
      dir.create(paste(database_file_dir,STATION_NAME,"/Pics/", sep = ""))
      database_file_dir_new = paste(database_file_dir,STATION_NAME,"/Data/", sep = "")
    }
    
    if(dwnl_info$Stop_DQC == 0){
      
      date_last_modif_file = as.character(format(file.mtime(paste(input_dir,FILE_NAME,sep = "")),format = datetime_format))
      
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
        file_name = FILE_NAME
        station_name = STATION_NAME
        start_date = dwnl_info$Last_date
        logger_info_file = logger_info_file
        record_check = dwnl_info$record_check
        
        output_file_report = paste("DQC_Report_",STATION_NAME,"_tmp.html",sep = "")
        
        rm(dwnl_info)
        
        DQC_results = DQC_function(input_dir,
                           output_dir_data,
                           output_dir_report,
                           project_dir,
                           data_from_row,
                           header_row_number,
                           datetime_header,
                           datetime_format,
                           datetime_sampling,
                           record_header,
                           range_file,
                           write_output_files,
                           write_output_report,
                           file_name,
                           station_name,
                           start_date,
                           database_dir,
                           logger_info_file,
                           record_check,
                           output_dir_raw)
        
        # rmarkdown::render(input = Rmd_report_generator ,
        #                   output_file = output_file_report,
        #                   output_dir = output_dir_report,
        #                   params = list(input_dir = input_dir ,
        #                                 output_dir_data = output_dir_data ,
        #                                 output_dir_raw = output_dir_raw,
        #                                 output_dir_report = output_dir_report ,
        #                                 project_dir = project_dir ,
        #                                 data_from_row = data_from_row ,
        #                                 header_row_number = header_row_number ,
        #                                 datetime_header = datetime_header ,
        #                                 datetime_format = datetime_format ,
        #                                 datetime_sampling = datetime_sampling ,
        #                                 record_header = record_header ,
        #                                 range_file = range_file ,
        #                                 write_output_files = write_output_files ,
        #                                 write_output_report = write_output_report ,
        #                                 database_dir = database_dir,
        #                                 file_name = file_name ,
        #                                 station_name = station_name,
        #                                 start_date = start_date,
        #                                 logger_info_file = logger_info_file,
        #                                 record_check = record_check))
        # 
        
        mydata = DQC_results[[1]]
        flags_df = DQC_results[[2]]
        file_names = DQC_results[[3]]

        mylist <- split(flags_df$value, seq(nrow(flags_df)))
        names(mylist) = flags_df$flag_names
        
        if(mylist$flag_empty == 0 & mylist$flag_logger_number == 0 & mylist$flag_error_df == 0 & mylist$flag_date == 0){
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
        
        # non serve?  
        # Verificare che si puo togliere da qui: 
        out_filename_report = paste("DQC_Report_",STATION_NAME,"_",out_filename_date,".html",sep = "")
        
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
        
        # a qui!
        # Report su script esterno! Nella funzione DQC_Function prevedere il salvataggio e l' append degli errori!
        
        if(!is.na(mylist$flag_missing_dates)){
          if(mylist$flag_logger_number == 0){
            if(mylist$flag_new_overlap == 1){
              if(write_output_report == TRUE){
                final_info = c(STATION_NAME, "Analyzed and write output",
                               flags_df$value,
                               paste(output_dir_report,out_filename_report,sep = ""),
                               NA,
                               NA)
              }else{
                final_info = c(STATION_NAME, "Analyzed and write output",
                               flags_df$value,
                               NA,
                               NA,
                               NA)
              }
            }else{
              download_table$Last_date[w_dwnl] = last_date
              download_table$Last_Modification[w_dwnl] = date_last_modif_file
              # download_table$record_check[w_dwnl] = 1    # NEW! Record check activated every time!
              write.csv(download_table,paste(download_table_dir,"download_table.csv",sep = ""),quote = F,row.names = F)
              
              if(write_output_report == TRUE){
                final_info = c(STATION_NAME, "Analyzed and write output",
                               flags_df$value,
                               paste(output_dir_report,out_filename_report,sep = ""),
                               paste(output_dir_data,sep = ""),
                               paste(file_names[length(file_names)],sep = ""))
              }else{
                final_info = c(STATION_NAME, "Analyzed and write output",
                               flags_df$value,
                               NA,
                               paste(output_dir_data_new,sep = ""),
                               paste(file_names[length(file_names)],sep = ""))
              }
            }
          }
        }else{
          # file_stopped = c(file_stopped, FILE)
          if(write_output_report == TRUE){
            final_info = c(STATION_NAME, "Analyzed with errors",
                           flags_df$value,
                           paste(output_dir_report,out_filename_report,sep = ""),
                           NA, NA )
          }else{
            final_info = c(STATION_NAME, "Analyzed with errors",
                           flags_df$value,
                           NA,
                           NA, NA )
          }
          
          
        }
        
      } else {
        warning(paste(STATION_NAME, "already analyzed!"))
        # file_already_processed = c(file_already_processed,FILE)
        final_info = c(STATION_NAME, "Already analyzed",
                       NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA,
                       NA,
                       NA, NA)
        output_dir_report = report_output_dir
        
      }
      
    }else{
      final_info = c(STATION_NAME, "Not analyzed",
                     NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA,
                     NA,
                     NA, NA)
      output_dir_report = report_output_dir
      
    }
    
    # final_dataframe = rbind(final_dataframe,final_info)
    final_dataframe[t,] = final_info
    
    gc(reset = T)
  }
  
  
  
  
  aaa = as.data.frame(final_dataframe)
  
  aaa$Station[which(aaa$Status == "Already analyzed")]
  # ..... Final Report .....................................................................................................................................
  
  
  input_final = paste(project_dir,"Rmd/DQC_Final_Report_Hourly_2.Rmd",sep = "")
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
