#-------------------------------------------------------------------------------------------------------------------------------------------------------
# File Title:   DQC_Hourly_Linux_v6.R
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

library(devtools)
library("DataQualityCheckEuracAlpEnv")
# install_github("alexsanjoseph/compareDF")
library(compareDF)
library(zoo)
library(knitr)
library(ggplot2)
library(reshape2)
library(DT)
library(htmltools)
library(rmarkdown)
library(yaml)
library(highr)
library(mailR)
library(XML)
library(xtable)
library(dygraphs)
library(xts)
library(hwriter)
library(labeling)
library(optparse)
library(optparse)


option_list = list(
  make_option(c("-md", "--maindir"), type="character", default="/shared/", 
              help="set the main dir", metavar="character"),
  make_option(c("-pd", "--prjdir"), type="character", default="/home/cbrida/DataQualityCheckEuracAlpEnv/", 
              help="set the project dir", metavar="character")
); 

opt_parser = OptionParser(option_list=option_list);
opt = parse_args(opt_parser);

main_dir = opt$maindir
project_dir = opt$prjdir

print(main_dir)
print(project_dir)

# Sys.setenv(RSTUDIO_PANDOC = "/usr/lib/rstudio/bin/pandoc/")
# .....................................................................................................................................................

# ..... Params section .....................................................................................................................................

# main_dir = "Z:/test_christian/"    # disattivare!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
# main_dir = "Z:/"    # disattivare!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
# main_dir = "/shared/"
# main_dir = "/shared/test_christian/"

main_dir_mapping_in = "/shared/"                                   # <-- "Z:/" or "/shared/" will be replaced with "\\\\smb.scientificnet.org\\alpenv"
main_dir_mapping_out = "\\\\smb.scientificnet.org\\alpenv"    # <-- "Z:/" or "/shared/" will be replaced with "\\\\smb.scientificnet.org\\alpenv"

# main_dir = "/shared/test_christian/"
# main_dir = "H:/Projekte/LTER/03_Arbeitsbereiche/BriCh/shared/test_christian/"

project_type = c("LTER","MONALISA")

PROJECT = "LTER" # Possible project: "LTER"; "MONALISA";

input_dir <- paste(main_dir,"/Stations_Data/Data/LoggerNet_Raw_Data/Data/",sep = "")                    # where input files are
# input_dir <- paste("/shared","/Stations_Data/Data/LoggerNet_Raw_Data/Data/",sep = "")                    # where input files are
# input_dir <- paste("Z:","/Stations_Data/Data/LoggerNet_Raw_Data/Data/",sep = "")                    # where input files are

# project_dir <- "/home/cbrida/DataQualityCheckEuracAlpEnv/"  # where package is developed or cloned from github
# project_dir <- "C:/Users/CBrida/Desktop/GitLab/dataqualitycheckeuracalpenv/"  # where package is developed or cloned from github # disattivare!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

# sapply(dir(paste(project_dir,"R/",sep = ""),pattern = ".R"),FUN = function(x) source(paste(project_dir,"R/",x,sep = ""))) # import all function from 


DQC_setting_dir <- paste(main_dir,"/Stations_Data/DQC/",sep = "")

logger_info_file <- paste(DQC_setting_dir,"/Process/Logger_number_and_software.csv", sep = "")
range_dir <- paste(DQC_setting_dir,"/Process/", sep = "")
download_table_dir <- paste(DQC_setting_dir,"/Process/Download_tables/Weekly/", sep = "")

warning_report_RMD = paste(project_dir,"/Rmd/DQC_Warning_Reports.Rmd",sep = "")

# !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!  TO REMOVE !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
# issue_counter_dir <- paste(DQC_setting_dir,"/Process/", sep = "")# 
# issue_flags_dir <- paste(DQC_setting_dir,"/Process/issue_flags", sep = "")
# MESSAGE_EVERY_TIMES = 24
# !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

HOURS_OFFLINE = 24       # <- no data update since 24h --> station broken?
# HOURS_OFFLINE = 2       # <- no data update since 24h --> station broken?
LOGGERNET_OFFLINE = 1    # <. all station offline since 1h --> loggernet doesn't work!

date_DQC = as.POSIXct(format(Sys.time(),format = "%Y-%m-%d %H:%M"), tz = 'Etc/GMT-1')

# loggernet_status = c()

mail_dir = paste(DQC_setting_dir,"Process/email_status/",sep = "")
mail_file = "mail_status.csv"
mail_file_alert = "out_of_range.csv"

# --- read mail configuration ---

mail_config_file = paste(mail_dir,"mail_config.xml",sep = "")
mail_config = xmlParse(mail_config_file, useInternalNodes = F)

# mail_config_info = mail_config_parsing_new(mail_config) ##################################################
mail_config_info = mail_config_parsing_new(mail_config)

sender = mail_config_info$sender
reciver = mail_config_info$reciver
# reciver = "Christian.Brida@eurac.edu"
my_smtp = mail_config_info$my_smtp
url_webservice = mail_config_info$url_webservice #########################################################
# -------------------------------
# * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * 

# To set TRUE if you wanto to check header and logger information (based on new station file standard)

HEADER_CHECK = TRUE

# * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * 

print(mail_config_file)
print(sender)
print(reciver)
print(input_dir)


if(!file.exists(paste(DQC_setting_dir,"lock_report.lock",sep = ""))){
  file.create(paste(DQC_setting_dir,"lock_report.lock",sep = ""))
}

# -------------------------------# -------------------------------# -------------------------------# -------------------------------# -------------------------------

for(PROJECT in project_type){
  data_output_dir   <- paste(main_dir,"Stations_Data/Data/DQC_Processed_Data/",PROJECT,"/Stations/",sep = "")  # where to put output files
  report_output_dir <- paste(data_output_dir,"00_DQC_Reports/",sep = "")  # where to put output reports
  # report_output_dir <- paste(main_dir,"Stations_Data/Data/DQC_Processed_Data/",PROJECT,"/Stations/00_DQC_Reports/",sep = "")  # where to put output reports
  database_file_dir <- paste(main_dir,"Stations_Data/Data/DQC_DB/",PROJECT,"/", sep = "")  # where to put output files (MODIFIED FOR DATABASE TESTING) -----> "Permission denied"
  
  # warning_file_dir <- paste(main_dir,"Stations_Data/Data/DQC_Warnings/",PROJECT,"/", sep = "")  # where to put warnings html files
  
  
  data_from_row =  5                                             # <-- Row number of first data
  header_row_number =  2                                         # <-- Row number of header
  datetime_header =  "TIMESTAMP"                                 # <-- header corresponding to TIMESTAMP
  datetime_format =  "%Y-%m-%d %H:%M"                          # <-- datetime format. Use only: Y -> year, m -> month, d -> day, H -> hour, M -> minute
  datetime_sampling =  "15 min"
  record_header =  "RECORD"
  range_file =  "Range.csv"
  # use_alert_station_flag = TRUE        # <-- IN REPORT DON'T SHOW ANALYZE VARIABLES SET AS 0 IN RANGE FILE
  use_alert_station_flag = TRUE        # <-- use range file flags. Default: TRUE
  use_realtime_station_flag = FALSE        # <-- use out_of_range file flags. Default: FALSE
  
  write_output_files =  "FALSE"
  # write_output_files =  "TRUE"
  write_output_report =  "FALSE"
  
  # general stations status --> loggernent doesn't work properly!
  # loggernet_status_prj = as.data.frame(matrix(ncol = 3))
  # colnames(loggernet_status_prj) = c("Station", "Status", "Last_modification")
  
  # file <- "M4s.dat"
  # start_date <- NA
  
  # ~~~ Default directory ~~~~
  
  # !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!  TO REMOVE !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  #
  # if(write_output_report == TRUE){
  #   Rmd_report_generator <- paste(project_dir, "Rmd/DQC_Report_Generator.Rmd",sep = "")
  # }else{
  #   Rmd_report_generator <- paste(project_dir, "Rmd/DQC_Calculator_2.Rmd",sep = "")
  # }
  #
  # !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  
  # ..........................................................................................................................................................
  
  # ..... files selection .....................................................................................................................................
  
  files_available_raw = dir(input_dir,pattern = ".dat")                  # <-- Admitted pattern:  ".dat" or ".csv"
  files_available_raw = files_available_raw[grepl(pattern = ".dat$",x = files_available_raw)]      
  
  files_available_raw = files_available_raw[!grepl(pattern = "backup",x = files_available_raw)]          # REMOVE FILES WITH WRONG NAMES (.dat.backup not admitted) 
  
  # if(PROJECT != "MONALISA"){
  #   files_available_raw = files_available_raw[!grepl(pattern = "IP",x = files_available_raw)]          # <- for MONALISA  IP in name is admitted 
  # }
  
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
    df_files = data.frame(files_available, logg_data_NAME, table_data_NAME,stringsAsFactors = F)
    colnames(df_files) = c("Files", "LoggerNet_name", "Datatable_name")
    
    files_available = df_files$Files[which(df_files$LoggerNet_name == df_files$Datatable_name)]
    
    # if(PROJECT == "LTER"){                                                                        # <--Filter files based on Project (diffent if is MONALISA or LTER)
    #   files_available = df_files$Files[which(df_files$LoggerNet_name == df_files$Datatable_name)]
    # }
    # 
    # if(PROJECT == "MONALISA"){                                                                        # <--Filter files based on Project (diffent if is MONALISA or LTER)
    #   # files_available = df_files$Files
    #   files_available = df_files$Files[which(df_files$Datatable_name == "MeteoVal")]
    #   
    # }
  } else{
    files_available = files_no_project
  }
  
  
  # ..........................................................................................................................................................
  
  # ..... download table section .....................................................................................................................................
  
  
  download_table = read_and_update_download_table(DOWNLOAD_TABLE_DIR = download_table_dir, FILES_AVAILABLE = files_available, DATETIME_FORMAT = datetime_format, PROJECT = PROJECT) # sopstare fuori dal ciclo Project! --> salvo old ogni volta che gira lo script e non ogni volta che cambio progettoS
  # issue_counter = read_and_update_issue_counter(ISSUE_COUNTER_DIR = issue_counter_dir, FILES_AVAILABLE = files_available, DATETIME_FORMAT = datetime_format, PROJECT = PROJECT)
  
  download_table_proj = download_table$Station[which(download_table$Project == PROJECT)]
  # issue_counter_proj = issue_counter$Station[which(issue_counter$Project == PROJECT)]
  
  files_available_project = files_available[which(substring(files_available,1, nchar(files_available)-4) %in% download_table_proj)]
  
  # ~ ~ ~ temporary solution to avoid issue with Vimes1500 station ---> remove this if when data file will fix! ~ ~ ~
  if("MONALISA_Vimes1500_Vimes1500.dat" %in% files_available_project){
    files_available_project = files_available_project[-which(files_available_project == "MONALISA_Vimes1500_Vimes1500.dat")]
  }
  # ~ ~ ~ ~ ~ ~ 
  
  ############################################
  
  # final_dataframe = matrix(ncol = 20, nrow = length(files_available_project))
  # colnames(final_dataframe) = c("Station", "Status",
  #                               "flag_empty","flag_logger_number", "flag_error_df","flag_date",
  #                               "flag_duplicates_rows","flag_overlap","flag_missing_records","flag_missing_dates",
  #                               "flag_range_variable_to_set","flag_range_variable_new","flag_out_of_range",
  #                               "flag_new_duplicates_rows","flag_new_overlap","flag_new_missing_dates", "flag_missing_records_new",   
  #                               "Report_link", "Data_folder", "File_name")
  
  report_dataframe = matrix(ncol = 16, nrow = length(files_available_project))
  colnames(report_dataframe) = c("Station",
                                 "Offline",
                                 "err_empty","err_logger_number","err_structure","err_structure_change","err_no_new_data","err_overlap","err_missing_record","err_restart_record",
                                 "err_date_missing","err_range_alert",
                                 "err_out_of_range","err_duplicates_rows",
                                 "var_flagged",
                                 "report_link")
  
  
  
  # report_start = Sys.time()
  
  t = 1
  
  for(t in  1: length(files_available_project)){
    gc(reset = T)
    
    # rm(list = setdiff(ls(all.names = TRUE),c("date_DQC","main_dir","PROJECT","DQC_setting_dir","t","data_from_row","datetime_format","datetime_header","datetime_sampling","loggernet_status",
    #                                          "download_table","download_table_dir","issue_counter", "issue_counter_dir","issue_counter_proj",
    #                                          "files_available","files_available_project","header_row_number","input_dir","data_output_dir","output_dir_raw","report_output_dir","project_dir",
    #                                          "range_dir","range_file","record_header","Rmd_report_generator","write_output_files","write_output_report","flag_names",
    #                                          "report_start", "final_dataframe","output_dir_report", "database_file_dir","logger_info_file","MESSAGE_EVERY_TIMES","issue_flags_dir",
    #                                          "warning_file_dir","warning_report_RMD","mail_config","mail_config_file","mail_config_info","mail_file","HOURS_OFFLINE","LOGGERNET_OFFLINE",
    #                                          "sender", "reciver" ,"my_smtp","loggernet_status_prj","loggernet_status","project_type",
    #                                          "report_info", "report_dataframe","use_alert_station_flag","mail_dir","url_webservice","mail_file_alert","use_realtime_station_flag")))
    # 
    
    
    FILE_NAME = files_available_project[t]
    
    u1 = gregexpr(FILE_NAME,pattern = "_")[[1]][1]      # <- here we find the first "[[1]][1]" underscore!!!!!
    u2 = gregexpr(FILE_NAME,pattern = "_")[[1]][2]      # <- here we find the second "[[1]][2]" underscore!!!!!
    
    # if(PROJECT == "MONALISA"){
    #   STATION_NAME = substring(FILE_NAME,u1+1, u1+9)
    # }else{
    #   STATION_NAME = substring(FILE_NAME,u1+1, u2-1)
    # }
    
    STATION_NAME = substring(FILE_NAME,u1+1, u2-1)
    
    w_dwnl = which(download_table$Station == substring(FILE_NAME, 1, nchar(FILE_NAME) - 4))
    dwnl_info = download_table[w_dwnl,]
    
    if(dir.exists(paste(data_output_dir,STATION_NAME,"/", sep = ""))){                # create subfolder to store data organized by station name
      if(dir.exists(paste(data_output_dir,STATION_NAME,"/Total/Files_dat/", sep = ""))){
        output_dir_data_new = paste(data_output_dir,STATION_NAME,"/Total/Files_dat/", sep = "")
        output_dir_raw_new = paste(data_output_dir,STATION_NAME,"/Raw/", sep = "")
        output_dir_report_new = paste(data_output_dir,STATION_NAME,"/Alerts/Reports/", sep = "")
        warning_file_dir_station = paste(data_output_dir,STATION_NAME,"/Alerts/Warnings/", sep = "")
      }else{
        dir.create(paste(data_output_dir,STATION_NAME,"/Alerts/", sep = ""))
        dir.create(paste(data_output_dir,STATION_NAME,"/Alerts/Reports/", sep = ""))
        dir.create(paste(data_output_dir,STATION_NAME,"/Alerts/Warnings/", sep = ""))
        dir.create(paste(data_output_dir,STATION_NAME,"/Raw/", sep = ""))
        dir.create(paste(data_output_dir,STATION_NAME,"/Total/", sep = ""))
        dir.create(paste(data_output_dir,STATION_NAME,"/Total/Files_dat/", sep = ""))
        dir.create(paste(data_output_dir,STATION_NAME,"/Processed/", sep = ""))
        dir.create(paste(data_output_dir,STATION_NAME,"/Pics/", sep = ""))
        output_dir_data_new = paste(data_output_dir,STATION_NAME,"/Total/Files_dat/", sep = "")
        output_dir_raw_new = paste(data_output_dir,STATION_NAME,"/Raw/", sep = "")
        output_dir_report_new = paste(data_output_dir,STATION_NAME,"/Alerts/Reports/", sep = "")
        warning_file_dir_station = paste(data_output_dir,STATION_NAME,"/Alerts/Warnings/", sep = "")
        
      }
    }else{
      dir.create(paste(data_output_dir,STATION_NAME,"/", sep = ""))      
      dir.create(paste(data_output_dir,STATION_NAME,"/Alerts/", sep = ""))
      dir.create(paste(data_output_dir,STATION_NAME,"/Alerts/Reports/", sep = ""))
      dir.create(paste(data_output_dir,STATION_NAME,"/Alerts/Warnings/", sep = ""))
      dir.create(paste(data_output_dir,STATION_NAME,"/Raw/", sep = ""))
      dir.create(paste(data_output_dir,STATION_NAME,"/Total/", sep = ""))
      dir.create(paste(data_output_dir,STATION_NAME,"/Total/Files_dat/", sep = ""))
      dir.create(paste(data_output_dir,STATION_NAME,"/Processed/", sep = ""))
      dir.create(paste(data_output_dir,STATION_NAME,"/Pics/", sep = ""))
      output_dir_data_new = paste(data_output_dir,STATION_NAME,"/Total/Files_dat/", sep = "")
      output_dir_raw_new = paste(data_output_dir,STATION_NAME,"/Raw/", sep = "")
      output_dir_report_new = paste(data_output_dir,STATION_NAME,"/Alerts/Reports/", sep = "")
      warning_file_dir_station = paste(data_output_dir,STATION_NAME,"/Alerts/Warnings/", sep = "")
      
    }
    
    # if(dir.exists(paste(database_file_dir,STATION_NAME,"/", sep = ""))){                # create subfolder to store mini files for database organized by station name 
    #   if(dir.exists(paste(database_file_dir,STATION_NAME,"/Data/", sep = ""))){ 
    #     database_file_dir_new = paste(database_file_dir,STATION_NAME,"/Data/", sep = "")
    #   }else{
    #     dir.create(paste(database_file_dir,STATION_NAME,"/Data/", sep = ""))
    #     dir.create(paste(database_file_dir,STATION_NAME,"/Pics/", sep = ""))
    #     database_file_dir_new = paste(database_file_dir,STATION_NAME,"/Data/", sep = "")
    #     
    #   }
    # }else{
    #   dir.create(paste(database_file_dir,STATION_NAME,"/", sep = "")) 
    #   dir.create(paste(database_file_dir,STATION_NAME,"/Data/", sep = ""))
    #   dir.create(paste(database_file_dir,STATION_NAME,"/Pics/", sep = ""))
    #   database_file_dir_new = paste(database_file_dir,STATION_NAME,"/Data/", sep = "")
    # }
    
    
    
    if(dwnl_info$Stop_DQC == 0){
      
      date_last_modif_file = as.character(format(file.mtime(paste(input_dir,FILE_NAME,sep = "")),format = datetime_format))
      
      # ----- station offline  ------
      
      h_last_modif_file = trunc(as.POSIXct(date_last_modif_file, tz = "Etc/GMT-1"),units = "hours")
      h_DQC = trunc(date_DQC,units = "hours")
      
      hours_diff = as.numeric(difftime(time1 = h_DQC, time2 = h_last_modif_file, tz = "Etc/GMT-1",units = "hours"))
      
      if(hours_diff >= 24 ){   # 24 =  24h offline
        offline_value = 3
      }else{
        offline_value = 0
      }
      
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
        # database_dir = database_file_dir_new
        file_name = FILE_NAME
        station_name = STATION_NAME
        start_date = dwnl_info$Last_date
        logger_info_file = logger_info_file
        record_check = dwnl_info$record_check
        use_alert_station_flag = use_alert_station_flag
        mail_file_alert = mail_file_alert
        use_realtime_station_flag = use_realtime_station_flag
        header_check = HEADER_CHECK
        
        # issue_flags_file = paste(issue_flags_dir,"/",STATION_NAME,".csv",sep = "")
        
        # output_file_report = paste("DQC_Report_",STATION_NAME,"_tmp.html",sep = "")
        
        # rm(dwnl_info)
        # DQC_results = DQC_function(input_dir,
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
                                   # database_dir,
                                   logger_info_file,
                                   record_check,
                                   output_dir_raw,
                                   use_alert_station_flag,
                                   mail_file_alert,
                                   use_realtime_station_flag,
                                   header_check)
        
        mydata = DQC_results[[1]]
        flags_df = DQC_results[[2]]
        file_names = DQC_results[[3]]
        errors = DQC_results[[4]]
        mydata_out_of_range = DQC_results[[5]]
        
        mylist <- split(flags_df$value, seq(nrow(flags_df)))
        names(mylist) = flags_df$flag_names
        
        status = unlist(lapply(errors,function(x) x[[1]]))
        
        if(all(status[names(status) %in% c( "err_no_new_data","err_empty","err_structure",
                                            "err_overlap")] == "N")){
          if(record_check != 1 | (record_check == 1 & all(status[names(status) %in% c( "err_missing_record","err_restart_record")] == "N"))){
            out_filename_date = paste(substring(mydata[nrow(mydata),which(colnames(mydata) == datetime_header)],1,4),
                                      substring(mydata[nrow(mydata),which(colnames(mydata) == datetime_header)],6,7),
                                      substring(mydata[nrow(mydata),which(colnames(mydata) == datetime_header)],9,10),
                                      # "_",
                                      substring(mydata[nrow(mydata),which(colnames(mydata) == datetime_header)],12,13),
                                      substring(mydata[nrow(mydata),which(colnames(mydata) == datetime_header)],15,16),
                                      sep = "")
            
            last_date = mydata[nrow(mydata),which(colnames(mydata)== datetime_header)]
          }else {
            out_filename_date = "no_datetime"
          }
        } else {
          out_filename_date = "no_datetime"
        }
        
        
        # ~ ~ ~ ~ Issue Management (on/off message) ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
        
        # date_DQC = as.POSIXct(format(Sys.time(),format = "%Y-%m-%d %H:%M"), tz = 'Etc/GMT-1')
        
        data_errors = lapply(errors,function(x) x[[2]])
        w_yes = which(status == "Y")
        
        critical_errors = c("err_empty","err_logger_number","err_structure","err_structure_change","err_no_new_data","err_overlap","err_missing_record","err_restart_record","err_date_missing")
        warning_errors = c("err_range_alert")
        report_errors = c("err_out_of_range","err_duplicates_rows")
        
        dqc_date = date_DQC
        
        df_status = data.frame(STATION_NAME,t(status))
        
        if(use_alert_station_flag == TRUE){
          range_flags = read.csv(paste(range_dir,range_file,sep = ""),stringsAsFactors = F)
          range_station = range_flags[,c(1,which(colnames(range_flags) == STATION_NAME))]
          colnames(range_station)[2] = "Station"
          variables_flagged = range_station$Variable[which(range_station$Station == 0)]
          
          if(length(variables_flagged) == 0){
            variables_flagged = NULL
          }
        }else{
          variables_flagged = NULL
        }
        
        if(any(status[-which(names(status) == "err_duplicates_rows")] == "Y")){
          
          station_name = STATION_NAME
          errors_list_critical = errors[critical_errors]
          errors_list_warning = errors[warning_errors]
          errors_list_report_errors = errors[report_errors]
          
          dqc_date_write = paste(format(dqc_date,"%Y"),format(dqc_date,"%m"),format(dqc_date,"%d"),sep = "")
          
          
          # generate a report of warnings
          
          output_file_report = paste(STATION_NAME,"_",dqc_date_write,".html",sep = "")
          
          issue_report_RMD = paste(project_dir,"/Rmd/DQC_Reports.Rmd",sep = "")
          
          issue_file_dir_station = output_dir_report_new
          
          input =  issue_report_RMD 
          output_file = output_file_report
          output_dir = issue_file_dir_station
          
          if(!is.null(mydata_out_of_range)){
            report_mydata = mydata_out_of_range
            report_mydata[,which(colnames(report_mydata) == datetime_header)] = as.POSIXct(report_mydata[,which(colnames(report_mydata) == datetime_header)] ,tz = "Etc/GMT-1")
          }else{
            report_mydata = NULL
          }
          
          if(!is.null(mydata)){
            mydata_to_report = mydata
            mydata_to_report[, which(colnames(mydata_to_report) == datetime_header)] = as.POSIXct(mydata_to_report[, which(colnames(mydata_to_report) == datetime_header)], format = datetime_format, tz = "Etc/GMT-1")
          }else{
            mydata_to_report = NULL
          }
          
          # if(use_alert_station_flag == TRUE){
          #   range_flags = read.csv(paste(range_dir,range_file,sep = ""),stringsAsFactors = F)
          #   range_station = range_flags[,c(1,which(colnames(range_flags) == STATION_NAME))]
          #   colnames(range_station)[2] = "Station"
          #   variables_flagged = range_station$Variable[which(range_station$Station == 0)]
          #   
          #   if(length(variables_flagged) == 0){
          #     variables_flagged = NULL
          #     }
          # }else{
          #   variables_flagged = NULL
          # }
          
          
          params_list = list(mydata_to_report,
                             report_mydata,
                             dqc_date,
                             station_name,
                             errors_list_critical,
                             errors_list_warning,
                             errors_list_report_errors,
                             variables_flagged)
          names(params_list) = c("mydata_to_report","report_mydata", "dqc_date","station_name","errors_list_critical","errors_list_warning","errors_list_report_errors","variables_flagged")
          
          gc(reset = T)
          rmarkdown::render(input = input,
                            output_file = output_file,
                            output_dir = output_dir,
                            params = params_list)
        }
        
        # Report su script esterno! Nella funzione DQC_Function prevedere il salvataggio e l' append degli errori!
        
        status_final = status
        status_final[which(status_final == "Y")] = 1
        status_final[which(status_final == "N")] = 0
        status_final = status_final[c(critical_errors,warning_errors, report_errors)]
        # status_final = as.numeric(status_final)
        
        if(dwnl_info$record_check == 0 ){
          if(status_final[which(names(status_final) == "err_missing_record" )] == "1"){
            w_rec_missing = which(names(status_final) == "err_missing_record")
            status_final[w_rec_missing] = 2
            
          }
          if(status_final[which(names(status_final) == "err_restart_record" )] == "1" ){
            w_rec_restart = which(names(status_final) == "err_restart_record" )
            status_final[w_rec_restart] = 2
          }
        }
        
        # if(dwnl_info$record_check == 0){
        #   w_rec = which(names(status_final) %in% c("err_missing_record","err_restart_record" ))
        #   status_final[w_rec] = 2
        # }
        
        if(any(status[-which(names(status) == "err_duplicates_rows")] == "Y")){
          # paste(substring(output_dir,nchar(main_dir)),output_file,sep = "")
          # link = paste(main_dir_mapping_out, substring(output_dir_report_new,nchar(main_dir_mapping_in)), output_file_report,sep = "")
          link = paste("/",PROJECT,substring(output_dir_report_new,nchar(data_output_dir)), output_file_report,sep = "")
          # link = paste("\\\\smb.scientificnet.org\\alpenv", substring(output_dir_report_new,nchar('/shared/')), output_file_report,sep = "")
        }else{
          link = NA
          # link = "---"
        }
        
        if(length(variables_flagged) == 0){
          var_flagged = 0
        }else{
          var_flagged = 1
        }
        
        report_info = c(STATION_NAME,offline_value,status_final,var_flagged, link)
        names(report_info) = c("Station",
                               "Offline",
                               "err_empty","err_logger_number","err_structure","err_structure_change","err_no_new_data","err_overlap","err_missing_record","err_restart_record",
                               "err_date_missing","err_range_alert",
                               "err_out_of_range","err_duplicates_rows",
                               "var_flagged",
                               "report_link")
        
        # ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
        if(all(status[names(status) %in% c( "err_no_new_data","err_empty","err_structure",
                                            "err_overlap")] == "N")){
          if(record_check != 1 | (record_check == 1 & all(status[names(status) %in% c( "err_missing_record","err_restart_record")] == "N"))){
            
            download_table$Last_date[w_dwnl] = format(last_date, format = datetime_format)
            download_table$Last_Modification[w_dwnl] = format(date_last_modif_file, format = datetime_format)
            download_table$record_check[w_dwnl] = 1    # NEW! Record check activated every time!
            
            write.csv(download_table,paste(download_table_dir,"download_table.csv",sep = ""),quote = F,row.names = F)
          }
        }
      } else {
        
        report_info = c(STATION_NAME,1,rep(NA,12),NA, NA)
        names(report_info) = c("Station",
                               "Offline",
                               "err_empty","err_logger_number","err_structure","err_structure_change","err_no_new_data","err_overlap","err_missing_record","err_restart_record",
                               "err_date_missing","err_range_alert",
                               "err_out_of_range","err_duplicates_rows",
                               "var_flagged",
                               "report_link")
        
        warning(paste(STATION_NAME, "already analyzed!"))
        
        output_dir_report = report_output_dir
      }
      
    }else{
      report_info = c(STATION_NAME,2,rep(NA,12),NA, NA)
      names(report_info) = c("Station",
                             "Offline",
                             "err_empty","err_logger_number","err_structure","err_structure_change","err_no_new_data","err_overlap","err_missing_record","err_restart_record",
                             "err_date_missing","err_range_alert",
                             "err_out_of_range","err_duplicates_rows",
                             "var_flagged",
                             "report_link")
      # final_info = c(STATION_NAME, "Not analyzed",
      #                NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA,
      #                NA,
      #                NA, NA)
      output_dir_report = report_output_dir
      
    }
    
    # final_dataframe = rbind(final_dataframe,final_info)
    # final_dataframe[t,] = final_info
    
    report_dataframe[t,] = report_info
    
    # loggernet_status_prj[t,1] = final_info[1]
    # loggernet_status_prj[t,2] = final_info[2]
    # # loggernet_status_prj[t,3] = date_last_modif_file
    # loggernet_status_prj[t,3] = date_DQC
    
    
    gc(reset = T)
    
    # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    # INSERIRE QUI CHE ERRORE DARE AD ICINGA
    # if(exists(text_W_Update_station))
    # text_W_Update_station
    # text_W_Empty_file
    # text_W_Logger_number
    # text_W_structure
    # text_W_date_issue
    # text_W_overlap
    # text_W_missing_records
    # text_W_restart_records
    # text_W_date_missing
    
    # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    
  }
  report_dataframe = as.data.frame(report_dataframe)
  # loggernet_status = rbind(loggernet_status,loggernet_status_prj)
  
  
  # ..... Final Report .....................................................................................................................................
  
  
  input_final = paste(project_dir,"/Rmd/DQC_Report_overview.Rmd",sep = "")
  # date_DQC 
  output_file_final =  paste(PROJECT,"_Report_",
                             format(date_DQC,format = "%Y"),
                             format(date_DQC,format = "%m"),
                             format(date_DQC,format = "%d"),".html", sep = "")
  
  output_dir_final = output_dir_report
  rm(params)
  rmarkdown::render(input = input_final,
                    output_file = output_file_final ,
                    output_dir = output_dir_final,
                    params = list(PROJECT = PROJECT,
                                  date_DQC = date_DQC ,
                                  report_dataframe = report_dataframe))
  
  
  # ..... Data preparation for Database .....................................................................................................................................
  
  # MANDARE MAIL !!!!
  print("--------------------------------------------------------------------------------------------------")
  
  report_output_dir <- paste(data_output_dir,"00_DQC_Reports/",sep = "")  # where to put output reports
  
  my_subject = paste(PROJECT,"report")
  # my_body = paste(output_dir_final,output_file_final,sep="")
  # my_body = paste(main_dir_mapping_out, substring(output_dir_final, nchar(main_dir_mapping_in)),output_file_final,sep="")
  my_body = paste(url_webservice,PROJECT,substring(report_output_dir, nchar(data_output_dir)),output_file_final,sep="")
  
  # my_body = paste(url_webservice,icinga_text,sep = "")
  # icinga_text = paste(substring(output_dir,nchar('/shared/')),output_file,sep = "")               # to disactivate when webservice is ready!
  # icinga_text = paste(substring(output_dir,nchar(data_output_dir)),output_file,sep = "")        # to activate when webservice is ready!
  
  send.mail(from = sender,
            to = reciver,
            subject = my_subject,
            body = my_body,
            smtp = my_smtp,
            authenticate = TRUE,
            send = TRUE)
}


file.remove(paste(DQC_setting_dir,"lock_report.lock",sep = ""))

print("------------------------------------------------------------------------------------------")
print(Sys.time())
print("--------------------- End script! --------------------------------------------------------")

