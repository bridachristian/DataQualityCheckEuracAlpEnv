#-------------------------------------------------------------------------------------------------------------------------------------------------------
# File Title:   DQC_Manually.R
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
  make_option(c("-ind", "--inptdir"), type="character", default="/shared/",
              help="set the input dir", metavar="character"),
  make_option(c("-pd", "--prjdir"), type="character",default="/home/cbrida/DataQualityCheckEuracAlpEnv/",
              help="set the project dir", metavar="character")
);

opt_parser = OptionParser(option_list=option_list);
opt = parse_args(opt_parser);

main_dir = opt$maindir
data_input_dir = opt$inptdir
project_dir = opt$prjdir

print(main_dir)
print(data_input_dir)
print(project_dir)


root_dir_home = "C:/Users/CBrida/Desktop/Anno_Zero/"
data_output_dir =paste(root_dir_home,"/Output/",sep="")
data_input_dir =paste(root_dir_home,"/Input/S3/RAW_0/",sep="")               # <- insert here the name of the folder to source data


# root_dir = "H:/Projekte/Klimawandel/Experiment/data/2order/DQC/Anno_Zero/"
root_dir = "C:/Users/CBrida/Desktop/Anno_Zero/"

# data_output_dir =paste(root_dir,"/Output/",sep="")
# data_input_dir =paste(root_dir,"/Input/S4/RAW_0/",sep="")               # <- insert here the name of the folder to source data
report_output_dir <- paste(data_output_dir,"/00_DQC_Reports/",sep = "")  # where to put output reports
DQC_setting_dir = paste(root_dir,"/Setting/",sep="")
# project_dir = paste(root_dir,"/DQC/dataqualitycheckeuracalpenv/",sep="")
project_dir = "C:/Users/CBrida/Desktop/GitLab/dataqualitycheckeuracalpenv/"

range_dir <- paste(DQC_setting_dir,"/Range/", sep = "")
download_table_dir <- paste(DQC_setting_dir,"/Download_tables/", sep = "")
logger_info_dir <- paste(DQC_setting_dir,"/Range/", sep = "")
mail_dir = paste(DQC_setting_dir,"/email_status/",sep = "")



# ! ! ! ! ! ! ! ! ! ! ! ! ! ! ! ! ! ! ! ! ! ! ! ! ! ! ! ! ! ! ! ! ! ! ! ! ! ! !

# To set TRUE if you wanto to bypass all files of data_input_dir
# If it is FALSE every file was checked using the download table flag record_check (0 -> bypass, 1 --> check)

BYPASS_ALL_RECORD_CHECK = FALSE

# ! ! ! ! ! ! ! ! ! ! ! ! ! ! ! ! ! ! ! ! ! ! ! ! ! ! ! ! ! ! ! ! ! ! ! ! ! ! !


# Sys.setenv(RSTUDIO_PANDOC = "/usr/lib/rstudio/bin/pandoc/")
# .....................................................................................................................................................

# ..... Params section .....................................................................................................................................

# project_type = c("LTER","MONALISA")

# PROJECT = "LTER" # Possible project: "LTER"; "MONALISA";

# data_input_dir <- paste(main_dir,"/Data/LoggerNet_Raw_Data/Data/",sep = "")                    # where input files are



logger_info_file <- paste(logger_info_dir,"Logger_number_and_software.csv", sep = "")


warning_report_RMD = paste(project_dir,"/Rmd/DQC_Warning_Reports.Rmd",sep = "")

HOURS_OFFLINE = 24       # <- no data update since 24h --> station broken?
LOGGERNET_OFFLINE = 1    # <. all station offline since 1h --> loggernet doesn't work!

date_DQC = as.POSIXct(format(Sys.time(),format = "%Y-%m-%d %H:%M"), tz = 'Etc/GMT-1')

# loggernet_status = c()

mail_file = "mail_status.csv"
mail_file_alert_ONE = "out_of_range.csv"
mail_file_alert_ZERO = "out_of_range_0.csv"

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
print(mail_config_file)
print(sender)
print(reciver)
print(data_input_dir)

# if(!file.exists(paste(DQC_setting_dir,"lock_report.lock",sep = ""))){
#   file.create(paste(DQC_setting_dir,"lock_report.lock",sep = ""))
# }

# -------------------------------# -------------------------------# -------------------------------# -------------------------------# -------------------------------

# for(PROJECT in project_type){


if(!dir.exists(report_output_dir)){
  dir.create(report_output_dir)
}

data_from_row =  5                                             # <-- Row number of first data
header_row_number =  2                                         # <-- Row number of header
datetime_header =  "TIMESTAMP"                                 # <-- header corresponding to TIMESTAMP
datetime_format =  "%Y-%m-%d %H:%M"                          # <-- datetime format. Use only: Y -> year, m -> month, d -> day, H -> hour, M -> minute
datetime_sampling =  "15 min"
record_header =  "RECORD"
range_file_ONE =  "Range.csv"
range_file_ZERO =  "Range_0.csv"
# use_alert_station_flag = TRUE        # <-- IN REPORT DON'T SHOW ANALYZE VARIABLES SET AS 0 IN RANGE FILE
use_alert_station_flag = TRUE        # <-- use range file flags. Default: TRUE
use_realtime_station_flag = FALSE        # <-- use out_of_range file flags. Default: FALSE

write_output_files =  "TRUE"
# write_output_files =  "TRUE"
write_output_report =  "FALSE"

# general stations status --> loggernent doesn't work properly!
# loggernet_status_prj = as.data.frame(matrix(ncol = 3))
# colnames(loggernet_status_prj) = c("Station", "Status", "Last_modification")

# ! ! ! ! ! ! ! ! ! ! ! ! ! ! ! ! ! ! ! ! ! ! ! ! ! ! ! ! ! ! ! ! ! ! ! ! ! ! ! ! ! ! ! ! ! ! ! ! ! ! ! !
#
# SELECTION OF RANGE FILE TO CONSIDER!!! BE CAREFUL DATA FOLDER SHOULD BE "RAW_0", "RAW_1", "RAW_2"
#
# ! ! ! ! ! ! ! ! ! ! ! ! ! ! ! ! ! ! ! ! ! ! ! ! ! ! ! ! ! ! ! ! ! ! ! ! ! ! ! ! ! ! ! ! ! ! ! ! ! ! ! !

if(grepl(pattern = "RAW_0", data_input_dir)){
  mail_file_alert  = mail_file_alert_ZERO
  range_file = range_file_ZERO
}else{
  mail_file_alert  = mail_file_alert_ONE
  range_file = range_file_ONE
}


# ..........................................................................................................................................................

# ..... files selection .....................................................................................................................................

files_available_raw = dir(data_input_dir,pattern = ".dat")                  # <-- Admitted pattern:  ".dat" or ".csv"
files_available_raw = files_available_raw[grepl(pattern = ".dat$",x = files_available_raw)]

files_available_raw = files_available_raw[!grepl(pattern = "backup",x = files_available_raw)]          # REMOVE FILES WITH WRONG NAMES (.dat.backup not admitted)


u1 = gregexpr(files_available_raw,pattern = "_")[[1]][1]
u2 = gregexpr(files_available_raw,pattern = "_")[[1]][2]
u3 = gregexpr(files_available_raw,pattern = "_")[[1]][3]

project_name = substring(text = files_available_raw,first = 1,last = u1-1)
logg_data_NAME = substring(text = files_available_raw,first = u1+1,last = u2-1)
table_data_NAME = substring(text = files_available_raw,first = u2+1,last = u3-1)
datetime_NAME = substring(text = files_available_raw,first = u3+1,last = nchar(files_available_raw)-4)
file_datetime = as.POSIXct(datetime_NAME, format = "%Y_%m_%d", tz = "Etc/GMT-1")

df_files = data.frame(files_available_raw, project_name, logg_data_NAME, table_data_NAME,file_datetime,stringsAsFactors = F)
colnames(df_files) = c("Files","Project", "LoggerNet_name", "Datatable_name", "file_datetime")

files_available = df_files$Files[which(df_files$LoggerNet_name == df_files$Datatable_name)]
files_available = files_available[order(df_files$file_datetime)]

PROJECT = project_name

file_group = paste(df_files$Project,df_files$LoggerNet_name, df_files$Datatable_name,sep = "_")
project_group = project_name
station_name_group = logg_data_NAME

# ..........................................................................................................................................................

# ..... download table section .....................................................................................................................................
if(length(unique(file_group))  > 1){
  warning("Put in the input folder only files belonging to the same station!")
}else{
  
  project_unique = unique(project_group)
  file_unique = paste(unique(file_group),".dat",sep = "")   # .dat per ricondurmi condizioni script automatico!
  station_unique = unique(station_name_group)
  
  download_table = read_and_update_download_table(DOWNLOAD_TABLE_DIR = download_table_dir, FILES_AVAILABLE = file_unique, DATETIME_FORMAT = datetime_format, PROJECT = project_unique) # sopstare fuori dal ciclo Project! --> salvo old ogni volta che gira lo script e non ogni volta che cambio progettoS
  
  # download_table_proj = download_table$Station[which(download_table$Project == PROJECT)]
  
  # files_available_project = files_available[which(substring(files_available,1, nchar(files_available)-4) %in% download_table_proj)]
  
  # ~ ~ ~ ~ ~ ~
  
  ############################################
  
  # final_dataframe = matrix(ncol = 20, nrow = length(files_available))
  # colnames(final_dataframe) = c("Station", "Status",
  #                               "flag_empty","flag_logger_number", "flag_error_df","flag_date",
  #                               "flag_duplicates_rows","flag_overlap","flag_missing_records","flag_missing_dates",
  #                               "flag_range_variable_to_set","flag_range_variable_new","flag_out_of_range",
  #                               "flag_new_duplicates_rows","flag_new_overlap","flag_new_missing_dates", "flag_missing_records_new",
  #                               "Report_link", "Data_folder", "File_name")
  
  report_dataframe = as.data.frame(matrix(ncol = 16, nrow = length(files_available)),stringsAsFactors = FALSE)
  colnames(report_dataframe) = c("Station",
                                 "Offline",
                                 "err_empty","err_logger_number","err_structure","err_structure_change","err_no_new_data","err_overlap","err_missing_record","err_restart_record",
                                 "err_date_missing","err_range_alert",
                                 "err_out_of_range","err_duplicates_rows",
                                 "var_flagged",
                                 "report_link")
  
  
  
  # report_start = Sys.time()
  
  t = 1
  
  for(t in  1: length(files_available)){
    # for(t in  1:3){
    
    gc(reset = T)
    
    FILE_NAME = files_available[t]
    
    u1 = gregexpr(FILE_NAME,pattern = "_")[[1]][1]
    u2 = gregexpr(FILE_NAME,pattern = "_")[[1]][2]
    u3 = gregexpr(FILE_NAME,pattern = "_")[[1]][3]
    
    
    datetime_NAME = substring(text = FILE_NAME,first = u3+1,last = nchar(FILE_NAME)-4)
    file_datetime = as.POSIXct(datetime_NAME, format = "%Y_%m_%d", tz = "Etc/GMT-1")
    
    STATION_NAME = substring(FILE_NAME,u1+1, u2-1)
    
    w_dwnl = which(download_table$Station == substring(FILE_NAME, 1, nchar(FILE_NAME) - 15))
    dwnl_info = download_table[w_dwnl,]
    
    if(dir.exists(paste(data_output_dir,STATION_NAME,"/", sep = ""))){                # create subfolder to store data organized by station name
      if(dir.exists(paste(data_output_dir,STATION_NAME,"/Total/Files_dat/", sep = ""))){
        output_dir_data_new = paste(data_output_dir,STATION_NAME,"/Total/Files_dat/", sep = "")
        output_dir_raw_new = paste(data_output_dir,STATION_NAME,"/Raw/", sep = "")
        output_dir_report_new = paste(data_output_dir,STATION_NAME,"/Report/", sep = "")
        # warning_file_dir_station = paste(data_output_dir,STATION_NAME,"/Alerts/Warnings/", sep = "")
      }else{
        dir.create(paste(data_output_dir,STATION_NAME,"/Report/", sep = ""))
        # dir.create(paste(data_output_dir,STATION_NAME,"/Alerts/Reports/", sep = ""))
        # dir.create(paste(data_output_dir,STATION_NAME,"/Alerts/Warnings/", sep = ""))
        dir.create(paste(data_output_dir,STATION_NAME,"/Raw/", sep = ""))
        dir.create(paste(data_output_dir,STATION_NAME,"/Total/", sep = ""))
        dir.create(paste(data_output_dir,STATION_NAME,"/Total/Files_dat/", sep = ""))
        # dir.create(paste(data_output_dir,STATION_NAME,"/Processed/", sep = ""))
        # dir.create(paste(data_output_dir,STATION_NAME,"/Pics/", sep = ""))
        output_dir_data_new = paste(data_output_dir,STATION_NAME,"/Total/Files_dat/", sep = "")
        output_dir_raw_new = paste(data_output_dir,STATION_NAME,"/Raw/", sep = "")
        output_dir_report_new = paste(data_output_dir,STATION_NAME,"/Report/", sep = "")
        # warning_file_dir_station = paste(data_output_dir,STATION_NAME,"/Alerts/Warnings/", sep = "")
        
      }
    }else{
      dir.create(paste(data_output_dir,STATION_NAME,"/", sep = ""))
      # dir.create(paste(data_output_dir,STATION_NAME,"/Alerts/", sep = ""))
      # dir.create(paste(data_output_dir,STATION_NAME,"/Alerts/Reports/", sep = ""))
      # dir.create(paste(data_output_dir,STATION_NAME,"/Alerts/Warnings/", sep = ""))
      dir.create(paste(data_output_dir,STATION_NAME,"/Raw/", sep = ""))
      dir.create(paste(data_output_dir,STATION_NAME,"/Total/", sep = ""))
      dir.create(paste(data_output_dir,STATION_NAME,"/Total/Files_dat/", sep = ""))
      dir.create(paste(data_output_dir,STATION_NAME,"/Report/", sep = ""))
      # dir.create(paste(data_output_dir,STATION_NAME,"/Processed/", sep = ""))
      # dir.create(paste(data_output_dir,STATION_NAME,"/Pics/", sep = ""))
      output_dir_data_new = paste(data_output_dir,STATION_NAME,"/Total/Files_dat/", sep = "")
      output_dir_raw_new = paste(data_output_dir,STATION_NAME,"/Raw/", sep = "")
      # output_dir_report_new = paste(data_output_dir,STATION_NAME,"/Alerts/Reports/", sep = "")
      output_dir_report_new = paste(data_output_dir,STATION_NAME,"/Report/", sep = "")
      # warning_file_dir_station = paste(data_output_dir,STATION_NAME,"/Alerts/Warnings/", sep = "")
      
    }
    
    
    if(dwnl_info$Stop_DQC == 0){
      
      file_info = file.info(paste(data_input_dir,FILE_NAME,sep = ""))
      
      date_last_modif_file = as.character(format(file_info$mtime),format = datetime_format)
      h_last_modif_file = trunc(as.POSIXct(date_last_modif_file, tz = "Etc/GMT-1"),units = "hours")
      h_DQC = trunc(date_DQC,units = "hours")
      
      hours_diff = as.numeric(difftime(time1 = h_DQC, time2 = h_last_modif_file, tz = "Etc/GMT-1",units = "hours"))
      
      if(hours_diff >= 24 ){   # 24 =  24h offline
        # offline_value = 3    # manual report by default don't show if the station is offline (obvious)
        offline_value = 0      # Evenutally activable setting offline_value = 3
      }else{
        offline_value = 0
      }
      
      if(file_datetime > as.POSIXct(dwnl_info$Last_Modification,tz = "Etc/GMT-1", format = datetime_format) | is.na(dwnl_info$Last_Modification)){
        
        input_dir = data_input_dir
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
                                   use_realtime_station_flag)
        
        mydata = DQC_results[[1]]
        flags_df = DQC_results[[2]]
        file_names = DQC_results[[3]]
        errors = DQC_results[[4]]
        mydata_out_of_range = DQC_results[[5]]
        
        mylist <- split(flags_df$value, seq(nrow(flags_df)))
        names(mylist) = flags_df$flag_names
        
        status = unlist(lapply(errors,function(x) x[[1]]))
        
        if(all(status[names(status) %in% c( "err_no_new_data","err_empty","err_structure",
                                            "err_overlap", "err_missing_record","err_restart_record")] == "N")){
          # if(mylist$flag_empty == 0 & mylist$flag_logger_number == 0 & mylist$flag_error_df == 0 & mylist$flag_date == 0){
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
          last_date = NA
        }
        
        
        # ~ ~ ~ ~ Issue Management (on/off message) ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
        
        # date_DQC = as.POSIXct(format(Sys.time(),format = "%Y-%m-%d %H:%M"), tz = 'Etc/GMT-1')
        
        # status = unlist(lapply(errors,function(x) x[[1]]))
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
          
          # output_file_report = paste(STATION_NAME,"_",dqc_date_write,".html",sep = "")
          output_file_report = paste(substring(file_name, 1,nchar(file_name)-4),".html",sep = "")
          
          issue_report_RMD = paste(project_dir,"/Rmd/DQC_Reports.Rmd",sep = "")
          
          issue_file_dir_station = output_dir_report_new
          
          input =  issue_report_RMD
          # output_file = output_file_report
          output_dir = issue_file_dir_station
          
          j=0
          
          repeat{
            if(!file.exists(paste(output_dir,output_file_report,sep = ""))){
              output_file = output_file_report
              break
            }else{
              j=j+1
              output_file_new = paste(substring(output_file_report,1, nchar(output_file_report)-5),"(",j,").html",sep = "")
              if(!file.exists(paste(output_dir,output_file_new,sep = ""))){
                output_file = output_file_new
                break
              }
            }
            
          }
          
          
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
        
        if(dwnl_info$record_check == 0){
          w_rec = which(names(status_final) %in% c("err_missing_record","err_restart_record" ))
          status_final[w_rec] = 2
        }
        
        if(any(status[-which(names(status) == "err_duplicates_rows")] == "Y")){
          # paste(substring(output_dir,nchar(main_dir)),output_file,sep = "")
          # link = paste(main_dir_mapping_out, substring(output_dir_report_new,nchar(main_dir_mapping_in)), output_file_report,sep = "")
          link = paste(output_dir_report_new, output_file,sep = "")
          # link = paste("/",project_unique,substring(output_dir_report_new,nchar(data_output_dir)), output_file_report,sep = "")
          # link = paste("////smb.scientificnet.org//alpenv", substring(output_dir_report_new,nchar('/shared/')), output_file_report,sep = "")
        }else{
          link = NA
          # link = "---"
        }
        
        if(length(variables_flagged) == 0){
          var_flagged = 0
        }else{
          var_flagged = 1
        }
        
        
        # report_info = c(STATION_NAME,0,status_final,var_flagged, link)
        
        report_info = c(substring(FILE_NAME, 1, nchar(FILE_NAME)-4),offline_value,status_final,var_flagged, link)
        names(report_info) = c("Station",
                               "Offline",
                               "err_empty","err_logger_number","err_structure","err_structure_change","err_no_new_data","err_overlap","err_missing_record","err_restart_record",
                               "err_date_missing","err_range_alert",
                               "err_out_of_range","err_duplicates_rows",
                               "var_flagged",
                               "report_link")
        
        # ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
        
        
        if(all(status[names(status) %in% c( "err_no_new_data","err_empty","err_structure",
                                            "err_overlap", "err_missing_record","err_restart_record")] == "N")){
          download_table$Last_date[w_dwnl] = last_date
          download_table$Last_Modification[w_dwnl] = format(file_datetime, format = datetime_format)
          
          ### ------ NEW, TO TEST ------
          
          if(BYPASS_ALL_RECORD_CHECK == TRUE & download_table$record_check[w_dwnl] == 0){           # to test! If bypass is activated every file
            download_table$record_check[w_dwnl] = 0
          }else{
            download_table$record_check[w_dwnl] = 1    # NEW! Record check activated every time!
          }
          
          ### --------------------------
          
          # download_table$record_check[w_dwnl] = 1    # NEW! Record check activated every time!
          
          
          write.csv(download_table,paste(download_table_dir,"download_table.csv",sep = ""),quote = F,row.names = F)
          
        }else{
          download_table$Stop_DQC[w_dwnl] = 1
          write.csv(download_table,paste(download_table_dir,"download_table.csv",sep = ""),quote = F,row.names = F)
        }
        
      }else{
        report_info = c(substring(FILE_NAME, 1, nchar(FILE_NAME)-4),1,rep(NA,12),NA, NA)
        names(report_info) = c("Station",
                               "Offline",
                               "err_empty","err_logger_number","err_structure","err_structure_change","err_no_new_data","err_overlap","err_missing_record","err_restart_record",
                               "err_date_missing","err_range_alert",
                               "err_out_of_range","err_duplicates_rows",
                               "var_flagged",
                               "report_link")
      }
    }else{
      report_info = c(substring(FILE_NAME, 1, nchar(FILE_NAME)-4),2,rep(NA,12),NA, NA)
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
    
    # # final_dataframe = rbind(final_dataframe,final_info)
    
    # final_dataframe[t,] = final_info
    
    report_dataframe[t,] = report_info
    
    # loggernet_status_prj[t,1] = final_info[1]
    # loggernet_status_prj[t,2] = final_info[2]
    # loggernet_status_prj[t,3] = date_last_modif_file
    
    
    gc(reset = T)
    
    
  }
  # reset download table stop_dqc at the end of loop
  w_stat_downl = which(download_table$Station == substring(file_unique, 1, nchar(file_unique)-4))
  download_table$Stop_DQC[w_stat_downl] = 0
  download_table$record_check[w_stat_downl] = 1     # added to reset at the original status when a folder ended!
  write.csv(download_table,paste(download_table_dir,"download_table.csv",sep = ""),quote = F,row.names = F)
  
  
  report_dataframe = as.data.frame(report_dataframe,stringsAsFactors = F)
  # loggernet_status = rbind(loggernet_status,loggernet_status_prj)
  
  
  # ..... Final Report .....................................................................................................................................
  
  
  input_final = paste(project_dir,"/Rmd/DQC_Report_overview.Rmd",sep = "")
  # date_DQC
  output_file_final =  paste(project_unique,"_",station_unique,"_Report_",
                             format(date_DQC,format = "%Y"),"_",
                             format(date_DQC,format = "%m"),"_",
                             format(date_DQC,format = "%d"),".html", sep = "")
  
  output_dir_final = output_dir_report
  
  j=0
  
  repeat{
    if(!file.exists(paste(output_dir_final,output_file_final,sep = ""))){
      output_file_TOT = output_file_final
      break
    }else{
      j=j+1
      output_file_final_new = paste(substring(output_file_final,1, nchar(output_file_final)-5),"(",j,").html",sep = "")
      if(!file.exists(paste(output_dir_final,output_file_final_new,sep = ""))){
        output_file_TOT = output_file_final_new
        break
      }
    }
    
  }
  
  rm(params)
  rmarkdown::render(input = input_final,
                    output_file = output_file_TOT ,
                    output_dir = output_dir_final,
                    params = list(PROJECT = project_unique,
                                  date_DQC = date_DQC ,
                                  report_dataframe = report_dataframe))
  
  
  # ..... Data preparation for Database .....................................................................................................................................
  
  # MANDARE MAIL !!!!
  print("--------------------------------------------------------------------------------------------------")
  
  report_output_dir <- paste(data_output_dir,"00_DQC_Reports/",sep = "")  # where to put output reports
  
  my_subject = paste("Manual report:", project_unique,station_unique)
  my_body = paste(output_dir_final,output_file_TOT,sep="")
  # my_body = paste(main_dir_mapping_out, substring(output_dir_final, nchar(main_dir_mapping_in)),output_file_final,sep="")
  # my_body = paste(url_webservice,project_unique,substring(report_output_dir, nchar(data_output_dir)),output_file_final,sep="")
  
  # my_body = paste(url_webservice,icinga_text,sep = "")
  # icinga_text = paste(substring(output_dir,nchar('/shared/')),output_file,sep = "")               # to disactivate when webservice is ready!
  # icinga_text = paste(substring(output_dir,nchar(data_output_dir)),output_file,sep = "")        # to activate when webservice is ready!
  
  
  send.mail(from = sender,
            to = reciver,
            subject = my_subject,
            body = " ",
            attach.files = my_body,
            inline = T,
            smtp = my_smtp,
            authenticate = TRUE,
            send = TRUE,html = F)
  
  
}


# file.remove(paste(DQC_setting_dir,"lock_report.lock",sep = ""))

print("------------------------------------------------------------------------------------------")
print(Sys.time())
print("--------------------- End script! --------------------------------------------------------")
