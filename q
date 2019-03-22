[1mdiff --git a/Main_script/DQC_Manually.R b/Main_script/DQC_Manually.R[m
[1mindex 3ccb6d94..6fa78972 100644[m
[1m--- a/Main_script/DQC_Manually.R[m
[1m+++ b/Main_script/DQC_Manually.R[m
[36m@@ -79,7 +79,7 @@[m [mDQC_setting_dir <- paste(main_dir,"/DQC/",sep = "")[m
 [m
 logger_info_file <- paste(DQC_setting_dir,"/Process/Logger_number_and_software.csv", sep = "")[m
 range_dir <- paste(DQC_setting_dir,"/Process/", sep = "")[m
[31m-download_table_dir <- paste(DQC_setting_dir,"/Process/Download_tables/Weekly/", sep = "")[m
[32m+[m[32mdownload_table_dir <- paste(DQC_setting_dir,"/Process/Download_tables/", sep = "")[m
 [m
 warning_report_RMD = paste(project_dir,"/Rmd/DQC_Warning_Reports.Rmd",sep = "")[m
 [m
[36m@@ -119,197 +119,126 @@[m [mprint(mail_config_file)[m
 print(sender)[m
 print(reciver)[m
 [m
[31m-if(!file.exists(paste(DQC_setting_dir,"lock_report.lock",sep = ""))){[m
[31m-  file.create(paste(DQC_setting_dir,"lock_report.lock",sep = ""))[m
[31m-}[m
[32m+[m[32m# if(!file.exists(paste(DQC_setting_dir,"lock_report.lock",sep = ""))){[m
[32m+[m[32m#   file.create(paste(DQC_setting_dir,"lock_report.lock",sep = ""))[m
[32m+[m[32m# }[m
 [m
 # -------------------------------# -------------------------------# -------------------------------# -------------------------------# -------------------------------[m
 [m
[31m-for(PROJECT in project_type){[m
[31m-  data_output_dir   <- paste(main_dir,"/Data/DQC_Processed_Data/",PROJECT,"/Stations/",sep = "")  # where to put output files[m
[31m-  report_output_dir <- paste(data_output_dir,"00_DQC_Reports/",sep = "")  # where to put output reports[m
[31m-  # report_output_dir <- paste(main_dir,"Stations_Data/Data/DQC_Processed_Data/",PROJECT,"/Stations/00_DQC_Reports/",sep = "")  # where to put output reports[m
[31m-  database_file_dir <- paste(main_dir,"/Data/DQC_DB/",PROJECT,"/", sep = "")  # where to put output files (MODIFIED FOR DATABASE TESTING) -----> "Permission denied"[m
[31m-  [m
[31m-  # warning_file_dir <- paste(main_dir,"Stations_Data/Data/DQC_Warnings/",PROJECT,"/", sep = "")  # where to put warnings html files[m
[31m-  [m
[31m-  [m
[31m-  data_from_row =  5                                             # <-- Row number of first data[m
[31m-  header_row_number =  2                                         # <-- Row number of header[m
[31m-  datetime_header =  "TIMESTAMP"                                 # <-- header corresponding to TIMESTAMP[m
[31m-  datetime_format =  "%Y-%m-%d %H:%M"                          # <-- datetime format. Use only: Y -> year, m -> month, d -> day, H -> hour, M -> minute[m
[31m-  datetime_sampling =  "15 min"[m
[31m-  record_header =  "RECORD"[m
[31m-  range_file =  "Range.csv"[m
[31m-  # use_alert_station_flag = TRUE        # <-- IN REPORT DON'T SHOW ANALYZE VARIABLES SET AS 0 IN RANGE FILE[m
[31m-  use_alert_station_flag = TRUE        # <-- use range file flags. Default: TRUE[m
[31m-  use_realtime_station_flag = FALSE        # <-- use out_of_range file flags. Default: FALSE[m
[31m-  [m
[31m-  write_output_files =  "TRUE"[m
[31m-  # write_output_files =  "TRUE"[m
[31m-  write_output_report =  "FALSE"[m
[31m-  [m
[31m-  # general stations status --> loggernent doesn't work properly![m
[31m-  loggernet_status_prj = as.data.frame(matrix(ncol = 3))[m
[31m-  colnames(loggernet_status_prj) = c("Station", "Status", "Last_modification")[m
[31m-  [m
[31m-  # file <- "M4s.dat"[m
[31m-  # start_date <- NA[m
[31m-  [m
[31m-  # ~~~ Default directory ~~~~[m
[31m-  [m
[31m-  # !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!  TO REMOVE !!!!!!!!!!!!!!!!!!!!!!!!!!!!!![m
[31m-  #[m
[31m-  # if(write_output_report == TRUE){[m
[31m-  #   Rmd_report_generator <- paste(project_dir, "Rmd/DQC_Report_Generator.Rmd",sep = "")[m
[31m-  # }else{[m
[31m-  #   Rmd_report_generator <- paste(project_dir, "Rmd/DQC_Calculator_2.Rmd",sep = "")[m
[31m-  # }[m
[31m-  #[m
[31m-  # !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!![m
[31m-  [m
[31m-  # ..........................................................................................................................................................[m
[31m-  [m
[31m-  # ..... files selection .....................................................................................................................................[m
[31m-  [m
[31m-  files_available_raw = dir(input_dir,pattern = ".dat")                  # <-- Admitted pattern:  ".dat" or ".csv"[m
[31m-  files_available_raw = files_available_raw[grepl(pattern = ".dat$",x = files_available_raw)]      [m
[31m-  [m
[31m-  files_available_raw = files_available_raw[!grepl(pattern = "backup",x = files_available_raw)]          # REMOVE FILES WITH WRONG NAMES (.dat.backup not admitted) [m
[31m-  [m
[31m-  # if(PROJECT != "MONALISA"){[m
[31m-  #   files_available_raw = files_available_raw[!grepl(pattern = "IP",x = files_available_raw)]          # <- for MONALISA  IP in name is admitted [m
[31m-  # }[m
[31m-  [m
[31m-  files_available = files_available_raw[grepl(pattern = paste("^",PROJECT,sep = ""),x = files_available_raw)]          [m
[31m-  [m
[31m-  files_no_project = substring(files_available, nchar(PROJECT)+2, nchar(files_available)-4)[m
[31m-  [m
[31m-  if(length(files_no_project) > 0){[m
[31m-    u1 =c()[m
[31m-    logg_data_NAME = c()[m
[31m-    table_data_NAME = c()[m
[31m-    [m
[31m-    for(h in 1:length(files_no_project)){[m
[31m-      u1[h] = gregexpr(files_no_project,pattern = "_")[[h]][1]   # <- here we find the sencond "[[1]][2]" underscore!!!!![m
[31m-      logg_data_NAME[h] = substring(text = files_no_project[h],first = 1,last = u1[h]-1)[m
[31m-      table_data_NAME[h] = substring(text = files_no_project[h],first = u1[h]+1,last = nchar(files_no_project[h]))[m
[31m-    }  [m
[31m-    df_files = data.frame(files_available, logg_data_NAME, table_data_NAME,stringsAsFactors = F)[m
[31m-    colnames(df_files) = c("Files", "LoggerNet_name", "Datatable_name")[m
[31m-    [m
[31m-    files_available = df_files$Files[which(df_files$LoggerNet_name == df_files$Datatable_name)][m
[31m-    [m
[31m-    # if(PROJECT == "LTER"){                                                                        # <--Filter files based on Project (diffent if is MONALISA or LTER)[m
[31m-    #   files_available = df_files$Files[which(df_files$LoggerNet_name == df_files$Datatable_name)][m
[31m-    # }[m
[31m-    # [m
[31m-    # if(PROJECT == "MONALISA"){                                                                        # <--Filter files based on Project (diffent if is MONALISA or LTER)[m
[31m-    #   # files_available = df_files$Files[m
[31m-    #   files_available = df_files$Files[which(df_files$Datatable_name == "MeteoVal")][m
[31m-    #   [m
[31m-    # }[m
[31m-  } else{[m
[31m-    files_available = files_no_project[m
[31m-  }[m
[31m-  [m
[31m-  [m
[31m-  # ..........................................................................................................................................................[m
[31m-  [m
[31m-  # ..... download table section .....................................................................................................................................[m
[31m-  [m
[31m-  [m
[31m-  download_table = read_and_update_download_table(DOWNLOAD_TABLE_DIR = download_table_dir, FILES_AVAILABLE = files_available, DATETIME_FORMAT = datetime_format, PROJECT = PROJECT) # sopstare fuori dal ciclo Project! --> salvo old ogni volta che gira lo script e non ogni volta che cambio progettoS[m
[31m-  # issue_counter = read_and_update_issue_counter(ISSUE_COUNTER_DIR = issue_counter_dir, FILES_AVAILABLE = files_available, DATETIME_FORMAT = datetime_format, PROJECT = PROJECT)[m
[31m-  [m
[31m-  download_table_proj = download_table$Station[which(download_table$Project == PROJECT)][m
[31m-  # issue_counter_proj = issue_counter$Station[which(issue_counter$Project == PROJECT)][m
[31m-  [m
[31m-  files_available_project = files_available[which(substring(files_available,1, nchar(files_available)-4) %in% download_table_proj)][m
[31m-  [m
[31m-  # ~ ~ ~ temporary solution to avoid issue with Vimes1500 station ---> remove this if when data file will fix! ~ ~ ~[m
[31m-  if("MONALISA_Vimes1500_Vimes1500.dat" %in% files_available_project){[m
[31m-    files_available_project = files_available_project[-which(files_available_project == "MONALISA_Vimes1500_Vimes1500.dat")][m
[31m-  }[m
[31m-  # ~ ~ ~ ~ ~ ~ [m
[32m+[m[32m# for(PROJECT in project_type){[m
[32m+[m
[32m+[m[32mdata_output_dir   <- paste(main_dir,"/Data/DQC_Processed_Data/",sep = "")  # where to put output files[m
[32m+[m[32mreport_output_dir <- paste(data_output_dir,"00_DQC_Reports/",sep = "")  # where to put output reports[m
[32m+[m[32m# report_output_dir <- paste(main_dir,"Stations_Data/Data/DQC_Processed_Data/",PROJECT,"/Stations/00_DQC_Reports/",sep = "")  # where to put output reports[m
[32m+[m[32m# database_file_dir <- paste(main_dir,"/Data/DQC_DB/", sep = "")  # where to put output files (MODIFIED FOR DATABASE TESTING) -----> "Permission denied"[m
[32m+[m
[32m+[m[32m# warning_file_dir <- paste(main_dir,"Stations_Data/Data/DQC_Warnings/",PROJECT,"/", sep = "")  # where to put warnings html files[m
[32m+[m
[32m+[m
[32m+[m[32mdata_from_row =  5                                             # <-- Row number of first data[m
[32m+[m[32mheader_row_number =  2                                         # <-- Row number of header[m
[32m+[m[32mdatetime_header =  "TIMESTAMP"                                 # <-- header corresponding to TIMESTAMP[m
[32m+[m[32mdatetime_format =  "%Y-%m-%d %H:%M"                          # <-- datetime format. Use only: Y -> year, m -> month, d -> day, H -> hour, M -> minute[m
[32m+[m[32mdatetime_sampling =  "15 min"[m
[32m+[m[32mrecord_header =  "RECORD"[m
[32m+[m[32mrange_file =  "Range.csv"[m
[32m+[m[32m# use_alert_station_flag = TRUE        # <-- IN REPORT DON'T SHOW ANALYZE VARIABLES SET AS 0 IN RANGE FILE[m
[32m+[m[32muse_alert_station_flag = TRUE        # <-- use range file flags. Default: TRUE[m
[32m+[m[32muse_realtime_station_flag = FALSE        # <-- use out_of_range file flags. Default: FALSE[m
[32m+[m
[32m+[m[32mwrite_output_files =  "TRUE"[m
[32m+[m[32m# write_output_files =  "TRUE"[m
[32m+[m[32mwrite_output_report =  "FALSE"[m
[32m+[m
[32m+[m[32m# general stations status --> loggernent doesn't work properly![m
[32m+[m[32mloggernet_status_prj = as.data.frame(matrix(ncol = 3))[m
[32m+[m[32mcolnames(loggernet_status_prj) = c("Station", "Status", "Last_modification")[m
[32m+[m
[32m+[m[32m# ..........................................................................................................................................................[m
[32m+[m
[32m+[m[32m# ..... files selection .....................................................................................................................................[m
[32m+[m
[32m+[m[32mfiles_available_raw = dir(input_dir,pattern = ".dat")                  # <-- Admitted pattern:  ".dat" or ".csv"[m
[32m+[m[32mfiles_available_raw = files_available_raw[grepl(pattern = ".dat$",x = files_available_raw)][m[41m      [m
[32m+[m
[32m+[m[32mfiles_available_raw = files_available_raw[!grepl(pattern = "backup",x = files_available_raw)]          # REMOVE FILES WITH WRONG NAMES (.dat.backup not admitted)[m[41m [m
[32m+[m
[32m+[m[32mif(length(files_available_raw)  > 1){[m
[32m+[m[32m  stop("Put in Raw data folder ONLY  a file!")[m
[32m+[m[32m}else{[m
[32m+[m[32m  u1 = gregexpr(files_available_raw,pattern = "_")[[1]][1][m[41m  [m
[32m+[m[32m  u2 = gregexpr(files_available_raw,pattern = "_")[[1]][2][m[41m  [m
   [m
[31m-  ############################################[m
[32m+[m[32m  project_name = substring(text = files_available_raw,first = 1,last = u1-1)[m
[32m+[m[32m  logg_data_NAME = substring(text = files_available_raw,first = u1+1,last = u2-1)[m
[32m+[m[32m  table_data_NAME = substring(text = files_available_raw,first = u2+1,last = nchar(files_available_raw)-4)[m
[32m+[m[32m  # }[m[41m  [m
[32m+[m[32m  df_files = data.frame(files_available_raw, project_name, logg_data_NAME, table_data_NAME,stringsAsFactors = F)[m
[32m+[m[32m  colnames(df_files) = c("Files","Project", "LoggerNet_name", "Datatable_name")[m
   [m
[31m-  final_dataframe = matrix(ncol = 20, nrow = length(files_available_project))[m
[31m-  colnames(final_dataframe) = c("Station", "Status",[m
[31m-                                "flag_empty","flag_logger_number", "flag_error_df","flag_date",[m
[31m-                                "flag_duplicates_rows","flag_overlap","flag_missing_records","flag_missing_dates",[m
[31m-                                "flag_range_variable_to_set","flag_range_variable_new","flag_out_of_range",[m
[31m-                                "flag_new_duplicates_rows","flag_new_overlap","flag_new_missing_dates", "flag_missing_records_new",   [m
[31m-                                "Report_link", "Data_folder", "File_name")[m
[32m+[m[32m  files_available = df_files$Files[which(df_files$LoggerNet_name == df_files$Datatable_name)][m
[32m+[m[32m  PROJECT = project_name[m
[32m+[m[32m}[m
[32m+[m
[32m+[m[32m# ..........................................................................................................................................................[m
[32m+[m
[32m+[m[32m# ..... download table section .....................................................................................................................................[m
[32m+[m
[32m+[m
[32m+[m[32mdownload_table = read_and_update_download_table(DOWNLOAD_TABLE_DIR = download_table_dir, FILES_AVAILABLE = files_available, DATETIME_FORMAT = datetime_format, PROJECT = PROJECT) # sopstare fuori dal ciclo Project! --> salvo old ogni volta che gira lo script e non ogni volta che cambio progettoS[m
[32m+[m
[32m+[m[32mdownload_table_proj = download_table$Station[which(download_table$Project == PROJECT)][m
[32m+[m
[32m+[m[32mfiles_available_project = files_available[which(substring(files_available,1, nchar(files_available)-4) %in% download_table_proj)][m
[32m+[m
[32m+[m[32m# ~ ~ ~ ~ ~ ~[m[41m [m
[32m+[m
[32m+[m[32m############################################[m
[32m+[m
[32m+[m[32mfinal_dataframe = matrix(ncol = 20, nrow = length(files_available_project))[m
[32m+[m[32mcolnames(final_dataframe) = c("Station", "Status",[m
[32m+[m[32m                              "flag_empty","flag_logger_number", "flag_error_df","flag_date",[m
[32m+[m[32m                              "flag_duplicates_rows","flag_overlap","flag_missing_records","flag_missing_dates",[m
[32m+[m[32m                              "flag_range_variable_to_set","flag_range_variable_new","flag_out_of_range",[m
[32m+[m[32m                              "flag_new_duplicates_rows","flag_new_overlap","flag_new_missing_dates", "flag_missing_records_new",[m[41m   [m
[32m+[m[32m                              "Report_link", "Data_folder", "File_name")[m
[32m+[m
[32m+[m[32mreport_dataframe = matrix(ncol = 15, nrow = length(files_available_project))[m
[32m+[m[32mcolnames(report_dataframe) = c("Station",[m
[32m+[m[32m                               "Offline",[m
[32m+[m[32m                               "err_empty","err_logger_number","err_structure","err_no_new_data","err_overlap","err_missing_record","err_restart_record",[m
[32m+[m[32m                               "err_date_missing","err_range_alert",[m
[32m+[m[32m                               "err_out_of_range","err_duplicates_rows",[m
[32m+[m[32m                               "var_flagged",[m
[32m+[m[32m                               "report_link")[m
[32m+[m
[32m+[m
[32m+[m
[32m+[m[32m# report_start = Sys.time()[m
[32m+[m
[32m+[m[32mt = 1[m
[32m+[m
[32m+[m[32mfor(t in  1: length(files_available_project)){[m
[32m+[m[32m  gc(reset = T)[m
   [m
[31m-  report_dataframe = matrix(ncol = 15, nrow = length(files_available_project))[m
[31m-  colnames(report_dataframe) = c("Station",[m
[31m-                                 "Offline",[m
[31m-                                 "err_empty","err_logger_number","err_structure","err_no_new_data","err_overlap","err_missing_record","err_restart_record",[m
[31m-                                 "err_date_missing","err_range_alert",[m
[31m-                                 "err_out_of_range","err_duplicates_rows",[m
[31m-                                 "var_flagged",[m
[31m-                                 "report_link")[m
[32m+[m[32m  FILE_NAME = files_available_project[t][m
   [m
[32m+[m[32m  u1 = gregexpr(FILE_NAME,pattern = "_")[[1]][1]      # <- here we find the first "[[1]][1]" underscore!!!!![m
[32m+[m[32m  u2 = gregexpr(FILE_NAME,pattern = "_")[[1]][2]      # <- here we find the second "[[1]][2]" underscore!!!!![m
   [m
   [m
[31m-  # report_start = Sys.time()[m
[32m+[m[32m  STATION_NAME = substring(FILE_NAME,u1+1, u2-1)[m
   [m
[31m-  t = 1[m
[32m+[m[32m  w_dwnl = which(download_table$Station == substring(FILE_NAME, 1, nchar(FILE_NAME) - 4))[m
[32m+[m[32m  dwnl_info = download_table[w_dwnl,][m
   [m
[31m-  for(t in  1: length(files_available_project)){[m
[31m-    gc(reset = T)[m
[31m-    [m
[31m-    # rm(list = setdiff(ls(all.names = TRUE),c("date_DQC","main_dir","PROJECT","DQC_setting_dir","t","data_from_row","datetime_format","datetime_header","datetime_sampling","loggernet_status",[m
[31m-    #                                          "download_table","download_table_dir","issue_counter", "issue_counter_dir","issue_counter_proj",[m
[31m-    #                                          "files_available","files_available_project","header_row_number","input_dir","data_output_dir","output_dir_raw","report_output_dir","project_dir",[m
[31m-    #                                          "range_dir","range_file","record_header","Rmd_report_generator","write_output_files","write_output_report","flag_names",[m
[31m-    #                                          "report_start", "final_dataframe","output_dir_report", "database_file_dir","logger_info_file","MESSAGE_EVERY_TIMES","issue_flags_dir",[m
[31m-    #                                          "warning_file_dir","warning_report_RMD","mail_config","mail_config_file","mail_config_info","mail_file","HOURS_OFFLINE","LOGGERNET_OFFLINE",[m
[31m-    #                                          "sender", "reciver" ,"my_smtp","loggernet_status_prj","loggernet_status","project_type",[m
[31m-    #                                          "report_info", "report_dataframe","use_alert_station_flag","mail_dir","url_webservice","mail_file_alert","use_realtime_station_flag")))[m
[31m-    # [m
[31m-    [m
[31m-    [m
[31m-    FILE_NAME = files_available_project[t][m
[31m-    [m
[31m-    u1 = gregexpr(FILE_NAME,pattern = "_")[[1]][1]      # <- here we find the first "[[1]][1]" underscore!!!!![m
[31m-    u2 = gregexpr(FILE_NAME,pattern = "_")[[1]][2]      # <- here we find the second "[[1]][2]" underscore!!!!![m
[31m-    [m
[31m-    # if(PROJECT == "MONALISA"){[m
[31m-    #   STATION_NAME = substring(FILE_NAME,u1+1, u1+9)[m
[31m-    # }else{[m
[31m-    #   STATION_NAME = substring(FILE_NAME,u1+1, u2-1)[m
[31m-    # }[m
[31m-    [m
[31m-    STATION_NAME = substring(FILE_NAME,u1+1, u2-1)[m
[31m-    [m
[31m-    w_dwnl = which(download_table$Station == substring(FILE_NAME, 1, nchar(FILE_NAME) - 4))[m
[31m-    dwnl_info = download_table[w_dwnl,][m
[31m-    [m
[31m-    if(dir.exists(paste(data_output_dir,STATION_NAME,"/", sep = ""))){                # create subfolder to store data organized by station name[m
[31m-      if(dir.exists(paste(data_output_dir,STATION_NAME,"/Total/", sep = ""))){[m
[31m-        output_dir_data_new = paste(data_output_dir,STATION_NAME,"/Total/", sep = "")[m
[31m-        output_dir_raw_new = paste(data_output_dir,STATION_NAME,"/Raw/", sep = "")[m
[31m-        output_dir_report_new = paste(data_output_dir,STATION_NAME,"/Alerts/Reports/", sep = "")[m
[31m-        warning_file_dir_station = paste(data_output_dir,STATION_NAME,"/Alerts/Warnings/", sep = "")[m
[31m-      }else{[m
[31m-        dir.create(paste(data_output_dir,STATION_NAME,"/Alerts/", sep = ""))[m
[31m-        dir.create(paste(data_output_dir,STATION_NAME,"/Alerts/Reports/", sep = ""))[m
[31m-        dir.create(paste(data_output_dir,STATION_NAME,"/Alerts/Warnings/", sep = ""))[m
[31m-        dir.create(paste(data_output_dir,STATION_NAME,"/Raw/", sep = ""))[m
[31m-        dir.create(paste(data_output_dir,STATION_NAME,"/Total/", sep = ""))[m
[31m-        dir.create(paste(data_output_dir,STATION_NAME,"/Processed/", sep = ""))[m
[31m-        dir.create(paste(data_output_dir,STATION_NAME,"/Pics/", sep = ""))[m
[31m-        output_dir_data_new = paste(data_output_dir,STATION_NAME,"/Total/", sep = "")[m
[31m-        output_dir_raw_new = paste(data_output_dir,STATION_NAME,"/Raw/", sep = "")[m
[31m-        output_dir_report_new = paste(data_output_dir,STATION_NAME,"/Alerts/Reports/", sep = "")[m
[31m-        warning_file_dir_station = paste(data_output_dir,STATION_NAME,"/Alerts/Warnings/", sep = "")[m
[31m-        [m
[31m-      }[m
[32m+[m[32m  if(dir.exists(paste(data_output_dir,STATION_NAME,"/", sep = ""))){                # create subfolder to store data organized by station name[m
[32m+[m[32m    if(dir.exists(paste(data_output_dir,STATION_NAME,"/Total/", sep = ""))){[m
[32m+[m[32m      output_dir_data_new = paste(data_output_dir,STATION_NAME,"/Total/", sep = "")[m
[32m+[m[32m      output_dir_raw_new = paste(data_output_dir,STATION_NAME,"/Raw/", sep = "")[m
[32m+[m[32m      output_dir_report_new = paste(data_output_dir,STATION_NAME,"/Alerts/Reports/", sep = "")[m
[32m+[m[32m      warning_file_dir_station = paste(data_output_dir,STATION_NAME,"/Alerts/Warnings/", sep = "")[m
     }else{[m
[31m-      dir.create(paste(data_output_dir,STATION_NAME,"/", sep = ""))      [m
       dir.create(paste(data_output_dir,STATION_NAME,"/Alerts/", sep = ""))[m
       dir.create(paste(data_output_dir,STATION_NAME,"/Alerts/Reports/", sep = ""))[m
       dir.create(paste(data_output_dir,STATION_NAME,"/Alerts/Warnings/", sep = ""))[m
[36m@@ -323,344 +252,334 @@[m [mfor(PROJECT in project_type){[m
       warning_file_dir_station = paste(data_output_dir,STATION_NAME,"/Alerts/Warnings/", sep = "")[m
       [m
     }[m
[32m+[m[32m  }else{[m
[32m+[m[32m    dir.create(paste(data_output_dir,STATION_NAME,"/", sep = ""))[m[41m      [m
[32m+[m[32m    dir.create(paste(data_output_dir,STATION_NAME,"/Alerts/", sep = ""))[m
[32m+[m[32m    dir.create(paste(data_output_dir,STATION_NAME,"/Alerts/Reports/", sep = ""))[m
[32m+[m[32m    dir.create(paste(data_output_dir,STATION_NAME,"/Alerts/Warnings/", sep = ""))[m
[32m+[m[32m    dir.create(paste(data_output_dir,STATION_NAME,"/Raw/", sep = ""))[m
[32m+[m[32m    dir.create(paste(data_output_dir,STATION_NAME,"/Total/", sep = ""))[m
[32m+[m[32m    dir.create(paste(data_output_dir,STATION_NAME,"/Processed/", sep = ""))[m
[32m+[m[32m    dir.create(paste(data_output_dir,STATION_NAME,"/Pics/", sep = ""))[m
[32m+[m[32m    output_dir_data_new = paste(data_output_dir,STATION_NAME,"/Total/", sep = "")[m
[32m+[m[32m    output_dir_raw_new = paste(data_output_dir,STATION_NAME,"/Raw/", sep = "")[m
[32m+[m[32m    output_dir_report_new = paste(data_output_dir,STATION_NAME,"/Alerts/Reports/", sep = "")[m
[32m+[m[32m    warning_file_dir_station = paste(data_output_dir,STATION_NAME,"/Alerts/Warnings/", sep = "")[m
     [m
[31m-    if(dir.exists(paste(database_file_dir,STATION_NAME,"/", sep = ""))){                # create subfolder to store mini files for database organized by station name [m
[31m-      if(dir.exists(paste(database_file_dir,STATION_NAME,"/Data/", sep = ""))){ [m
[31m-        database_file_dir_new = paste(database_file_dir,STATION_NAME,"/Data/", sep = "")[m
[31m-      }else{[m
[31m-        dir.create(paste(database_file_dir,STATION_NAME,"/Data/", sep = ""))[m
[31m-        dir.create(paste(database_file_dir,STATION_NAME,"/Pics/", sep = ""))[m
[31m-        database_file_dir_new = paste(database_file_dir,STATION_NAME,"/Data/", sep = "")[m
[31m-        [m
[31m-      }[m
[31m-    }else{[m
[31m-      dir.create(paste(database_file_dir,STATION_NAME,"/", sep = "")) [m
[31m-      dir.create(paste(database_file_dir,STATION_NAME,"/Data/", sep = ""))[m
[31m-      dir.create(paste(database_file_dir,STATION_NAME,"/Pics/", sep = ""))[m
[31m-      database_file_dir_new = paste(database_file_dir,STATION_NAME,"/Data/", sep = "")[m
[31m-    }[m
[32m+[m[32m  }[m
[32m+[m[32m  #[m[41m [m
[32m+[m[32m  # if(dir.exists(paste(database_file_dir,STATION_NAME,"/", sep = ""))){                # create subfolder to store mini files for database organized by station name[m[41m [m
[32m+[m[32m  #   if(dir.exists(paste(database_file_dir,STATION_NAME,"/Data/", sep = ""))){[m[41m [m
[32m+[m[32m  #     database_file_dir_new = paste(database_file_dir,STATION_NAME,"/Data/", sep = "")[m
[32m+[m[32m  #   }else{[m
[32m+[m[32m  #     dir.create(paste(database_file_dir,STATION_NAME,"/Data/", sep = ""))[m
[32m+[m[32m  #     dir.create(paste(database_file_dir,STATION_NAME,"/Pics/", sep = ""))[m
[32m+[m[32m  #     database_file_dir_new = paste(database_file_dir,STATION_NAME,"/Data/", sep = "")[m
[32m+[m[32m  #[m[41m     [m
[32m+[m[32m  #   }[m
[32m+[m[32m  # }else{[m
[32m+[m[32m  #   dir.create(paste(database_file_dir,STATION_NAME,"/", sep = ""))[m[41m [m
[32m+[m[32m  #   dir.create(paste(database_file_dir,STATION_NAME,"/Data/", sep = ""))[m
[32m+[m[32m  #   dir.create(paste(database_file_dir,STATION_NAME,"/Pics/", sep = ""))[m
[32m+[m[32m  #   database_file_dir_new = paste(database_file_dir,STATION_NAME,"/Data/", sep = "")[m
[32m+[m[32m  # }[m
[32m+[m[41m  [m
[32m+[m[41m  [m
[32m+[m[41m  [m
[32m+[m[32m  if(dwnl_info$Stop_DQC == 0){[m
     [m
[32m+[m[32m    date_last_modif_file = as.character(format(file.mtime(paste(input_dir,FILE_NAME,sep = "")),format = datetime_format))[m
     [m
[32m+[m[32m    # ----- station offline  ------[m
     [m
[31m-    if(dwnl_info$Stop_DQC == 0){[m
[31m-      [m
[31m-      date_last_modif_file = as.character(format(file.mtime(paste(input_dir,FILE_NAME,sep = "")),format = datetime_format))[m
[31m-      [m
[31m-      # ----- station offline  ------[m
[32m+[m[32m    h_last_modif_file = trunc(as.POSIXct(date_last_modif_file, tz = "Etc/GMT-1"),units = "hours")[m
[32m+[m[32m    h_DQC = trunc(date_DQC,units = "hours")[m
[32m+[m[41m    [m
[32m+[m[32m    hours_diff = as.numeric(difftime(time1 = h_DQC, time2 = h_last_modif_file, tz = "Etc/GMT-1",units = "hours"))[m
[32m+[m[41m    [m
[32m+[m[41m    [m
[32m+[m[32m    if(date_last_modif_file != dwnl_info$Last_Modification | is.na(dwnl_info$Last_Modification)){[m
       [m
[31m-      h_last_modif_file = trunc(as.POSIXct(date_last_modif_file, tz = "Etc/GMT-1"),units = "hours")[m
[31m-      h_DQC = trunc(date_DQC,units = "hours")[m
[32m+[m[32m      input_dir = input_dir[m
[32m+[m[32m      output_dir_data = output_dir_data_new[m
[32m+[m[32m      output_dir_raw = output_dir_raw_new[m
[32m+[m[32m      output_dir_report = report_output_dir[m
[32m+[m[32m      project_dir = project_dir[m
[32m+[m[32m      data_from_row = data_from_row[m
[32m+[m[32m      header_row_number = header_row_number[m
[32m+[m[32m      datetime_header = datetime_header[m
[32m+[m[32m      datetime_format = datetime_format[m
[32m+[m[32m      datetime_sampling = datetime_sampling[m
[32m+[m[32m      record_header = record_header[m
[32m+[m[32m      range_file = range_file[m
[32m+[m[32m      write_output_files = write_output_files[m
[32m+[m[32m      write_output_report = write_output_report[m
[32m+[m[32m      # database_dir = database_file_dir_new[m
[32m+[m[32m      file_name = FILE_NAME[m
[32m+[m[32m      station_name = STATION_NAME[m
[32m+[m[32m      start_date = dwnl_info$Last_date[m
[32m+[m[32m      logger_info_file = logger_info_file[m
[32m+[m[32m      record_check = dwnl_info$record_check[m
[32m+[m[32m      use_alert_station_flag = use_alert_station_flag[m
[32m+[m[32m      mail_file_alert = mail_file_alert[m
[32m+[m[32m      use_realtime_station_flag = use_realtime_station_flag[m
[32m+[m[41m     [m
[32m+[m[32m      DQC_results = DQC_function(input_dir,[m
[32m+[m[32m                                 output_dir_data,[m
[32m+[m[32m                                 output_dir_report,[m
[32m+[m[32m                                 project_dir,[m
[32m+[m[32m                                 data_from_row,[m
[32m+[m[32m                                 header_row_number,[m
[32m+[m[32m                                 datetime_header,[m
[32m+[m[32m                                 datetime_format,[m
[32m+[m[32m                                 datetime_sampling,[m
[32m+[m[32m                                 record_header,[m
[32m+[m[32m                                 range_file,[m
[32m+[m[32m                                 write_output_files,[m
[32m+[m[32m                                 write_output_report,[m
[32m+[m[32m                                 file_name,[m
[32m+[m[32m                                 station_name,[m
[32m+[m[32m                                 start_date,[m
[32m+[m[32m                                 # database_dir,[m
[32m+[m[32m                                 logger_info_file,[m
[32m+[m[32m                                 record_check,[m
[32m+[m[32m                                 output_dir_raw,[m
[32m+[m[32m                                 use_alert_station_flag,[m
[32m+[m[32m                                 mail_file_alert,[m
[32m+[m[32m                                 use_realtime_station_flag)[m
       [m
[31m-      hours_diff = as.numeric(difftime(time1 = h_DQC, time2 = h_last_modif_file, tz = "Etc/GMT-1",units = "hours"))[m
[32m+[m[32m      mydata = DQC_results[[1]][m
[32m+[m[32m      flags_df = DQC_results[[2]][m
[32m+[m[32m      file_names = DQC_results[[3]][m
[32m+[m[32m      errors = DQC_results[[4]][m
[32m+[m[32m      mydata_out_of_range = DQC_results[[5]][m
       [m
[32m+[m[32m      mylist <- split(flags_df$value, seq(nrow(flags_df)))[m
[32m+[m[32m      names(mylist) = flags_df$flag_names[m
       [m
[31m-      if(date_last_modif_file != dwnl_info$Last_Modification | is.na(dwnl_info$Last_Modification)){[m
[31m-        [m
[31m-        input_dir = input_dir[m
[31m-        output_dir_data = output_dir_data_new[m
[31m-        output_dir_raw = output_dir_raw_new[m
[31m-        output_dir_report = report_output_dir[m
[31m-        project_dir = project_dir[m
[31m-        data_from_row = data_from_row[m
[31m-        header_row_number = header_row_number[m
[31m-        datetime_header = datetime_header[m
[31m-        datetime_format = datetime_format[m
[31m-        datetime_sampling = datetime_sampling[m
[31m-        record_header = record_header[m
[31m-        range_file = range_file[m
[31m-        write_output_files = write_output_files[m
[31m-        write_output_report = write_output_report[m
[31m-        database_dir = database_file_dir_new[m
[31m-        file_name = FILE_NAME[m
[31m-        station_name = STATION_NAME[m
[31m-        start_date = dwnl_info$Last_date[m
[31m-        logger_info_file = logger_info_file[m
[31m-        record_check = dwnl_info$record_check[m
[31m-        use_alert_station_flag = use_alert_station_flag[m
[31m-        mail_file_alert = mail_file_alert[m
[31m-        use_realtime_station_flag = use_realtime_station_flag[m
[31m-        # issue_flags_file = paste(issue_flags_dir,"/",STATION_NAME,".csv",sep = "")[m
[32m+[m[32m      if(mylist$flag_empty == 0 & mylist$flag_logger_number == 0 & mylist$flag_error_df == 0 & mylist$flag_date == 0){[m
[32m+[m[32m        out_filename_date = paste(substring(mydata[nrow(mydata),which(colnames(mydata) == datetime_header)],1,4),[m
[32m+[m[32m                                  substring(mydata[nrow(mydata),which(colnames(mydata) == datetime_header)],6,7),[m
[32m+[m[32m                                  substring(mydata[nrow(mydata),which(colnames(mydata) == datetime_header)],9,10),[m
[32m+[m[32m                                  # "_",[m
[32m+[m[32m                                  substring(mydata[nrow(mydata),which(colnames(mydata) == datetime_header)],12,13),[m
[32m+[m[32m                                  substring(mydata[nrow(mydata),which(colnames(mydata) == datetime_header)],15,16),[m
[32m+[m[32m                                  sep = "")[m
         [m
[31m-        # output_file_report = paste("DQC_Report_",STATION_NAME,"_tmp.html",sep = "")[m
[32m+[m[32m        last_date = mydata[nrow(mydata),which(colnames(mydata)== datetime_header)][m
         [m
[31m-        # rm(dwnl_info)[m
[31m-        # DQC_results = DQC_function(input_dir,[m
[31m-        DQC_results = DQC_function(input_dir,[m
[31m-                                   output_dir_data,[m
[31m-                                   output_dir_report,[m
[31m-                                   project_dir,[m
[31m-                                   data_from_row,[m
[31m-                                   header_row_number,[m
[31m-                                   datetime_header,[m
[31m-                                   datetime_format,[m
[31m-                                   datetime_sampling,[m
[31m-                                   record_header,[m
[31m-                                   range_file,[m
[31m-                                   write_output_files,[m
[31m-                                   write_output_report,[m
[31m-                                   file_name,[m
[31m-                                   station_name,[m
[31m-                                   start_date,[m
[31m-                                   database_dir,[m
[31m-                                   logger_info_file,[m
[31m-                                   record_check,[m
[31m-                                   output_dir_raw,[m
[31m-                                   use_alert_station_flag,[m
[31m-                                   mail_file_alert,[m
[31m-                                   use_realtime_station_flag)[m
[31m-        [m
[31m-        mydata = DQC_results[[1]][m
[31m-        flags_df = DQC_results[[2]][m
[31m-        file_names = DQC_results[[3]][m
[31m-        errors = DQC_results[[4]][m
[31m-        mydata_out_of_range = DQC_results[[5]][m
[31m-        [m
[31m-        mylist <- split(flags_df$value, seq(nrow(flags_df)))[m
[31m-        names(mylist) = flags_df$flag_names[m
[32m+[m[32m      } else {[m
[32m+[m[32m        out_filename_date = "no_datetime"[m
[32m+[m[32m      }[m
[32m+[m[41m      [m
[32m+[m[41m      [m
[32m+[m[32m      # ~ ~ ~ ~ Issue Management (on/off message) ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~[m
[32m+[m[41m      [m
[32m+[m[32m      # date_DQC = as.POSIXct(format(Sys.time(),format = "%Y-%m-%d %H:%M"), tz = 'Etc/GMT-1')[m
[32m+[m[41m      [m
[32m+[m[32m      status = unlist(lapply(errors,function(x) x[[1]]))[m
[32m+[m[32m      data_errors = lapply(errors,function(x) x[[2]])[m
[32m+[m[32m      w_yes = which(status == "Y")[m
[32m+[m[41m      [m
[32m+[m[32m      critical_errors = c("err_empty","err_logger_number","err_structure","err_no_new_data","err_overlap","err_missing_record","err_restart_record","err_date_missing")[m
[32m+[m[32m      warning_errors = c("err_range_alert")[m
[32m+[m[32m      report_errors = c("err_out_of_range","err_duplicates_rows")[m
[32m+[m[41m      [m
[32m+[m[32m      dqc_date = date_DQC[m
[32m+[m[41m      [m
[32m+[m[32m      df_status = data.frame(STATION_NAME,t(status))[m
[32m+[m[41m      [m
[32m+[m[32m      if(use_alert_station_flag == TRUE){[m
[32m+[m[32m        range_flags = read.csv(paste(range_dir,range_file,sep = ""),stringsAsFactors = F)[m
[32m+[m[32m        range_station = range_flags[,c(1,which(colnames(range_flags) == STATION_NAME))][m
[32m+[m[32m        colnames(range_station)[2] = "Station"[m
[32m+[m[32m        variables_flagged = range_station$Variable[which(range_station$Station == 0)][m
         [m
[31m-        if(mylist$flag_empty == 0 & mylist$flag_logger_number == 0 & mylist$flag_error_df == 0 & mylist$flag_date == 0){[m
[31m-          out_filename_date = paste(substring(mydata[nrow(mydata),which(colnames(mydata) == datetime_header)],1,4),[m
[31m-                                    substring(mydata[nrow(mydata),which(colnames(mydata) == datetime_header)],6,7),[m
[31m-                                    substring(mydata[nrow(mydata),which(colnames(mydata) == datetime_header)],9,10),[m
[31m-                                    # "_",[m
[31m-                                    substring(mydata[nrow(mydata),which(colnames(mydata) == datetime_header)],12,13),[m
[31m-                                    substring(mydata[nrow(mydata),which(colnames(mydata) == datetime_header)],15,16),[m
[31m-                                    sep = "")[m
[31m-          [m
[31m-          last_date = mydata[nrow(mydata),which(colnames(mydata)== datetime_header)][m
[31m-          [m
[31m-        } else {[m
[31m-          out_filename_date = "no_datetime"[m
[32m+[m[32m        if(length(variables_flagged) == 0){[m
[32m+[m[32m          variables_flagged = NULL[m
         }[m
[32m+[m[32m      }else{[m
[32m+[m[32m        variables_flagged = NULL[m
[32m+[m[32m      }[m
[32m+[m[41m      [m
[32m+[m[32m      if(any(status[-which(names(status) == "err_duplicates_rows")] == "Y")){[m
         [m
[32m+[m[32m        station_name = STATION_NAME[m
[32m+[m[32m        errors_list_critical = errors[critical_errors][m
[32m+[m[32m        errors_list_warning = errors[warning_errors][m
[32m+[m[32m        errors_list_report_errors = errors[report_errors][m
         [m
[31m-        # ~ ~ ~ ~ Issue Management (on/off message) ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~[m
[31m-        [m
[31m-        # date_DQC = as.POSIXct(format(Sys.time(),format = "%Y-%m-%d %H:%M"), tz = 'Etc/GMT-1')[m
[31m-        [m
[31m-        status = unlist(lapply(errors,function(x) x[[1]]))[m
[31m-        data_errors = lapply(errors,function(x) x[[2]])[m
[31m-        w_yes = which(status == "Y")[m
[31m-        [m
[31m-        critical_errors = c("err_empty","err_logger_number","err_structure","err_no_new_data","err_overlap","err_missing_record","err_restart_record","err_date_missing")[m
[31m-        warning_errors = c("err_range_alert")[m
[31m-        report_errors = c("err_out_of_range","err_duplicates_rows")[m
[32m+[m[32m        dqc_date_write = paste(format(dqc_date,"%Y"),format(dqc_date,"%m"),format(dqc_date,"%d"),sep = "")[m
         [m
[31m-        dqc_date = date_DQC[m
         [m
[31m-        df_status = data.frame(STATION_NAME,t(status))[m
[32m+[m[32m        # generate a report of warnings[m
         [m
[31m-        if(use_alert_station_flag == TRUE){[m
[31m-          range_flags = read.csv(paste(range_dir,range_file,sep = ""),stringsAsFactors = F)[m
[31m-          range_station = range_flags[,c(1,which(colnames(range_flags) == STATION_NAME))][m
[31m-          colnames(range_station)[2] = "Station"[m
[31m-          variables_flagged = range_station$Variable[which(range_station$Station == 0)][m
[31m-          [m
[31m-          if(length(variables_flagged) == 0){[m
[31m-            variables_flagged = NULL[m
[31m-          }[m
[31m-        }else{[m
[31m-          variables_flagged = NULL[m
[31m-        }[m
[31m-        [m
[31m-        if(any(status[-which(names(status) == "err_duplicates_rows")] == "Y")){[m
[31m-          [m
[31m-          station_name = STATION_NAME[m
[31m-          errors_list_critical = errors[critical_errors][m
[31m-          errors_list_warning = errors[warning_errors][m
[31m-          errors_list_report_errors = errors[report_errors][m
[31m-          [m
[31m-          dqc_date_write = paste(format(dqc_date,"%Y"),format(dqc_date,"%m"),format(dqc_date,"%d"),sep = "")[m
[31m-          [m
[31m-          [m
[31m-          # generate a report of warnings[m
[31m-          [m
[31m-          output_file_report = paste(STATION_NAME,"_",dqc_date_write,".html",sep = "")[m
[31m-          [m
[31m-          issue_report_RMD = paste(project_dir,"/Rmd/DQC_Reports.Rmd",sep = "")[m
[31m-          [m
[31m-          issue_file_dir_station = output_dir_report_new[m
[31m-          [m
[31m-          input =  issue_report_RMD [m
[31m-          output_file = output_file_report[m
[31m-          output_dir = issue_file_dir_station[m
[31m-          [m
[31m-          if(!is.null(mydata_out_of_range)){[m
[31m-            report_mydata = mydata_out_of_range[m
[31m-            report_mydata[,which(colnames(report_mydata) == datetime_header)] = as.POSIXct(report_mydata[,which(colnames(report_mydata) == datetime_header)] ,tz = "Etc/GMT-1")[m
[31m-          }else{[m
[31m-            report_mydata = NULL[m
[31m-          }[m
[31m-          [m
[31m-          # if(use_alert_station_flag == TRUE){[m
[31m-          #   range_flags = read.csv(paste(range_dir,range_file,sep = ""),stringsAsFactors = F)[m
[31m-          #   range_station = range_flags[,c(1,which(colnames(range_flags) == STATION_NAME))][m
[31m-          #   colnames(range_station)[2] = "Station"[m
[31m-          #   variables_flagged = range_station$Variable[which(range_station$Station == 0)][m
[31m-          #   [m
[31m-          #   if(length(variables_flagged) == 0){[m
[31m-          #     variables_flagged = NULL[m
[31m-          #     }[m
[31m-          # }else{[m
[31m-          #   variables_flagged = NULL[m
[31m-          # }[m
[31m-          [m
[31m-          [m
[31m-          params_list = list(report_mydata,[m
[31m-                             dqc_date,[m
[31m-                             station_name,[m
[31m-                             errors_list_critical,[m
[31m-                             errors_list_warning,[m
[31m-                             errors_list_report_errors,[m
[31m-                             variables_flagged)[m
[31m-          names(params_list) = c("report_mydata", "dqc_date","station_name","errors_list_critical","errors_list_warning","errors_list_report_errors","variables_flagged")[m
[31m-          [m
[31m-          gc(reset = T)[m
[31m-          rmarkdown::render(input = input,[m
[31m-                            output_file = output_file,[m
[31m-                            output_dir = output_dir,[m
[31m-                            params = params_list)[m
[31m-        }[m
[32m+[m[32m        output_file_report = paste(STATION_NAME,"_",dqc_date_write,".html",sep = "")[m
         [m
[31m-        # Report su script esterno! Nella funzione DQC_Function prevedere il salvataggio e l' append degli errori![m
[32m+[m[32m        issue_report_RMD = paste(project_dir,"/Rmd/DQC_Reports.Rmd",sep = "")[m
         [m
[31m-        status_final = status[m
[31m-        status_final[which(status_final == "Y")] = 1[m
[31m-        status_final[which(status_final == "N")] = 0[m
[31m-        status_final = status_final[c(critical_errors,warning_errors, report_errors)][m
[32m+[m[32m        issue_file_dir_station = output_dir_report_new[m
         [m
[32m+[m[32m        input =  issue_report_RMD[m[41m [m
[32m+[m[32m        output_file = output_file_report[m
[32m+[m[32m        output_dir = issue_file_dir_station[m
         [m
[31m-        if(any(status[-which(names(status) == "err_duplicates_rows")] == "Y")){[m
[31m-          # paste(substring(output_dir,nchar(main_dir)),output_file,sep = "")[m
[31m-          # link = paste(main_dir_mapping_out, substring(output_dir_report_new,nchar(main_dir_mapping_in)), output_file_report,sep = "")[m
[31m-          link = paste("/",PROJECT,substring(output_dir_report_new,nchar(data_output_dir)), output_file_report,sep = "")[m
[31m-          # link = paste("\\\\smb.scientificnet.org\\alpenv", substring(output_dir_report_new,nchar('/shared/')), output_file_report,sep = "")[m
[32m+[m[32m        if(!is.null(mydata_out_of_range)){[m
[32m+[m[32m          report_mydata = mydata_out_of_range[m
[32m+[m[32m          report_mydata[,which(colnames(report_mydata) == datetime_header)] = as.POSIXct(report_mydata[,which(colnames(report_mydata) == datetime_header)] ,tz = "Etc/GMT-1")[m
         }else{[m
[31m-          link = NA[m
[31m-          # link = "---"[m
[32m+[m[32m          report_mydata = NULL[m
         }[m
         [m
[31m-        if(length(variables_flagged) == 0){[m
[31m-          var_flagged = 0[m
[31m-        }else{[m
[31m-          var_flagged = 1[m
[31m-        }[m
[32m+[m[32m        # if(use_alert_station_flag == TRUE){[m
[32m+[m[32m        #   range_flags = read.csv(paste(range_dir,range_file,sep = ""),stringsAsFactors = F)[m
[32m+[m[32m        #   range_station = range_flags[,c(1,which(colnames(range_flags) == STATION_NAME))][m
[32m+[m[32m        #   colnames(range_station)[2] = "Station"[m
[32m+[m[32m        #   variables_flagged = range_station$Variable[which(range_station$Station == 0)][m
[32m+[m[32m        #[m[41m   [m
[32m+[m[32m        #   if(length(variables_flagged) == 0){[m
[32m+[m[32m        #     variables_flagged = NULL[m
[32m+[m[32m        #     }[m
[32m+[m[32m        # }else{[m
[32m+[m[32m        #   variables_flagged = NULL[m
[32m+[m[32m        # }[m
         [m
[31m-        report_info = c(STATION_NAME,0,status_final,var_flagged, link)[m
[31m-        names(report_info) = c("Station",[m
[31m-                               "Offline",[m
[31m-                               "err_empty","err_logger_number","err_structure","err_no_new_data","err_overlap","err_missing_record","err_restart_record",[m
[31m-                               "err_date_missing","err_range_alert",[m
[31m-                               "err_out_of_range","err_duplicates_rows",[m
[31m-                               "var_flagged",[m
[31m-                               "report_link")[m
         [m
[31m-        # ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~[m
[32m+[m[32m        params_list = list(report_mydata,[m
[32m+[m[32m                           dqc_date,[m
[32m+[m[32m                           station_name,[m
[32m+[m[32m                           errors_list_critical,[m
[32m+[m[32m                           errors_list_warning,[m
[32m+[m[32m                           errors_list_report_errors,[m
[32m+[m[32m                           variables_flagged)[m
[32m+[m[32m        names(params_list) = c("report_mydata", "dqc_date","station_name","errors_list_critical","errors_list_warning","errors_list_report_errors","variables_flagged")[m
         [m
[31m-        if(!is.na(mylist$flag_missing_dates)){[m
[31m-          if(mylist$flag_logger_number == 0){[m
[31m-            if(mylist$flag_new_overlap == 1){[m
[31m-              if(write_output_report == TRUE){[m
[31m-                final_info = c(STATION_NAME, "Analyzed and write output",[m
[31m-                               flags_df$value,[m
[31m-                               paste(output_dir_report,out_filename_report,sep = ""),[m
[31m-                               NA,[m
[31m-                               NA)[m
[31m-              }else{[m
[31m-                final_info = c(STATION_NAME, "Analyzed and write output",[m
[31m-                               flags_df$value,[m
[31m-                               NA,[m
[31m-                               NA,[m
[31m-                               NA)[m
[31m-              }[m
[32m+[m[32m        gc(reset = T)[m
[32m+[m[32m        rmarkdown::render(input = input,[m
[32m+[m[32m                          output_file = output_file,[m
[32m+[m[32m                          output_dir = output_dir,[m
[32m+[m[32m                          params = params_list)[m
[32m+[m[32m      }[m
[32m+[m[41m      [m
[32m+[m[32m      # Report su script esterno! Nella funzione DQC_Function prevedere il salvataggio e l' append degli errori![m
[32m+[m[41m      [m
[32m+[m[32m      status_final = status[m
[32m+[m[32m      status_final[which(status_final == "Y")] = 1[m
[32m+[m[32m      status_final[which(status_final == "N")] = 0[m
[32m+[m[32m      status_final = status_final[c(critical_errors,warning_errors, report_errors)][m
[32m+[m[41m      [m
[32m+[m[41m      [m
[32m+[m[32m      if(any(status[-which(names(status) == "err_duplicates_rows")] == "Y")){[m
[32m+[m[32m        # paste(substring(output_dir,nchar(main_dir)),output_file,sep = "")[m
[32m+[m[32m        # link = paste(main_dir_mapping_out, substring(output_dir_report_new,nchar(main_dir_mapping_in)), output_file_report,sep = "")[m
[32m+[m[32m        link = paste("/",PROJECT,substring(output_dir_report_new,nchar(data_output_dir)), output_file_report,sep = "")[m
[32m+[m[32m        # link = paste("\\\\smb.scientificnet.org\\alpenv", substring(output_dir_report_new,nchar('/shared/')), output_file_report,sep = "")[m
[32m+[m[32m      }else{[m
[32m+[m[32m        link = NA[m
[32m+[m[32m        # link = "---"[m
[32m+[m[32m      }[m
[32m+[m[41m      [m
[32m+[m[32m      if(length(variables_flagged) == 0){[m
[32m+[m[32m        var_flagged = 0[m
[32m+[m[32m      }else{[m
[32m+[m[32m        var_flagged = 1[m
[32m+[m[32m      }[m
[32m+[m[41m      [m
[32m+[m[32m      report_info = c(STATION_NAME,0,status_final,var_flagged, link)[m
[32m+[m[32m      names(report_info) = c("Station",[m
[32m+[m[32m                             "Offline",[m
[32m+[m[32m                             "err_empty","err_logger_number","err_structure","err_no_new_data","err_overlap","err_missing_record","err_restart_record",[m
[32m+[m[32m                             "err_date_missing","err_range_alert",[m
[32m+[m[32m                             "err_out_of_range","err_duplicates_rows",[m
[32m+[m[32m                             "var_flagged",[m
[32m+[m[32m                             "report_link")[m
[32m+[m[41m      [m
[32m+[m[32m      # ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~[m
[32m+[m[41m      [m
[32m+[m[32m      if(!is.na(mylist$flag_missing_dates)){[m
[32m+[m[32m        if(mylist$flag_logger_number == 0){[m
[32m+[m[32m          if(mylist$flag_new_overlap == 1){[m
[32m+[m[32m            if(write_output_report == TRUE){[m
[32m+[m[32m              final_info = c(STATION_NAME, "Analyzed and write output",[m
[32m+[m[32m                             flags_df$value,[m
[32m+[m[32m                             paste(output_dir_report,out_filename_report,sep = ""),[m
[32m+[m[32m                             NA,[m
[32m+[m[32m                             NA)[m
             }else{[m
[31m-              download_table$Last_date[w_dwnl] = last_date[m
[31m-              download_table$Last_Modification[w_dwnl] = date_last_modif_file[m
[31m-              [m
[31m-              # if(download_table$record_check[w_dwnl] == 0){[m
[31m-              #   download_table$record_check[w_dwnl] = 1[m
[31m-              # }[m
[31m-              [m
[31m-              download_table$record_check[w_dwnl] = 1    # NEW! Record check activated every time![m
[31m-              write.csv(download_table,paste(download_table_dir,"download_table.csv",sep = ""),quote = F,row.names = F)[m
[31m-              [m
[31m-              if(write_output_report == TRUE){[m
[31m-                final_info = c(STATION_NAME, "Analyzed and write output",[m
[31m-                               flags_df$value,[m
[31m-                               paste(output_dir_report,out_filename_report,sep = ""),[m
[31m-                               paste(output_dir_data,sep = ""),[m
[31m-                               paste(file_names[length(file_names)],sep = ""))[m
[31m-              }else{[m
[31m-                final_info = c(STATION_NAME, "Analyzed and write output",[m
[31m-                               flags_df$value,[m
[31m-                               NA,[m
[31m-                               paste(output_dir_data_new,sep = ""),[m
[31m-                               paste(file_names[length(file_names)],sep = ""))[m
[31m-              }[m
[32m+[m[32m              final_info = c(STATION_NAME, "Analyzed and write output",[m
[32m+[m[32m                             flags_df$value,[m
[32m+[m[32m                             NA,[m
[32m+[m[32m                             NA,[m
[32m+[m[32m                             NA)[m
             }[m
[31m-          }[m
[31m-        }else{[m
[31m-          [m
[31m-          [m
[31m-          [m
[31m-          # file_stopped = c(file_stopped, FILE)[m
[31m-          if(write_output_report == TRUE){[m
[31m-            final_info = c(STATION_NAME, "Analyzed with errors",[m
[31m-                           flags_df$value,[m
[31m-                           paste(output_dir_report,out_filename_report,sep = ""),[m
[31m-                           NA, NA )[m
           }else{[m
[31m-            final_info = c(STATION_NAME, "Analyzed with errors",[m
[31m-                           flags_df$value,[m
[31m-                           NA,[m
[31m-                           NA, NA )[m
[32m+[m[32m            download_table$Last_date[w_dwnl] = last_date[m
[32m+[m[32m            download_table$Last_Modification[w_dwnl] = date_last_modif_file[m
[32m+[m[41m            [m
[32m+[m[32m            # if(download_table$record_check[w_dwnl] == 0){[m
[32m+[m[32m            #   download_table$record_check[w_dwnl] = 1[m
[32m+[m[32m            # }[m
[32m+[m[41m            [m
[32m+[m[32m            download_table$record_check[w_dwnl] = 1    # NEW! Record check activated every time![m
[32m+[m[32m            write.csv(download_table,paste(download_table_dir,"download_table.csv",sep = ""),quote = F,row.names = F)[m
[32m+[m[41m            [m
[32m+[m[32m            if(write_output_report == TRUE){[m
[32m+[m[32m              final_info = c(STATION_NAME, "Analyzed and write output",[m
[32m+[m[32m                             flags_df$value,[m
[32m+[m[32m                             paste(output_dir_report,out_filename_report,sep = ""),[m
[32m+[m[32m                             paste(output_dir_data,sep = ""),[m
[32m+[m[32m                             paste(file_names[length(file_names)],sep = ""))[m
[32m+[m[32m            }else{[m
[32m+[m[32m              final_info = c(STATION_NAME, "Analyzed and write output",[m
[32m+[m[32m                             flags_df$value,[m
[32m+[m[32m                             NA,[m
[32m+[m[32m                             paste(output_dir_data_new,sep = ""),[m
[32m+[m[32m                             paste(file_names[length(file_names)],sep = ""))[m
[32m+[m[32m            }[m
           }[m
[31m-          [m
[31m-          [m
         }[m
[32m+[m[32m      }else{[m
         [m
[31m-        # reset counter if file is updated[m
[31m-        # w_1 = which(issue_counter$Station == substring(FILE_NAME, 1,nchar(FILE_NAME)-4)) [m
[31m-        # issue_counter$W_Update_station[w_1] = 0[m
[31m-        # write.csv(issue_counter, paste(issue_counter_dir,"issue_counter.csv",sep = ""),quote = F,row.names = F) [m
         [m
[31m-      } else {[m
         [m
[31m-        # ~~~~~~~[m
[31m-        # update counter if file is not update[m
[31m-        # [m
[31m-        # w_1 = which(issue_counter$Station == substring(FILE_NAME, 1,nchar(FILE_NAME)-4))[m
[31m-        # issue_counter$W_Update_station[w_1] = issue_counter$W_Update_station[w_1]+1[m
[31m-        # write.csv(issue_counter, paste(issue_counter_dir,"issue_counter.csv",sep = ""),quote = F,row.names = F) [m
[31m-        # [m
[31m-        # # send message[m
[31m-        # if(issue_counter$W_Update_station[w_1] %% MESSAGE_EVERY_TIMES == 0){[m
[31m-        #   text_W_Update_station = paste(FILE_NAME, "not updated since",dwnl_info$Last_Modification)[m
[31m-        #   warning(text_W_Update_station)[m
[31m-        # }[m
[31m-        # [m
[31m-        # ~~~~~~~[m
[31m-        report_info = c(STATION_NAME,1,rep(NA,11),NA, NA)[m
[31m-        names(report_info) = c("Station",[m
[31m-                               "Offline",[m
[31m-                               "err_empty","err_logger_number","err_structure","err_no_new_data","err_overlap","err_missing_record","err_restart_record",[m
[31m-                               "err_date_missing","err_range_alert",[m
[31m-                               "err_out_of_range","err_duplicates_rows",[m
[31m-                               "var_flagged",[m
[31m-                               "report_link")[m
[32m+[m[32m        # file_stopped = c(file_stopped, FILE)[m
[32m+[m[32m        if(write_output_report == TRUE){[m
[32m+[m[32m          final_info = c(STATION_NAME, "Analyzed with errors",[m
[32m+[m[32m                         flags_df$value,[m
[32m+[m[32m                         paste(output_dir_report,out_filename_report,sep = ""),[m
[32m+[m[32m                         NA, NA )[m
[32m+[m[32m        }else{[m
[32m+[m[32m          final_info = c(STATION_NAME, "Analyzed with errors",[m
[32m+[m[32m                         flags_df$value,[m
[32m+[m[32m                         NA,[m
[32m+[m[32m                         NA, NA )[m
[32m+[m[32m        }[m
         [m
[31m-        warning(paste(STATION_NAME, "already analyzed!"))[m
[31m-        # file_already_processed = c(file_already_processed,FILE)[m
[31m-        final_info = c(STATION_NAME, "Already analyzed",[m
[31m-                       NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA,[m
[31m-                       NA,[m
[31m-                       NA, NA)[m
[31m-        output_dir_report = report_output_dir[m
         [m
       }[m
       [m
[31m-    }else{[m
[31m-      report_info = c(STATION_NAME,2,rep(NA,11),NA, NA)[m
[32m+[m[32m      # reset counter if file is updated[m
[32m+[m[32m      # w_1 = which(issue_counter$Station == substring(FILE_NAME, 1,nchar(FILE_NAME)-4))[m[41m [m
[32m+[m[32m      # issue_counter$W_Update_station[w_1] = 0[m
[32m+[m[32m      # write.csv(issue_counter, paste(issue_counter_dir,"issue_counter.csv",sep = ""),quote = F,row.names = F)[m[41m [m
[32m+[m[41m      [m
[32m+[m[32m    } else {[m
[32m+[m[41m      [m
[32m+[m[32m      # ~~~~~~~[m
[32m+[m[32m      # update counter if file is not update[m
[32m+[m[32m      #[m[41m [m
[32m+[m[32m      # w_1 = which(issue_counter$Station == substring(FILE_NAME, 1,nchar(FILE_NAME)-4))[m
[32m+[m[32m      # issue_counter$W_Update_station[w_1] = issue_counter$W_Update_station[w_1]+1[m
[32m+[m[32m      # write.csv(issue_counter, paste(issue_counter_dir,"issue_counter.csv",sep = ""),quote = F,row.names = F)[m[41m [m
[32m+[m[32m      #[m[41m [m
[32m+[m[32m      # # send message[m
[32m+[m[32m      # if(issue_counter$W_Update_station[w_1] %% MESSAGE_EVERY_TIMES == 0){[m
[32m+[m[32m      #   text_W_Update_station = paste(FILE_NAME, "not updated since",dwnl_info$Last_Modification)[m
[32m+[m[32m      #   warning(text_W_Update_station)[m
[32m+[m[32m      # }[m
[32m+[m[32m      #[m[41m [m
[32m+[m[32m      # ~~~~~~~[m
[32m+[m[32m      report_info = c(STATION_NAME,1,rep(NA,11),NA, NA)[m
       names(report_info) = c("Station",[m
                              "Offline",[m
                              "err_empty","err_logger_number","err_structure","err_no_new_data","err_overlap","err_missing_record","err_restart_record",[m
[36m@@ -668,7 +587,10 @@[m [mfor(PROJECT in project_type){[m
                              "err_out_of_range","err_duplicates_rows",[m
                              "var_flagged",[m
                              "report_link")[m
[31m-      final_info = c(STATION_NAME, "Not analyzed",[m
[32m+[m[41m      [m
[32m+[m[32m      warning(paste(STATION_NAME, "already analyzed!"))[m
[32m+[m[32m      # file_already_processed = c(file_already_processed,FILE)[m
[32m+[m[32m      final_info = c(STATION_NAME, "Already analyzed",[m
                      NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA,[m
                      NA,[m
                      NA, NA)[m
[36m@@ -676,82 +598,85 @@[m [mfor(PROJECT in project_type){[m
       [m
     }[m
     [m
[31m-    # final_dataframe = rbind(final_dataframe,final_info)[m
[31m-    final_dataframe[t,] = final_info[m
[31m-    [m
[31m-    report_dataframe[t,] = report_info[m
[31m-    [m
[31m-    loggernet_status_prj[t,1] = final_info[1][m
[31m-    loggernet_status_prj[t,2] = final_info[2][m
[31m-    loggernet_status_prj[t,3] = date_last_modif_file[m
[31m-    [m
[31m-    [m
[31m-    gc(reset = T)[m
[31m-    [m
[31m-    # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~[m
[31m-    # INSERIRE QUI CHE ERRORE DARE AD ICINGA[m
[31m-    # if(exists(text_W_Update_station))[m
[31m-    # text_W_Update_station[m
[31m-    # text_W_Empty_file[m
[31m-    # text_W_Logger_number[m
[31m-    # text_W_structure[m
[31m-    # text_W_date_issue[m
[31m-    # text_W_overlap[m
[31m-    # text_W_missing_records[m
[31m-    # text_W_restart_records[m
[31m-    # text_W_date_missing[m
[31m-    [m
[31m-    # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~[m
[32m+[m[32m  }else{[m
[32m+[m[32m    report_info = c(STATION_NAME,2,rep(NA,11),NA, NA)[m
[32m+[m[32m    names(report_info) = c("Station",[m
[32m+[m[32m                           "Offline",[m
[32m+[m[32m                           "err_empty","err_logger_number","err_structure","err_no_new_data","err_overlap","err_missing_record","err_restart_record",[m
[32m+[m[32m                           "err_date_missing","err_range_alert",[m
[32m+[m[32m                           "err_out_of_range","err_duplicates_rows",[m
[32m+[m[32m                           "var_flagged",[m
[32m+[m[32m                           "report_link")[m
[32m+[m[32m    final_info = c(STATION_NAME, "Not analyzed",[m
[32m+[m[32m                   NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA,[m
[32m+[m[32m                   NA,[m
[32m+[m[32m                   NA, NA)[m
[32m+[m[32m    output_dir_report = report_output_dir[m
     [m
   }[m
[31m-  report_dataframe = as.data.frame(report_dataframe)[m
[31m-  loggernet_status = rbind(loggernet_status,loggernet_status_prj)[m
[31m-  [m
[31m-  [m
[31m-  # ..... Final Report .....................................................................................................................................[m
[31m-  [m
   [m
[31m-  input_final = paste(project_dir,"/Rmd/DQC_Report_overview.Rmd",sep = "")[m
[31m-  # date_DQC [m
[31m-  output_file_final =  paste(PROJECT,"_Report_",[m
[31m-                             format(date_DQC,format = "%Y"),[m
[31m-                             format(date_DQC,format = "%m"),[m
[31m-                             format(date_DQC,format = "%d"),".html", sep = "")[m
[32m+[m[32m  # final_dataframe = rbind(final_dataframe,final_info)[m
[32m+[m[32m  final_dataframe[t,] = final_info[m
   [m
[31m-  output_dir_final = output_dir_report[m
[31m-  rm(params)[m
[31m-  rmarkdown::render(input = input_final,[m
[31m-                    output_file = output_file_final ,[m
[31m-                    output_dir = output_dir_final,[m
[31m-                    params = list(PROJECT = PROJECT,[m
[31m-                                  date_DQC = date_DQC ,[m
[31m-                                  report_dataframe = report_dataframe))[m
[32m+[m[32m  report_dataframe[t,] = report_info[m
   [m
[32m+[m[32m  loggernet_status_prj[t,1] = final_info[1][m
[32m+[m[32m  loggernet_status_prj[t,2] = final_info[2][m
[32m+[m[32m  loggernet_status_prj[t,3] = date_last_modif_file[m
   [m
[31m-  # ..... Data preparation for Database .....................................................................................................................................[m
   [m
[31m-  # MANDARE MAIL !!!![m
[31m-  print("--------------------------------------------------------------------------------------------------")[m
[31m-  [m
[31m-  report_output_dir <- paste(data_output_dir,"00_DQC_Reports/",sep = "")  # where to put output reports[m
[31m-  [m
[31m-  my_subject = paste(PROJECT,"report")[m
[31m-  # my_body = paste(output_dir_final,output_file_final,sep="")[m
[31m-  # my_body = paste(main_dir_mapping_out, substring(output_dir_final, nchar(main_dir_mapping_in)),output_file_final,sep="")[m
[31m-  my_body = paste(url_webservice,PROJECT,substring(report_output_dir, nchar(data_output_dir)),output_file_final,sep="")[m
[31m-  [m
[31m-  # my_body = paste(url_webservice,icinga_text,sep = "")[m
[31m-  # icinga_text = paste(substring(output_dir,nchar('/shared/')),output_file,sep = "")               # to disactivate when webservice is ready![m
[31m-  # icinga_text = paste(substring(output_dir,nchar(data_output_dir)),output_file,sep = "")        # to activate when webservice is ready![m
[31m-  [m
[31m-  send.mail(from = sender,[m
[31m-            to = reciver,[m
[31m-            subject = my_subject,[m
[31m-            body = my_body,[m
[31m-            smtp = my_smtp,[m
[31m-            authenticate = TRUE,[m
[31m-            send = TRUE)[m
[32m+[m[32m  gc(reset = T)[m
[32m+[m[41m [m
 }[m
[32m+[m[32mreport_dataframe = as.data.frame(report_dataframe)[m
[32m+[m[32mloggernet_status = rbind(loggernet_status,loggernet_status_prj)[m
[32m+[m
[32m+[m
[32m+[m[32m# ..... Final Report .....................................................................................................................................[m
[32m+[m
[32m+[m
[32m+[m[32minput_final = paste(project_dir,"/Rmd/DQC_Report_overview.Rmd",sep = "")[m
[32m+[m[32m# date_DQC[m[41m [m
[32m+[m[32moutput_file_final =  paste(PROJECT,"_Report_",[m
[32m+[m[32m                           format(date_DQC,format = "%Y"),[m
[32m+[m[32m                           format(date_DQC,format = "%m"),[m
[32m+[m[32m                           format(date_DQC,format = "%d"),".html", sep = "")[m
[32m+[m
[32m+[m[32moutput_dir_final = output_dir_report[m
[32m+[m[32mrm(params)[m
[32m+[m[32mrmarkdown::render(input = input_final,[m
[32m+[m[32m                  output_file = output_file_final ,[m
[32m+[m[32m                  output_dir = output_dir_final,[m
[32m+[m[32m                  params = list(PROJECT = PROJECT,[m
[32m+[m[32m                                date_DQC = date_DQC ,[m
[32m+[m[32m                                report_dataframe = report_dataframe))[m
[32m+[m
[32m+[m
[32m+[m[32m# ..... Data preparation for Database .....................................................................................................................................[m
[32m+[m
[32m+[m[32m# MANDARE MAIL !!!![m
[32m+[m[32mprint("--------------------------------------------------------------------------------------------------")[m
[32m+[m
[32m+[m[32mreport_output_dir <- paste(data_output_dir,"00_DQC_Reports/",sep = "")  # where to put output reports[m
[32m+[m
[32m+[m[32mmy_subject = paste(PROJECT,"report")[m
[32m+[m[32m# my_body = paste(output_dir_final,output_file_final,sep="")[m
[32m+[m[32m# my_body = paste(main_dir_mapping_out, substring(output_dir_final, nchar(main_dir_mapping_in)),output_file_final,sep="")[m
[32m+[m[32mmy_body = paste(url_webservice,PROJECT,substring(report_output_dir, nchar(data_output_dir)),output_file_final,sep="")[m
[32m+[m
[32m+[m[32m# my_body = paste(url_webservice,icinga_text,sep = "")[m
[32m+[m[32m# icinga_text = paste(substring(output_dir,nchar('/shared/')),output_file,sep = "")               # to disactivate when webservice is ready![m
[32m+[m[32m# icinga_text = paste(substring(output_dir,nchar(data_output_dir)),output_file,sep = "")        # to activate when webservice is ready![m
[32m+[m
[32m+[m[32msend.mail(from = sender,[m
[32m+[m[32m          to = reciver,[m
[32m+[m[32m          subject = my_subject,[m
[32m+[m[32m          body = my_body,[m
[32m+[m[32m          smtp = my_smtp,[m
[32m+[m[32m          authenticate = TRUE,[m
[32m+[m[32m          send = TRUE)[m
[32m+[m
[32m+[m[32m# }[m
 [m
 [m
 file.remove(paste(DQC_setting_dir,"lock_report.lock",sep = ""))[m
[1mdiff --git a/Rmd/DQC_Warning_Pics.Rmd b/Rmd/DQC_Warning_Pics.Rmd[m
[1mindex 97475af4..dbedda41 100644[m
[1m--- a/Rmd/DQC_Warning_Pics.Rmd[m
[1m+++ b/Rmd/DQC_Warning_Pics.Rmd[m
[36m@@ -39,7 +39,7 @@[m [mif(errors_output$err_corrupted$Status == "Y"){[m
   [m
   r_link = ff$pics_link[m
   r_link_text =  ff$pics_corrupted[m
[31m-    [m
[32m+[m[41m  [m
   # r_link_text =  gsub(pattern = "/",replacement = "\\\\",r_link)[m
   #   w1 = which(is.na(r_link_text))[m
   # w2 = which(!is.na(r_link_text))[m
[36m@@ -53,10 +53,10 @@[m [mif(errors_output$err_corrupted$Status == "Y"){[m
   new_ff = as.data.frame(ff$pics_link)[m
   colnames(new_ff) = c( "Pics")[m
   [m
[31m-[m
[31m-  # knitr::kable(new_ff,row.names = FALSE)[m
[31m-    cat(hwrite(new_ff, border=0, center=TRUE,  width='1000px', row.names=FALSE, row.style=list('font-weight:bold')))[m
[31m-[m
[32m+[m[41m  [m
[32m+[m[32m  knitr::kable(new_ff,row.names = FALSE)[m
[32m+[m[32m  # cat(hwrite(new_ff, border=0, center=TRUE,  width='1000px', row.names=FALSE, row.style=list('font-weight:bold')))[m
[32m+[m[41m  [m
   [m
 }[m
 [m
