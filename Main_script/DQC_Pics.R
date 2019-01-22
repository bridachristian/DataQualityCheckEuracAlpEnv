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
# library(devtools,lib.loc = '/home/cbrida/Libraries_DataQualityCheckEuracAlpEnv/')
# install_github("bridachristian/DataQualityCheckEuracAlpEnv")
# library("DataQualityCheckEuracAlpEnv")
# install_github("alexsanjoseph/compareDF")
# library(compareDF)
# 
# library(zoo,lib.loc = '/home/cbrida/Libraries_DataQualityCheckEuracAlpEnv/')
# library(knitr,lib.loc = '/home/cbrida/Libraries_DataQualityCheckEuracAlpEnv/')
# library(ggplot2,lib.loc = '/home/cbrida/Libraries_DataQualityCheckEuracAlpEnv/')
# library(reshape2,lib.loc = '/home/cbrida/Libraries_DataQualityCheckEuracAlpEnv/')
# library(DT,lib.loc = '/home/cbrida/Libraries_DataQualityCheckEuracAlpEnv/')
# library(htmltools,lib.loc = '/home/cbrida/Libraries_DataQualityCheckEuracAlpEnv/')
# library(rmarkdown,lib.loc = '/home/cbrida/Libraries_DataQualityCheckEuracAlpEnv/')
# library(yaml,lib.loc = '/home/cbrida/Libraries_DataQualityCheckEuracAlpEnv/')
# library(highr,lib.loc = '/home/cbrida/Libraries_DataQualityCheckEuracAlpEnv/')
# 
# library(mailR,lib.loc = '/home/cbrida/Libraries_DataQualityCheckEuracAlpEnv/')
# 
# library(XML,lib.loc = '/home/cbrida/Libraries_DataQualityCheckEuracAlpEnv/')
# library(xtable, lib.loc = "/home/cbrida/Libraries_DataQualityCheckEuracAlpEnv/")
# library(dygraphs, lib.loc = "/home/cbrida/Libraries_DataQualityCheckEuracAlpEnv/")
# library(xts, lib.loc = "/home/cbrida/Libraries_DataQualityCheckEuracAlpEnv/")
# library(hwriter, lib.loc = "/home/cbrida/Libraries_DataQualityCheckEuracAlpEnv/")
# library(labeling, lib.loc =  "/home/cbrida/Libraries_DataQualityCheckEuracAlpEnv/")
# library(stringr,lib.loc = "/home/cbrida/Libraries_DataQualityCheckEuracAlpEnv/")

# install.packages("stringr", lib = "/home/cbrida/Libraries_DataQualityCheckEuracAlpEnv/" )

# install.packages("hwriter" )
library(devtools)
install_github("bridachristian/DataQualityCheckEuracAlpEnv")
library("DataQualityCheckEuracAlpEnv")
install_github("alexsanjoseph/compareDF")
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


# Sys.setenv(RSTUDIO_PANDOC = "/usr/lib/rstudio/bin/pandoc/")
# .....................................................................................................................................................

# ..... Params section .....................................................................................................................................

main_dir = "Z:/test_christian/"
# main_dir = "/shared/test_christian/"

main_dir_mapping_in = "/shared/"                                   # <-- "Z:/" or "/shared/" will be replaced with "\\\\smb.scientificnet.org\\alpenv"
main_dir_mapping_out = "\\\\smb.scientificnet.org\\alpenv"    # <-- "Z:/" or "/shared/" will be replaced with "\\\\smb.scientificnet.org\\alpenv"

# main_dir = "/shared/test_christian/"
# main_dir = "H:/Projekte/LTER/03_Arbeitsbereiche/BriCh/shared/test_christian/"

project_type = c("LTER","MONALISA")

PROJECT = "LTER" # Possible project: "LTER"; "MONALISA";


input_dir <- paste(main_dir,"/Stations_Data/Data/LoggerNet_Raw_Data/Data/Pics",sep = "")                    # where input files are
# input_dir <- paste("Z:","/Stations_Data/Data/LoggerNet_Raw_Data/Data/Pics",sep = "")                    # where input files are

backup_dir <- paste(main_dir,"/Stations_Data/Data/Pics_Backup",sep = "")                    # where input files are
corrupt_dir <-paste(main_dir,"/Stations_Data/Data/Pics_Corrupted",sep = "")

# input_dir <- paste("/shared","/Stations_Data/Data/LoggerNet_Raw_Data/Data/",sep = "")                    # where input files are

project_dir <- "/home/cbrida/DataQualityCheckEuracAlpEnv/"  # where package is developed or cloned from github
# project_dir <- "C:/Users/CBrida/Desktop/myDQC/DataQualityCheckEuracAlpEnv/"  # where package is developed or cloned from github

DQC_setting_dir <- paste(main_dir,"/Stations_Data/DQC/",sep = "")

date_DQC = as.POSIXct(format(Sys.time(),format = "%Y-%m-%d %H:%M"), tz = 'Etc/GMT-1')
dqc_date_write = paste(format(date_DQC,"%Y"),format(date_DQC,"%m"),format(date_DQC,"%d"),format(date_DQC,"%H"),format(date_DQC,"%M"),sep = "")

datetime_pics_format = "%y%m%d%H%M"
datetime_pics_format_new = "%Y%m%d%H%M"

warning_pics_RMD = paste(project_dir,"/Rmd/DQC_Warning_Pics.Rmd",sep = "")

mail_file = paste(DQC_setting_dir,"Process/email_status/mail_status.csv",sep = "")

# --- read mail configuration ---

mail_config_file = paste(DQC_setting_dir,"Process/email_status/mail_config.xml",sep = "")
mail_config = xmlParse(mail_config_file, useInternalNodes = F)

mail_config_info = mail_config_parsing(mail_config)

sender = mail_config_info$sender
# reciver = mail_config_info$reciver 
reciver = "Christian.Brida@eurac.edu" 
my_smtp = mail_config_info$my_smtp
# -------------------------------

# if(!file.exists(paste(DQC_setting_dir,"lock_pics.lock",sep = ""))){
#   file.create(paste(DQC_setting_dir,"lock_pics.lock",sep = ""))
# }

# -------------------------------# -------------------------------# -------------------------------# -------------------------------# -------------------------------

for(PROJECT in project_type){
  data_output_dir <- paste(main_dir,"Stations_Data/Data/DQC_Processed_Data/",PROJECT,"/Stations/",sep = "")  # where to put output files
  # warning_file_dir <- paste(main_dir,"Stations_Data/Data/DQC_Warnings/",PROJECT,"/", sep = "")  # where to put warnings html files
  
  
  # ..... files selection .....................................................................................................................................
  
  folders_available_project = dir(input_dir,pattern = PROJECT)                  # <-- Admitted pattern:  ".dat" or ".csv"
  
  t = 1
  
  for(t in  1: length(folders_available_project)){
    
    gc(reset = T)
    
    FOLDER_NAME = folders_available_project[t]
    
    u1 = gregexpr(FOLDER_NAME,pattern = "_")[[1]][1]      # <- here we find the first "[[1]][1]" underscore!!!!!
    
    STATION_NAME = substring(FOLDER_NAME,u1+1)
    
    
    #  ---- define path and folders ----
    
    inpur_dir_pics = paste(input_dir,"/", FOLDER_NAME,sep = "")
    
    if(dir.exists(paste(data_output_dir,STATION_NAME,"/", sep = ""))){                # create subfolder to store data organized by station name
      if(dir.exists(paste(data_output_dir,STATION_NAME,"/Pics/", sep = ""))){
        output_dir_pics_new = paste(data_output_dir,STATION_NAME,"/Pics/", sep = "")
        warning_file_dir_station = paste(data_output_dir,STATION_NAME,"/Alerts/Warnings/", sep = "")
      }else{
        dir.create(paste(data_output_dir,STATION_NAME,"/Pics/", sep = ""))
        dir.create(paste(data_output_dir,STATION_NAME,"/Alerts/Warnings/", sep = ""))
        output_dir_pics_new = paste(data_output_dir,STATION_NAME,"/Pics/", sep = "")
        warning_file_dir_station = paste(data_output_dir,STATION_NAME,"/Alerts/Warnings/", sep = "")
        
      }
    }else{
      dir.create(paste(data_output_dir,STATION_NAME,"/", sep = ""))
      dir.create(paste(data_output_dir,STATION_NAME,"/Pics/", sep = ""))
      dir.create(paste(data_output_dir,STATION_NAME,"/Alerts/Warnings/", sep = ""))
      output_dir_pics_new = paste(data_output_dir,STATION_NAME,"/Pics/", sep = "")
      warning_file_dir_station = paste(data_output_dir,STATION_NAME,"/Alerts/Warnings/", sep = "")
      
    }
    
    
    backup_dir_pics = paste(backup_dir,"/", FOLDER_NAME,sep = "")
    if(!dir.exists(backup_dir)){
      dir.create(backup_dir)
      dir.create(backup_dir_pics)
    }else{
      if(!dir.exists(backup_dir_pics)){
        dir.create(backup_dir_pics)
      }
    }
    
    corrupt_dir_pics = paste( corrupt_dir,"/", FOLDER_NAME,sep = "")
    if(!dir.exists(corrupt_dir)){
      dir.create(corrupt_dir)
      dir.create(corrupt_dir_pics)
    }else{
      if(!dir.exists(corrupt_dir_pics)){
        dir.create(corrupt_dir_pics)
      }
    }
    
    
    #  ---- import files and folders ----
    
    # inpur_dir_pics
    # backup_dir_pics
    # output_dir_pics_new
    # corrupt_dir_pics
    # warning_file_dir_station
    
    file_raw = list.files(inpur_dir_pics )
    file_raw = file_raw[!grepl(pattern = "Thumbs.db",x = file_raw)] 
    file = list.files(inpur_dir_pics, full.names = T)
    file = file[!grepl(pattern = "Thumbs.db",x = file)] 
    
    # create new_names
    color = substring(file_raw,1,1)
    d_pics = paste(substring(file_raw,2,nchar(file_raw)-4),"0",sep = "")
    datetime = as.POSIXct(d_pics,format = datetime_pics_format, tz= "Etc/GMT-1")
    
    d_to_write = format(datetime,format = datetime_pics_format_new)
    
    color_new = color
    color_new[which(color == "I")] = "IR" 
    color_new[which(color == "R")] = "RGB" 

    file_new_names = paste(STATION_NAME, "_", color_new, "_", d_to_write,".jpg",sep = "")
    df = data.frame(file_raw, file_new_names, datetime)
    df = df[order(df$datetime),]
    
    
    if(length(file) > 0 ){   
      output_no_pics = list("N", NA)
      names(output_no_pics) =c("Status", "Values")
      
      
      file.copy(from = file,to = paste(backup_dir_pics,"/", file_raw,sep = ""))
      # NB --> file.copy NON sovrascrive !!!!
      
      w = which(file.size(file) > 10000)  # move to a specific folder pics corrupted. The treshold on file size is 10 KB (= 10000 B)
      pics_ok = file_raw[w]
      pics_corrupted = file_raw[-w]
      
      if(length(pics_ok) > 0 ){
        files_old = list.files(output_dir_pics_new)
       # files_
          
        file.rename(from = paste(inpur_dir_pics,"/", pics_ok,sep = "") , to = paste(output_dir_pics_new,"/", pics_ok, sep = "")) 
        # NB --> file.rename sovrascrive !!!! --> assicurarsi di copiare nella cartella di ouput solo i file diversi!
      }
      
      if(length(pics_corrupted) > 0 ){
        file.rename(from = paste(inpur_dir_pics,"/", pics_corrupted,sep = ""), to = paste(corrupt_dir_pics,"/", pics_corrupted, sep = ""))
        # NB --> file.rename sovrascrive !!!! --> assicurarsi di copiare nella cartella di ouput solo i file diversi!
        
        
        pics_link = paste(main_dir_mapping_out,"/" ,substring(output_dir_pics_new, nchar(main_dir_mapping_in)+1),"/",pics_corrupted,sep = "")
        df = data.frame(pics_corrupted, pics_link)
        
        ### ordinare df per data immagini! 
        
        output_corrupted = list("Y", df)
        names(output_corrupted) =c("Status", "Values")
        
        errors_output = list(output_no_pics,
                             output_corrupted)
        names(errors_output) = c("err_no_pics",
                                 "err_corrupted")
        
        
        input = warning_pics_RMD
        output_file = paste(STATION_NAME,"_",dqc_date_write,"_pics_corrupted.html",sep = "")
        output_dir = warning_file_dir_station
        params_list = list(station_name = STATION_NAME,
                           errors_output = errors_output,
                           dqc_date = date_DQC)
        
        rmarkdown::render(input = input,
                          output_file = output_file,
                          output_dir = output_dir,
                          params = params_list)
        
        
        
        
        my_subject = paste(STATION_NAME,"- pics corrupted")
        my_body = paste(main_dir_mapping_out,"/",substring(output_dir,nchar(main_dir_mapping_in)+1),"/", output_file,sep = "")
        
        send.mail(from = sender,
                  # to = reciver,
                  to = "Christian.Brida@eurac.edu",
                  subject = my_subject,
                  body = my_body,
                  smtp = my_smtp,
                  authenticate = TRUE,
                  send = TRUE)
        
      }else{
        output_corrupted = list("N", NA)
        names(output_corrupted) =c("Status", "Values")
        
      }
      
      # color = substring(file_raw,1,1)
      # d_pics = paste(substring(file_raw,2,nchar(file_raw)-4),"0",sep = "")
      # datetime = as.POSIXct(d_pics,format = datetime_pics_format, tz= "Etc/GMT-1")
      # corrup = rep(1, times = length(file_raw))
      # corrup[w] = 0
      # 
      # df = data.frame(file_raw, color, datetime, corrup)
      # df = df[order(df$datetime),]
      
      
    }      
    output_no_pics = list("Y", NA)
    names(output_no_pics) =c("Status", "Values")
    
    output_corrupted = list("N", NA)
    names(output_corrupted) =c("Status", "Values")
    
    errors_output = list(output_no_pics,
                         output_corrupted)
    names(errors_output) = c("err_no_pics",
                             "err_corrupted")
    
    # completare con   verifica ultimo downlaoad --> controllare nella cartella di output quale e`  il file piu aggiornato!
    
    
    
    

    
  }
  
  # MANDARE MAIL !!!!
  # print("--------------------------------------------------------------------------------------------------")
  # 
  # my_subject = paste(PROJECT,"report")
  # # my_body = paste(output_dir_final,output_file_final,sep="")
  # my_body = paste(main_dir_mapping_out, substring(output_dir_final, nchar(main_dir_mapping_in)),output_file_final,sep="")
  # 
  # send.mail(from = sender,
  #           to = reciver,
  #           # to = "Christian.Brida@eurac.edu",
  #           subject = my_subject,
  #           body = my_body,
  #           smtp = my_smtp,
  #           authenticate = TRUE,
  #           send = TRUE)
}





# file.remove(paste(DQC_setting_dir,"lock_pics.lock",sep = ""))


