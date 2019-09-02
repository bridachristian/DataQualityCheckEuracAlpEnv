#-------------------------------------------------------------------------------------------------------------------------------------------------------
# File Title:   DQC_move_wrong_files.R
# TITLE:        Data quality check LTER on different files in scheduling folder
# Author:       Brida Christian, Genova Giulio, Zandonai Alessandro
#               Institute for Alpine Environment
# Data:         13/02/2018
# Version:      2.0
#
#------------------------------------------------------------------------------------------------------------------------------------------------------

rm(list = ls(all.names = TRUE))

print("--------------------------------------------------------------------------------------------------")
print(paste("DQC_move_wrong_files:",Sys.time()))

# ..... Libraries .....................................................................................................................................

library(devtools)
library("DataQualityCheckEuracAlpEnv")
# install_github("alexsanjoseph/compareDF")
# library(compareDF)
# library(zoo)
# library(knitr)
# library(ggplot2)
# library(reshape2)
# library(DT)
# library(htmltools)
# library(rmarkdown)
# library(yaml)
# library(highr)
# library(mailR)
# library(XML)
# library(xtable)
# library(dygraphs)
# library(xts)
# library(hwriter)
# library(labeling)
# library(optparse)
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

# main_dir = "Z:/test_christian/"
# project_dir = "C:/Users/CBrida/Desktop/GitLab/dataqualitycheckeuracalpenv/"

print(main_dir)
print(project_dir)

# Sys.setenv(RSTUDIO_PANDOC = "/usr/lib/rstudio/bin/pandoc/")
# .....................................................................................................................................................

# ..... Params section .....................................................................................................................................

project_type = c("LTER","MONALISA")

PROJECT = "LTER" # Possible project: "LTER"; "MONALISA";

input_dir <- paste(main_dir,"/Stations_Data/Data/LoggerNet_Raw_Data/Data/",sep = "")                    # where input files are
ip_errors_dir <- paste(main_dir,"/Stations_Data/Data/LoggerNet_Raw_Data/Data/IP_errors/",sep = "")                    # where input files are

DQC_setting_dir <- paste(main_dir,"/Stations_Data/DQC/",sep = "")

print(input_dir)
print(ip_errors_dir)

if(!file.exists(paste(DQC_setting_dir,"lock_ip_wrong.lock",sep = ""))){
  file.create(paste(DQC_setting_dir,"lock_ip_wrong.lock",sep = ""))
}

for(PROJECT in project_type){
  files_available_raw = dir(input_dir,pattern = ".dat")                  # <-- Admitted pattern:  ".dat" or ".csv"
  
  files_backup = files_available_raw[grepl(pattern = "backup",x = files_available_raw)]
  
  files_available_raw = files_available_raw[grepl(pattern = ".dat$",x = files_available_raw)]      
  files_available_raw = files_available_raw[!grepl(pattern ="backup",x = files_available_raw)]          # REMOVE FILES WITH WRONG NAMES (.dat.backup not admitted) 
  
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
    
    files_wrong = df_files$Files[which(df_files$LoggerNet_name != df_files$Datatable_name)]
    
  } else{
    files_wrong = files_no_project
  }
  
  if(length(files_wrong) > 0 ){
    for(k in 1:length(files_wrong)){
      f =  file.copy(from = paste(input_dir,files_wrong[k],sep = ""), to = paste(ip_errors_dir,files_wrong[k],sep = ""),overwrite = F)
      f
      if(f == FALSE){
        j=0
        repeat{
          j=j+1
          files_wrong_new = paste(substring(files_wrong[k],1, nchar(files_wrong[k])-4),"(",j,").dat",sep = "")
          if(!file.exists(paste(ip_errors_dir,files_wrong_new,sep = ""))){
            break
          }
        }
        
        f2 = file.copy(from = paste(input_dir,files_wrong[k],sep = ""), to = paste(ip_errors_dir,files_wrong_new,sep = ""),overwrite = F)
        
        if(f2 == TRUE){
          file.remove(paste(input_dir,files_wrong[k],sep = ""))
          
        }
      }else{
        file.remove(paste(input_dir,files_wrong[k],sep = ""))
      }
    }
  }
  
}

file.remove(paste(DQC_setting_dir,"lock_ip_wrong.lock",sep = ""))

print("---------------------------------------------------------------------------------------------------------")
print(Sys.time())
print("----- End Script ----------------------------------------------------------------------------------------")


