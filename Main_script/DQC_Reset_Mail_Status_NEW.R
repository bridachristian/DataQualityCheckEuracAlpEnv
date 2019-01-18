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

# ..... Libraries .....................................................................................................................................
library(devtools,lib.loc = '/home/cbrida/Libraries_DataQualityCheckEuracAlpEnv/')
install_github("bridachristian/DataQualityCheckEuracAlpEnv")
library("DataQualityCheckEuracAlpEnv")

# Sys.setenv(RSTUDIO_PANDOC = "/usr/lib/rstudio/bin/pandoc/")
# .....................................................................................................................................................

# ..... Params section .....................................................................................................................................

main_dir = "/shared/"
# main_dir = "/shared/test_christian/"
DQC_setting_dir <- paste(main_dir,"/Stations_Data/DQC/",sep = "")

if(!file.exists(paste(DQC_setting_dir,"lock_reset.lock",sep = ""))){
  file.create(paste(DQC_setting_dir,"lock_reset.lock",sep = ""))
}

mail_status_dir <- paste(main_dir,"/Stations_Data/DQC/Process/email_status/",sep = "")  
mail_status_file <- "mail_status.csv"

oor_status_dir <- paste(main_dir,"/Stations_Data/DQC/Process/email_status/",sep = "")  
oor_status_file <- "out_of_range.csv"

mail_file = read.csv(paste(mail_status_dir,mail_status_file,sep = ""),stringsAsFactors = F)     # read and reset mail_status.csv 

mail_file[mail_file == 0] = 1
mail_file[which(mail_file$Err_name %in% c("err_out_of_range","err_duplicates_rows")), 3:ncol(mail_file)] = 0
mail_file[which(mail_file$Err_name %in% c("err_range_alert")), 3:ncol(mail_file)] = 1

oor_file = read.csv(paste(oor_status_dir,oor_status_file,sep = ""),stringsAsFactors = F)        # read and reset out_of_range.csv

oor_file[oor_file == 0] = 1

write.csv(mail_file,paste(mail_status_dir,mail_status_file,sep = ""),quote = F,row.names = F, na = "")
write.csv(oor_file,paste(oor_status_dir,oor_status_file,sep = ""),quote = F,row.names = F, na = "")

file.remove(paste(DQC_setting_dir,"lock_reset.lock",sep = ""))

