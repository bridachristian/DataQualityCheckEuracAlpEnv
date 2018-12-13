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

# main_dir = "/shared/"
main_dir = "/shared/test_christian/"
"/shared/Stations_Data/DQC/Process/email_status/mail_status.csv"

if(!file.exists(paste(DQC_setting_dir,"lock_reset.lock",sep = ""))){
  file.create(paste(DQC_setting_dir,"lock_reset.lock",sep = ""))
}

mail_status_dir <- paste(main_dir,"/Stations_Data/DQC/Process/email_status/",sep = "")  
mail_status_file <- "mail_status.csv"

mail_file = read.csv(paste(mail_status_dir,mail_status_file,sep = ""),stringsAsFactors = F)
mail_file$Status = 0

write.csv(mail_file,paste(mail_status_dir,mail_status_file,sep = ""),quote = F,row.names = F)

file.remove(paste(DQC_setting_dir,"lock_reset.lock",sep = ""))

