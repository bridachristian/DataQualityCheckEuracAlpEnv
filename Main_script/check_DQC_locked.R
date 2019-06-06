rm(list = ls())

library(mailR)
library(XML)
library(optparse)
library(DataQualityCheckEuracAlpEnv)

option_list = list(
  make_option(c("-md", "--maindir"), type="character", default="/shared/", 
              help="set the main dir", metavar="character"),
  make_option(c("-pd", "--prjdir"), type="character", default="/home/cbrida/DataQualityCheckEuracAlpEnv/", 
              help="set the project dir", metavar="character")
); 

opt_parser = OptionParser(option_list=option_list);
opt = parse_args(opt_parser);

main_dir = opt$maindir
project_dir = opt$prjdir     # never used!

Sys.setenv(TZ = "Etc/GMT-1")


DQC_lock = "lock_DQC.lock"
report_lock = "lock_report.lock"

mail_file = "mail_config.xml"

DQC_setting_dir <- paste(main_dir,"/Stations_Data/DQC/",sep = "")

mail_config_file = paste(DQC_setting_dir,"Process/email_status/",mail_file,sep = "")
# mail_config_file = paste(mail_dir,"mail_config.xml",sep = "")
mail_config = xmlParse(mail_config_file, useInternalNodes = F)

mail_config_info = mail_config_parsing_new(mail_config)

sender = mail_config_info$sender
# reciver = mail_config_info$reciver
reciver = "Christian.Brida@eurac.edu"
my_smtp = mail_config_info$my_smtp
url_webservice = mail_config_info$url_webservice

print(mail_config_file)
print(sender)
print(reciver)

# -----------------------------------------------------------------------------------------------------
# DQC_locked
# -----------------------------------------------------------------------------------------------------

file_to_check = paste(main_dir,"Stations_Data/DQC/",DQC_lock,sep = "")

sys_time = Sys.time()

if(file.exists(file_to_check)){
  # file.mtime(file_to_check)
  date_last_modif_file = as.character(format(file.mtime(file_to_check),format = "%Y-%m-%d %H:%M"))
  
  # ----- station offline  ------
  
  h_last_modif_file = trunc(as.POSIXct(date_last_modif_file, tz = "Etc/GMT-1"),units = "hours")
  h_system = trunc(sys_time,units = "hours")
  
  hours_diff = as.numeric(difftime(time1 = h_system, time2 = h_last_modif_file, tz = "Etc/GMT-1",units = "hours"))
  
  if(hours_diff == 1 | hours_diff%%12 == 0){ # <-- no resto => hours_diff is multiple of HOURS_OFFLINE. exclude case of hours_diff is less than 24h 
    
    my_subject = paste("DQC locked!")
    my_body = paste("DQC locked from:", date_last_modif_file)
    
    my_smtp = list(host.name = my_smtp$host.name,port = my_smtp$port,user.name = my_smtp$user.name,passwd = my_smtp$passwd,ssl = my_smtp$ssl)
    

    send.mail(from = sender,
              to = reciver,
              subject = my_subject,
              body = my_body,
              smtp = my_smtp,
              authenticate = TRUE,
              send = TRUE)
    
  }
}

# -----------------------------------------------------------------------------------------------------
# report locked
# -----------------------------------------------------------------------------------------------------

file_to_check = paste(main_dir,"Stations_Data/DQC/",report_lock,sep = "")


if(file.exists(file_to_check)){
  # file.mtime(file_to_check)
  date_last_modif_file = as.character(format(file.mtime(file_to_check),format = "%Y-%m-%d %H:%M"))
  
  # ----- station offline  ------
  
  h_last_modif_file = trunc(as.POSIXct(date_last_modif_file, tz = "Etc/GMT-1"),units = "hours")
  h_system = trunc(sys_time,units = "hours")
  
  hours_diff = as.numeric(difftime(time1 = h_system, time2 = h_last_modif_file, tz = "Etc/GMT-1",units = "hours"))
  
  if(hours_diff == 168+1 | hours_diff%%168 == 0){ # <-- no resto => hours_diff is multiple of HOURS_OFFLINE. exclude case of hours_diff is less than 24h 
    
    my_subject = paste("Report locked!")
    my_body = paste("Report locked from:", date_last_modif_file)
    my_smtp = list(host.name = my_smtp$host.name,port = my_smtp$port,user.name = my_smtp$user.name,passwd = my_smtp$passwd,ssl = my_smtp$ssl)
    
    send.mail(from = sender,
              to = reciver,
              subject = my_subject,
              body = my_body,
              smtp = my_smtp,
              authenticate = TRUE,
              send = TRUE)
    
  }
}

cat ("--------------------------------------------------------------")
cat(as.character(Sys.time()))
cat ("--------------------------------------------------------------\n")

