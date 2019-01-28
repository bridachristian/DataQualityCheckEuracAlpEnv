rm(list = ls())
# library(mailR,lib.loc = '/home/cbrida/Libraries_DataQualityCheckEuracAlpEnv/')
library(optparse)

option_list = list(
  make_option(c("-md", "--maindir"), type="character", default="/shared/", 
              help="set the main dir", metavar="character"),
  make_option(c("-pd", "--prjdir"), type="character", default="/home/cbrida/DataQualityCheckEuracAlpEnv/", 
              help="set the project dir", metavar="character")
); 

Sys.setenv(TZ = "Etc/GMT-1")

main_dir = "Z:/"

file_to_check = paste(main_dir,"Stations_Data/DQC/lock_DQC.lock",sep = "")
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
    my_smtp = list(host.name = 'smtp.gmail.com',port = 465,user.name = "data.quality.check",passwd = 'alpenv78',ssl = T)
    
    send.mail(from = "data.quality.check@gmail.com",
              to = "Christian.Brida@eurac.edu",
              subject = my_subject,
              body = my_body,
              smtp = my_smtp,
              authenticate = TRUE,
              send = TRUE)
    
  }
}

file_to_check = paste(main_dir,"Stations_Data/DQC/lock_report.lock",sep = "")


if(file.exists(file_to_check)){
  # file.mtime(file_to_check)
  date_last_modif_file = as.character(format(file.mtime(file_to_check),format = "%Y-%m-%d %H:%M"))
  
  # ----- station offline  ------
  
  h_last_modif_file = trunc(as.POSIXct(date_last_modif_file, tz = "Etc/GMT-1"),units = "hours")
  h_system = trunc(sys_time,units = "hours")
  
  hours_diff = as.numeric(difftime(time1 = h_system, time2 = h_last_modif_file, tz = "Etc/GMT-1",units = "hours"))
  
  if(hours_diff == 168+1 | hours_diff%%12 == 0){ # <-- no resto => hours_diff is multiple of HOURS_OFFLINE. exclude case of hours_diff is less than 24h 
    
    my_subject = paste("Report locked!")
    my_body = paste("Report locked from:", date_last_modif_file)
    my_smtp = list(host.name = 'smtp.gmail.com',port = 465,user.name = "data.quality.check",passwd = 'alpenv78',ssl = T)
    
    send.mail(from = "data.quality.check@gmail.com",
              to = "Christian.Brida@eurac.edu",
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

