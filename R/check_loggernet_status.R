check_loggernet_status = function(issue_counter_dir, issue_counter, input_dir, MESSAGE_EVERY_TIMES = 24){
  
  if(!file.exists(paste(issue_counter_dir,"loggernet_counter.rds",sep = ""))){
    loggernet_counter = data.frame(0,NA)
    colnames(loggernet_counter) = c("counter", "Date_stop")
    saveRDS(loggernet_counter,paste(issue_counter_dir,"loggernet_counter.rds",sep = ""))
  } else{
    loggernet_counter = readRDS(paste(issue_counter_dir,"loggernet_counter.rds",sep = ""))
  }
  
  if(all(issue_counter$W_Update_station != 0)){
    loggernet_counter$counter = loggernet_counter$counter + 1
    if(loggernet_counter$counter == 1){
      loggernet_counter$Date_stop = format(max(file.mtime(paste(input_dir,dir(input_dir,pattern = ".dat"),sep = ""))), tz = "Etc/GMT-1")
    }
  }else{
    loggernet_counter$counter = 0
    loggernet_counter$Date_stop = NA
  }
  saveRDS(loggernet_counter,paste(issue_counter_dir,"loggernet_counter.rds",sep = ""))
  
  
  if(loggernet_counter$counter != 0){
    if(loggernet_counter$counter == 1 | loggernet_counter$counter %% MESSAGE_EVERY_TIMES == 0){
      text_W_loggernet_locked = paste("Loggernet download last data at", loggernet_counter$Date_stop)
      # warning(text_W_loggernet_locked)
    }
  } 
  
  if(!exists("text_W_loggernet_locked")){
    
    text_W_loggernet_locked = NULL
    
  }
  
  return(text_W_loggernet_locked)
}