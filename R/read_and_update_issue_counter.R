#' This function check the file size of input file
#'
#' @param ISSUE_COUNTER_DIR Directory where download table is.
#' @param FILES_AVAILABLE File name (.dat) in a specific folder.
#' @param DATETIME_FORMAT Datetime format
#' @param PROJECT 
#' 
#' @return A dataframe called download_table which contain the name of station, the last download data, the last data file modification and a flag used to avoid Data Quality Check
#' @export
#' @examples
#' check_empty_file(INPUT_DATA_DIR = "~/Data/Input/",FILE = "M4s.dat", DATETIME_FORMAT = "%Y-%m-%d %H:%M")
#' check_empty_file(INPUT_DATA_DIR = "Your input folder",FILE = "Your file to check")
#' 


# output:
# 1. download_table
read_and_update_issue_counter = function(ISSUE_COUNTER_DIR,FILES_AVAILABLE,DATETIME_FORMAT,PROJECT = NA){
  issue_counter_file = paste(ISSUE_COUNTER_DIR,"issue_counter.csv",sep = "") 
  
  if(!file.exists(issue_counter_file)){              # <- define or extact info from download table
    
    first_issue_counter = data.frame(substring(FILES_AVAILABLE,1, nchar(FILES_AVAILABLE)-4), 
                                    rep(PROJECT,times = length(FILES_AVAILABLE)),
                                    rep(0, times = length(FILES_AVAILABLE)),
                                    rep(0, times = length(FILES_AVAILABLE)),
                                    rep(0, times = length(FILES_AVAILABLE)),
                                    rep(0, times = length(FILES_AVAILABLE)),
                                    rep(0, times = length(FILES_AVAILABLE)),
                                    rep(0, times = length(FILES_AVAILABLE)),
                                    rep(0, times = length(FILES_AVAILABLE)))
    
    colnames(first_issue_counter) = c("Station", "Project", "W_Update_station","W_Empty_file","W_Logger_number","W_Structure_issues","W_date_issue",
                                      "W_overlap", "W_missing_records")
    
    issue_counter = first_issue_counter
    
    write.csv(first_issue_counter,issue_counter_file,quote = F,row.names = F)
    # file.copy(from = issue_counter_file, to = paste(substring(issue_counter_file,1,nchar(issue_counter_file)-4),"_old.csv",sep = ""),overwrite = TRUE)
    
  } else{
    
    # file.copy(from = issue_counter_file, to = paste(substring(issue_counter_file,1,nchar(issue_counter_file)-4),"_old.csv",sep = ""),overwrite = TRUE)
    
    issue_counter = read.csv(issue_counter_file,stringsAsFactors = F)
    
    station_to_process = substring(FILES_AVAILABLE,1, nchar(FILES_AVAILABLE)-4)
    station_already_register = issue_counter$Station
    station_to_add = setdiff(station_to_process,station_already_register)
    
    w = which(substring(FILES_AVAILABLE,1, nchar(FILES_AVAILABLE)-4) %in% station_to_add)
    
    
    if(length(w) != 0){
      
      df_to_add = data.frame(station_to_add,
                             rep(PROJECT, times = length(station_to_add)),
                             rep(0, times = length(station_to_add)),
                             rep(0, times = length(station_to_add)),
                             rep(0, times = length(station_to_add)),
                             rep(0, times = length(station_to_add)),
                             rep(0, times = length(station_to_add)),
                             rep(0, times = length(station_to_add)),
                             rep(0, times = length(station_to_add)))
      
      # as.character(file.mtime(paste(input_dir,FILES_AVAILABLE[w],sep = ""))))
      colnames(df_to_add) = c("Station", "Project", "W_Update_station","W_Empty_file","W_Logger_number","W_Structure_issues","W_date_issue",
                              "W_overlap", "W_missing_records")
      
      issue_counter = rbind(issue_counter, df_to_add)

      
    }
  }
  return(issue_counter)
}
