#' This function check the file size of input file
#'
#' @param DOWNLOAD_TABLE_DIR Directory where download table is.
#' @param FILES_AVAILABLE File name (.dat) in a specific folder.
#' @param DATETIME_FORMAT Datetime format
#'
#' @return A dataframe called download_table which contain the name of station, the last download data, the last data file modification and a flag used to avoid Data Quality Check
#' @export
#' @examples
#' check_empty_file(INPUT_DATA_DIR = "~/Data/Input/",FILE = "M4s.dat", DATETIME_FORMAT = "%Y-%m-%d %H:%M")
#' check_empty_file(INPUT_DATA_DIR = "Your input folder",FILE = "Your file to check")
#' 


# output:
# 1. download_table
read_and_update_download_table = function(DOWNLOAD_TABLE_DIR,FILES_AVAILABLE,DATETIME_FORMAT,PROJECT = NA){
  download_table_file = paste(DOWNLOAD_TABLE_DIR,"download_table.csv",sep = "") 
  
  if(!file.exists(download_table_file)){              # <- define or extact info from download table
    
    first_download_table = data.frame(substring(FILES_AVAILABLE,1, nchar(FILES_AVAILABLE)-4), 
                                      rep(NA, times = length(FILES_AVAILABLE)),
                                      rep(0,times = length(FILES_AVAILABLE)),
                                      rep(NA, times = length(FILES_AVAILABLE)),
                                      rep(1,times = length(FILES_AVAILABLE)),
                                      rep(PROJECT,times = length(FILES_AVAILABLE)))
    
    colnames(first_download_table) = c("Station", "Last_date", "Stop_DQC", "Last_Modification", "record_check", "Project")
    
    download_table = first_download_table
    
    write.csv(first_download_table,download_table_file,quote = F,row.names = F)
    file.copy(from = download_table_file, to = paste(substring(download_table_file,1,nchar(download_table_file)-4),"_old.csv",sep = ""),overwrite = TRUE)
    
  } else{
    
    file.copy(from = download_table_file, to = paste(substring(download_table_file,1,nchar(download_table_file)-4),"_old.csv",sep = ""),overwrite = TRUE)
    
    download_table = read.csv(download_table_file,stringsAsFactors = F)
    
    station_to_process = substring(FILES_AVAILABLE,1, nchar(FILES_AVAILABLE)-4)
    station_already_register = download_table$Station
    station_to_add = setdiff(station_to_process,station_already_register)
    
    w = which(substring(FILES_AVAILABLE,1, nchar(FILES_AVAILABLE)-4) %in% station_to_add)
    

    if(length(w) != 0){
      
      df_to_add = data.frame(station_to_add,
                             rep(NA, times = length(station_to_add)),
                             rep(0,times = length(station_to_add)),
                             rep(NA, times = length(station_to_add)),
                             rep(1,times = length(station_to_add)),
                             rep(PROJECT, times = length(FILES_AVAILABLE)))
                             
                             # as.character(file.mtime(paste(input_dir,FILES_AVAILABLE[w],sep = ""))))
      colnames(df_to_add) = c("Station", "Last_date", "Stop_DQC", "Last_Modification", "record_check", "Project")
      
      download_table = rbind(download_table, df_to_add)
      
    }
  }
  return(download_table)
}
