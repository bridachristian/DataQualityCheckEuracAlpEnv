#' This function check the file size of input file
#'
#' @param DOWNLOAD_TABLE_DIR Directory where download table is.
#' @param FOLDERS_AVAILABLE File name (.dat) in a specific folder.
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
read_download_table_pics = function(DOWNLOAD_TABLE_DIR, DOWNLOAD_TABLE_FILE, FOLDERS_AVAILABLE,DATETIME_FORMAT,PROJECT = NA){
  download_table_file = paste(DOWNLOAD_TABLE_DIR,DOWNLOAD_TABLE_FILE,sep = "") 
  
  if(!file.exists(download_table_file)){              # <- define or extact info from download table

    first_download_table = data.frame(FOLDERS_AVAILABLE, 
                                      rep(NA, times = length(FOLDERS_AVAILABLE)),
                                      rep(NA, times = length(FOLDERS_AVAILABLE)),
                                      substring(FOLDERS_AVAILABLE,1,as.numeric(regexpr(pattern ="_",FOLDERS_AVAILABLE))-1))
    
    colnames(first_download_table) = c("Station", "Last_date","Last_Modification", "Project")
    
    download_table = first_download_table
    
    write.csv(first_download_table,download_table_file,quote = F,row.names = F)
    file.copy(from = download_table_file, to = paste(substring(download_table_file,1,nchar(download_table_file)-4),"_old.csv",sep = ""),overwrite = TRUE)
    
  } else{
    
    file.copy(from = download_table_file, to = paste(substring(download_table_file,1,nchar(download_table_file)-4),"_old.csv",sep = ""),overwrite = TRUE)
    
    download_table = read.csv(download_table_file,stringsAsFactors = F)
    
    station_to_process = FOLDERS_AVAILABLE
    station_already_register = download_table$Station
    station_to_add = setdiff(station_to_process,station_already_register)
    
    prj_to_process = substring(FOLDERS_AVAILABLE,1,as.numeric(regexpr(pattern ="_",FOLDERS_AVAILABLE))-1)
    prj_to_add = substring(station_to_add,1,as.numeric(regexpr(pattern ="_",station_to_add))-1)
    
    
    
    w = which(FOLDERS_AVAILABLE %in% station_to_add)
    
    
    if(length(w) != 0){
      
      df_to_add = data.frame(station_to_add,
                             rep(NA, times = length(station_to_add)),
                             rep(NA, times = length(station_to_add)),
                             prj_to_add)
                             # rep(PROJECT, times = length(station_to_add)))
      
      # as.character(file.mtime(paste(input_dir,FOLDERS_AVAILABLE[w],sep = ""))))
      colnames(df_to_add) = c("Station", "Last_date",  "Last_Modification", "Project")
      
      download_table = rbind(download_table, df_to_add)
      
    }
  }
  return(download_table)
}
