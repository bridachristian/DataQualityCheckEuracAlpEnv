#-------------------------------------------------------------------------------------------------------------------------------------------------------
# File Title:   DQC_Script.R
# TITLE:        Data quality check LTER on different files in scheduling folder
# Author:       Brida Christian, Genova Giulio, Zandonai Alessandro
#               Institute for Alpine Environment
# Data:         13/12/2017
# Version:      1.0
#
#------------------------------------------------------------------------------------------------------------------------------------------------------

rm(list = ls(all.names = TRUE))


# ..... Libraries .....................................................................................................................................

library(devtools)
install_github("bridachristian/DataQualityCheckEuracAlpEnv")
library("DataQualityCheckEuracAlpEnv")

library(zoo)
library(knitr)
library(ggplot2)
library(reshape2)
library(DT)
library(htmltools)

# ..... Input section .................................................................................................................................

## Description:
# input_dir:             # dir:  Directory where to put input files.
# output_dir_data:       # dir:  Directory where to save output files
# output_dir_report:     # dir:  Directory where to save output report
# project_dir:           # dir:  Directory of DataQualityCheckEuracAlpEnv package. Inside there are R script, Rmarkdown document and support files.
# data_from_row:         # num:  The row number of the first data row. (How many rows are dedicated to headers? + 1)
# header_row_number:     # num:  The row number of the header names
# datetime_header:       # chr:  The string to use to recognize datetime
# datetime_format:       # chr:  The string to use to recognize datetime. Use only: y -> year, m -> month, d -> day, H -> hour, M -> minute
# datetime_sampling:     # chr:  The string that define the time sampling (in POSIXct format)
# record_header:         # chr:  The string to use to recognize record header
# range_file:            # chr:  The string that indicate the name of range file saved in folder "project_dir/Data/Support_files/Range"
# write_output_files:    # log:  Logical status (TRUE/FALSE) to decide if save output data or not
# write_output_report:   # log:  Logical status (TRUE/FALSE) to decide if save output report or not
# file:                  # chr:  The string that indicate the name of file to process
# start_date:            # date: First date to consider for the analysis. Date before are not considered

# ..... Params section .....................................................................................................................................

input_dir <- "H:/Projekte/Klimawandel/Experiment/data/2order/DataQualityCheckEuracAlpEnv/Data/Input/"                # where input files are
output_dir_data <- "H:/Projekte/Klimawandel/Experiment/data/2order/DataQualityCheckEuracAlpEnv/Data/Output/data/"   # where to put output files
output_dir_report <- "H:/Projekte/Klimawandel/Experiment/data/2order/DataQualityCheckEuracAlpEnv/Data/Output/report/"   # where to put output reports
project_dir <- "H:/Projekte/Klimawandel/Experiment/data/2order/DataQualityCheckEuracAlpEnv/"  # where package is developed or cloned from github

data_from_row =  5                                             # <-- Row number of first data
header_row_number =  2                                         # <-- Row number of header
datetime_header =  "TIMESTAMP"                                 # <-- header corresponding to TIMESTAMP
datetime_format =  "%Y-%m-%d %H:%M"                          # <-- datetime format. Use only: Y -> year, m -> month, d -> day, H -> hour, M -> minute
datetime_sampling =  "15 min"
record_header =  "RECORD"
range_file =  "Range.csv"

write_output_files =  "TRUE"
write_output_report =  "FALSE"
file <- "M3.dat"
start_date <- "2017-11-23 05:45:00"

# ~~~ Default directory ~~~~

# ..... Body ..........................................................................................................................................
range_dir <- paste(project_dir, "Data/Support_files/Range/",sep = "")


# .....................................................................................................................................................

# ..... Define flags ..................................................................................................................................

flag_empty = NA
flag_error_df = NA
flag_date = NA
flag_duplicates_rows = NA
flag_overlap = NA
flag_missing_dates = NA
flag_range_variable_to_set = NA
flag_range_variable_new = NA
flag_out_of_range = NA

if(check_empty_file(INPUT_DATA_DIR = input_dir, FILE_NAME = file) == TRUE){

  flag_empty = 1

}else{

  flag_empty = 0

  data_import <- read_data(INPUT_DATA_DIR = input_dir, FILE_NAME = file,
                           DATETIME_HEADER = datetime_header, DATETIME_FORMAT = datetime_format,
                           DATA_FROM_ROW = data_from_row, HEADER_ROW_NUMBER = header_row_number)
  header = data_import [[1]]
  header_colnames = data_import [[2]]
  data = data_import [[3]]
  flag_error_df = data_import [[4]]

  rm(data_import)

  if(flag_error_df == 0){
    time_data = data[,which(colnames(data)==datetime_header)]

    if(is.na(start_date)){

      original <- data
      mydata <- data
      flag_date = 0

      rm(data)

    }else{

      if(as.POSIXct(start_date) < time_data[length(time_data)]){
        original = data[(which(time_data == as.POSIXct(start_date))+1):nrow(data),]      # possible issues in data subset!!! to check
        mydata = data[(which(time_data == as.POSIXct(start_date))+1):nrow(data),]

        flag_date = 0

        rm(data)

      } else {

        flag_date = 1
      }
    }


    if(flag_date == 0){
      deletes_duplcated <- deletes_duplcated_data(DATA = mydata,DATETIME_HEADER = datetime_header)         # <- Deletes identical rows if found
      mydata = deletes_duplcated [[1]]
      duplicated_data = deletes_duplcated [[2]]
      duplicated_data = time_to_char(DATA = duplicated_data, DATETIME_HEADER = datetime_header, DATETIME_FORMAT = datetime_format)

      rm(deletes_duplcated)

      if(unique(as.character(duplicated_data[1,])) == "---"){
        flag_duplicates_rows = 0
      } else{
        flag_duplicates_rows = 1
      }

      data_in_old_files <- deletes_old_datetime(DATA = mydata,DATETIME_HEADER = datetime_header)
      mydata = data_in_old_files [[1]]
      old_data = data_in_old_files[[2]]

      rm(data_in_old_files)

      overlap <- detect_overlap(DATA = mydata,DATETIME_HEADER = datetime_header, RECORD_HEADER = record_header)          # <- Detect overlap


      if(length(overlap) != 0){

        flag_overlap = 1
        overlap[,1]<- overlap[,1] + data_from_row - 1
        colnames(overlap)[1]= "File Row"

      }else{

        flag_overlap = 0

        missing  <- missing_dates(DATA = mydata, DATETIME_HEADER = datetime_header, RECORD_HEADER = record_header, DATETIME_SAMPLING = datetime_sampling)  # <- fill missing dates with NA
        mydata = missing[[1]]
        missing_index_date = missing[[2]]

        rm(missing)


        range <- exclude_out_of_range(DATA = mydata,DATETIME_HEADER = datetime_header, RANGE_DIR = range_dir, RANGE_FILE = range_file) # <- Substitute with NA data out of phisical range
        mydata = range[[1]]
        check_out_of_range = range[[2]]
        variable_new = range[[3]]
        variable_to_set = range[[4]]

        rm(range)
        # ..... Flags .....................................................................................................................................

        if(length(variable_to_set) != 0){
          flag_range_variable_to_set = 1
        }else{
          flag_range_variable_to_set = 0
        }

        if(length(variable_new) != 0){
          flag_range_variable_new = 1
        }else{
          flag_range_variable_new = 0
        }


        if(1 %in% unique(unlist(apply(X = check_out_of_range[,-which(colnames(check_out_of_range) == datetime_header)],MARGIN = 2, unique)))){
          flag_out_of_range = 1
        }else{
          if(-1 %in% unique(unlist(apply(X = check_out_of_range[,-which(colnames(check_out_of_range) == datetime_header)],MARGIN = 2, unique)))){
            flag_out_of_range = 1
          }else{
            flag_out_of_range = 0
          }
        }


        time_tot = as.POSIXct(mydata[,which(colnames(mydata) == datetime_header)], format = datetime_format, tz = 'Etc/GMT-1')
        time_missing = missing_index_date[,2]

        if(length(which(time_tot %in% time_missing )) == 0){
          flag_missing_dates = 0      # No missing dates
        }else{
          flag_missing_dates = 1      # YES missing dates
        }

        mydata <- time_to_char(DATA = mydata, DATETIME_HEADER = datetime_header, DATETIME_FORMAT = datetime_format)

      }
    }
  }
}



# ..... Output ..........................................................................................................................................

if(flag_empty == 0){
  if(flag_error_df == 0){
    if(flag_date == 0){
      if(flag_overlap == 0){
        if(write_output_files == TRUE){
          #~~~~~~~~~~
          colnames(header) = header[1,]

          out_my = mydata
          colnames(out_my) = colnames(header)

          out_mydata=rbind(header[-1,],out_my)

          out_date = paste(substring(mydata[nrow(mydata),which(colnames(mydata) == datetime_header)],1,4),
                           substring(mydata[nrow(mydata),which(colnames(mydata) == datetime_header)],6,7),
                           substring(mydata[nrow(mydata),which(colnames(mydata) == datetime_header)],9,10),
                           # "_",
                           substring(mydata[nrow(mydata),which(colnames(mydata) == datetime_header)],12,13),
                           substring(mydata[nrow(mydata),which(colnames(mydata) == datetime_header)],15,16),
                           sep = "")
          out_filename_data = paste("DQCok_",substring(file,1,nchar(file)-4),"_",out_date,".csv",sep = "")
          out_filename_dupli = paste("Duplicated_",substring(file,1,nchar(file)-4),"_",out_date,".csv",sep = "")

          output_dir_data_DQC_OK = paste(output_dir_data, "DQC_OK/",sep = "")
          ifelse(test = !dir.exists(output_dir_data_DQC_OK),yes = dir.create(output_dir_data_DQC_OK),no = FALSE)

          output_dir_data_duplicated = paste(output_dir_data, "Duplicated/",sep = "")
          ifelse(test = !dir.exists(output_dir_data_duplicated),yes = dir.create(output_dir_data_duplicated),no = FALSE)

          write.csv(out_mydata,paste(output_dir_data_DQC_OK,out_filename_data,sep = ""),quote = F,row.names = F, na = "NaN")

          rm(out_my)
          rm(out_mydata)
          #~~~~~~~~~~
          out_duplicated = duplicated_data
          colnames(out_duplicated) = colnames(header)

          out_duplicated_data=rbind(header[-1,],out_duplicated)
          write.csv(out_duplicated_data,paste(output_dir_data_duplicated,out_filename_dupli,sep = ""),quote = F,row.names = F, na = "NaN")
          rm(out_duplicated)
          rm(out_duplicated_data)
        }
      }
    }
  }
}


flags_names = c("flag_empty","flag_error_df","flag_date","flag_duplicates_rows","flag_overlap","flag_missing_dates","flag_range_variable_to_set","flag_range_variable_new","flag_out_of_range")
flags_df = data.frame(flags_names, rep(NA,times = length(flags_names)))
colnames(flags_df) = c("flag_names", "value")

for(i in 1: nrow(flags_df)){
  if(exists(flags_names[i])){
    flags_df$value[i] = eval(parse(text = flags_names[i]))
  }
}
