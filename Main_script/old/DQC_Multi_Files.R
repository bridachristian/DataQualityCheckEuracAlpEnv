#-------------------------------------------------------------------------------------------------------------------------------------------------------
# File Title:   DQC_Multi_Files.R
# TITLE:        Data quality check LTER on different files in scheduling folder
# Author:       Brida Christian, Genova Giulio, Zandonai Alessandro
#               Institute for Alpine Environment
# Data:         13/02/2018
# Version:      2.0
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
# .....................................................................................................................................................

# ..... Params section .....................................................................................................................................

input_dir <- "H:/Projekte/Klimawandel/Experiment/data/2order/DQC/DataQualityCheck_results/Input/"                # where input files are
# input_dir <- "H:/Projekte/Klimawandel/Experiment/data/2order/DataQualityCheckEuracAlpEnv/Data/Input/"                # where input files are
data_output_dir <- "H:/Projekte/Klimawandel/Experiment/data/2order/DQC/DataQualityCheck_results/Output/Data/"   # where to put output files
# output_dir_data <- "H:/Projekte/Klimawandel/Experiment/data/2order/DataQualityCheckEuracAlpEnv/Data/Output/data/"   # where to put output files
report_output_dir <- "H:/Projekte/Klimawandel/Experiment/data/2order/DQC/DataQualityCheck_results/Output/Report/"   # where to put output reports
# report_output_dir <- "H:/Projekte/Klimawandel/Experiment/data/2order/DataQualityCheckEuracAlpEnv/Data/Output/report/"   # where to put output reports
project_dir <- "H:/Projekte/Klimawandel/Experiment/data/2order/DQC/DataQualityCheckEuracAlpEnv/"  # where package is developed or cloned from github

# input_dir <- "C:/Users/azandonai/Desktop/Data/DQC/in/"                # where input files are
# data_output_dir <- "C:/Users/azandonai/Desktop/Data/DQC/out/"   # where to put output files
# report_output_dir <- "C:/Users/azandonai/Desktop/Data/DQC/out/"   # where to put output reports
# project_dir <- "H:/Projekte/Klimawandel/Experiment/data/2order/DQC/DataQualityCheckEuracAlpEnv/"  # where package is developed or cloned from github
# range_dir <- "C:/Users/azandonai/Desktop/Data/DQC/process/"
# download_table_dir <- "C:/Users/azandonai/Desktop/Data/DQC/process/"

data_from_row =  5                                             # <-- Row number of first data
header_row_number =  2                                         # <-- Row number of header
datetime_header =  "TIMESTAMP"                                 # <-- header corresponding to TIMESTAMP
# datetime_format =  "yyyy-mm-dd HH:MM"                          # <-- datetime format. Use only: y -> year, m -> month, d -> day, H -> hour, M -> minute
datetime_format =  "%Y-%m-%d %H:%M"                          # <-- datetime format. Use only: Y -> year, m -> month, d -> day, H -> hour, M -> minute
datetime_sampling =  "15 min"
record_header =  "RECORD"
range_file =  "Range.csv"

write_output_files =  "TRUE"
write_output_report =  "TRUE"


# file <- "M4s.dat"
# start_date <- NA

# ~~~ Default directory ~~~~


range_dir <-"H:/Projekte/Klimawandel/Experiment/data/2order/DQC/DataQualityCheck_results/Process/"
# download_table_dir <- paste(project_dir, "Data/Support_files/Download_table/",sep = "")
download_table_dir <- "H:/Projekte/Klimawandel/Experiment/data/2order/DQC/DataQualityCheck_results/Process/"
Rmd_report_generator <- paste(project_dir, "Rmd/DQC_Report_Generator.Rmd",sep = "")

# ..........................................................................................................................................................

# ..... files selection .....................................................................................................................................

files_available = dir(input_dir,pattern = ".dat")                  # <-- Admitted pattern:  ".dat" or ".csv"

# ..........................................................................................................................................................

# ..... download table section .....................................................................................................................................


download_table = read_and_update_download_table(DOWNLOAD_TABLE_DIR = download_table_dir, FILES_AVAILABLE = files_available, DATETIME_FORMAT = datetime_format)


############################################
t = 1

# final_dataframe = data.frame(t(rep(NA, times = 14)))
final_dataframe = matrix(ncol = 14, nrow = length(files_available))

colnames(final_dataframe) = c("Station", "Status",
                              "flag_empty","flag_error_df","flag_date",
                              "flag_duplicates_rows","flag_overlap","flag_missing_dates",
                              "flag_range_variable_to_set","flag_range_variable_new","flag_out_of_range", "Report_link", "Data_folder", "File_name")



report_start = as.POSIXct(Sys.time(), tz = "Etc/GMT-1")


for(t in  1: length(files_available)){
  gc(reset = T)

  rm(list = setdiff(ls(all.names = TRUE),c("tf","t","data_from_row","datetime_format","datetime_header","datetime_sampling","download_table","download_table_dir",
                                           "files_available","header_row_number","input_dir","data_output_dir","report_output_dir","project_dir",
                                           "range_dir","range_file","record_header","Rmd_report_generator","write_output_files","write_output_report",
                                           "report_start", "final_dataframe","output_dir_report")))


  FILE = files_available[t]

  cat(paste("********* a. File to process:", FILE, "*********"),sep = "\n")

  w_dwnl = which(download_table$Station == substring(FILE, 1, nchar(FILE) - 4))
  dwnl_info = download_table[w_dwnl,]

  # if(dir.exists(paste(data_output_dir,substring(FILE,1,nchar(FILE)-4),"/", sep = ""))){                # create subfolder to store data organized by station name
  #   output_dir_data_new = paste(data_output_dir,substring(FILE,1,nchar(FILE)-4),"/", sep = "")
  # }else{
  #   dir.create(paste(data_output_dir,substring(FILE,1,nchar(FILE)-4),"/", sep = ""))
  #   output_dir_data_new = paste(data_output_dir,substring(FILE,1,nchar(FILE)-4),"/", sep = "")
  # }


  if(dwnl_info$Stop_DQC == 0){

    date_last_modif_file = as.character(format(file.mtime(paste(input_dir,FILE,sep = "")),format = datetime_format))

    if(date_last_modif_file != dwnl_info$Last_Modification | is.na(dwnl_info$Last_Modification)){

      input_dir = input_dir
      output_dir_data = data_output_dir
      output_dir_report = report_output_dir
      project_dir = project_dir
      data_from_row = data_from_row
      header_row_number = header_row_number
      datetime_header = datetime_header
      datetime_format = datetime_format
      datetime_sampling = datetime_sampling
      record_header = record_header
      range_file = range_file
      write_output_files = write_output_files
      write_output_report = write_output_report

      file = FILE
      start_date = dwnl_info$Last_date

      rm(dwnl_info)

      output_file_report = paste("DQC_Report_",substring(FILE,1,nchar(FILE)-4),"_tmp.html",sep = "")

      cat(paste("********* b. File under processing:", FILE, "*********"),sep = "\n")

      rmarkdown::render(input = Rmd_report_generator ,
                        output_file = output_file_report,
                        output_dir = output_dir_report,
                        params = list(input_dir = input_dir ,
                                      output_dir_data = output_dir_data ,
                                      output_dir_report = output_dir_report ,
                                      project_dir = project_dir ,
                                      data_from_row = data_from_row ,
                                      header_row_number = header_row_number ,
                                      datetime_header = datetime_header ,
                                      datetime_format = datetime_format ,
                                      datetime_sampling = datetime_sampling ,
                                      record_header = record_header ,
                                      range_file = range_file ,
                                      write_output_files = write_output_files ,
                                      write_output_report = write_output_report ,
                                      file = file ,
                                      start_date = start_date))

      gc(reset = T)

      if(flag_empty == 0 & flag_error_df == 0){
        out_filename_date = paste(substring(mydata[nrow(mydata),which(colnames(mydata) == datetime_header)],1,4),
                                  substring(mydata[nrow(mydata),which(colnames(mydata) == datetime_header)],6,7),
                                  substring(mydata[nrow(mydata),which(colnames(mydata) == datetime_header)],9,10),
                                  # "_",
                                  # substring(mydata[nrow(mydata),which(colnames(mydata) == datetime_header)],12,13),
                                  # substring(mydata[nrow(mydata),which(colnames(mydata) == datetime_header)],15,16),
                                  sep = "")

        last_date = mydata[nrow(mydata),which(colnames(mydata)== datetime_header)]

      } else {
        out_filename_date = "no_datetime"
      }


      out_filename_report = paste("DQC_Report_",substring(FILE,1,nchar(FILE)-4),"_",out_filename_date,".html",sep = "")

      if(file.exists(paste(output_dir_report,out_filename_report,sep = ""))){

        j=0
        repeat{
          j=j+1
          out_filename_report_new = paste(substring(out_filename_report,1, nchar(out_filename_report)-5),"_",j,".html",sep = "")
          if(!file.exists(paste(output_dir_report,out_filename_report_new,sep = ""))){
            break
          }
        }
      } else {
        out_filename_report_new = out_filename_report
      }

      out_filename_report = out_filename_report_new
      output_file_report = file.rename(from = paste(output_dir_report,output_file_report,sep = ""),
                                       to = paste(output_dir_report,out_filename_report,sep = ""))





      if(!is.na(flag_missing_dates)){
        download_table$Last_date[w_dwnl] = last_date
        download_table$Last_Modification[w_dwnl] = date_last_modif_file
        write.csv(download_table,paste(download_table_dir,"download_table.csv",sep = ""),quote = F,row.names = F)
        # file_ok = c(file_ok,FILE)

        final_info = c(substring(FILE,1,nchar(FILE)-4), "Analyzed and write output",
                       flags_df$value,
                       paste(output_dir_report,out_filename_report,sep = ""),
                       paste(output_dir_data,sep = ""),
                       # paste(substring(FILE,1,nchar(FILE)-4),"_",out_filename_date, ".csv",sep = ""))
                       paste(out_filename_data_new,sep = ""))
      }else{
        # file_stopped = c(file_stopped, FILE)

        final_info = c(substring(FILE,1,nchar(FILE)-4), "Analyzed with errors",
                       flags_df$value,
                       paste(output_dir_report,out_filename_report,sep = ""),
                       NA, NA )

      }

    } else {
      warning(paste("File",FILE, "already analyzed!"))
      # file_already_processed = c(file_already_processed,FILE)
      final_info = c(substring(FILE,1,nchar(FILE)-4), "Already analyzed",
                     NA, NA, NA, NA, NA, NA, NA, NA, NA,
                     NA,
                     NA, NA)
    }

  }else{
    final_info = c(substring(FILE,1,nchar(FILE)-4), "Not analyzed",
                   NA, NA, NA, NA, NA, NA, NA, NA, NA,
                   NA,
                   NA, NA)
  }

  # final_dataframe = rbind(final_dataframe,final_info)
  final_dataframe[t,] = final_info

  gc(reset = T)
  cat(paste("********* c.", FILE, "analyzed! *********"),sep = "\n")

}



input_final = paste(project_dir,"Rmd/DQC_Final_Report.Rmd",sep = "")
output_file_final =  paste("DQC_Report_",substring(report_start,1,4),
                           substring(report_start,6,7),
                           substring(report_start,9,10),
                           # substring(report_start,12,13),
                           # substring(report_start,15,16),
                           ".html", sep = "")
# output_dir_final = output_dir_report
output_dir_final = report_output_dir

if(file.exists(paste(output_dir_final,output_file_final,sep = ""))){
  j=0
  repeat{
    j=j+1
    output_file_final_new = paste(substring(output_file_final,1, nchar(output_file_final)-5),"_",j,".html",sep = "")
    if(!file.exists(paste(output_dir_final,output_file_final_new,sep = ""))){
      break
    }
  }
} else {
  output_file_final_new = output_file_final
}

rmarkdown::render(input = input_final,
                  output_file = output_file_final_new ,
                  output_dir = output_dir_final,
                  params = list(report_start = report_start ,
                                final_dataframe = final_dataframe))





