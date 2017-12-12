rm(list = ls())

# scheduling_dir =  "H:/Projekte/Klimawandel/Experiment/data/2order/DataQualityCheckEuracAlpEnv/Data/Input/" 
scheduling_dir =  "H:/Projekte/Klimawandel/Experiment/data/2order/DQC_BrC_test_data/"
report_dir =  "H:/Projekte/Klimawandel/Experiment/data/2order/DataQualityCheckEuracAlpEnv/Report/"
# output_dir =  "H:/Projekte/Klimawandel/Experiment/data/2order/DataQualityCheckEuracAlpEnv/Data/Output/" 
output_dir =  "H:/Projekte/Klimawandel/Experiment/data/2order/DQC_BrC_test_data/test_output/" 
support_dir =  "H:/Projekte/Klimawandel/Experiment/data/2order/DataQualityCheckEuracAlpEnv/Data/Support_files/"    
write_output =  "TRUE"
RANGE_FILE =  "Range.csv"
DATA_FROM_ROW =  5                                             # <-- Row number of first data
HEADER_ROW_NUMBER =  2                                         # <-- Row number of header
DATETIME_HEADER =  "TIMESTAMP"                                 # <-- header corresponding to TIMESTAMP
DATETIME_FORMAT =  "yyyy-mm-dd HH:MM"                          # <-- datetime format. Use only: y -> year, m -> month, d -> day, H -> hour, M -> minute
DATETIME_SAMPLING =  "15 min"
RECORD_HEADER =  "RECORD"    

files = dir(scheduling_dir,pattern = ".dat")


for(i in 1: length(files)){
  rmarkdown::render(input = paste(report_dir,"DQC_Manual_Multi_Files.Rmd",sep = ""),
                    output_file = paste("DQC_Report_",substring(files[i],1,nchar(files[1])-4),".html",sep = ""),
                    output_dir = paste(report_dir,"/Output_report/",sep = ""),
                    params = list(file = files[i],
                                  scheduling_dir = scheduling_dir,
                                  report_dir = report_dir,
                                  output_dir = output_dir ,
                                  support_dir = support_dir,
                                  write_output = write_output,
                                  RANGE_FILE = RANGE_FILE,
                                  DATA_FROM_ROW = DATA_FROM_ROW,
                                  HEADER_ROW_NUMBER = HEADER_ROW_NUMBER,
                                  DATETIME_HEADER = DATETIME_HEADER,
                                  DATETIME_FORMAT = DATETIME_FORMAT,
                                  DATETIME_SAMPLING = DATETIME_SAMPLING,
                                  RECORD_HEADER = RECORD_HEADER ))
}

i=1
rmarkdown::render(input = paste(report_dir,"DQC_Manual_Multi_Files.Rmd",sep = ""),
                  output_file = paste("DQC_Report_",substring(files[i],1,nchar(files[1])-4),".html",sep = ""),
                  output_dir = paste(report_dir,"/Output_report/",sep = ""),
                  params = "ask")
   