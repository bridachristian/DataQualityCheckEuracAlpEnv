scheduling_dir <- "H:/Projekte/Klimawandel/Experiment/data/2order/DataQualityCheckEuracAlpEnv/Data/Input/"                   # <-- schelduling directory: for files to be processed
report_dir <- "H:/Projekte/Klimawandel/Experiment/data/2order/DataQualityCheckEuracAlpEnv/Report/"                   # <-- schelduling directory: for files to be processed

files = dir(scheduling_dir,pattern = ".dat")

for(i in 1: length(files)){
  rmarkdown::render(input = paste(report_dir,"DQC_Manual_Multi_Files.Rmd",sep = ""),
                    output_file = paste("DQC_Report_",substring(files[i],1,nchar(files[1])-4),".html",sep = ""),
                    output_dir =  "H:/Projekte/Klimawandel/Experiment/data/2order/DataQualityCheckEuracAlpEnv/Report/",
                    params = list(file = files[i]))
}