rm(list = ls())
# library(readxl)
# library(plyr)

#--- input section ---

path_input_folder = "/shared/loggernet/data_quality_check_test/Database/total_files/B1_new/" # Folder where stations data are storaged

print("Select one of this Input File: ")
dir(path_input_folder, pattern = ".csv")

input_file = "B1_new_201611140615_201612312345.csv"  # File (organized by station) to split in different files per sensors (one sensor could measure one or more parameter)
feature_of_interest="B1mes1000" # Feature of interest of station

# station_parameters_folder = "/shared/loggernet/data_quality_check_test/Database/config_stations/config_files/" # Excel table where the connection between database paramters and station parameters are specified
config_folder = "/shared/loggernet/data_quality_check_test/Database/config_stations/config_files/" # Excel table where the connection between database paramters and station parameters are specified

data_template_folder = "/shared/loggernet/data_quality_check_test/Database/config_stations/template_out_files/" # Folder where there are some template of data for each sensor model

data_from_row =  5                                             # <-- Row number of first data
header_row_number =  2                                         # <-- Row number of header
datetime_header =  "TIMESTAMP"                                 # <-- header corresponding to TIMESTAMP
datetime_format =  "%Y-%m-%d %H:%M"                          # <-- datetime format. Use only: Y -> year, m -> month, d -> day, H -> hour, M -> minute
datetime_sampling =  "15 min"
record_header =  "RECORD"

output_folder = "/shared/loggernet/data_quality_check_test/Database/splitted_files/" # Folder to storage data splitted by sensors.

# backup_input_folder = "H:/Projekte/LTER/11_DataBase/LTER_test/Input/Uploaded/" # Folder where stations data are storaged as backup


# Attention! Input data should have 4 rows before the first data recorded: 
#                   1. Metadata of logger and some other information (TOA5, CR1000, ...)
#                   2. Name of parameter measured (T_Air, SWC_WC_05, ...)
#                   3. Units (Volts, Deg C, %, ...)
#                   4. Type of measure (Smp, Avg, Tot, Std ...)
#---------------------


#--- controllare QUI che header del file di config e header dati siano uguali ---
config_file = read.csv(paste(config_folder,substring(input_file,1, nchar(input_file)-30), ".csv", sep = ""),header = F,stringsAsFactors = F,na.strings = c(NA, "NaN"))
config_file_header = config_file[1:(data_from_row-1),]

new_row = paste(config_file[7,],config_file[6,],config_file[8,],sep = "-")
new_row[which(config_file[8,] == "")] = NA
new_row[which(config_file[2,] == datetime_header)] = datetime_header

# file_conf = rbind(config_file, new_row)

#--- read data and metadata ---

data_to_split = read.csv(paste(path_input_folder,input_file,sep = ""), header = F, stringsAsFactors = F)
header = data_to_split[1:(data_from_row-1),]
header_colnames = data_to_split[header_row_number,]
data_to_split_new = data_to_split[-c(1:(data_from_row-1)),]
colnames(data_to_split_new) = header_colnames


#------------------------------

if(identical(header, config_file[1:(data_from_row-1),])){  # split data only headers of data are identical with configuration files. Otherwise update configuration files
  
  # ---prepare data to split ---
  data = data_to_split_new
  colnames(data) = new_row
  data = data[,!is.na(new_row)]
  data = data[,-which(colnames(data) == datetime_header)]
  
  # --- prepare TIMESTAMP ---
  TIMESTAMP = data_to_split_new[,which(colnames(data_to_split_new) == datetime_header)] # extract date and time from input data
  
  
  
  
  #--- assign station_category and sensor ID ---
  
  st_cat=unique(substring(colnames(data),1,4)) # extract from column names the station category (id_model)
  
  r=regexpr("-",substring(colnames(data),6,nchar(colnames(data))))
  
  EuracID=unique(substring(colnames(data),6,r+4))  # extract from column names the station category (id_model)
  
  template=dir(data_template_folder,pattern = ".csv") # show all files available in folder Template_data
  
  i=1 # initialize
  j=1 # initialize
  k=1 # initialize
  
  for( j in 1:length(EuracID)){
    
    # categ=unique(file_sensors$id_model[file_sensors$EuracID==EuracID[j]]) # create a vector containing the id_model (~ station_category)  
    categ= unique(as.character(config_file[7,which(config_file[6,]==EuracID[j])]))
    
    # for( i in 1:length(st_cat)){
    for( i in 1:length(categ)){
      rm(template_data) # initialize
      rm(data_new)      # initialize
      w = which(categ[i]==substring(template,1,4)) # index of file (in template data) to load
      
      template_data = read.csv(paste(data_template_folder,template[w],sep = ""),nrows = 1,header = T,stringsAsFactors = F) #import the template data selected with id_model
      cn=colnames(template_data) # extract colnames
      data_new = matrix(data = NA,nrow = length(TIMESTAMP),ncol = length(cn)) # create empty matrix
      data_new=as.data.frame(data_new) # convert matrix in a dataframe
      # colnames(data_new)=cn # assign colnames as in template_data
      colnames(data_new)=paste(EuracID[j],cn,sep = "-") # assign colnames as in template_data
      
      rr=regexpr("-",substring(colnames(data),6,nchar(colnames(data))))
      v = which(EuracID[j]==substring(colnames(data),6,rr+4)) # select one sensor
      # var = substring(colnames(data)[v],rr[v]+6,nchar(colnames(data)[v])) # find the parameter measured (as in database)
      var = substring(colnames(data)[v],rr[v]+6,nchar(colnames(data)[v])) # find the parameter measured (as in database)
      
      
      for( k in 1:length(var)){
        t = which(colnames(template_data) == var[k]) # find the column in template data corresponding to parameter measured
        data_new[,1]=TIMESTAMP # assing to the first column the date and time in the new format
        
        rrr=regexpr("-",substring(colnames(data),6,nchar(colnames(data))))
        
        s=which(substring(colnames(data),6,nchar(colnames(data)))== paste(EuracID[j],var[k],sep = "-"))  # find the column in data corresponding to variable selected rrr+6
        data_new[,t]=data[,s] # paste the data of variable selected in the proper place
        
      }
      colnames(data_new)=cn
      name_output=paste(EuracID[j],"_",unique(as.character(config_file[5,which(config_file[6,]==EuracID[j])])),"_",
                        substring(TIMESTAMP[1],1,4),substring(TIMESTAMP[1],6,7),substring(TIMESTAMP[1],9,10),
                        substring(TIMESTAMP[1],12,13),substring(TIMESTAMP[1],15,16),"-",
                        substring(TIMESTAMP[length(TIMESTAMP)],1,4),substring(TIMESTAMP[length(TIMESTAMP)],6,7),substring(TIMESTAMP[length(TIMESTAMP)],9,10),
                        substring(TIMESTAMP[length(TIMESTAMP)],12,13),substring(TIMESTAMP[length(TIMESTAMP)],15,16),sep = "") # write name of output file
      
      
      
      write.csv(data_new, paste(output_folder,name_output,".csv",sep = ""),na = "-999999",row.names = F,quote = F) # write output
    }
  }
}

# file.rename(from = paste(path_input_folder, input_file, sep = ""), to=paste(backup_input_folder, input_file, sep = ""))

