#-------------------------------------------------------------------------------------------------------------------------------------------------------
# File Title:   DQC_Split_Database_Linux.R
# TITLE:        Splitter of data for database structure
# Author:       Brida Christian, Genova Giulio, Zandonai Alessandro
#               Institute for Alpine Environment
# Data:         29/03/2018
# Version:      1.0
#
#------------------------------------------------------------------------------------------------------------------------------------------------------

rm(list = ls())
cat()
cat(paste("--- DQC_Split_Database_Linux.R start:",Sys.time(),"----------------------------------------------------------"), sep = "\n")

database_dir = "/shared/loggernet/data_quality_check_test/Database/"

config_file_dir = paste(database_dir,"config_stations/config_files/", sep = "")
total_file_dir = paste(database_dir,"total_files/", sep = "")
data_template_dir = paste(database_dir,"config_stations/template_out_files/", sep = "")
output_dir = paste(database_dir,"splitted_files/", sep = "")
delete_total_file_dir = paste(database_dir,"delete/", sep = "")


data_from_row =  5                                             # <-- Row number of first data
header_row_number =  2                                         # <-- Row number of header
datetime_header =  "TIMESTAMP"                                 # <-- header corresponding to TIMESTAMP
datetime_format =  "%Y-%m-%d %H:%M"                          # <-- datetime format. Use only: Y -> year, m -> month, d -> day, H -> hour, M -> minute
DB_datetime_format =  "%Y-%m-%dT%H:%M"                          # <-- datetime format. Use only: Y -> year, m -> month, d -> day, H -> hour, M -> minute
datetime_sampling =  "15 min"
record_header =  "RECORD"

configuration_files = substring(dir(config_file_dir, pattern = ".csv"), 1,nchar(dir(config_file_dir, pattern = ".csv"))-4)

tt = 1
for(tt in 1:length(configuration_files)){
  cat(paste("***** Start splitting wiht:", configuration_files[tt], "*****"),sep = "\n")
  path_input_folder = paste(total_file_dir, configuration_files[tt],"/", sep = "")# "/shared/loggernet/data_quality_check_test/Database/total_files/B1_new/"# Folder where stations data are storaged
  files_available = dir(path_input_folder, pattern = ".csv")
  
  input_file = files_available[1]
  for(input_file in files_available){
    
    #--- read and update header columns in configuration files ---
    config_file = read.csv(paste(config_file_dir,substring(input_file,1, nchar(input_file)-30), ".csv", sep = ""),header = F,stringsAsFactors = F,na.strings = c(NA, "NaN"))
    config_file_header = config_file[1:(data_from_row-1),]
    
    new_row = paste(config_file[data_from_row+2,],config_file[data_from_row+1,],config_file[data_from_row+3,],sep = "-")
    new_row[which(config_file[data_from_row+3,] == "")] = NA
    new_row[which(config_file[header_row_number,] == datetime_header)] = datetime_header
    
    
    #--- read data and metadata ---
    
    data_to_split = read.csv(paste(path_input_folder,input_file,sep = ""), header = F, stringsAsFactors = F,na.strings = c(NA, "NaN"))
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
      TIMESTAMP = as.POSIXct(strptime(TIMESTAMP,format = datetime_format),tz = "Etc/GMT-1")
      TIMESTAMP_new = format(TIMESTAMP,format = DB_datetime_format)

      #--- assign station_category and sensor ID ---
      
      st_cat=unique(substring(colnames(data),1,4)) # extract from column names the station category (id_model)
      
      r=regexpr("-",substring(colnames(data),6,nchar(colnames(data))))
      
      EuracID=unique(substring(colnames(data),6,r+4))  # extract from column names the station category (id_model)
      
      template=dir(data_template_dir,pattern = ".csv") # show all files available in folder Template_data
      
      i=1 # initialize
      j=1 # initialize
      k=1 # initialize
      
      for( j in 1:length(EuracID)){
        
        # categ=unique(file_sensors$id_model[file_sensors$EuracID==EuracID[j]]) # create a vector containing the id_model (~ station_category)  
        categ = unique(as.character(config_file[data_from_row+2,which(config_file[data_from_row+1,]==EuracID[j])]))
        
        
        # for( i in 1:length(st_cat)){
        for( i in 1:length(categ)){
          rm(template_data) # initialize
          rm(data_new)      # initialize
          w = which(categ[i]==substring(template,1,4)) # index of file (in template data) to load
          
          categ_name = substring(template[w], 6, nchar(template[w])-4)
          
          template_data = read.csv(paste(data_template_dir,template[w],sep = ""),nrows = 1,header = T,stringsAsFactors = F,na.strings = c(NA, "NaN")) #import the template data selected with id_model
          cn=colnames(template_data) # extract colnames
          data_new = matrix(data = NA,nrow = length(TIMESTAMP_new),ncol = length(cn)) # create empty matrix
          data_new=as.data.frame(data_new) # convert matrix in a dataframe
          # colnames(data_new)=cn # assign colnames as in template_data
          colnames(data_new)=paste(EuracID[j],cn,sep = "-") # assign colnames as in template_data
          
          rr=regexpr("-",substring(colnames(data),6,nchar(colnames(data))))
          v = which(EuracID[j]==substring(colnames(data),6,rr+4)) # select one sensor
          var = substring(colnames(data)[v],rr[v]+6,nchar(colnames(data)[v])) # find the parameter measured (as in database)
          
          
          for( k in 1:length(var)){
            t = which(colnames(template_data) == var[k]) # find the column in template data corresponding to parameter measured
            data_new[,1]=TIMESTAMP_new # assing to the first column the date and time in the new format
            
            rrr=regexpr("-",substring(colnames(data),6,nchar(colnames(data))))
            
            s=which(substring(colnames(data),6,nchar(colnames(data)))== paste(EuracID[j],var[k],sep = "-"))  # find the column in data corresponding to variable selected rrr+6
            data_new[,t]=data[,s] # paste the data of variable selected in the proper place
            
          }
          colnames(data_new)=cn
          name_output=paste(EuracID[j],"_",unique(as.character(config_file[data_from_row,which(config_file[data_from_row+1,]==EuracID[j])])),"_",
                            substring(TIMESTAMP_new[1],1,4),substring(TIMESTAMP_new[1],6,7),substring(TIMESTAMP_new[1],9,10),
                            substring(TIMESTAMP_new[1],12,13),substring(TIMESTAMP_new[1],15,16),"-",
                            substring(TIMESTAMP_new[length(TIMESTAMP_new)],1,4),substring(TIMESTAMP_new[length(TIMESTAMP_new)],6,7),substring(TIMESTAMP_new[length(TIMESTAMP_new)],9,10),
                            substring(TIMESTAMP_new[length(TIMESTAMP_new)],12,13),substring(TIMESTAMP_new[length(TIMESTAMP_new)],15,16),sep = "") # write name of output file
          
          if(dir.exists(paste(output_dir, categ_name,"/",EuracID[j],"/",sep = ""))){
            output_path = paste(output_dir, categ_name,"/",EuracID[j],"/",sep = "")
          }else{
            dir.create(paste(output_dir, categ_name,"/",EuracID[j],"/",sep = ""))
            output_path = paste(output_dir, categ_name,"/",EuracID[j],"/",sep = "")
            
          }
          # data_new[,which(colnames(data_new) == datetime_header)] = paste 
          write.csv(data_new, paste(output_path,name_output,".csv",sep = ""),na = "-999999",row.names = F,quote = F) # write output
          
        }
      }
    }
    
    file.rename(from = paste(path_input_folder, input_file, sep = ""), to=paste(delete_total_file_dir, input_file, sep = "")) # remove data processed from scheduling folder (to delete in the future)
    }
  cat(paste("*****", configuration_files[tt], "completed! *****"),sep = "\n")
  
  
}

cat(paste("----- Finish -----"), sep = "\n")
cat(sep = "\n" )
