exclude_out_of_range = function(DATA, SUPPORT_DIR, RANGE_FILE,Rfunctions_dir){
  
  source(paste(Rfunctions_dir,"clean_data.R",sep = ""))
  
  range = read.csv(paste(SUPPORT_DIR, RANGE_FILE,sep = ""),stringsAsFactors = FALSE)          # <- import table that contains for each variable the permissible range
  range[,3] = as.numeric(range[,3])
  range[,4] = as.numeric(range[,4])
  new = DATA
  
  # This loop checks if variables in result are in the range list.
  # It could be a good index to see if there are issues in headers
  to_add = c() 
  for(i in 1:ncol(new)){
    if(colnames(new)[i] %in% range$Variable){}else{
      to_add = c(to_add, colnames(new)[i]) 
      writeLines(paste("Header:",colnames(new)[i],"does't exist in file: Range_settings.csv."))
      writeLines("Please add it or check for any typing mistakes and fix!")
      writeLines("Path:H:/Projekte/Klimawandel/Experiment/data/2order/Auto_data_quality_check/Support_files/")
    }
  }
  # Apply the function clean data to all variable in the "range list"
  # project=which(range$Project=="Klimawandel")
  
  too_low=list()
  too_high=list()
  
  for(i in 1:nrow(range)){          
    output_clean=clean_data(DATA = DATA, set_variable = range$Variable[i], min = as.numeric(range$min[i]), max = as.numeric(range$max[i]))
    # new_results=output_clean[[1]]
    new=output_clean[[1]]
    too_low[[i]]=output_clean[[2]]
    too_high[[i]]=output_clean[[3]]
  }
  
  names(too_low)=range$Variable
  names(too_high)=range$Variable
  
  # if(any(lengths(too_low)>0)){
  #   writeLines("parameter and number of data below min limits:")
  #   print(lengths(too_low[lengths(too_low)>0]))
  # }
  # 
  # if(any(lengths(too_high)>0)){
  #   writeLines("parameter and number of data above max limits:")
  #   print(lengths(too_high[lengths(too_high)>0]))
  # }
  
  return(new)
}

