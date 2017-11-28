exclude_out_of_range = function(DATA, SUPPORT_DIR, RANGE_FILE){

  # source(paste(Rfunctions_dir,"clean_data.R",sep = ""))

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

  clean_data=function(DATA, set_variable, min, max){

    # Exclude ***_Std***
    head_set_var=set_variable
    head_no_std=head_set_var[!grepl("_Std", head_set_var)]

    # apply thresholds
    if( set_variable %in% colnames(new)){     # Return a message if the variable in headers aren't in the "range list"
      if(!is.na(min) & !is.na(max)){          # Exclude data without a range set
        column_filtered=which(colnames(new)==head_no_std[1])
        #column_filtered_STD=which(colnames(new)==paste(substring(head_no_std[],1,nchar(head_no_std[1])-4),"_Std", sep = ""))
        # if(length(column_filtered)!=0){                   # Every Variable out of dataset are not filtered

        new[which(new[,column_filtered]<min),column_filtered]=NA # substitute low data with NA
        new[which(new[,column_filtered]>max),column_filtered]=NA # substitute high data with NA

        #new[new[!is.na(new[,column_filtered]),column_filtered]<(min),column_filtered_STD]=NA  # fix _Std according with new data
        #new[new[!is.na(new[,column_filtered]),column_filtered]>(max),column_filtered_STD]=NA # WRONG SCRIPTING!!!

        out_min=DATA[which(DATA[!is.na(DATA[,column_filtered]),column_filtered]<(min)),1]  # dates of data out of range (min)
        out_max=DATA[which(DATA[!is.na(DATA[,column_filtered]),column_filtered]>(max)),1]  # dates of data out of range (max)

        # } else{
        #   out_min=character(0)
        #   out_max=character(0)
        # }

        output=list(new,out_min,out_max)
        names(output)=c("new","min_out","max_out")

        return(output)

      }else{

        output=list(new,character(0),character(0))

        return(output)
      }
    } else {
      output=list(new,character(0),character(0))
      return(output)

    }
  }

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




