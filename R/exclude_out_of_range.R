#' This function reject all values greater than max and smaller than min in a data.frame
#'
#' @param DATA data.frame of timeseries
#' @param DATETIME_HEADER header of data corresponding to datetime
#' @param SUPPORT_DIR directory where support files are stored
#' @param RANGE_FILE name of filr where min/max thresholds are defined for each variable. Thi file is in SUPPORT_DIR
#'
#' @return A data.frame with all values in its physical range
#'
#' @export
#'
#' @examples
#' exclude_out_of_range(DATA = mydata, SUPPORT_DIR = support_dir, RANGE_FILE = "Range.csv")
#'

exclude_out_of_range = function(DATA,DATETIME_HEADER = "TIMESTAMP", SUPPORT_DIR, RANGE_FILE){

  range = read.csv(paste(SUPPORT_DIR, RANGE_FILE,sep = ""),stringsAsFactors = FALSE)          # <- import table that contains for each variable the permissible range
  range[,which(colnames(range) == "min")] = as.numeric(range[,which(colnames(range) == "min")])
  range[,which(colnames(range) == "max")] = as.numeric(range[,which(colnames(range) == "max")])

  range = range[order(range$Variable),] # reorder range file based on variable
  
  new = DATA # define new dataframe called new that is a copy of DATA

  new_status = new # create a dataframe with the same structure that DATA. Inside there is only 0. When data are out of range 0 is subsitute wiht -1 or 1
  new_status[,-which(colnames(new_status) == DATETIME_HEADER )] = 0

  # This loop checks if variables in result are in the range list.
  # It could be a good index to see if there are issues in headers
  to_add = c()
  
  for(k in 1:ncol(new)){
    if(colnames(new)[k] %in% range$Variable){
      w = which(range$Variable == colnames(new)[k])

      range$Variable[w]
      lower_limit = range$min[w]
      upper_limit = range$max[w]

      if(!is.na(lower_limit) & !is.na(upper_limit)){          # Exclude data without a range set

        new_status[,k] = ifelse(new[,k] < lower_limit, -1, new_status[,k])
        new_status[,k] = ifelse(new[,k] > upper_limit, 1, new_status[,k])

        new_status[is.na(new_status[,k]),k] = 0

        new[,k] = ifelse(new[,k] < lower_limit, NA, new[,k])
        new[,k] = ifelse(new[,k] > upper_limit, NA, new[,k])
      }

    }else{
      to_add = c(to_add, colnames(new)[k])
    }
  }
  
  df_to_add = data.frame(to_add, rep(-Inf, times=length(to_add)),rep(Inf, times=length(to_add)))
  colnames(df_to_add) = colnames(range)
  
  range = rbind(range,df_to_add)
  write.csv(range,paste(SUPPORT_DIR, RANGE_FILE,sep = ""),quote = F,row.names = F)

  out = list(new, new_status, to_add)

  return(out)
}




#
# w_names = which(range$Variable %in% colnames(new))
#
# unique(range$Variable[w_names]) == colnames(new)
# [order(colnames(new))]
#
# lower_limit = range$min[w_names]
# upper_limit = range$max[w_names]
#
# for(i in 1:ncol(new)){
#   if(!is.na(lower_limit[i]) & !is.na(upper_limit[i])){          # Exclude data without a range set
#     new[,i] = ifelse(new[,i] < lower_limit[i], NA, new[,i])
#     new[,i] = ifelse(new[,i] > upper_limit[i], NA, new[,i])
#   }
# }
#
#
#
# clean_data=function(DATA,STATUS_FILE, set_variable, min, max){
#
#   file_new_orig = DATA
#   file_new  = DATA
#   file_new_status = STATUS_FILE
#
#   # Exclude ***_Std***
#   head_set_var=set_variable
#   # head_no_std=head_set_var[!grepl("_Std", head_set_var)]
#
#
#   # apply thresholds
#   if( set_variable %in% colnames(new)){     # Return a message if the variable in headers aren't in the "range list"
#     if(!is.na(min) & !is.na(max)){          # Exclude data without a range set
#
#       column_filtered=which(colnames(file_new)==head_no_std[1])
#       #column_filtered_STD=which(colnames(new)==paste(substring(head_no_std[],1,nchar(head_no_std[1])-4),"_Std", sep = ""))
#       # if(length(column_filtered)!=0){                   # Every Variable out of dataset are not filtered
#
#       file_new[which(file_new_orig[,column_filtered]<min),column_filtered]=NA # substitute low data with NA
#       file_new[which(file_new_orig[,column_filtered]>max),column_filtered]=NA # substitute high data with NA
#
#       # new[which(new[,column_filtered]<min),column_filtered]=NA # substitute low data with NA
#       # new[which(new[,column_filtered]>max),column_filtered]=NA # substitute high data with NA
#
#       file_new_status[which(file_new_orig[,column_filtered]<min),column_filtered]= -1 # substitute low data with NA
#       file_new_status[which(file_new_orig[,column_filtered]>max),column_filtered]= 1 # substitute high data with NA
#
#       # new_status[which(new[,column_filtered]<min),column_filtered]= -1 # substitute low data with NA
#       # new_status[which(new[,column_filtered]>max),column_filtered]= 1 # substitute high data with NA
#
#
#       # out_min=DATA[which(DATA[!is.na(DATA[,column_filtered]),column_filtered]<(min)),1]  # dates of data out of range (min)
#       # out_max=DATA[which(DATA[!is.na(DATA[,column_filtered]),column_filtered]>(max)),1]  # dates of data out of range (max)
#
#       # } else{
#       #   out_min=character(0)
#       #   out_max=character(0)
#       # }
#
#       output=list(file_new,file_new_status)
#       names(output)=c("new","new_status")
#
#       return(output)
#
#     }else{
#
#       output=list(file_new, file_new_status)
#       names(output)=c("new","new_status")
#
#       # character(0),character(0),
#
#       return(output)
#     }
#   } else {
#     output=list(file_new, file_new_status)
#     names(output)=c("new","new_status")
#
#     # character(0),character(0)
#     return(output)
#
#   }
# }
#
# for(i in 1:nrow(range)){
#   output_clean=clean_data(DATA = new, STATUS_FILE = new_status, set_variable = range$Variable[i], min = as.numeric(range$min[i]), max = as.numeric(range$max[i]))
#   # new_results=output_clean[[1]]
#   new=output_clean[[1]]
#   # too_low[[i]]=output_clean[[2]]
#   # too_high[[i]]=output_clean[[3]]
#   new_status=output_clean[[2]]
# }
#
# names(too_low)=range$Variable
# names(too_high)=range$Variable
#
# # if(any(lengths(too_low)>0)){
# #   writeLines("parameter and number of data below min limits:")
# #   print(lengths(too_low[lengths(too_low)>0]))
# # }
# #
# # if(any(lengths(too_high)>0)){
# #   writeLines("parameter and number of data above max limits:")
# #   print(lengths(too_high[lengths(too_high)>0]))
# # }
# return(list(new, to_add))
# }
#



