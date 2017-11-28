#' This function check substitue data out of admitted range with NA
#'
#' @param DATA data.frame that contians timeseries to filter
#' @param set_variable  variable selected to apply thresholds
#' @param min minimum limit. Data smaller than min are substitute with NA
#' @param max maximum limit. Data larger than min are substitute with NA
#'
#' @export
#' @examples
#' clean_data(DATA = mydataframe ,set_variable = "Air_T", min = -50 , max = 50 )
#' clean_data(DATA = your data.frame ,set_variable = "Your variable", min = your lower limit  , max = your upper limit )
#'

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
