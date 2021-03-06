#' This function delete duplcated data in a data.frame
#'
#' @param DATA data.frame
#' @param DATETIME_HEADER header corresponding to datetime

#'
#' @return A list containing a data.frame without duplicated rows and a dataframe with duplicated rows
#' @export
#' @examples
#' deletes_duplcated_data(DATA = mydata, DATETIME_HEADER = "TIMESTAMP")
#' deletes_duplcated_data(DATA = your data.frame, DATETIME_HEADER = "Your datetime header")
#'


deletes_duplcated_data = function(DATA, DATETIME_HEADER = "TIMESTAMP"){

  if(any(duplicated(DATA))){

    duplicated = DATA[duplicated(DATA),]

    duplicate_number = nrow(duplicated)

    datetime_duplicated = duplicated[,which(colnames(duplicated) == DATETIME_HEADER)]

    duplicate_start = duplicated[1,1]

    duplicate_end = duplicated[nrow(duplicated),1]

    n_duplicated = cbind(as.character(duplicate_start),as.character(duplicate_end),duplicate_number,round(duplicate_number/96,2),round(duplicate_number/4,2))

    colnames(n_duplicated) = c("Start_date","End_date","N_record","Days","Hours")

    # writeLines(paste("Duplicated rows:",duplicate_number))
    #
    # print(n_duplicated)
    #
    # writeLines('')

    DATA <- DATA[!duplicated(DATA),] # deletes identical rows

  }else{

    duplicated = data.frame(t(rep("---", times = ncol(DATA))))
    colnames(duplicated) = colnames(DATA)


  }

  output = list(DATA, duplicated)
  return(output)
}

