#' This function delete duplcated data in a data.frame
#'
#' @param DATA data.frame
#'
#' @return A data.frame without duplicated rows
#' @export
#' @examples
#' deletes_duplcated_data(DATA = mydata)
#' deletes_duplcated_data(DATA = your data.framel)
#'


deletes_duplcated_data = function(DATA){

  if(any(duplicated(DATA))){

    duplicated=DATA[duplicated(DATA),]

    duplicate_number=nrow(duplicated)

    duplicate_start=duplicated[1,1]

    duplicate_end=duplicated[nrow(duplicated),1]

    n_duplicated=cbind(as.character(duplicate_start),as.character(duplicate_end),duplicate_number,round(duplicate_number/96,2),round(duplicate_number/4,2))

    colnames(n_duplicated)=c("Start_date","End_date","N_record","Days","Hours")

    writeLines(paste("Duplicated rows:",duplicate_number))

    print(n_duplicated)

    writeLines('')

    DATA <- DATA[!duplicated(DATA),] # deletes identical rows

  }
  return(DATA)
}

