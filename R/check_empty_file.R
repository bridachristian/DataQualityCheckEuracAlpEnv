#' This function check the file size of input file
#'
#' @param INPUT_DATA_DIR Directory where input file is stored
#' @param FILE_NAME File name (.dat)
#'
#' @return A logig value that indicates if the file is empty or not
#' @export
#' @examples
#' check_empty_file(INPUT_DATA_DIR = "~/Data/Input/",FILE = "M4s.dat")
#' check_empty_file(INPUT_DATA_DIR = "Your input folder",FILE = "Your file to check")

check_empty_file = function(INPUT_DATA_DIR,FILE_NAME){

  info <- file.info(paste(INPUT_DATA_DIR,FILE_NAME, sep = ""))
  if(info$size == 0){
    empty <- TRUE
  }else{
    empty <- FALSE
  }


  gc(reset = T)
  return(empty)
}
