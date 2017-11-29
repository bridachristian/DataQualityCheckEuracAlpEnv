#' This function explore the file in a specific folder
#'
#' @param SCHEDULING_DIR directory where files are stored
#'
#' @return a vector of files available in SCHEDULING_DIR
#' 
#' @export
#' 
#' @examples
#' files_in_scheduling_dir(SCHEDULING_DIR = scheduling_dir)
#' files_in_scheduling_dir(SCHEDULING_DIR = scheduling_dir)
#'

files_in_scheduling_dir = function(SCHEDULING_DIR = scheduling_dir){
  files<- list.files(path = SCHEDULING_DIR,
                     pattern = "*.dat",full.names = F,recursive = F)
  # if(any(grepl(pattern = "backup",x = files))){
  #   files<-files[-c(grep("backup",files))]
  # }
  return(files)
}