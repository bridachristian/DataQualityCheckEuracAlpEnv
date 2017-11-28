# This function check the file size of input file
#
# @param SCHEDULING_DIR Directory where input file is stored
# @param FILE File name (.dat)
# @export
# @examples
# check_empty_files(SCHEDULING_DIR = "~/Data/Input/",FILE = "M4s.dat")
# check_empty_files(SCHEDULING_DIR = "Your input folder",FILE = "Your file to check")

check_empty_files = function(SCHEDULING_DIR,FILE){

  info <- file.info(paste(SCHEDULING_DIR,FILE, sep = ""))
  if(info$size == 0){
    empty <- TRUE
  }else{
    empty <- FALSE
  }
  return(empty)
}
