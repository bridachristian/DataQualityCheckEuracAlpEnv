#' This function check pictures corruption. If the lower half of the picture is on the same color (= identical value) the picture is classified corrupted
#' 
#' @param pics The extended path and file name of the picture
#' 
#' @return A logical value that indicate if the pic is corrupted or not (corrupted == TRUE) 

corr_pics = function(pics){
  pname = pics
  corr = c()
  i=1
  # for(i in 1: length(pname)){
  
  possibleError <- tryCatch(    # catch the possible error due  to wrong jpeg format (image corrupted)
    expr ={
      readJPEG(pname[i],native = F)
    },
    error = function(e) e
  )
  if(inherits(possibleError, "error")){
    err = TRUE      # <- if there is an error err = TRUE , if no error in readJPEG  err = FALSE
  }else{
    err = FALSE
  }
  
  
  if(err == FALSE){  # <- if no error in readJPEG  err = FALSE read and check pics color, else the pics is cataloged as corrutped
    # p = readJPEG(pname[i],native = F)
    p = readJPEG(pname,native = F)
    p2 = as.data.frame(p)
    p3 = p2[(nrow(p2)/2):nrow(p2),]
    
    a_r = apply(X = p3,MARGIN = 1,FUN = unique)
    if(length(unique(a_r)) == 1){
      # corr[i] = TRUE
      corr = TRUE
    }else{
      # corr[i] = FALSE
      corr = FALSE
    }
  }else{
    # corr[i] = TRUE
    corr = TRUE
  }
  
  
  return(corr)
}


