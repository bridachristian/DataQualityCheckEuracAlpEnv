#' This function check pictures corruption. If the lower half of the picture is on the same color (= identical value) the picture is classified corrupted
#' 
#' @param pics The extended path and file name of the picture
#' 
#' @return A logical value that indicate if the pic is corrupted or not (corrupted == TRUE) 

corr_pics = function(pics){
  pname = pics
  p = readJPEG(pname,native = F)
  
  p2 = as.data.frame(p)
  p3 = p2[(nrow(p2)/2):nrow(p2),]
  
  a_r = apply(X = p3,MARGIN = 1,FUN = unique)
  if(length(unique(a_r)) == 1){
    corr = TRUE
  }else{
    corr = FALSE
  }
  
  return(corr)
}


