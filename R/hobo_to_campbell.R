#' This function convert HOBO header in a the CambpellScientific TOA5 format
#' 
#' @param HEADER is the header of input file
#' @param DATETIME_HEADER is the name of the column indicates datetime
#' @param RECORD_HEADE is the name of the column indicates record
#' 
#' @return Return the new header in the CampbellScientific TOA5 format 
#' 

hobo_to_campbell = function(HEADER, DATETIME_HEADER,RECORD_HEADER){
  new_header =  as.data.frame(matrix(ncol = ncol(HEADER), nrow = 4))
  
  extraxt_brackets = function(x) {
    brackets =  substring(text = x,first = gregexpr(pattern = "\\(", x)[[1]][1]+1, last = gregexpr(pattern = "\\)", x)[[1]][1]-1)
    brackets = gsub(","," -",brackets) 
    # brackets = gsub(" ","",brackets) 
    return(brackets)
  }
  
  extraxt_units = function(x) {
    comma_pos = gregexpr(pattern = ",", x)[[1]][1]
    open_brack_pos = gregexpr(pattern = "\\(", x)[[1]][1]
    if(comma_pos == -1 & open_brack_pos == -1){
      units =" "
    }else{
      if(open_brack_pos == -1){
        units = substring(text = x, first = comma_pos+1)
      }else{
        units = substring(text = x, first = comma_pos+1,last = open_brack_pos-1)
      }
    }
    units = gsub(" ","",units) 
    return(units)
  }
  
  extraxt_variables = function(x) {
    comma_pos = gregexpr(pattern = ",", x)[[1]][1]
    if(comma_pos == -1){
      variables = x
    }else{
      variables = substring(text = x,first = 1, last = gregexpr(pattern = ",", x)[[1]][1]-1)
    }
    # units = gsub(" ","",units) 
    return(variables)
  }
  
  extraxt_colons = function(x) {
    colons_pos = gregexpr(pattern = ":", x)[[1]][1]
    if(colons_pos == -1){
      title = x
    }else{
      title = substring(text = x,first = colons_pos+1)
    }
    return(title)
  }
  
  
  for(i in 1:ncol(HEADER)){
    new_header[1,1] = "Logger_number"       
    new_header[1,2] = extraxt_colons(HEADER[1,1])       # <- rules defined only for the first cell
    new_header[1,3:ncol(HEADER)] = ""
    new_header[2,i] = extraxt_variables(HEADER[2,i])
    new_header[3,i] = extraxt_units(HEADER[2,i])
    new_header[4,i] = extraxt_brackets(HEADER[2,i])
    
  }
  
  new_header[2,which(new_header[2,] == RECORD_HEADER)] = "RECORD"
  new_header[2,which(HEADER[2,] == DATETIME_HEADER)] = "TIMESTAMP"
  
  return(new_header)
}

