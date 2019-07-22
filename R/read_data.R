#' This function read .csv and .dat file and adjust headers and time format
#'
#'  @param INPUT_DATA_DIR directory where input file is stored
#'  @param FILE_NAME name of file to read. Admitted files .csv and .dat
#'  @param DATETIME_HEADER header corresponding to datetime
#'  @param DATETIME_FORMAT format of datetime (E.g. "yyyy-mm-dd HH:MM")
#'  @param DATETIME_SAMPLING datetime sampling (default "15 min")
#'  @param DATA_FROM_ROW the number of row of the first value
#'  @param HEADER_ROW_NUMBER the number of row of the header
#'
#'  @return a list containing a data.frame of header, a data.frame of column names, a data.frame of data
#'
#'  @export
#'
#'  @examples
#'  read_data(INPUT_DATA_DIR = "~/Data/Input/", FILE_NAME = "M4s.dat", DATETIME_HEADER = "TIMESTAMP" , DATETIME_FORMAT = "yyyy-mm-dd HH:MM", DATA_FROM_ROW = 5, HEADER_ROW_NUMBER = 2)
#'  read_data(INPUT_DATA_DIR = "Your input file storage", FILE_NAME = "Your data name", DATETIME_HEADER = "Your datetime headere" , DATETIME_FORMAT = "Your datetime format", DATA_FROM_ROW = "The row of your first data", HEADER_ROW_NUMBER = "The row of your data column names")

# DATETIME_FORMAT = "%Y-%m-d %H:%M"
read_data = function(INPUT_DATA_DIR, FILE_NAME, DATETIME_HEADER = "TIMESTAMP" , DATETIME_FORMAT = "%Y-%m-%d %H:%M",DATETIME_SAMPLING = "15 min", DATA_FROM_ROW = 5, HEADER_ROW_NUMBER = 2){
  
  header <- read.csv(paste(INPUT_DATA_DIR, FILE_NAME,sep = ""), nrows = DATA_FROM_ROW - 1,header = F,stringsAsFactors = F,na.strings = c(NA, "NaN", "NAN"))
  header_colnames <- header[HEADER_ROW_NUMBER,]
  data <- read.csv(paste(INPUT_DATA_DIR, FILE_NAME,sep = ""), skip = DATA_FROM_ROW - 1,header = F,stringsAsFactors = F,na.strings = c(NA, "NaN", "NAN"))
  
  data_star <- read.csv(paste(INPUT_DATA_DIR, FILE_NAME,sep = ""), skip = HEADER_ROW_NUMBER - 1,header = F,stringsAsFactors = F)
  data_star = data_star[-c(1:(DATA_FROM_ROW-HEADER_ROW_NUMBER)),]
  
  # max_col = max(count.fields(paste(INPUT_DATA_DIR, FILE_NAME,sep = ""), sep = ','), na.rm = T)
  
  ccc  = count.fields(paste(INPUT_DATA_DIR, FILE_NAME,sep = ""), sep = ',')
  header_ccc = ccc[1:(DATA_FROM_ROW-1)]
  data_ccc = ccc[(DATA_FROM_ROW):length(ccc)]
  
  u_ccc = unique(data_ccc)
  u_ccc = u_ccc[-which(u_ccc == ncol(header_colnames))]
  
  # detect rows with number of column shorter than header colnames
  more = which(data_ccc >  ncol(header_colnames))
  less = which(data_ccc <  ncol(header_colnames))   
  
  df_out = NULL
  
  if(length(more) > 0){
    u_more = unique(data_ccc[more])
    more_df_tot = as.data.frame(matrix(nrow = 0, ncol = 3))
    # colnames(more_df_tot) = c("From row", "To row", "From datetime", "To datetime", "N.col (diff)")
    colnames(more_df_tot) = c( "Too many columns","From", "To")
    for(k in 1:length(u_more)){
      more_k = more[data_ccc[more] == u_more[k]]
      group_more = unname(tapply(more_k, cumsum(c(1, diff(more_k)) != 1), range))
      more_df = as.data.frame(matrix(data = NA, nrow = length(group_more), ncol = ncol(more_df_tot) ))
      colnames(more_df) = colnames(more_df_tot)
      
      for (j in 1:length(group_more)){
        more_df$`Too many columns`[j] = paste("N.col: ",data_ccc[group_more[[j]][2]],"  (diff: +",as.character(data_ccc[group_more[[j]][2]]-ncol(header_colnames)), ")",sep ="")
        more_df$`From`[j] = paste("row",group_more[[j]][1])
        more_df$`To`[j] = paste("row",group_more[[j]][2])
        # more_df$`From datetime`[j] = data[group_more[[j]][1], which(header_colnames == DATETIME_HEADER)]
        # more_df$`To datetime`[j] = data[group_more[[j]][2], which(header_colnames == DATETIME_HEADER)]
      }
      more_df_tot = rbind(more_df_tot, more_df)
    }
    flag_error_df = 1
    df_out = more_df_tot
  }else{
    if(length(less) > 0){
      u_less = unique(data_ccc[less])
      less_df_tot = as.data.frame(matrix(nrow = 0, ncol = 3))
      colnames(less_df_tot) = c( "Too few columns","From", "To")
      for(k in 1:length(u_less)){
        less_k = less[data_ccc[less] == u_less[k]]
        
        group_less = unname(tapply(less_k, cumsum(c(1, diff(less_k)) != 1), range))
        less_df = as.data.frame(matrix(data = NA, nrow = length(group_less), ncol = ncol(less_df_tot)))
        colnames(less_df) = colnames(less_df_tot)
        
        for (j in 1:length(group_less)){
          less_df$`Too few columns`[j] = paste("N.col: ",data_ccc[group_less[[j]][2]],"  (diff: -",ncol(header_colnames)-data_ccc[group_less[[j]][2]], ")",sep ="")
          less_df$`From`[j] = paste("row",group_less[[j]][1])
          less_df$`To`[j] = paste("row",group_less[[j]][2])
          # less_df$`From datetime`[j] = data[group_less[[j]][1], which(header_colnames == DATETIME_HEADER)]
          # less_df$`To datetime`[j] = data[group_less[[j]][2], which(header_colnames == DATETIME_HEADER)]
        }
        less_df_tot = rbind(less_df_tot, less_df)
      }
      flag_error_df = -1
      df_out = less_df_tot
    }else{
      if(any(data_star == "", na.rm = T)){
        flag_error_df = 2
        colnames(data_star) = header_colnames
        w1 = as.data.frame(which(data_star == "",arr.ind = T))
        names(w1) = c("row", "col")
        df = data.frame( w1$row, data_star[w1$row, which(header_colnames == DATETIME_HEADER )], as.character(header_colnames)[w1$col], stringsAsFactors = F)
        colnames(df) = c("row",DATETIME_HEADER, "Variable")
        
        u = unique(df$Variable)
        
        empty_cell = data.frame(matrix(ncol = 3, nrow = 0))
        colnames(empty_cell) = c( "Empty cells", "From", "To") 
        
        i=1
        for(i in 1:length(u)){
          row_i = df$row[df$Variable == u[i]]
          group_row = unname(tapply(row_i, cumsum(c(1, diff(row_i)) != 1), range))
          empty_cell_i = data.frame(matrix(ncol = 3, nrow = length(group_row)))
          colnames(empty_cell_i) = colnames(empty_cell)
          for(h in 1: length(group_row)){
            empty_cell_i$`Empty cells`[h] = u[i]
            
            if(u[i] == DATETIME_HEADER){
              empty_cell_i$`From`[h] = paste("row", group_row[[h]][1])
              empty_cell_i$`To`[h] = paste("row",group_row[[h]][2])
            }else{
              empty_cell_i$`From`[h] = df[which(df$row ==group_row[[h]][1]), which(colnames(df) == DATETIME_HEADER)]
              empty_cell_i$`To`[h] = df[which(df$row ==group_row[[h]][2]), which(colnames(df) == DATETIME_HEADER)]
            }
          }
          empty_cell = rbind(empty_cell,empty_cell_i)
        }
        df_out = empty_cell
      }else{
        flag_error_df = 0
        colnames(data) = header_colnames
        
        w <- which(colnames(data) == DATETIME_HEADER)
        date_chr = data[,w]
        time <- as.POSIXct(strptime(x = date_chr, format = DATETIME_FORMAT), tz = 'Etc/GMT-1') # Error in strptime(x = as.character(date_chr), format = DATETIME_FORMAT):input string is too long
        
        data[,w] <- time
        not_w <- which(colnames(data) != DATETIME_HEADER)
        
        for(i in not_w){
          data[,i] <- as.numeric(data[,i])
        }
        
        matr_data = as.matrix(data)
        ind_NA = which(apply(matr_data,1, function(x) all(is.na(x))))
        
        if(length(ind_NA) != 0){
          data = data[-ind_NA,]
        }
      }
      
    }
  }
  
  out = list(header,header_colnames,data,flag_error_df,df_out)
  gc(reset = T)
  # out = list(header,header_colnames,data,flag_error_df,data_star, max_col)
  return(out)
  
}
