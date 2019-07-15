#' This function reject all values greater than max and smaller than min in a data.frame
#'
#' @param DATA data.frame of timeseries
#' @param DATETIME_HEADER header of data corresponding to datetime
#' @param RANGE_DIR directory where support files are stored
#' @param RANGE_FILE name of filr where min/max thresholds are defined for each variable. Thi file is in RANGE_DIR
#' @param MAIL_DIR aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa
#' @param MAIL_FILE_ALERT aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa
#' @param STATION name of station
#' @param USE_FLAG true/false --> decide to use or not the station flags in range file
#' @param USE_RT_FLAG true/false --> decide to use or not the station flags in range file
#' @param DATETIME_FORMAT kkkkkkkkkkkkkkkkkkkkkkkkkkkkkkkkkkkkkkkkkkkkkkkkkkkkkkkkkkkkkkkkkkkkkkk
#' @return A data.frame with all values in its physical range
#'
#' @export
#'
#' @examples
#' exclude_out_of_range(DATA = mydata, RANGE_DIR = RANGE_DIR, RANGE_FILE = "Range.csv")
#'

alert_range_notify = function(DATA,DATETIME_HEADER = "TIMESTAMP",DATETIME_FORMAT = "%Y-%m-%d %H:%M",RECORD_HEADER, 
                                    RANGE_DIR, RANGE_FILE, 
                                    MAIL_DIR, MAIL_FILE_ALERT,
                                    STATION, USE_FLAG,USE_RT_FLAG){
  
  ######
  # DATA = cbind(DATA,rep(1000, times = nrow(DATA)))
  # # DATA = DATA[,-18]
  # colnames(DATA)[ncol(DATA)] = "pippo"
  ######
  
  options(scipen = 999)
  range = read.csv(paste(RANGE_DIR, RANGE_FILE,sep = ""),stringsAsFactors = FALSE)          # <- import table that contains for each variable the permissible range
  oor_flag = read.csv(paste(MAIL_DIR, MAIL_FILE_ALERT,sep = ""),stringsAsFactors = FALSE)  
  
  range$Alert_min = as.numeric(range$Alert_min)
  range$Alert_Max = as.numeric(range$Alert_Max)
  
  # ATTENZIONE: Possibili problemi se i 2 file hanno lista variabili e nomi delle stazioni diverse (PER MODIFICHE MANUALI!!!)
  
  if(!(STATION %in% colnames(range)) & !(STATION %in% colnames(oor_flag))){       # possibili solo F&F e T&T
    void_vect = c(rep(NA, times = nrow(range)))
    void_vect_oor = c(rep(NA, times = nrow(oor_flag)))
    
    range = cbind(range,void_vect)
    colnames(range)[ncol(range)] = STATION
    oor_flag = cbind(oor_flag,void_vect_oor)
    colnames(oor_flag)[ncol(oor_flag)] = STATION
    
    w_col_data = which(range$Variable %in% colnames(DATA))
    w_station = which(colnames(range) == STATION)
    w_col_data_oor = which(oor_flag$Variable %in% colnames(DATA))
    w_station_oor = which(colnames(oor_flag) == STATION)
    
    range[w_col_data,w_station] = 1
    oor_flag[w_col_data_oor,w_station_oor] = 1
    
  }else{
    w_col_data = which(range$Variable %in% colnames(DATA))
    w_station = which(colnames(range) == STATION)
    
    range[w_col_data,w_station][is.na(range[w_col_data,w_station])] = 1
    range[-w_col_data,w_station] = NA 
    
    w_col_data_oor = which(oor_flag$Variable %in% colnames(DATA))
    w_station_oor = which(colnames(oor_flag) == STATION)
    
    oor_flag[w_col_data_oor,w_station_oor][is.na( oor_flag[w_col_data_oor,w_station_oor])] = 1
    oor_flag[-w_col_data_oor,w_station_oor] = NA
  }
  
  
  # range = range[order(range$Variable),] # reorder range file based on variable
  # oor_flag = oor_flag[order(oor_flag$Variable),]
  
  new = DATA # define new dataframe called new that is a copy of DATA
  
  # new = cbind(DATA, rep(100,times = nrow(DATA))) ####################################################################
  # colnames(new)[ncol(new)] = "PIPPO"             ####################################################################
  
  new_status = new # create a dataframe with the same structure that DATA. Inside there is only 0. When data are out of range 0 is subsitute wiht -1 or 1
  new_status[,-which(colnames(new_status) == DATETIME_HEADER )] = 0
  
  # This loop checks if variables in result are in the range list.
  # It could be a good index to see if there are issues in headers
  to_add = c()
  df_to_add = as.data.frame(matrix(ncol = ncol(range)))
  colnames(df_to_add)  = colnames(range)
  
  to_add_oor = c()
  df_to_add_oor = as.data.frame(matrix(ncol = ncol(oor_flag)))
  colnames(df_to_add_oor)  = colnames(oor_flag)
  
  df_upper = as.data.frame(matrix(ncol = 5, nrow = 0))
  colnames(df_upper) = c("Variable", "From", "To", "Hours", "Extreme_value")
  df_lower = as.data.frame(matrix(ncol = 5, nrow = 0))
  colnames(df_lower) = c("Variable", "From", "To", "Hours", "Extreme_value")
  df_NA = as.data.frame(matrix(ncol = 5, nrow = 0))
  colnames(df_NA) = c("Variable", "From", "To", "Hours", "Extreme_value")
  
  for(k in 1:ncol(new)){
    if((colnames(new)[k] %in% range$Variable) & (colnames(new)[k] %in% oor_flag$Variable)){
      w = which(range$Variable == colnames(new)[k])
      w2 = which(oor_flag$Variable == colnames(new)[k])
      
      # range$Variable[w]
      # oor_flag[w2,which(colnames(oor_flag)==STATION)]
      
      lower_limit = range$Alert_min[w]
      upper_limit = range$Alert_Max[w]
      
      if(USE_FLAG == TRUE){
        if(range[w,which(colnames(range) == STATION)] == 0 ){      #  Check variables activated when manual range is 1 and automatic flag = 1
          lower_limit = NA
          upper_limit = NA
        }
      }
      
      if(USE_RT_FLAG == TRUE){
        if(oor_flag[w2,which(colnames(oor_flag)==STATION)] == 0 ){      #  Check variables activated when manual range is 1 and automatic flag = 1
          lower_limit = NA
          upper_limit = NA
        }
      }
      # NB. il controllo dei paramtetri avviene solo se sia il range file  sia il out_of_range file abilita il controlle del parametro X (entrambi devono essere 1)
      
      if(!is.na(lower_limit) & !is.na(upper_limit)) {         # Exclude data without a range set
        
        # ~ ~ ~ ~ data below lower limit ~ ~ ~ ~
        
        w_low = which(new[,k] < lower_limit)
        
        x = cumsum(c(1,diff(w_low)!=1))
        sss = split(w_low,x)
        
        if(length(sss[[1]]) >= 1){
          for(s in 1:length(sss)){
            # sss[[s]]
            
            start_sss = as.POSIXct(new[ sss[[s]][1] ,which(colnames(new) == DATETIME_HEADER)],tz = "Etc/GMT-1")
            end_sss = as.POSIXct( new[ sss[[s]][length(sss[[s]])] ,which(colnames(new) == DATETIME_HEADER)],tz = "Etc/GMT-1")
            hour_diff = end_sss - start_sss
            units(hour_diff) = "hours"
            
            num_hour_diff = as.numeric(hour_diff)+0.25
            
            
            df_lower_tmp = data.frame(colnames(new)[k],
                                      format(start_sss,format = DATETIME_FORMAT),
                                      format(end_sss,format = DATETIME_FORMAT),
                                      as.numeric(num_hour_diff),
                                      min(new[ sss[[s]] ,k],na.rm = T))
            colnames(df_lower_tmp) = colnames(df_lower)
            
            
            df_lower = rbind(df_lower,df_lower_tmp)
          }
        }
        # else{
        #   df_lower_tmp =  as.data.frame(matrix(ncol = 5, nrow = 0))
        #   colnames(df_lower_tmp) = c("Variable", "From", "To", "Hours", "Extreme_value")
        # }
        
        # ~ ~ ~ ~ data above upper limit ~ ~ ~ ~
        
        w_high = which(new[,k] > upper_limit)
        
        y = cumsum(c(1,diff(w_high)!=1))
        ttt = split(w_high,y)
        
        if(length(ttt[[1]]) >= 1){
          for(t in 1:length(ttt)){
            # ttt[[t]]
            
            start_ttt = as.POSIXct(new[ ttt[[t]][1] ,which(colnames(new) == DATETIME_HEADER)],tz = "Etc/GMT-1")
            end_ttt = as.POSIXct( new[ ttt[[t]][length(ttt[[t]])] ,which(colnames(new) == DATETIME_HEADER)],tz = "Etc/GMT-1")
            hour_diff = end_ttt - start_ttt
            units(hour_diff) = "hours"
            
            num_hour_diff = as.numeric(hour_diff)+0.25
            
            
            df_upper_tmp = data.frame(colnames(new)[k],
                                      format(start_ttt,format = DATETIME_FORMAT),
                                      format(end_ttt,format = DATETIME_FORMAT),
                                      # as.character(start_ttt),
                                      # as.character(end_ttt),
                                      as.character(num_hour_diff),
                                      max(new[ ttt[[t]] ,k],na.rm = T))
            colnames(df_upper_tmp) = colnames(df_upper)
            
            
            df_upper = rbind(df_upper,df_upper_tmp)
          }
        }
        # else{
        #   df_upper_tmp =  as.data.frame(matrix(ncol = 5, nrow = 0))
        #   colnames(df_upper_tmp) = c("Variable", "From", "To", "Hours", "Extreme_value")
        # }
        
        
        # ~ ~ ~ ~ data NA ~ ~ ~ ~
        
        
        w_NA = which(is.na(new[,k]) & new[,which(colnames(new) == RECORD_HEADER)] != -1)  # extract NaN data (exclude rows filled with missing date)
        
        z = cumsum(c(1,diff(w_NA)!=1))
        nnn = split(w_NA,z)
        
        if(length(nnn[[1]]) >= 1){
          for(n in 1:length(nnn)){
            # nnn[[n]]
            
            start_nnn = as.POSIXct(new[ nnn[[n]][1] ,which(colnames(new) == DATETIME_HEADER)],tz = "Etc/GMT-1")
            end_nnn = as.POSIXct( new[ nnn[[n]][length(nnn[[n]])] ,which(colnames(new) == DATETIME_HEADER)],tz = "Etc/GMT-1")
            hour_diff = end_nnn - start_nnn
            units(hour_diff) = "hours"
            
            num_hour_diff = as.numeric(hour_diff)+0.25
            
            
            df_NA_tmp = data.frame(colnames(new)[k],
                                   format(start_nnn,format = DATETIME_FORMAT),
                                   format(end_nnn,format = DATETIME_FORMAT),
                                   # as.character(start_nnn),
                                   # as.character(end_nnn),
                                   as.numeric(num_hour_diff),
                                   "NaN")
            colnames(df_NA_tmp) = colnames(df_NA)
            
            
            df_NA = rbind(df_NA,df_NA_tmp)
          }
        }
        else{
          df_NA_tmp =  as.data.frame(matrix(ncol = 5, nrow = 0))
          colnames(df_NA_tmp) = c("Variable", "From", "To", "Hours", "Extreme_value")
        }
        
        if(length(c(w_low,w_high,w_NA)) > 0){
          oor_flag[w2,which(colnames(oor_flag)==STATION)] = 0
        }
        # new_status[,k] = ifelse(new[,k] < lower_limit, -1, new_status[,k])
        # new_status[,k] = ifelse(new[,k] > upper_limit, 1, new_status[,k])
        
        # new_status[is.na(new_status[,k]),k] = 0
        
        # new[,k] = ifelse(new[,k] < lower_limit, NA, new[,k])
        # new[,k] = ifelse(new[,k] > upper_limit, NA, new[,k])
      }
      
      ####
      
    }else{ 
      
      ##########
      
      # AGGIUNGERE QUI IL CODICE CHE PERMETTE L' AGGIORNAMENTO DEL FILE "out_of_range.csv" per variabili nuove --> legarlo a range file? 
      
      ##########
      to_add = c(to_add, colnames(new)[k])
      df_to_add = rbind(df_to_add,rep(NA,times = nrow(df_to_add)))
      
      df_to_add$Variable[nrow(df_to_add)] = colnames(new)[k]
      df_to_add$min[nrow(df_to_add)] = NA
      df_to_add$max [nrow(df_to_add)]= NA
      df_to_add$to_set[nrow(df_to_add)] = 1
      df_to_add$Alert_min[nrow(df_to_add)] = NA
      df_to_add$Alert_Max [nrow(df_to_add)]= NA
      df_to_add[nrow(df_to_add),which(colnames(df_to_add) == STATION)] = 1
      
      to_add_oor = c(to_add_oor, colnames(new)[k])
      df_to_add_oor = rbind(df_to_add_oor,rep(NA,times = nrow(df_to_add_oor)))
      
      df_to_add_oor$Variable[nrow(df_to_add_oor)] = colnames(new)[k]
      df_to_add_oor[nrow(df_to_add_oor),which(colnames(df_to_add_oor) == STATION)] = 1
    }
  }
  
  # df_lower
  # df_upper
  # df_NA
  
  
  df_lower_merge = cbind(rep("Too_low", nrow(df_lower)),df_lower)
  colnames(df_lower_merge)[1] =  "Error"
  df_upper_merge = cbind(rep("Too_high", nrow(df_upper)),df_upper)
  colnames(df_upper_merge)[1] =  "Error"
  df_NA_merge = cbind(rep("NaN_value", nrow(df_NA)),df_NA)
  colnames(df_NA_merge)[1] =  "Error"   
  
  
  
  df_out_of_range = rbind(df_lower_merge,df_upper_merge,df_NA_merge)
  df_out_of_range = df_out_of_range[order(df_out_of_range$From),]
  df_out_of_range = df_out_of_range[,c(2,1,3:ncol(df_out_of_range))]
  
  
  
  if(length(to_add) != 0){
    range = rbind(range,df_to_add[-1,])
  }
  if(length(to_add_oor) != 0){
    oor_flag = rbind(oor_flag,df_to_add_oor[-1,])
  }
  
  variable_new = to_add
  variable_to_set = range$Variable[which(range$to_set == 1)]
  variable_to_set = setdiff( variable_to_set, variable_new)
  
  range$Alert_min = as.character(range$Alert_min)
  range$Alert_Max = as.character(range$Alert_Max)
  
  write.csv(range,paste(RANGE_DIR, RANGE_FILE,sep = ""),quote = F,row.names = F, na = "")
  write.csv(oor_flag,paste(MAIL_DIR, MAIL_FILE_ALERT,sep = ""),quote = F,row.names = F, na = "")
  
  
  out = list(df_out_of_range, variable_new, variable_to_set)     # no new_status
  
  return(out)
}
