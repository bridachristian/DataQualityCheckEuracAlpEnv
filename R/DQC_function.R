

DQC_function= function(input_dir,
                       output_dir_data,
                       output_dir_report,
                       project_dir,
                       data_from_row,
                       header_row_number,
                       datetime_header,
                       datetime_format,
                       datetime_sampling,
                       record_header,
                       range_file,
                       write_output_files,
                       write_output_report,
                       file_name,
                       station_name,
                       start_date,
                       # database_dir,
                       logger_info_file,
                       record_check,
                       output_dir_raw,
                       use_alert_station_flag,
                       mail_file_alert,
                       use_realtime_station_flag){
  
  # ..... Define flags ..................................................................................................................................
  
  flag_empty = NA
  flag_logger_number = NA
  flag_error_df = NA
  flag_date = NA
  flag_duplicates_rows = NA
  flag_overlap = NA
  flag_missing_records = NA
  flag_missing_dates = NA
  flag_range_variable_to_set = NA
  flag_range_variable_new = NA
  flag_out_of_range = NA
  
  flag_new_duplicates_rows = NA
  flag_new_overlap = NA
  flag_new_missing_dates = NA
  
  flag_missing_records_new = NA
  
  flag_out_of_range_ALERT = NA
  
  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # PART 1 --> ANALYZE AND WRITE DATA
  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  
  # ..... Body ..........................................................................................................................................
  
  if(check_empty_file(INPUT_DATA_DIR = input_dir, FILE_NAME = file_name) == TRUE){
    
    flag_empty = 1
    
  }else{
    
    flag_empty = 0
    
    data_import <- read_data(INPUT_DATA_DIR = input_dir, FILE_NAME = file_name,                             # read and import data well formatted
                             DATETIME_HEADER = datetime_header, DATETIME_FORMAT = datetime_format,
                             DATA_FROM_ROW = data_from_row, HEADER_ROW_NUMBER = header_row_number)  
    header = data_import [[1]]
    header_colnames = data_import [[2]]
    data = data_import [[3]]
    flag_error_df = data_import [[4]]
    data_star = data_import [[5]]
    max_col  = data_import [[6]]
    rm(data_import)
    
    # logger_number = header[1,4]                                                                   # check logger numbers
    # software_version = header[1,6]
    logger_info_csv = read.csv(file = logger_info_file, stringsAsFactors = F)
    w_logger = which(logger_info_csv[,1] == station_name)
    header_info = header[1,1:8]
    
    if(length(w_logger) == 0){
      # logger_info_csv = rbind(logger_info_csv, c(station_name, logger_number, software_version))
      new_logger_info = cbind(station_name,header_info)
      colnames(new_logger_info) = colnames(logger_info_csv)
      logger_info_csv = rbind(logger_info_csv, new_logger_info)
      write.csv(logger_info_csv,logger_info_file,row.names = F, na = "")
      flag_logger_number = 0
    }else{
      logger_info = logger_info_csv[w_logger,]
      if(any(header_info[,-c(1,2)] != logger_info[,-c(1,2,3)])){
        flag_logger_number = 1
        
        header_info = header_info[,-c(1,2)]     # difference on TOA and on Station_Name are admitted
        logger_info = logger_info[,-c(2,3)]     # difference on TOA and on Station_Name are admitted
        
        w_diff = which(header_info != logger_info[,-1]) 
        cc = colnames(logger_info[,-1])[w_diff]
        new_h = as.character(header_info[w_diff])
        old_h = as.character(logger_info[,-1][w_diff])
        logger_difference = data.frame(cc,old_h,new_h)
        colnames(logger_difference) = c("Column", "Old", "New")
        
        # logger_difference = gsub("_", " ", logger_difference$Column)   # remove underescore 
        
      }else{
        flag_logger_number = 0
      }
    }
    
    # if(flag_logger_number == 0){
    if(flag_error_df == 0){
      time_data = data[,which(colnames(data)==datetime_header)]
      time_data = time_data[order(time_data)]
      
      data = data[order(data[,which(colnames(data)==datetime_header)]),] 
      
      if(is.na(start_date)){
        
        original = data
        mydata = data    
        flag_date = 0
        
        rm(data)
        
      }else{
        recent_date = format(time_data[length(time_data)], format = datetime_format)
        if(as.POSIXct(start_date,tz = 'Etc/GMT-1') < time_data[length(time_data)]){
          w_date = which(time_data == as.POSIXct(start_date,tz = 'Etc/GMT-1'))
          
          if(length(w_date) != 0){
            original = data[(w_date[1] + 1):nrow(data),]      # possible issues in data subset!!! to check 
            mydata = data[(w_date[1] + 1):nrow(data),]
            
            flag_date = 0
            
            rm(data)
          }else{
            original = data
            mydata = data 
            
            flag_date = 0 
            
            rm(data)
          } 
        } else {
          
          flag_date = 1
        }
      }
      
      
      if(flag_date == 0){
        deletes_duplcated <- deletes_duplcated_data(DATA = mydata,DATETIME_HEADER = datetime_header)         # <- Deletes identical rows if found
        mydata = deletes_duplcated [[1]]
        duplicated_data = deletes_duplcated [[2]]
        duplicated_data = time_to_char(DATA = duplicated_data, DATETIME_HEADER = datetime_header, DATETIME_FORMAT = datetime_format)
        
        rm(deletes_duplcated)
        
        if(unique(as.character(duplicated_data[1,])) == "---"){
          flag_duplicates_rows = 0
        } else{
          flag_duplicates_rows = 1
        }
        
        data_in_old_files <- deletes_old_datetime(DATA = mydata,DATETIME_HEADER = datetime_header)  
        mydata = data_in_old_files [[1]]
        old_data = data_in_old_files[[2]]
        
        rm(data_in_old_files)
        
        orig_wihtout_dupli = mydata
        
        overlap <- detect_overlap(DATA = mydata,DATETIME_HEADER = datetime_header, RECORD_HEADER = record_header)          # <- Detect overlap
        
        
        if(length(overlap) != 0){
          
          flag_overlap = 1
          overlap[,1]<- overlap[,1] + data_from_row - 1
          colnames(overlap)[1]= "File Row"
          
        }else{
          
          flag_overlap = 0
          
          # inserire qui controllo sul numero dei record. Ricorda di togliere le date inserite (missing dates --> record = -1) 
          
          if(record_check == 1){
            rec_miss  <- missing_record(DATA = mydata, DATETIME_HEADER = datetime_header, RECORD_HEADER = record_header, DATETIME_SAMPLING = datetime_sampling, DATETIME_FORMAT = datetime_format)  # <- fill missing dates with NA
            flag_missing_records = rec_miss[[1]]
            records_missing = rec_miss[[2]]
            records_restart = rec_miss[[3]]
          }else{
            flag_missing_records = 50
          }
          
          if(flag_missing_records != 1){
            
            missing  <- missing_dates(DATA = mydata, DATETIME_HEADER = datetime_header, RECORD_HEADER = record_header, DATETIME_SAMPLING = datetime_sampling)  # <- fill missing dates with NA
            mydata = missing[[1]]
            missing_index_date = missing[[2]]
            
            rm(missing)
            
            # ALERT OUT OF RANGE --> ANY MYDATA MODIFICATION
            alert_range <- alert_range_notify(DATA = mydata, DATETIME_HEADER = datetime_header, DATETIME_FORMAT = datetime_format, RECORD_HEADER = record_header,
                                              RANGE_DIR = range_dir, RANGE_FILE = range_file, 
                                              MAIL_DIR = mail_dir, MAIL_FILE_ALERT = mail_file_alert,
                                              STATION = STATION_NAME, 
                                              USE_FLAG = use_alert_station_flag,USE_RT_FLAG = use_realtime_station_flag) # <- Substitute with NA data out of phisical range
            alert_out_of_range_table = alert_range[[1]]
            alert_variable_new = alert_range[[2]]
            alert_variable_to_set = alert_range[[3]]
            
            
            # OUT OF RANGE --> DELATE DATA OUT OF RANGE
            range <- exclude_out_of_range_v3(DATA = mydata,DATETIME_HEADER = datetime_header, RECORD_HEADER = record_header, 
                                             RANGE_DIR = range_dir, RANGE_FILE = range_file) # <- Substitute with NA data out of phisical range
            mydata_out_of_range = range[[1]]               # don't subsitute out of range data with NaN 
            out_of_range_table = range[[2]]
            # check_out_of_range = range[[2]]
            variable_new = range[[3]]
            variable_to_set = range[[4]]
            
            rm(range)
            
            
            # ..... Flags .....................................................................................................................................
            
            if(length(variable_to_set) != 0 | length(alert_variable_to_set) != 0){
              flag_range_variable_to_set = 1
            }else{
              flag_range_variable_to_set = 0
            }
            
            if(length(variable_new) != 0 | length(alert_variable_new) != 0 ){
              flag_range_variable_new = 1
            }else{
              flag_range_variable_new = 0
            }
            
            if(nrow(alert_out_of_range_table) == 0){
              flag_out_of_range_ALERT = 0
            }else{
              flag_out_of_range_ALERT = 1
            }
            
            
            if(nrow(out_of_range_table) == 0){
              flag_out_of_range = 0
            }else{
              flag_out_of_range = 1
            }
            
            # if(1 %in% unique(unlist(apply(X = check_out_of_range[,-which(colnames(check_out_of_range) == datetime_header)],MARGIN = 2, unique)))){
            #   flag_out_of_range = 1
            # }else{
            #   if(-1 %in% unique(unlist(apply(X = check_out_of_range[,-which(colnames(check_out_of_range) == datetime_header)],MARGIN = 2, unique)))){
            #     flag_out_of_range = 1
            #   }else{
            #     flag_out_of_range = 0
            #   }
            # }
            
            
            # time_tot = as.POSIXct(mydata[,which(colnames(mydata) == datetime_header)], format = datetime_format, tz = 'Etc/GMT-1')
            time_tot = as.POSIXct(mydata_out_of_range[,which(colnames(mydata_out_of_range) == datetime_header)], format = datetime_format, tz = 'Etc/GMT-1')
            time_missing = missing_index_date[,2]
            
            if(length(which(time_tot %in% time_missing )) == 0){
              flag_missing_dates = 0      # No missing dates
            }else{
              flag_missing_dates = 1      # YES missing dates
            }
            
            mydata <- time_to_char(DATA = mydata_out_of_range, DATETIME_HEADER = datetime_header, DATETIME_FORMAT = datetime_format)
            # mydata_1 = mydata
          }
        }
      }
    }
    # }
  }
  
  
  
  # missing_index_date
  
  # ..... Output ..........................................................................................................................................
  file_names = NULL
  
  
  if(flag_empty == 0){
    # if(flag_logger_number == 0){
    if(flag_error_df == 0){
      if(flag_date == 0){
        if(flag_overlap == 0){
          if(flag_missing_records != 1){
            # if(write_output_files == TRUE){    # here????
            time_mydata = as.POSIXct(mydata[,which(colnames(mydata)== datetime_header)],format = datetime_format, tz = 'Etc/GMT-1')
            time_orig = as.POSIXct(orig_wihtout_dupli[,which(colnames(orig_wihtout_dupli)== datetime_header)],format = datetime_format, tz = 'Etc/GMT-1')
            years = as.numeric(unique(format(time_mydata, format = "%Y")))
            file_names = paste(station_name,"_", years,".dat",sep = "")
            
            flag_new_duplicates_rows_tmp = c()
            flag_new_overlap_tmp = c()
            flag_new_missing_dates_tmp = c()
            flag_missing_records_new_tmp = c()
            df_difference = as.data.frame(matrix(ncol = 4, nrow = 0))
            colnames(df_difference) = c("Column", "Row", "Old", "New")
            
            new_missing_index_date_tot = c()
            new_overlap_tot = c()
            k=1
            for(k in 1: length(years)){
              
              if(file.exists(paste(output_dir_data,file_names[k],sep = ""))){
                
                # import old data
                old_data_list = read_data(INPUT_DATA_DIR = output_dir_data,
                                          FILE_NAME = file_names[k],
                                          DATETIME_HEADER = datetime_header,
                                          DATETIME_FORMAT = datetime_format, 
                                          DATA_FROM_ROW = data_from_row, 
                                          HEADER_ROW_NUMBER = header_row_number)
                
                old_original_list = read_data(INPUT_DATA_DIR = output_dir_raw,
                                              FILE_NAME = paste(substring(file_names[k],1, nchar(file_names[k])-4),".dat",sep = ""),
                                              DATETIME_HEADER = datetime_header,
                                              DATETIME_FORMAT = datetime_format,
                                              DATA_FROM_ROW = data_from_row,
                                              HEADER_ROW_NUMBER = header_row_number)
                
                
                old_header = old_data_list [[1]]
                old_header_colnames = old_data_list [[2]]
                old_data = old_data_list [[3]]
                
                last_old_datetime = old_data[nrow(old_data),which(colnames(old_data) == datetime_header)]
                last_old_record = old_data[nrow(old_data),which(colnames(old_data) == record_header)]
                
                rm(old_data_list)
                
                old_orig_header = old_original_list [[1]]
                old_orig_header_colnames = old_original_list [[2]]
                old_orig_data = old_original_list [[3]]
                
                last_old_orig_datetime = old_orig_data[nrow(old_orig_data),which(colnames(old_orig_data) == datetime_header)]
                last_old_orig_record = old_orig_data[nrow(old_orig_data),which(colnames(old_orig_data) == record_header)]
                
                rm(old_original_list)
                
                if(identical(old_header[-1,], header[-1,])){   # <-- delete  [-1,] when all station are updated. Substitute header new in old datatable. 
                  
                  # -- considero il dato delle yyyy-01-01 00:00 come appartenente all' anno precedente -- 
                  w_first = which(format(time_mydata, format = "%m") == "01" &
                                    format(time_mydata, format = "%d") == "01" &
                                    format(time_mydata, format = "%H") == "00" &
                                    format(time_mydata, format = "%M") == "00" )
                  y_first = as.numeric(format(time_mydata, format = "%Y")[w_first])
                  
                  if(length(w_first)!= 0){
                    w1 = which(y_first == years[k])
                    w2 = which(y_first == years[k]+1)
                    
                    w_tot = which(format(time_mydata, format = "%Y") == years[k])
                    w_tot = c(w_tot,w_first[w2])
                    
                    if(length(w1)!= 0){
                      if(w_first[w1] %in% w_tot){
                        w_tot_2 = w_tot[-c(which(w_tot == w_first[w1]))]
                      }else{
                        w_tot_2 = w_tot
                      }
                    }else{
                      w_tot_2 = w_tot
                    }
                    
                    df_toadd = mydata[c(w_tot_2),]
                    
                    
                  }else{
                    df_toadd = mydata[which(format(time_mydata, format = "%Y") == years[k]),]
                    
                  }
                  #######################
                  w_first = which(format(time_orig, format = "%m") == "01" &
                                    format(time_orig, format = "%d") == "01" &
                                    format(time_orig, format = "%H") == "00" &
                                    format(time_orig, format = "%M") == "00" )
                  y_first = as.numeric(format(time_orig, format = "%Y")[w_first])
                  
                  if(length(w_first)!= 0){
                    w1 = which(y_first == years[k])
                    w2 = which(y_first == years[k]+1)
                    
                    w_tot = which(format(time_orig, format = "%Y") == years[k])
                    w_tot = c(w_tot,w_first[w2])
                    
                    if(length(w1)!= 0){
                      if(w_first[w1] %in% w_tot){
                        w_tot_2 = w_tot[-c(which(w_tot == w_first[w1]))]
                      }else{
                        w_tot_2 = w_tot
                      }
                    }else{
                      w_tot_2 = w_tot
                    }
                    
                    df_toadd_raw = orig_wihtout_dupli[c(w_tot_2),]
                    
                    
                  }else{
                    df_toadd_raw = orig_wihtout_dupli[which(format(time_orig, format = "%Y") == years[k]),]
                    
                  }
                  # -------------------------------------------------------------------------------
                  # append new data to old data if headers new and old are the same
                  
                  # df_toadd =  mydata[which(format(time_mydata, format = "%Y") == years[k]),]
                  df_toadd[,which(colnames(df_toadd)== datetime_header)] = as.POSIXct(format(df_toadd[,which(colnames(df_toadd)== datetime_header)],format = datetime_format),tz = "Etc/GMT-1")
                  
                  
                  # new[order(new$TIMESTAMP),]
                  new = rbind(old_data,df_toadd)
                  new = new[order(new[,which(colnames(new) == datetime_header)]),]
                  
                  # append new raw data to old data if headers new and old are the same
                  # df_toadd_raw = orig_wihtout_dupli[which(format(time_orig, format = "%Y") == years[k]),]
                  df_toadd_raw[,which(colnames(df_toadd_raw)== datetime_header)] = as.POSIXct(format(df_toadd_raw[,which(colnames(df_toadd_raw)== datetime_header)],format = datetime_format),tz = "Etc/GMT-1")
                  
                  new_raw = rbind(old_orig_data,df_toadd_raw)
                  new_raw = new_raw[order(new_raw[,which(colnames(new_raw) == datetime_header)]),]
                  
                  new_deletes_duplcated <- deletes_duplcated_data(DATA = new,DATETIME_HEADER = datetime_header)        
                  new_mydata = new_deletes_duplcated [[1]]
                  new_duplicated_data = new_deletes_duplcated [[2]]
                  
                  raw_new_deletes_duplcated <- deletes_duplcated_data(DATA = new_raw,DATETIME_HEADER = datetime_header)        
                  raw_new_mydata = raw_new_deletes_duplcated [[1]]
                  
                  orig_data_new = raw_new_mydata
                  
                  raw_new_duplicated_data = raw_new_deletes_duplcated [[2]]
                  
                  # non ha senso vedere se ci sono righe duplicate tra quelle scaricate e file vecchi --> gia processiati!
                  if(unique(as.character(new_duplicated_data[1,])) == "---"){
                    if(write_output_files == FALSE){   
                      flag_new_duplicates_rows_tmp = c(flag_new_duplicates_rows_tmp,0)
                      
                    }else{  
                      flag_new_duplicates_rows_tmp = c(flag_new_duplicates_rows_tmp,0)
                    }
                  } else{
                    if(write_output_files == FALSE){  
                      flag_new_duplicates_rows_tmp = c(flag_new_duplicates_rows_tmp,0)
                      
                    }else{    
                      flag_new_duplicates_rows_tmp = c(flag_new_duplicates_rows_tmp,1)
                    }
                  }
                  
                  
                  
                  new_duplicated_data = time_to_char(DATA = new_duplicated_data, DATETIME_HEADER = datetime_header, DATETIME_FORMAT = datetime_format)
                  raw_new_duplicated_data = time_to_char(DATA = raw_new_duplicated_data, DATETIME_HEADER = datetime_header, DATETIME_FORMAT = datetime_format)
                  
                  new_overlap <- detect_overlap(DATA = new_mydata,DATETIME_HEADER = datetime_header, RECORD_HEADER = record_header) 
                  new_overlap_tot = rbind(new_overlap_tot, new_overlap)
                  if(length(new_overlap) == 0){
                    
                    flag_new_overlap_tmp = c(flag_new_overlap_tmp,0)
                    
                    if(record_check == 1){
                      w_last = which(new_mydata[,which(colnames(new_mydata) == datetime_header)] == last_old_datetime)
                      if(length(w_last) == 0){
                        rec_miss  <- missing_record(DATA = new_mydata[w_last:nrow(new_mydata),], DATETIME_HEADER = datetime_header, RECORD_HEADER = record_header, DATETIME_SAMPLING = datetime_sampling, DATETIME_FORMAT = datetime_format)  # <- fill missing dates with NA
                        
                      }else{
                        rec_miss  <- missing_record(DATA = new_mydata[w_last:nrow(new_mydata),], DATETIME_HEADER = datetime_header, RECORD_HEADER = record_header, DATETIME_SAMPLING = datetime_sampling, DATETIME_FORMAT = datetime_format)  # <- fill missing dates with NA
                      }
                      flag_missing_records_new_tmp = rec_miss[[1]]
                      records_missing_new = rec_miss[[2]]
                      records_restart_new = rec_miss[[3]]
                    }else{
                      flag_missing_records_new_tmp = 50
                    }
                    
                    if(flag_missing_records_new_tmp != 1){
                      new_missing  <- missing_dates(DATA = new_mydata,
                                                    DATETIME_HEADER = datetime_header,
                                                    RECORD_HEADER = record_header, 
                                                    DATETIME_SAMPLING = datetime_sampling)  # <- fill missing dates with NA
                      new_mydata = new_missing[[1]]
                      new_missing_index_date = new_missing[[2]]
                      
                      new_missing_index_date_tot = rbind(new_missing_index_date_tot,new_missing_index_date)
                      
                      
                      
                      if(record_check != 1 | flag_missing_records_new_tmp != 1){     
                        # We avoid to write output if record control is active (record_check = 1) and record has some issues (indicated by flag_append_new = -1)
                        
                        # prepare data for output
                        
                        new_mydata <- time_to_char(DATA = new_mydata, DATETIME_HEADER = datetime_header, DATETIME_FORMAT = datetime_format)
                        orig_data_new <- time_to_char(DATA = orig_data_new, DATETIME_HEADER = datetime_header, DATETIME_FORMAT = datetime_format)
                        
                        
                        new_time_tot = as.POSIXct(new_mydata[,which(colnames(new_mydata) == datetime_header)], format = datetime_format, tz = 'Etc/GMT-1')
                        new_time_orig = as.POSIXct(orig_data_new[,which(colnames(orig_data_new) == datetime_header)], format = datetime_format, tz = 'Etc/GMT-1')
                        
                        new_time_missing = new_missing_index_date[,2]
                        
                        if(length(which(new_time_tot %in% new_time_missing )) == 0){
                          flag_new_missing_dates_tmp = c(flag_new_missing_dates_tmp,0)      # No missing dates
                        }else{
                          flag_new_missing_dates_tmp = c(flag_new_missing_dates_tmp,1)      # YES missing dates
                        }
                        
                        rm(new_missing)
                        
                        colnames(header) = header[1,]
                        out_my = new_mydata
                        colnames(out_my) = colnames(header)
                        out_mydata=rbind(header[-1,],out_my)
                        file_name_output = file_names[k]
                        flag_missing_records_new_tmp = c(flag_missing_records_new_tmp, 0)  
                        
                        
                        # out_orig = orig_data_new[which(format(new_time_orig, format = "%Y") == years[k]),]
                        out_orig = orig_data_new
                        out_orig[,which(colnames(out_orig)== datetime_header)] = format(out_orig[,which(colnames(out_orig)== datetime_header)], format = datetime_format)
                        colnames(out_orig) = colnames(header)
                        out_original=rbind(header[-1,],out_orig)
                        # file_name_original = paste(substring(file_names[k], 1, nchar(file_names[k])-4), ".dat",sep = "")
                        file_name_original = file_names[k]
                        
                        # create a dataframe database formatted
                        
                        # db_mydata = new_mydata
                        # db_mydata[, which(colnames(db_mydata) == datetime_header)] = as.POSIXct(db_mydata[, which(colnames(db_mydata) == datetime_header)],tz ='Etc/GMT-1',format = datetime_format)
                        # first_row_selected = which(db_mydata[, which(colnames(db_mydata) == datetime_header)] == last_old_datetime)+1
                        # db_mydata = db_mydata[first_row_selected: nrow(db_mydata),]
                        # from_date = db_mydata[1,which(colnames(db_mydata) == datetime_header)]
                        # to_date = db_mydata[nrow(db_mydata),which(colnames(db_mydata) == datetime_header)]
                        # db_mydata[, which(colnames(db_mydata) == datetime_header)] = format(db_mydata[, which(colnames(db_mydata) == datetime_header)],format = datetime_format ) #change here if you want change datetime output format (for example  in database "%Y-%m-%dT%H:%M")
                        # colnames(db_mydata) = colnames(header)
                        # db_mydata=rbind(header[-1,],db_mydata)
                        # date_to_print_filename = paste(paste(format(from_date,format = "%Y"),format(from_date,format = "%m"),format(from_date,format = "%d"),
                        #                                      format(from_date,format = "%H"),format(from_date,format = "%M"),sep = ""),
                        #                                paste(format(to_date,format = "%Y"),format(to_date,format = "%m"),format(to_date,format = "%d"),
                        #                                      format(to_date,format = "%H"),format(to_date,format = "%M"),sep = "" ), sep = "_")
                        
                        if(write_output_files == TRUE){    # here????
                          
                          # keep updtate logger_info_file!
                          w_logger = which(logger_info_csv[,1] == station_name)
                          new_logger_info = cbind(station_name,header[1,1:8])
                          colnames(new_logger_info) = colnames(logger_info_csv)
                          logger_info_csv[w_logger,] = new_logger_info
                          write.csv(logger_info_csv,logger_info_file,row.names = F, na = "")
                          
                          # write total .dat
                          write.csv(out_mydata,paste(output_dir_data,file_name_output,sep = ""),quote = F,row.names = F, na = "NaN")
                          write.csv(out_original,paste(output_dir_raw,file_name_original,sep = ""),quote = F,row.names = F, na = "NaN")
                          
                          # write total .csv
                          file_name_output_csv = paste(substring(file_name_output, 1, nchar(file_name_output)-4),".csv",sep="") 
                          output_dir_data_csv = substring(output_dir_data, 1, nchar(output_dir_data)-10)  ### NOTA: cartella livello sopra (elimino il num di caratteri di Files_dat)
                          file.copy(from = paste(output_dir_data,file_name_output,sep = ""), to = paste(output_dir_data_csv,file_name_output_csv,sep = ""), overwrite = T)
                          # write.csv(db_mydata, paste(database_dir ,substring(file_name_output,1, nchar(file_name_output)-8),date_to_print_filename, ".csv",sep = ""),quote = F,row.names = F, na = "NaN")
                        }
                        
                      }
                    }
                  }else{
                    flag_new_overlap_tmp = c(flag_new_overlap_tmp,1)
                    new_overlap[,1]<- new_overlap[,1] + data_from_row - 1
                    colnames(new_overlap)[1]= "File Row"
                  }
                  
                }else{
                  
                  ######### new section ##########
                  # ~~~~~~~~~
                  if(write_output_files == TRUE){    # here???? 
                    
                    # rename total file
                    
                    j=0
                    repeat{
                      j=j+1
                      file_names_old = paste(substring(file_names[k],1, nchar(file_names[k])-4),"_old",j,".dat",sep = "")
                      if(!file.exists(paste(output_dir_data,file_names_old,sep = ""))){
                        break
                      }
                    }
                    file_names_total_data = file_names[k]
                    
                    file.rename(from = paste(output_dir_data,file_names_total_data,sep = ""),to = paste(output_dir_data,file_names_old,sep = ""))
                    
                    output_dir_data_csv = substring(output_dir_data, 1, nchar(output_dir_data)-10)
                    file_names_old_csv = paste(substring(file_names_old, 1, nchar(file_names_old)-4),".csv",sep = "")
                    file.copy(from = paste(output_dir_data,file_names_old,sep = ""),to = paste(output_dir_data_csv,file_names_old_csv,sep = ""))
                    
                    # rename raw data
                    
                    j=0
                    repeat{
                      j=j+1
                      file_names_original_old = paste(substring(file_names[k],1, nchar(file_names[k])-4),"_old",j,".dat",sep = "")
                      if(!file.exists(paste(output_dir_raw,file_names_original_old,sep = ""))){
                        break
                      }
                    }
                    file_names_raw_data = paste(substring(file_names[k],1,nchar(file_names[k])-4),".dat", sep = "")
                    file.rename(from = paste(output_dir_raw,file_names_raw_data,sep = ""),to = paste(output_dir_raw,file_names_original_old,sep = ""))
                    
                    
                    # ~~~~~~~~~
                    w_first = which(format(time_mydata, format = "%m") == "01" &
                                      format(time_mydata, format = "%d") == "01" &
                                      format(time_mydata, format = "%H") == "00" &
                                      format(time_mydata, format = "%M") == "00" )
                    y_first = as.numeric(format(time_mydata, format = "%Y")[w_first])
                    
                    if(length(w_first)!= 0){
                      w1 = which(y_first == years[k])
                      w2 = which(y_first == years[k]+1)
                      
                      w_tot = which(format(time_mydata, format = "%Y") == years[k])
                      w_tot = c(w_tot,w_first[w2])
                      
                      if(length(w1)!= 0){
                        if(w_first[w1] %in% w_tot){
                          w_tot_2 = w_tot[-c(which(w_tot == w_first[w1]))]
                        }else{
                          w_tot_2 = w_tot
                        }
                      }else{
                        w_tot_2 = w_tot
                      }
                      
                      out_my = mydata[c(w_tot_2),]
                      
                      
                    }else{
                      out_my = mydata[which(format(time_mydata, format = "%Y") == years[k]),]
                      
                    }
                    #######################
                    w_first = which(format(time_orig, format = "%m") == "01" &
                                      format(time_orig, format = "%d") == "01" &
                                      format(time_orig, format = "%H") == "00" &
                                      format(time_orig, format = "%M") == "00" )
                    y_first = as.numeric(format(time_orig, format = "%Y")[w_first])
                    
                    if(length(w_first)!= 0){
                      w1 = which(y_first == years[k])
                      w2 = which(y_first == years[k]+1)
                      
                      w_tot = which(format(time_orig, format = "%Y") == years[k])
                      w_tot = c(w_tot,w_first[w2])
                      
                      if(length(w1)!= 0){
                        if(w_first[w1] %in% w_tot){
                          w_tot_2 = w_tot[-c(which(w_tot == w_first[w1]))]
                        }else{
                          w_tot_2 = w_tot
                        }
                      }else{
                        w_tot_2 = w_tot
                      }
                      
                      out_orig = orig_wihtout_dupli[c(w_tot_2),]
                      
                      
                    }else{
                      out_orig = orig_wihtout_dupli[which(format(time_orig, format = "%Y") == years[k]),]
                      
                    }
                    
                    # add missing records before in the new structure files
                    first_new_datetime = as.POSIXct(out_my[1,which(colnames(out_my) == datetime_header)],tz = "Etc/GMT-1")
                    
                    all_dates = seq(from = last_old_datetime, to = first_new_datetime, by = datetime_sampling)
                    all_dates = all_dates[-c(1,length(all_dates))]
                    
                    if(length(all_dates)>0){
                      all_dates_df =  data.frame(matrix(nrow =length(all_dates), ncol = ncol(out_my)))
                      colnames(all_dates_df) = colnames(out_my)
                      all_dates_df[,which(colnames(all_dates_df) == datetime_header)] = format(all_dates,format = datetime_format,tz = "Etc/GMT-1")
                      all_dates_df[,which(colnames(all_dates_df) == record_header)] = -1        # Record gap filled with NaN were flagged with RECORD = -1
                      
                      out_my = rbind(all_dates_df, out_my)
                    }
                    
                    colnames(header) = header[1,]
                    # out_my = mydata[which(format(time_mydata, format = "%Y") == years[k]),]
                    colnames(out_my) = colnames(header)
                    out_mydata=rbind(header[-1,],out_my)
                    file_name_output = file_names[k]
                    
                    
                    # out_orig = orig_wihtout_dupli[which(format(time_orig, format = "%Y") == years[k]),]
                    out_orig[,which(colnames(out_orig)== datetime_header)] = format(out_orig[,which(colnames(out_orig)== datetime_header)], format = datetime_format)
                    colnames(out_orig) = colnames(header)
                    out_original=rbind(header[-1,],out_orig)
                    file_name_original = paste(substring(file_names[k], 1, nchar(file_names[k])-4), ".dat",sep = "")
                    
                    # keep updtate logger_info_file!
                    w_logger = which(logger_info_csv[,1] == station_name)
                    new_logger_info = cbind(station_name,header[1,1:8])
                    colnames(new_logger_info) = colnames(logger_info_csv)
                    logger_info_csv[w_logger,] = new_logger_info
                    write.csv(logger_info_csv,logger_info_file,row.names = F, na = "")
                    
                    
                    # write total .dat
                    write.csv(out_mydata,paste(output_dir_data,file_name_output,sep = ""),quote = F,row.names = F, na = "NaN")
                    write.csv(out_original,paste(output_dir_raw,file_name_original,sep = ""),quote = F,row.names = F, na = "NaN")
                    
                    # write total .csv
                    file_name_output_csv = paste(substring(file_name_output, 1, nchar(file_name_output)-4),".csv",sep="") 
                    output_dir_data_csv = substring(output_dir_data, 1, nchar(output_dir_data)-10)  ### NOTA: cartella livello sopra (elimino il num di caratteri di Files_dat)
                    file.copy(from = paste(output_dir_data,file_name_output,sep = ""), to = paste(output_dir_data_csv,file_name_output_csv,sep = ""), overwrite = T)
                    #
                  }
                  ######### end new section ##########
                  
                  flag_missing_records_new_tmp = c(flag_missing_records_new_tmp, 1)
                  
                  header_t = as.data.frame(t(header))
                  old_header_t = as.data.frame(t(old_header))
                  
                  if( nrow(header_t) < nrow(old_header_t)){
                    diff_df = matrix(data = "", nrow = nrow(old_header_t) - nrow(header_t), ncol = ncol(header_t))
                    header_t = rbind(header_t,diff_df)
                  }else{
                    if( nrow(header_t) > nrow(old_header_t)){
                      diff_df = matrix(data = "", nrow = nrow(header_t) - nrow(old_header_t), ncol = ncol(header_t))
                      old_header_t = rbind(old_header_t,diff_df) 
                    }
                  }
                  
                  header_t = cbind(rep(NA, times = nrow(header_t)),header_t )
                  colnames(header_t) = c("NA","Station_info", "Header","Units", "Sampling_method")
                  # colnames(header_t) = paste("row_",seq(1:ncol(header_t))-1,sep = "")
                  rownames(header_t) = paste("col_",seq(1:nrow(header_t))-1,sep = "")
                  header_t = header_t[,-c(1:2)]
                  
                  # old_header_t = as.data.frame(t(old_header))
                  old_header_t = cbind(rep(NA, times = nrow(old_header_t)),old_header_t )
                  colnames(old_header_t) = c("NA","Station_info", "Header","Units", "Sampling_method")
                  # colnames(old_header_t) = paste("row_",seq(1:ncol(old_header_t))-1,sep = "")
                  rownames(old_header_t) = paste("col_",seq(1:nrow(old_header_t))-1,sep = "")
                  old_header_t = old_header_t[,-c(1:2)]
                  
                  # header_t[old_header_t != header_t]
                  # old_header_t[old_header_t != header_t]
                  nrow(old_header_t)
                  nrow(header_t)
                  
                  # ------ NEW -------
                  
                  # intersect(old_header_t$Header,header_t$Header)
                  
                  old_eq= old_header_t[which(old_header_t$Header %in% intersect(old_header_t$Header,header_t$Header)),]
                  new_eq=     header_t[which(    header_t$Header %in% intersect(old_header_t$Header,header_t$Header)),]
                  
                  new_eq = new_eq[ match(old_eq$Header,new_eq$Header), ]
                  w_df = as.data.frame(which(old_eq != new_eq,arr.ind = T))
                  
                  o = old_eq[w_df$row,]
                  n = new_eq[w_df$row,]
                  
                  m_o = melt(o,id.vars = "Header")
                  m_n = melt(n,id.vars = "Header")
                  
                  colnames(m_o)[2:3] = c("Row" ,"Old")
                  colnames(m_n)[2:3] = c("Row" ,"New")
                  
                  mer = merge(m_o, m_n)
                  
                  mer$Header = factor(mer$Header)
                  mer$Row = factor(mer$Row)
                  
                  level_header = intersect(old_header_t$Header,header_t$Header)
                  level_row = c("Units","Sampling_method")
                  
                  mer = mer[order(match(mer$Header, level_header),match(mer$Row, level_row)),]
                  mer = data.frame(mer, stringsAsFactors = F)
                  colnames(mer) = c("Column", "Row", "Old", "New")
                  mer$Column = as.character(mer$Column)
                  mer$Row = as.character(mer$Row)
                  
                  old_h = setdiff(old_header_t$Header,header_t$Header)
                  new_h = setdiff(header_t$Header,old_header_t$Header)
                  
                  
                  old_df = data.frame(match(old_h, old_header_t$Header),rep("Header", times = length(old_h)), old_h,rep("", times = length(old_h)))
                  colnames(old_df) = colnames(mer)
                  old_df$Column = as.character(old_df$Column)
                  
                  
                  new_df = data.frame(match(new_h, header_t$Header),rep("Header", times = length(new_h)),rep("", times = length(new_h)), new_h)
                  colnames(new_df) = colnames(mer)
                  new_df$Column = as.character(new_df$Column)
                  
                  
                  add_remove = rbind(old_df, new_df)
                  
                  if(nrow(mer) == 0  & nrow(add_remove) == 0){
                    df_difference_tmp = data.frame("","Headers reorded", "", "")
                  }else{
                    df_difference_tmp = rbind(mer, add_remove)
                  }
                  
                  # ------------------
                  
                  # w_df = as.data.frame(which(old_header_t != header_t,arr.ind = T))
                  # 
                  # df_difference_tmp = data.frame(rownames(header_t)[w_df$row],
                  #                                colnames(header_t)[w_df$col],
                  #                                old_header_t[old_header_t != header_t],
                  #                                header_t[old_header_t != header_t])
                  
                  colnames(df_difference_tmp) = c("Column", "Row", "Old", "New")
                  df_difference = rbind(df_difference,df_difference_tmp)
                }
                
              }else{
                colnames(header) = header[1,]
                
                
                # -- considero il dato delle yyyy-01-01 00:00 come appartenente all' anno precedente -- 
                w_first = which(format(time_mydata, format = "%m") == "01" &
                                  format(time_mydata, format = "%d") == "01" &
                                  format(time_mydata, format = "%H") == "00" &
                                  format(time_mydata, format = "%M") == "00" )
                y_first = as.numeric(format(time_mydata, format = "%Y")[w_first])
                
                if(length(w_first)!= 0){
                  w1 = which(y_first == years[k])
                  w2 = which(y_first == years[k]+1)
                  
                  w_tot = which(format(time_mydata, format = "%Y") == years[k])
                  w_tot = c(w_tot,w_first[w2])
                  
                  if(length(w1)!= 0){
                    if(w_first[w1] %in% w_tot){
                      w_tot_2 = w_tot[-c(which(w_tot == w_first[w1]))]
                    }else{
                      w_tot_2 = w_tot
                    }
                  }else{
                    w_tot_2 = w_tot
                  }
                  
                  out_my = mydata[c(w_tot_2),]
                  
                  
                }else{
                  out_my = mydata[which(format(time_mydata, format = "%Y") == years[k]),]
                  
                }
                #######################
                w_first = which(format(time_orig, format = "%m") == "01" &
                                  format(time_orig, format = "%d") == "01" &
                                  format(time_orig, format = "%H") == "00" &
                                  format(time_orig, format = "%M") == "00" )
                y_first = as.numeric(format(time_orig, format = "%Y")[w_first])
                
                if(length(w_first)!= 0){
                  w1 = which(y_first == years[k])
                  w2 = which(y_first == years[k]+1)
                  
                  w_tot = which(format(time_orig, format = "%Y") == years[k])
                  w_tot = c(w_tot,w_first[w2])
                  
                  if(length(w1)!= 0){
                    if(w_first[w1] %in% w_tot){
                      w_tot_2 = w_tot[-c(which(w_tot == w_first[w1]))]
                    }else{
                      w_tot_2 = w_tot
                    }
                  }else{
                    w_tot_2 = w_tot
                  }
                  
                  out_orig = orig_wihtout_dupli[c(w_tot_2),]
                  
                  
                }else{
                  out_orig = orig_wihtout_dupli[which(format(time_orig, format = "%Y") == years[k]),]
                  
                }
                
                # -----------------------------------------------------------------------------------------
                
                # out_my = mydata[which(format(time_mydata, format = "%Y") == years[k]),]
                colnames(out_my) = colnames(header)
                out_mydata=rbind(header[-1,],out_my)
                file_name_output = file_names[k]
                
                if(nrow(out_my) == 0){            #  <- risolvo problema legato alla generazione di file senza dati. 
                  write_output_files = FALSE
                }
                
                
                # out_orig = orig_wihtout_dupli[which(format(time_orig, format = "%Y") == years[k]),]
                out_orig[,which(colnames(out_orig)== datetime_header)] = format(out_orig[,which(colnames(out_orig)== datetime_header)], format = datetime_format)
                colnames(out_orig) = colnames(header)
                out_original=rbind(header[-1,],out_orig)
                file_name_original = file_names[k]
                # file_name_original = paste(substring(file_names[k], 1, nchar(file_names[k])-4), ".dat",sep = "")
                
                
                # create a dataframe database formatted
                
                # db_mydata = mydata[which(format(time_mydata, format = "%Y") == years[k]),]
                # db_mydata[, which(colnames(db_mydata) == datetime_header)] = as.POSIXct(db_mydata[, which(colnames(db_mydata) == datetime_header)],tz ='Etc/GMT-1',format = datetime_format)
                # from_date = db_mydata[1,which(colnames(db_mydata) == datetime_header)]
                # to_date = db_mydata[nrow(db_mydata),which(colnames(db_mydata) == datetime_header)]
                # db_mydata[, which(colnames(db_mydata) == datetime_header)] = format(db_mydata[, which(colnames(db_mydata) == datetime_header)],format = datetime_format)
                # colnames(db_mydata) = colnames(header)
                # db_mydata=rbind(header[-1,],db_mydata)
                # date_to_print_filename = paste(paste(format(from_date,format = "%Y"),format(from_date,format = "%m"),format(from_date,format = "%d"),
                #                                      format(from_date,format = "%H"),format(from_date,format = "%M"),sep = ""),
                #                                paste(format(to_date,format = "%Y"),format(to_date,format = "%m"),format(to_date,format = "%d"),
                #                                      format(to_date,format = "%H"),format(to_date,format = "%M"),sep = "" ), sep = "_")
                
                if(write_output_files == TRUE){
                  
                  # keep updtate logger_info_file!
                  w_logger = which(logger_info_csv[,1] == station_name)
                  new_logger_info = cbind(station_name,header[1,1:8])
                  colnames(new_logger_info) = colnames(logger_info_csv)
                  logger_info_csv[w_logger,] = new_logger_info
                  write.csv(logger_info_csv,logger_info_file,row.names = F, na = "")
                  
                  
                  # write total .dat
                  write.csv(out_mydata,paste(output_dir_data,file_name_output,sep = ""),quote = F,row.names = F, na = "NaN")
                  write.csv(out_original,paste(output_dir_raw,file_name_original,sep = ""),quote = F,row.names = F, na = "NaN")
                  
                  # write total .csv
                  file_name_output_csv = paste(substring(file_name_output, 1, nchar(file_name_output)-4),".csv",sep="") 
                  output_dir_data_csv = substring(output_dir_data, 1, nchar(output_dir_data)-10)  ### NOTA: cartella livello sopra (elimino il num di caratteri di Files_dat)
                  file.copy(from = paste(output_dir_data,file_name_output,sep = ""), to = paste(output_dir_data_csv,file_name_output_csv,sep = ""), overwrite = T)
                  # write.csv(db_mydata, paste(database_dir ,substring(file_name_output,1, nchar(file_name_output)-8),date_to_print_filename, ".csv",sep = ""),quote = F,row.names = F, na = "NaN")
                }
                
              }
              
              
              # inserire qui conversione flag tmp
              if(all(flag_new_duplicates_rows_tmp == 0)){
                flag_new_duplicates_rows = 0
              } else{
                flag_new_duplicates_rows = 1
              }
              
              if(all(flag_new_overlap_tmp == 0)){
                flag_new_overlap = 0
              } else{
                flag_new_overlap = 1
              }
              
              if(all(flag_new_missing_dates_tmp == 0)){
                flag_new_missing_dates = 0
              } else{
                flag_new_missing_dates = 1
              }
              
              if(record_check == 1){
                if(all(flag_missing_records_new_tmp == 0)){
                  flag_missing_records_new = 0
                } else{
                  flag_missing_records_new = 1
                }
              }else{
                flag_missing_records_new = 50
              }
            }
            
          }
          
        }
      }
    }
    # }
  }
  
  # new_missing_index_date_tot
  
  flags_names = c("flag_empty","flag_logger_number","flag_error_df","flag_date","flag_duplicates_rows","flag_overlap","flag_missing_records","flag_missing_dates","flag_range_variable_to_set","flag_range_variable_new","flag_out_of_range", "flag_new_duplicates_rows", "flag_new_overlap", "flag_new_missing_dates" ,"flag_missing_records_new")
  flags_df = data.frame(flags_names, rep(NA,times = length(flags_names)))
  colnames(flags_df) = c("flag_names", "value")
  
  for(i in 1: nrow(flags_df)){
    if(exists(flags_names[i])){
      flags_df$value[i] = eval(parse(text = flags_names[i]))
    }
  }
  
  if(!exists("mydata")){
    mydata= NULL
    
  }
  
  if(!exists("mydata_out_of_range")){
    mydata_out_of_range= NULL
    
  }
  
  # output1 = list(mydata, flags_df,file_names)
  
  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # PART 2 --> PREPARE STATISTICS AND REPORT INFORMATION
  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  
  # - - - -  Provide information of empty file - - - - - - - - - - - - - 
  
  if(!is.na(flag_empty) & flag_empty == 1){
    
    output_empty = list("Y", NA)
    names(output_empty) =c("Status", "Values")
  }else{
    output_empty = list("N",NA)
    names(output_empty) =c("Status", "Values")
  }
  
  # - - - -  Provide difference on logger numbers - - - - - - - - - - - - - 
  
  if(!is.na(flag_logger_number) & flag_logger_number == 1){
    # logger_difference
    
    # file_logger_numb = logger_number 
    # old_logger_numb = logger_info[,2]
    # logger_numbers=c(old_logger_numb,file_logger_numb)
    # names(logger_numbers) = c("old", "new")
    
    output_logger_number = list("Y",logger_difference)
    names(output_logger_number) = c("Status", "Values")
    
  }else{
    output_logger_number =list("N", NA)
    names(output_logger_number) = c("Status", "Values")
  }
  
  # - - - -  Provide difference on data structure - - - - - - - - - - - - - 
  if(!is.na(flag_error_df) & (flag_error_df == 2)){
    w1 = as.data.frame(which(data_star == "",arr.ind = T))
    names(w1) = c("row", "col")
    df = data.frame(  data_star[w1$row, which(header_colnames == datetime_header )], as.character(header_colnames)[w1$col])
    colnames(df) = c(datetime_header, "Variable")
    
    u = unique(df$Variable)
    
    out_df = data.frame(matrix(ncol = 3, nrow = length(u)))
    colnames(out_df) = c( "Empty cells", "From", "To")  
    i=1
    for(i in 1:length(u)){
      df_date = as.POSIXct(df[,1],format = datetime_format, tz = "Etc/GMT-1") 
      min_date = min(df_date)
      max_date = max(df_date)
      
      seq_date = seq(min_date, max_date, by = datetime_sampling)
      
      
      stat = rep(0, times = length(seq_date))
      stat[which(seq_date %in% df_date )] = 1
      stat = c(0,stat,0)
      diff = diff(stat)
      
      start_blank = seq_date[which(diff == 1)]
      end_blank = seq_date[which(diff == -1)-1]
      
      out_df$From[i] = format(start_blank, format = datetime_format)
      out_df$To[i] =  format(end_blank, format = datetime_format)
      out_df[i,1] = u[i]
      
    }
    
    output_structure = list("Y",out_df)
    names(output_structure) = c("Status", "Values")
    
    
  }else{
    
    if(!is.na(flag_error_df) & (flag_error_df == 1 | flag_error_df == -1 )){
      
      if(max_col == ncol(header) | max_col == ncol(header)){  #### To check! Not sure that it works!
        ncol_vect = c(ncol(header),ncol(data))
        names(ncol_vect) = c("ncol_header", "ncol_data")
      }else{
        if(max_col >  ncol(header)){
          ncol_vect = c(ncol(header),max_col)
          names(ncol_vect) = c("ncol_header", "ncol_data")
        }else{
          if(max_col >  ncol(data)){
            ncol_vect = c(max_col,ncol(header))
            names(ncol_vect) = c("ncol_header", "ncol_data")
          }
        }
      }
      
      output_structure = list("Y",ncol_vect)
      names(output_structure) = c("Status", "Values")
    }else{
      output_structure = list("N",NA)
      names(output_structure) = c("Status", "Values")
      
      # if(exists("df_difference")){
      #   if(!is.na(flag_error_df) & (flag_error_df == 0  &  nrow(df_difference) != 0 )){
      #     # structure_message = df_difference
      #     output_structure = list("Y",df_difference)
      #     names(output_structure) = c("Status", "Values")
      #   }else{
      #     output_structure = list("N",NA)
      #     names(output_structure) = c("Status", "Values")
      #   }
      # }else{
      #   output_structure = list("N",NA)
      #   names(output_structure) = c("Status", "Values")
      # }
    }
  }
  
  # - - - -  Structure change: warning - - - - - - - - - - - - - 
  
  if(exists("df_difference")){
    if(!is.na(flag_error_df) & (flag_error_df == 0  &  nrow(df_difference) != 0 )){
      # structure_message = df_difference
      output_structure_change = list("Y",df_difference)
      names(output_structure_change) = c("Status", "Values")
    }else{
      output_structure_change = list("N",NA)
      names(output_structure_change) = c("Status", "Values")
    }
  }else{
    output_structure_change = list("N",NA)
    names(output_structure_change) = c("Status", "Values")
  }
  
  # - - - -  Provide date issue - - - - - - - - - - - - - 
  
  if(!is.na(flag_date) & flag_date == 1){
    dates_flag_date = c(start_date, recent_date)
    names(dates_flag_date) = c("Download_table_date", "Last_file_date")
    output_no_new_data = list("Y", dates_flag_date)
    names(output_no_new_data) =c("Status", "Values")
  }else{
    output_no_new_data = list("N",NA)
    names(output_no_new_data) =c("Status", "Values")
  }
  
  
  # - - - -  Provide overlaps - - - - - - - - - - - - -   # da modificare! --> no lista date ma periodo (inizio/fine)
  
  
  if(!is.na(flag_overlap) & flag_overlap == 1){
    # overlap_date = as.character(as.POSIXct(unique(overlap[,which(colnames(overlap) == datetime_header)]), tz = "Etc/GMT-1"))
    overlap_date = as.POSIXct(unique(overlap[,which(colnames(overlap) == datetime_header)]), tz = "Etc/GMT-1")
    
    min_date = min(overlap_date)
    max_date = max(overlap_date)
    
    seq_date = seq(min_date, max_date, by = datetime_sampling)
    
    
    stat = rep(0, times = length(seq_date))
    stat[which(seq_date %in% overlap_date )] = 1
    stat = c(0,stat,0)
    diff = diff(stat)
    
    start_overlap = seq_date[which(diff == 1)]
    end_overlap = seq_date[which(diff == -1)-1]
    
    n_record = which(diff == -1)-which(diff == 1)
    
    # end_tmp = seq(end_overlap,by = datetime_sampling,length.out = 2)[2]  # fix error in difference!
    # hour_overlap =  difftime(time1 = end_tmp,time2 = start_overlap,units = "hours")
    hour_overlap =  difftime(time1 = end_overlap,time2 = start_overlap,units = "hours")
    
    overl_table = data.frame(start_overlap, end_overlap, n_record, hour_overlap)
    colnames(overl_table) = c("From", "To", "Number of Record", "Hours")
    
    output_overlap = list("Y", overl_table)
    names(output_overlap) =c("Status", "Values")
  }else{
    if(!is.na(flag_new_overlap) & flag_new_overlap == 1){                                   #  possibile errore su new overlap --> testare se prende gli overlap corretti!
      overlap_date = as.POSIXct(unique(new_overlap_tot[,which(colnames(new_overlap_tot) == datetime_header)]), tz = "Etc/GMT-1")
      min_date = min(overlap_date)
      max_date = max(overlap_date)
      
      seq_date = seq(min_date, max_date, by = datetime_sampling)
      
      
      stat = rep(0, times = length(seq_date))
      stat[which(seq_date %in% overlap_date )] = 1
      stat = c(0,stat,0)
      diff = diff(stat)
      
      start_overlap = seq_date[which(diff == 1)]
      end_overlap = seq_date[which(diff == -1)-1]
      
      n_record = which(diff == -1)-which(diff == 1)
      
      hour_overlap =  difftime(time1 = end_overlap,time2 = start_overlap,units = "hours")
      
      overl_table = data.frame(start_overlap, end_overlap, n_record, hour_overlap)
      colnames(overl_table) = c("From", "To", "Number of Record", "Hours")
      
      output_overlap = list("Y", overl_table)
      names(output_overlap) =c("Status", "Values")
    }else{
      output_overlap = list("N", NA)
      names(output_overlap) =c("Status", "Values")
    }
  }
  
  # - - - -  Provide duplicated rows - - - - - - - - - - - - -   # da modificare! --> no lista date ma periodo (inizio/fine)
  
  
  if(!is.na(flag_duplicates_rows) & flag_duplicates_rows == 1){
    output_duplicates_rows = list("Y", NA)
    names(output_duplicates_rows) =c("Status", "Values")
  }else{
    if(!is.na(flag_new_duplicates_rows) & flag_new_duplicates_rows == 1){
      output_duplicates_rows = list("Y", NA)
      names(output_duplicates_rows) =c("Status", "Values")
    }else{
      output_duplicates_rows = list("N", NA)
      names(output_duplicates_rows) =c("Status", "Values")
    }
  }
  # - - - -  Provide table of missing records - - - - - - - - - - - - - 
  
  if(!exists("records_missing")){
    records_missing = as.data.frame(matrix(ncol = 6, nrow = 0))
    colnames(records_missing) = c("Datetime_From","Datetime_To", "Datetime_Missing"," Record_From", "Record_To","Record_Missing")
  }
  if(!exists("records_missing_new")){
    records_missing_new = as.data.frame(matrix(ncol = 6, nrow = 0))
    colnames(records_missing_new) = c("Datetime_From","Datetime_To", "Datetime_Missing"," Record_From", "Record_To","Record_Missing")
  }
  
  if(!exists("records_restart")){
    records_restart = as.data.frame(matrix(ncol = 6, nrow = 0))
    colnames(records_restart) = c("Datetime_From","Datetime_To", "Datetime_Missing"," Record_From", "Record_To","Record_Missing")
  }
  if(!exists("records_restart_new")){
    records_restart_new = as.data.frame(matrix(ncol = 6, nrow = 0))
    colnames(records_restart_new) = c("Datetime_From","Datetime_To", "Datetime_Missing"," Record_From", "Record_To","Record_Missing")
  }
  
  if(nrow(records_missing) != 0 | nrow(records_missing_new) != 0){
    table_missing_record = rbind(records_missing[,c(1:5)],records_missing_new[,c(1:5)])
    colnames(table_missing_record) = c("Last.date.Before", "First.date.After", "Date.Gap","Last.record.Before","First.record.After")
  }else{
    table_missing_record = data.frame()
  }
  
  if(nrow(records_restart) != 0 | nrow(records_restart_new) != 0){
    table_restart_record = rbind(records_restart[,c(1:5)],records_restart_new[,c(1:5)])
    colnames(table_restart_record) =  c("Last.date.Before", "First.date.After", "Date.Gap","Last.record.Before","First.record.After")
    table_restart_record = table_restart_record[table_restart_record$Date.Gap != 0,]
  }else{
    table_restart_record = data.frame()
  }
  
  if(nrow(table_missing_record) != 0){
    output_missing_record = list("Y", table_missing_record)
    names(output_missing_record) =c("Status", "Values")
  }else{
    output_missing_record = list("N", NA)
    names(output_missing_record) =c("Status", "Values")
  }
  
  if(nrow(table_restart_record) != 0){
    output_restart_record = list("Y", table_restart_record)
    names(output_restart_record) =c("Status", "Values")
  }else{
    output_restart_record = list("N", NA)
    names(output_restart_record) =c("Status", "Values")
  }
  
  
  
  # - - - -  Provide missing dates - - - - - - - - - - - - - 
  # missing_index_date
  
  
  if((!is.na(flag_missing_dates) & flag_missing_dates == 1)|(!is.na(flag_new_missing_dates) & flag_new_missing_dates == 1)){
    
    date_missing = rbind(missing_index_date,new_missing_index_date_tot)
    
    
    # #####
    # seq.POSIXt(from = date_missing$Date[1],to = date_missing$Date[length(date_missing$Date)], by = datetime_sampling)
    # 
    # #####
    
    time_tot <- as.POSIXct(mydata[,which(colnames(mydata) == datetime_header)], format = datetime_format, tz = 'Etc/GMT-1' )
    # time_tot <- c(new_missing_index_date$Date, time_tot) 
    # time_tot <- unique(c(date_missing$Date,time_tot)) # don't work! issues whith datatime --> unexpected timezone conversion! Why? 
    time_tot <- as.POSIXct(unique(c(as.character(date_missing$Date), as.character(time_tot))),tz = "Etc/GMT-1") 
    
    time_tot = time_tot[order(time_tot)]
    
    # time_missing <- missing_index_date[,2]
    time_missing <- date_missing[,2]
    
    
    df_missing <- data.frame(time_tot,rep("Dates in original file",times = length(time_tot)))
    colnames(df_missing) = c("time","Status")
    df_missing[which(time_tot %in% time_missing ),2] = "Missing dates filled"
    y = rep(1, times = length(time_tot))
    
    Status_num = rep(1,times = length(time_tot))
    Status_num[which(time_tot %in% time_missing )] = 0
    
    df_missing = cbind(df_missing, y,Status_num)
    Status_num_NA=df_missing
    Status_num_NA = Status_num_NA[,-c(2,3)]
    
    differ = c(0,diff(Status_num_NA$Status_num))
    
    if(Status_num_NA$Status_num[1] == 0){
      differ[1] = -1
    }
    
    start = which(differ == -1)
    end  = which(differ == 1) - 1
    gap_lenght = end - start + 1
    
    date_start = Status_num_NA$time[start]
    date_end = Status_num_NA$time[end]
    
    if(length(date_end) != 0){
      date_end_tmp = as.POSIXct("1990-01-01 00:00")    # this for cycle is to fix a bug on time difference
      for(k in 1:length(date_end)){
        
        date_end_tmp[k] =  seq.POSIXt(date_end[k], by = datetime_sampling, length. =  2)[2]
      }
      gap_hour = difftime(time1 = date_end_tmp,time2 = date_start,units = "hours")
    }else{
      gap_hour = numeric(0)
    }
    
    statistic_missing = data.frame(date_start,date_end,gap_lenght,gap_hour)
    colnames(statistic_missing) = c("From", "To", "Number of Record", "Hours")
    statistic_missing[,1:2] = format(statistic_missing[,1:2], format = datetime_format)
    
    date_missing_period = statistic_missing
    
    output_date_missing = list("Y",date_missing_period )
    # output_restart_record = list("N", NA)
    names(output_date_missing) =c("Status", "Values")
  }else{
    # date_missing = NULL
    output_date_missing = list("N", NA)
    names(output_date_missing) =c("Status", "Values")
  }
  
  # - - - -  Provide ALERT out of range - - - - - - - - - - - - -
  
  
  if((!is.na(flag_out_of_range_ALERT) & flag_out_of_range_ALERT == 1)){
    output_out_of_range_ALERT = list("Y", alert_out_of_range_table)
    names(output_out_of_range_ALERT) =c("Status", "Values")
  }else{
    output_out_of_range_ALERT = list("N", NA)
    names(output_out_of_range_ALERT) =c("Status", "Values")
  }
  
  # - - - -  Provide out of range - - - - - - - - - - - - -
  
  
  if((!is.na(flag_out_of_range) & flag_out_of_range == 1)){
    # out_of_range_table[,3:4] = format(out_of_range_table[,3:4], format = datetime_format)
    output_out_of_range = list("Y", out_of_range_table)
    names(output_out_of_range) =c("Status", "Values")
  }else{
    output_out_of_range = list("N", NA)
    names(output_out_of_range) =c("Status", "Values")
  }
  
  
  
  
  # - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -  
  # output_empty
  # output_logger_number
  # output_structure
  # output_no_new_data
  # output_overlap
  # output_missing_record
  # output_restart_record
  # output_date_missing
  # output_out_of_range
  
  errors_output = list(output_empty,
                       output_logger_number,
                       output_structure,
                       output_structure_change,
                       output_no_new_data,
                       output_overlap,
                       output_duplicates_rows,
                       output_missing_record,
                       output_restart_record,
                       output_date_missing,
                       output_out_of_range_ALERT,
                       output_out_of_range )
  
  names(errors_output) = c("err_empty",
                           "err_logger_number",
                           "err_structure",
                           "err_structure_change",
                           "err_no_new_data",    # "err_date_issue",
                           "err_overlap",
                           "err_duplicates_rows",
                           "err_missing_record",
                           "err_restart_record",
                           "err_date_missing",
                           "err_range_alert",
                           "err_out_of_range")
  
  
  # status = lapply(errors_output, function(x) x[[1]])
  
  
  
  # output2 = list(mydata, flags_df,file_names, logger_numbers, structure_message, overlap_date, table_missing_record, table_restart_record,date_missing)
  output2 = list(mydata, flags_df, file_names, errors_output, mydata_out_of_range)
  
  
  
  
  return(output2)
}

