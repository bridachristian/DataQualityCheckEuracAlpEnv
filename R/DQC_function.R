

DQC_function = function(input_dir,
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
                        database_dir,
                        logger_info_file,
                        record_check,
                        output_dir_raw){
  
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
    
    rm(data_import)
    
    logger_number = header[1,4]                                                                   # check logger numbers
    software_version = header[1,6]
    logger_info_csv = read.csv(file = logger_info_file, stringsAsFactors = F)
    w_logger = which(logger_info_csv[,1] == station_name)
    
    if(length(w_logger) == 0){
      logger_info_csv = rbind(logger_info_csv, c(station_name, logger_number, software_version))
      write.csv(logger_info_csv,logger_info_file,row.names = F,col.names = F )
    }else{
      logger_info = logger_info_csv[w_logger,]
      if(logger_number != logger_info[,2]){
        flag_logger_number = 1
      }else{
        flag_logger_number = 0
      }
    }
    
    if(flag_logger_number == 0){
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
              
              range <- exclude_out_of_range(DATA = mydata,DATETIME_HEADER = datetime_header, RANGE_DIR = range_dir, RANGE_FILE = range_file) # <- Substitute with NA data out of phisical range
              mydata = range[[1]]
              check_out_of_range = range[[2]]
              variable_new = range[[3]]
              variable_to_set = range[[4]]
              
              rm(range)
              # ..... Flags .....................................................................................................................................
              
              if(length(variable_to_set) != 0){
                flag_range_variable_to_set = 1
              }else{
                flag_range_variable_to_set = 0
              }
              
              if(length(variable_new) != 0){
                flag_range_variable_new = 1
              }else{
                flag_range_variable_new = 0
              }
              
              
              if(1 %in% unique(unlist(apply(X = check_out_of_range[,-which(colnames(check_out_of_range) == datetime_header)],MARGIN = 2, unique)))){
                flag_out_of_range = 1
              }else{
                if(-1 %in% unique(unlist(apply(X = check_out_of_range[,-which(colnames(check_out_of_range) == datetime_header)],MARGIN = 2, unique)))){
                  flag_out_of_range = 1
                }else{
                  flag_out_of_range = 0
                }
              }
              
              
              time_tot = as.POSIXct(mydata[,which(colnames(mydata) == datetime_header)], format = datetime_format, tz = 'Etc/GMT-1')
              time_missing = missing_index_date[,2]
              
              if(length(which(time_tot %in% time_missing )) == 0){
                flag_missing_dates = 0      # No missing dates
              }else{
                flag_missing_dates = 1      # YES missing dates
              }
              
              mydata <- time_to_char(DATA = mydata, DATETIME_HEADER = datetime_header, DATETIME_FORMAT = datetime_format)
              
            }
          }
        }
      }
    }
  }
  
  
  
  
  
  # ..... Output ..........................................................................................................................................
  file_names = NULL
  
  
  if(flag_empty == 0){
    if(flag_logger_number == 0){
      if(flag_error_df == 0){
        if(flag_date == 0){
          if(flag_overlap == 0){
            if(flag_missing_records != 1){
              if(write_output_files == TRUE){
                time_mydata = as.POSIXct(mydata[,which(colnames(mydata)== datetime_header)],format = datetime_format, tz = 'Etc/GMT-1')
                time_orig = as.POSIXct(orig_wihtout_dupli[,which(colnames(orig_wihtout_dupli)== datetime_header)],format = datetime_format, tz = 'Etc/GMT-1')
                years = as.numeric(unique(format(time_mydata, format = "%Y")))
                file_names = paste(station_name,"_", years,".csv",sep = "")
                
                flag_new_duplicates_rows_tmp = c()
                flag_new_overlap_tmp = c()
                flag_new_missing_dates_tmp = c()
                flag_missing_records_new_tmp = c()
                df_difference = as.data.frame(matrix(ncol = 4, nrow = 0))
                colnames(df_difference) = c("Column", "Row", "Old", "New")
                
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
                      
                      # append new data to old data if headers new and old are the same
                      df_toadd =  mydata[which(format(time_mydata, format = "%Y") == years[k]),]
                      df_toadd[,which(colnames(df_toadd)== datetime_header)] = as.POSIXct(format(df_toadd[,which(colnames(df_toadd)== datetime_header)],format = datetime_format),tz = "Etc/GMT-1")
                      new = rbind(old_data,df_toadd)
                      new[order(new$TIMESTAMP),]
                      new = new[order(new[,which(colnames(new) == datetime_header)]),]
                      
                      # append new raw data to old data if headers new and old are the same
                      df_toadd_raw = orig_wihtout_dupli[which(format(time_orig, format = "%Y") == years[k]),]
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
                      
                      if(unique(as.character(new_duplicated_data[1,])) == "---"){
                        flag_new_duplicates_rows_tmp = c(flag_new_duplicates_rows_tmp,0)
                      } else{
                        flag_new_duplicates_rows_tmp = c(flag_new_duplicates_rows_tmp,1)
                      }
                      
                      
                      new_duplicated_data = time_to_char(DATA = new_duplicated_data, DATETIME_HEADER = datetime_header, DATETIME_FORMAT = datetime_format)
                      raw_new_duplicated_data = time_to_char(DATA = raw_new_duplicated_data, DATETIME_HEADER = datetime_header, DATETIME_FORMAT = datetime_format)
                      
                      new_overlap <- detect_overlap(DATA = new_mydata,DATETIME_HEADER = datetime_header, RECORD_HEADER = record_header) 
                      
                      if(length(new_overlap) == 0){
                        
                        flag_new_overlap_tmp = c(flag_new_overlap_tmp,0)
                        
                        if(record_check == 1){
                          w_last = which(new_mydata[,which(colnames(new_mydata) == datetime_header)] == last_old_datetime)
                          rec_miss  <- missing_record(DATA = new_mydata[w_last:nrow(new_mydata),], DATETIME_HEADER = datetime_header, RECORD_HEADER = record_header, DATETIME_SAMPLING = datetime_sampling, DATETIME_FORMAT = datetime_format)  # <- fill missing dates with NA
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
                          
                          
                          # r = 0
                          # repeat{
                          #   r = r+1 
                          #   tmp_new_datetime = new_mydata[which(new_mydata[,which(colnames(new_mydata) == datetime_header)] == last_old_datetime)+r, which(colnames(new_mydata) == datetime_header)] # check if date is too late!!
                          #   
                          #   tmp_new_record = new_mydata[which(new_mydata[,which(colnames(new_mydata) == datetime_header)] == last_old_datetime)+r, which(colnames(new_mydata) == record_header)] 
                          #   if(tmp_new_record != -1){
                          #     break()
                          #   }
                          # }
                          # 
                          # first_new_datetime = new_mydata[which(new_mydata[,which(colnames(new_mydata) == datetime_header)] == last_old_datetime)+r, which(colnames(new_mydata) == datetime_header)]
                          # 
                          # first_new_record = new_mydata[which(new_mydata[,which(colnames(new_mydata) == datetime_header)] == last_old_datetime)+r, which(colnames(new_mydata) == record_header)] 
                          # 
                          # if(first_new_record == last_old_record + 1| first_new_record == 0 | first_new_record == -1){
                          #   flag_append_tmp = 0
                          #   flag_append_new_tmp = c(flag_append_new_tmp,flag_append_tmp)
                          # } else {
                          #   flag_append_tmp = -1
                          #   flag_append_new_tmp = c(flag_append_new_tmp, flag_append_tmp)
                          # }
                          
                          
                          if(record_check != 1 | flag_missing_records_new_tmp != 1){     
                            # We avoid to write output if record control is active (record_check = 1) and record has some issues (indicated by flag_append_new = -1)
                            
                            # prepare tata for output
                            
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
                            
                            rm(missing)
                            
                            colnames(header) = header[1,]
                            out_my = new_mydata
                            colnames(out_my) = colnames(header)
                            out_mydata=rbind(header[-1,],out_my)
                            file_name_output = file_names[k]
                            flag_missing_records_new_tmp = c(flag_missing_records_new_tmp, 0)
                            write.csv(out_mydata,paste(output_dir_data,file_name_output,sep = ""),quote = F,row.names = F, na = "NaN")
                            
                            out_orig = orig_data_new[which(format(new_time_orig, format = "%Y") == years[k]),]
                            out_orig[,which(colnames(out_orig)== datetime_header)] = format(out_orig[,which(colnames(out_orig)== datetime_header)], format = datetime_format)
                            colnames(out_orig) = colnames(header)
                            out_original=rbind(header[-1,],out_orig)
                            file_name_original = paste(substring(file_names[k], 1, nchar(file_names[k])-4), ".dat",sep = "")
                            write.csv(out_original,paste(output_dir_raw,file_name_original,sep = ""),quote = F,row.names = F, na = "NaN")
                            
                            # create a dataframe database formatted
                            
                            db_mydata = new_mydata
                            db_mydata[, which(colnames(db_mydata) == datetime_header)] = as.POSIXct(db_mydata[, which(colnames(db_mydata) == datetime_header)],tz ='Etc/GMT-1',format = datetime_format)
                            first_row_selected = which(db_mydata[, which(colnames(db_mydata) == datetime_header)] == last_old_datetime)+1
                            db_mydata = db_mydata[first_row_selected: nrow(db_mydata),]
                            from_date = db_mydata[1,which(colnames(db_mydata) == datetime_header)]
                            to_date = db_mydata[nrow(db_mydata),which(colnames(db_mydata) == datetime_header)]
                            db_mydata[, which(colnames(db_mydata) == datetime_header)] = format(db_mydata[, which(colnames(db_mydata) == datetime_header)],format = datetime_format ) #change here if you want change datetime output format (for example  in database "%Y-%m-%dT%H:%M")
                            colnames(db_mydata) = colnames(header)
                            db_mydata=rbind(header[-1,],db_mydata)
                            date_to_print_filename = paste(paste(format(from_date,format = "%Y"),format(from_date,format = "%m"),format(from_date,format = "%d"),
                                                                 format(from_date,format = "%H"),format(from_date,format = "%M"),sep = ""),
                                                           paste(format(to_date,format = "%Y"),format(to_date,format = "%m"),format(to_date,format = "%d"),
                                                                 format(to_date,format = "%H"),format(to_date,format = "%M"),sep = "" ), sep = "_")
                            
                            write.csv(db_mydata, paste(database_dir ,substring(file_name_output,1, nchar(file_name_output)-8),date_to_print_filename, ".csv",sep = ""),quote = F,row.names = F, na = "NaN")
                            
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
                      
                      # rename total file
                      
                      j=0
                      repeat{
                        j=j+1
                        file_names_old = paste(substring(file_names[k],1, nchar(file_names[k])-4),"_old",j,".csv",sep = "")
                        if(!file.exists(paste(output_dir_data,file_names_old,sep = ""))){
                          break
                        }
                      }
                      file_names_total_data = file_names[k]
                      
                      file.rename(from = paste(output_dir_data,file_names_total_data,sep = ""),to = paste(output_dir_data,file_names_old,sep = ""))
                      
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
                      
                      colnames(header) = header[1,]
                      out_my = mydata[which(format(time_mydata, format = "%Y") == years[k]),]
                      colnames(out_my) = colnames(header)
                      out_mydata=rbind(header[-1,],out_my)
                      file_name_output = file_names[k]
                      write.csv(out_mydata,paste(output_dir_data,file_name_output,sep = ""),quote = F,row.names = F, na = "NaN")
                      
                      
                      out_orig = orig_wihtout_dupli[which(format(time_orig, format = "%Y") == years[k]),]
                      out_orig[,which(colnames(out_orig)== datetime_header)] = format(out_orig[,which(colnames(out_orig)== datetime_header)], format = datetime_format)
                      colnames(out_orig) = colnames(header)
                      out_original=rbind(header[-1,],out_orig)
                      file_name_original = paste(substring(file_names[k], 1, nchar(file_names[k])-4), ".dat",sep = "")
                      write.csv(out_original,paste(output_dir_raw,file_name_original,sep = ""),quote = F,row.names = F, na = "NaN")
                      
                      ######### end new section ##########
                      
                      flag_missing_records_new_tmp = c(flag_missing_records_new_tmp, 1)
                      
                      header_t = as.data.frame(t(header))
                      header_t = cbind(rep(NA, times = nrow(header_t)),header_t )
                      colnames(header_t) = c("NA","Station_info", "Header","Units", "Sampling_method")
                      # colnames(header_t) = paste("row_",seq(1:ncol(header_t))-1,sep = "")
                      rownames(header_t) = paste("col_",seq(1:nrow(header_t))-1,sep = "")
                      header_t = header_t[,-c(1:2)]
                      
                      old_header_t = as.data.frame(t(old_header))
                      old_header_t = cbind(rep(NA, times = nrow(old_header_t)),old_header_t )
                      colnames(old_header_t) = c("NA","Station_info", "Header","Units", "Sampling_method")
                      # colnames(old_header_t) = paste("row_",seq(1:ncol(old_header_t))-1,sep = "")
                      rownames(old_header_t) = paste("col_",seq(1:nrow(old_header_t))-1,sep = "")
                      old_header_t = old_header_t[,-c(1:2)]
                      
                      # header_t[old_header_t != header_t]
                      # old_header_t[old_header_t != header_t]
                      
                      w_df = as.data.frame(which(old_header_t != header_t,arr.ind = T))
                      
                      df_difference_tmp = data.frame(rownames(header_t)[w_df$row],
                                                     colnames(header_t)[w_df$col],
                                                     old_header_t[old_header_t != header_t],
                                                     header_t[old_header_t != header_t])
                      
                      colnames(df_difference_tmp) = c("Column", "Row", "Old", "New")
                      df_difference = rbind(df_difference,df_difference_tmp)
                    }
                    
                  }else{
                    colnames(header) = header[1,]
                    out_my = mydata[which(format(time_mydata, format = "%Y") == years[k]),]
                    colnames(out_my) = colnames(header)
                    out_mydata=rbind(header[-1,],out_my)
                    file_name_output = file_names[k]
                    write.csv(out_mydata,paste(output_dir_data,file_name_output,sep = ""),quote = F,row.names = F, na = "NaN")
                    
                    
                    out_orig = orig_wihtout_dupli[which(format(time_orig, format = "%Y") == years[k]),]
                    out_orig[,which(colnames(out_orig)== datetime_header)] = format(out_orig[,which(colnames(out_orig)== datetime_header)], format = datetime_format)
                    colnames(out_orig) = colnames(header)
                    out_original=rbind(header[-1,],out_orig)
                    file_name_original = paste(substring(file_names[k], 1, nchar(file_names[k])-4), ".dat",sep = "")
                    write.csv(out_original,paste(output_dir_raw,file_name_original,sep = ""),quote = F,row.names = F, na = "NaN")
                    
                    
                    # create a dataframe database formatted
                    
                    db_mydata = mydata[which(format(time_mydata, format = "%Y") == years[k]),]
                    db_mydata[, which(colnames(db_mydata) == datetime_header)] = as.POSIXct(db_mydata[, which(colnames(db_mydata) == datetime_header)],tz ='Etc/GMT-1',format = datetime_format)
                    from_date = db_mydata[1,which(colnames(db_mydata) == datetime_header)]
                    to_date = db_mydata[nrow(db_mydata),which(colnames(db_mydata) == datetime_header)]
                    db_mydata[, which(colnames(db_mydata) == datetime_header)] = format(db_mydata[, which(colnames(db_mydata) == datetime_header)],format = datetime_format)
                    colnames(db_mydata) = colnames(header)
                    db_mydata=rbind(header[-1,],db_mydata)
                    date_to_print_filename = paste(paste(format(from_date,format = "%Y"),format(from_date,format = "%m"),format(from_date,format = "%d"),
                                                         format(from_date,format = "%H"),format(from_date,format = "%M"),sep = ""),
                                                   paste(format(to_date,format = "%Y"),format(to_date,format = "%m"),format(to_date,format = "%d"),
                                                         format(to_date,format = "%H"),format(to_date,format = "%M"),sep = "" ), sep = "_")
                    write.csv(db_mydata, paste(database_dir ,substring(file_name_output,1, nchar(file_name_output)-8),date_to_print_filename, ".csv",sep = ""),quote = F,row.names = F, na = "NaN")
                    
                    
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
      }
    }
  }
  
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
  
  output1 = list(mydata, flags_df,file_names)
  
  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # PART 2 --> PREPARE STATISTICS AND REPORT INFORMATION
  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  
  # - - - -  Provide difference on logger numbers - - - - - - - - - - - - - 
  
  if(flag_logger_number == 1){
    file_logger_numb = logger_number 
    old_logger_numb = logger_info[,2]
    logger_numbers=c(old_logger_numb,file_logger_numb)
    names(logger_numbers) = c("old", "new")
  }else{
    logger_numbers = NULL
  }
  
  # - - - -  Provide difference on data structure - - - - - - - - - - - - - 
  
  if(flag_error_df == 1 | flag_error_df == -1){
    structure_message = paste("Headers has",ncol(header), "columns while data has",ncol(data),"columns")
  }else{
    if(flag_error_df == 0  & nrow(df_difference) != 0 ){
      structure_message = df_difference
    }else{
      structure_message = NULL
    }
  }
  
  # - - - -  Provide overlaps - - - - - - - - - - - - - 
  
  if(flag_overlap == 1){
    overlap_date = as.POSIXct(unique(overlap$TIMESTAMP), tz = "Etc/GMT-1")
  }else{
    if(flag_new_overlap == 1){
      overlap_date = as.POSIXct(unique(new_overlap$TIMESTAMP), tz = "Etc/GMT-1")
    }else{
      overlap_date = NULL
    }
  }

  
  # - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -  
  
  output2 = list(mydata, flags_df,file_names, logger_numbers, structure_message, overlap_date )
  
  return(output2)
}

