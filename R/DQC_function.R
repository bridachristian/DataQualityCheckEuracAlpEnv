

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
              
              range <- exclude_out_of_range_v2(DATA = mydata,DATETIME_HEADER = datetime_header, RANGE_DIR = range_dir, RANGE_FILE = range_file) # <- Substitute with NA data out of phisical range
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
              mydata_1 = mydata
            }
          }
        }
      }
    }
  }
  
  
  
  # missing_index_date
  
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
                
                new_missing_index_date_tot = c()
                
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
                      # new[order(new$TIMESTAMP),]
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
                          
                          new_missing_index_date_tot = rbind(new_missing_index_date_tot,new_missing_index_date)
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
                            
                            rm(new_missing)
                            
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
  
  output1 = list(mydata, flags_df,file_names)
  
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
    file_logger_numb = logger_number 
    old_logger_numb = logger_info[,2]
    logger_numbers=c(old_logger_numb,file_logger_numb)
    names(logger_numbers) = c("old", "new")
    
    output_logger_number = list("Y",logger_numbers)
    names(output_logger_number) = c("Status", "Values")
    
  }else{
    output_logger_number =list("N", NA)
    names(output_logger_number) = c("Status", "Values")
  }
  
  # - - - -  Provide difference on data structure - - - - - - - - - - - - - 
  
  if(!is.na(flag_error_df) & (flag_error_df == 1 | flag_error_df == -1)){
    ncol_vect = c(ncol(header),ncol(data))
    names(ncol_vect) = c("ncol_header", "ncol_data")
    
    output_structure = list("Y",ncol_vect)
    names(output_structure) = c("Status", "Values")
  }else{
    if(exists("df_difference")){
      if(!is.na(flag_error_df) & (flag_error_df == 0  &  nrow(df_difference) != 0 )){
        # structure_message = df_difference
        output_structure = list("Y",df_difference)
        names(output_structure) = c("Status", "Values")
      }else{
        output_structure = list("N",NA)
        names(output_structure) = c("Status", "Values")
      }
    }else{
      output_structure = list("N",NA)
      names(output_structure) = c("Status", "Values")
    }
  }
  
  # - - - -  Provide date issue - - - - - - - - - - - - - 
  
  if(!is.na(flag_date) & flag_date == 1){
    
    output_date_issue = list("Y", NA)
    names(output_date_issue) =c("Status", "Values")
  }else{
    output_date_issue = list("N",NA)
    names(output_date_issue) =c("Status", "Values")
  }
  
  
  # - - - -  Provide overlaps - - - - - - - - - - - - - 
  
  if(!is.na(flag_overlap) & flag_overlap == 1){
    
    overlap_date = as.character(as.POSIXct(unique(overlap$TIMESTAMP), tz = "Etc/GMT-1"))
    output_overlap = list("Y", overlap_date)
    names(output_overlap) =c("Status", "Values")
  }else{
    if(!is.na(flag_new_overlap) & flag_new_overlap == 1){
      overlap_date = as.POSIXct(unique(new_overlap$TIMESTAMP), tz = "Etc/GMT-1")
      output_overlap = list("Y", overlap_date)
      names(output_overlap) =c("Status", "Values")
    }else{
      output_overlap = list("N", NA)
      names(output_overlap) =c("Status", "Values")
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
    table_restart_record = table_restart_record[table_restart_record$`Date Gap` != 0]
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
    
    time_tot <- as.POSIXct(mydata[,which(colnames(mydata) == datetime_header)], format = datetime_format, tz = 'Etc/GMT-1' )
    time_missing <- missing_index_date[,2]
    
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
  
  # # - - - -  Provide out of range - - - - - - - - - - - - - 
  # 
  # 
  # if((!is.na(flag_out_of_range) & flag_out_of_range == 1)){
  #   for(j in 1:ncol(check_out_of_range)){
  #     gc(reset = T)
  #     if(colnames(check_out_of_range)[j] !=  datetime_header){
  #       
  #       # ~~~~~~ preparation data for ggplot ~~~~~~
  #       
  #       df_tmp = check_out_of_range[,c(which(colnames(check_out_of_range)==datetime_header), j)]
  #       df_data = mydata[,c(which(colnames(check_out_of_range)==datetime_header), j)]
  #       
  #       df_tmp_new = cbind(df_tmp, df_data[,2])
  #       df_tmp_new[is.na(df_data[,2]),3] = 1
  #       df_tmp_new[!is.na(df_data[,2]),3] = 0
  #       df_tmp_new[which(df_tmp[,2] != 0),3] = 0
  #       colnames(df_tmp_new)[3] = "na_flag"
  #       
  #       df_factor = df_tmp
  #       
  #       df_factor[,2]=as.character(df_factor[,2])
  #       
  #       y = rep(1,times = nrow(df_factor))
  #       df_factor = cbind(df_factor,y)
  #       colnames(df_factor) = c("time", "Variable", "y")
  #       df_factor$y[which(df_factor$Variable == -1)] = 0.9
  #       df_factor$y[which(df_factor$Variable == 1)] = 1.1
  #       # colnames(df_factor) =c("time","Variable","y")
  #       
  #       # ~~~~~~ preparation data for statistic table ~~~~~~
  #       
  #       c_oor = colnames(check_out_of_range)[j]
  #       
  #       df_oor0 = df_tmp_new[,c(1,3)]
  #       colnames(df_oor0)= c("time", "variable")
  #       
  #       nan = rep(0, times = nrow(df_oor0))
  #       
  #       how_many_nan = length(which(df_oor0$variable == 1))
  #       
  #       if(length(which(df_oor0$variable == 1)) != 0){
  #         nan[which(df_oor0$variable == 1)] = 1
  #       }
  #       
  #       df_oor = df_tmp
  #       colnames(df_oor)= c("time", "variable")
  #       
  #       over_range = rep(0, times = nrow(df_oor))
  #       
  #       how_many_over_range = length(which(df_oor$variable == 1))
  #       
  #       if(length(which(df_oor$variable == 1)) != 0){
  #         over_range[which(df_oor$variable == 1)] = 1
  #       }
  #       
  #       under_range = rep(0, times = nrow(df_oor))
  #       
  #       how_many_under_range = length(which(df_oor$variable == -1))
  #       if(length(which(df_oor$variable == -1)) != 0){
  #         under_range[which(df_oor$variable == -1)] = 1
  #       }
  #       
  #       df_oor2 = data.frame(df_oor$time, under_range, over_range, nan)
  #       
  #       df_oor3 = rbind(df_oor2[1,],df_oor2,df_oor2[nrow(df_oor2),])
  #       
  #       
  #       df_oor3[1,2] = 0 ; df_oor3[1,3] = 0; df_oor3[1,4] = 0
  #       df_oor3[nrow(df_oor3),2] = 0 ; df_oor3[nrow(df_oor3),3] = 0; df_oor3[nrow(df_oor3),4] = 0
  #       
  #       a=as.data.frame(rbind(c(0,0),apply(df_oor3[,-1],2, diff)))
  #       time_a =c(df_oor$time[1],df_oor$time,df_oor$time[nrow(df_oor)])
  #       diff_df = cbind(time_a,a)
  #       
  #       if(any(unique(df_oor[,2])!= 0)){
  #         
  #         # ~~~~~~ preparation data for ggplot ~~~~~~
  #         
  #         theme_new2 = theme_bw()+theme(axis.title.y=element_blank(),
  #                                       axis.text.y=element_blank(),
  #                                       axis.ticks.y=element_blank(),
  #                                       axis.text.x = element_text(angle = 90, hjust = 1),
  #                                       legend.title = element_blank())
  #         
  #         
  #         # set_limits = data.frame(as.POSIXct(c(df_factor$time[1], df_factor$time[nrow(df_factor)%/%2], df_factor$time[nrow(df_factor)])), c(-1,0,1), c(0,0,0))
  #         
  #         set_limits = data.frame(rep(as.POSIXct("1900-01-01 00:00",format = "%Y-%m-%d %H:%M", tz = "Etc/GMT-1"),times = 4),
  #                                 c(-1,0,2,1), c(0.9,1,1,1.1))
  #         colnames(set_limits) = colnames(df_factor)
  #         
  #         df_tmp_new[,2] = rep(2, times = nrow(df_tmp_new))
  #         colnames(df_tmp_new) = colnames(df_factor)
  #         df_na = df_tmp_new[which(df_tmp_new[,3] == 1),]
  #         
  #         df_factor = rbind(set_limits,df_factor)
  #         df_factor = rbind(df_factor, df_na)
  #         df_factor = df_factor[order(df_factor$time),]
  #         
  #         df_factor$Variable =as.numeric(df_factor$Variable)
  #         
  #         df_factor$Variable = factor(df_factor$Variable, ordered = TRUE)
  #         df_factor$Variable <- factor( df_factor$Variable, levels=rev(levels( df_factor$Variable)))
  #         
  #         
  #         
  #         
  #         
  #         p1 = ggplot(df_factor,aes(x = time, y = y,colour = Variable))+
  #           geom_point(aes(size = Variable,shape = Variable ))+
  #           scale_colour_manual(values = c("#777777","#F8766D","#7CAE00","#0000FF"),labels = c("NaN", "Above upper limit", "In the range","Below lower limit"))+
  #           scale_shape_manual(values = c(124,124,20,124),labels =c("NaN","Above upper limit", "In the range","Below lower limit"))+
  #           scale_size_manual(values = c(7,10,1,10),labels = c("NaN","Above upper limit", "In the range","Below lower limit"))+
  #           theme_new2 +
  #           scale_y_continuous(limits = c(0.7,1.3))+
  #           scale_x_datetime(limits = as.POSIXct(c(df_factor$time[5], df_factor$time[nrow(df_factor)]),tz = "Etc/GMT-1") )+
  #           labs(title=paste(colnames(check_out_of_range)[j]),
  #                subtitle=paste("Total NaN: ", how_many_nan, "\n",
  #                               "Total Above upper limit: ", how_many_over_range, "\n",
  #                               "Total Below lower limit: ", how_many_under_range, sep = ""))
  #         
  #         print(p1)
  #         
  #         # ~~~~~~ preparation data for statistic table ~~~~~~
  #         
  #         # NaN
  #         nan_start_oor = which(diff_df$nan == 1) - 1
  #         nan_end_oor  = which(diff_df$nan == -1) - 2
  #         
  #         
  #         if(length(nan_start_oor) != 0 & length(nan_end_oor) != 0){
  #           if(nan_end_oor[1] < nan_start_oor[1]){
  #             nan_start_oor = c(1,nan_start_oor)
  #           }
  #           if(nan_start_oor[length(nan_start_oor)] > nan_end_oor[length(nan_end_oor)] ){
  #             nan_end_oor = c(nan_end_oor,nrow(diff_df))
  #           }
  #         }
  #         
  #         nan_gap_lenght_oor = nan_end_oor - nan_start_oor + 1
  #         nan_date_start_oor = df_oor[nan_start_oor,1]
  #         nan_date_end_oor = df_oor[nan_end_oor,1]
  #         
  #         if(length(nan_date_end_oor) != 0){
  #           nan_date_end_oor_tmp = as.POSIXct("1990-01-01 00:00")    # this for cycle is to fix a bug on time difference
  #           for(k in 1:length(nan_date_end_oor)){
  #             
  #             nan_date_end_oor_tmp[k] =  seq.POSIXt(nan_date_end_oor[k], by = datetime_sampling, length. =  2)[2]
  #           }
  #           
  #           nan_gap_hour_oor = difftime(time1 = nan_date_end_oor_tmp,time2 = nan_date_start_oor,units = "hours")
  #         }else{
  #           nan_gap_hour_oor = numeric(0)
  #         }
  #         
  #         nan_statistic_oor = data.frame(rep("NaN", times = length(nan_date_start_oor)),
  #                                        nan_date_start_oor,
  #                                        nan_date_end_oor,
  #                                        nan_gap_lenght_oor,
  #                                        nan_gap_hour_oor)
  #         colnames(nan_statistic_oor) = c(" ","From", "To", "Number of Record", "Hours")
  #         
  #         # under
  #         
  #         under_start_oor = which(diff_df$under_range == 1) - 1
  #         under_end_oor  = which(diff_df$under_range == -1) - 2
  #         
  #         
  #         if(length(under_start_oor) != 0 & length(under_end_oor) != 0){
  #           if(under_end_oor[1] < under_start_oor[1]){
  #             under_start_oor = c(1,under_start_oor)
  #           }
  #           if(under_start_oor[length(under_start_oor)] > under_end_oor[length(under_end_oor)] ){
  #             under_end_oor = c(under_end_oor,nrow(diff_df))
  #           }
  #         }
  #         
  #         under_gap_lenght_oor = under_end_oor - under_start_oor + 1
  #         under_date_start_oor = df_oor[under_start_oor,1]
  #         under_date_end_oor = df_oor[under_end_oor,1]
  #         
  #         if(length(under_date_end_oor) != 0){
  #           under_date_end_oor_tmp = as.POSIXct("1990-01-01 00:00")    # this for cycle is to fix a bug on time difference
  #           for(k in 1:length(under_date_end_oor)){
  #             
  #             under_date_end_oor_tmp[k] =  seq.POSIXt(under_date_end_oor[k], by = datetime_sampling, length. =  2)[2]
  #           }
  #           
  #           under_gap_hour_oor = difftime(time1 = under_date_end_oor_tmp,time2 = under_date_start_oor,units = "hours")
  #         }else{
  #           under_gap_hour_oor = numeric(0)
  #         }
  #         
  #         under_statistic_oor = data.frame(rep("Under lower limit", times = length(under_date_start_oor)),
  #                                          under_date_start_oor,
  #                                          under_date_end_oor,
  #                                          under_gap_lenght_oor,
  #                                          under_gap_hour_oor)
  #         colnames(under_statistic_oor) = c(" ","From", "To", "Number of Record", "Hours")
  #         
  #         # over
  #         
  #         over_start_oor = which(diff_df$over_range == 1) - 1
  #         over_end_oor  = which(diff_df$over_range == -1) - 2
  #         
  #         
  #         if(length(over_start_oor) != 0 & length(over_end_oor) != 0){
  #           if(over_end_oor[1] < over_start_oor[1]){
  #             over_start_oor = c(1,over_start_oor)
  #           }
  #           if(over_start_oor[length(over_start_oor)] > over_end_oor[length(over_end_oor)] ){
  #             over_end_oor = c(over_end_oor,nrow(diff_df))
  #           }
  #         }
  #         
  #         over_gap_lenght_oor = over_end_oor - over_start_oor + 1
  #         over_date_start_oor = df_oor[over_start_oor,1]
  #         over_date_end_oor = df_oor[over_end_oor,1]
  #         
  #         if(length(over_date_end_oor) != 0){
  #           over_date_end_oor_tmp = as.POSIXct("1990-01-01 00:00")    # this for cycle is to fix a bug on time difference
  #           for(k in 1:length(over_date_end_oor)){
  #             
  #             over_date_end_oor_tmp[k] =  seq.POSIXt(over_date_end_oor[k], by = datetime_sampling, length. =  2)[2]
  #           }
  #           over_gap_hour_oor = difftime(time1 = over_date_end_oor_tmp,time2 = over_date_start_oor,units = "hours")
  #         }else{
  #           over_gap_hour_oor =  numeric(0)
  #         }
  #         
  #         over_statistic_oor = data.frame(rep("Above upper limit", times = length(over_date_start_oor)), over_date_start_oor,over_date_end_oor,over_gap_lenght_oor, over_gap_hour_oor)
  #         colnames(over_statistic_oor) = c(" ","From", "To", "Number of Record", "Hours")
  #         
  #         
  #         if(nrow(under_statistic_oor) == 0){
  #           statistic_oor_under_over = over_statistic_oor
  #         }else{
  #           if(nrow(over_statistic_oor) == 0){
  #             statistic_oor_under_over = under_statistic_oor
  #           }else{
  #             statistic_oor_under_over= rbind(under_statistic_oor,over_statistic_oor)
  #           }
  #         }
  #         
  #         if(nrow(nan_statistic_oor) == 0){
  #           statistic_oor = statistic_oor_under_over
  #         }else{
  #           if(nrow(under_statistic_oor) == 0 & nrow(over_statistic_oor) == 0){
  #             statistic_oor = nan_statistic_oor
  #           }else{
  #             statistic_oor = rbind(nan_statistic_oor,statistic_oor_under_over)
  #           }
  #         }
  #         
  #         
  #         statistic_oor = statistic_oor[order(statistic_oor$From),]
  #         statistic_oor[,2:3] = format(statistic_oor[,2:3], format = datetime_format)
  #         
  #         # print(htmltools::tagList(datatable(statistic_oor)))
  #         # print(xtable(statistic_oor),type = "html")
  #         
  #         # if(nrow(statistic_oor) == 1){
  #         #   print(kable(statistic_oor, format = "html",align = "c",row.names = F)%>%
  #         #           kable_styling() %>%
  #         #           scroll_box( height = "120px") )
  #         # }else{
  #         #   if(nrow(statistic_oor) == 2){
  #         #     print(kable(statistic_oor, format = "html",align = "c",row.names = F)%>%
  #         #             kable_styling() %>%
  #         #             scroll_box( height = "150px") )
  #         #   }else{
  #         #     if(nrow(statistic_oor) == 3){
  #         #       print(kable(statistic_oor, format = "html",align = "c",row.names = F)%>%
  #         #               kable_styling() %>%
  #         #               scroll_box( height = "200px") )
  #         #     }else{
  #         #       if(nrow(statistic_oor) >  3){
  #         #         print(kable(statistic_oor, format = "html",align = "c",row.names = F)%>%
  #         #                 kable_styling() %>%
  #         #                 scroll_box( height = "220px") )
  #         #       }
  #         #     }
  #         #   }
  #         # }
  #         
  #         
  #         
  #       }
  #       rm(df_tmp)
  #       rm(df_factor)
  #       rm(y)
  #       rm(c_oor)
  #       rm(df_oor)
  #       rm(over_range)
  #       rm(how_many_over_range)
  #       rm(under_range)
  #       rm(how_many_under_range)
  #       rm(df_oor2)
  #       rm(df_oor3)
  #       rm(a)
  #       rm(time_a)
  #       rm(diff_df)
  #     }
  #   }
  #   
  #   xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx = "PIPPO"
  #   output_out_of_range = list("Y", xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx)
  #   names(output_out_of_range) =c("Status", "Values")
  # }else{
  #   output_out_of_range = list("N", NA)
  #   names(output_out_of_range) =c("Status", "Values")
  # }
  
  # - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -  
  output_empty
  output_logger_number
  output_structure
  output_date_issue
  output_overlap
  output_missing_record
  output_restart_record
  output_date_missing
  output_out_of_range
  
  # output2 = list(mydata, flags_df,file_names, logger_numbers, structure_message, overlap_date, table_missing_record, table_restart_record,date_missing)
  
  return(output1)
}

