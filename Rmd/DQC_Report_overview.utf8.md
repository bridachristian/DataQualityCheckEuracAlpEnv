---
output: html_document
params:
    date_DQC:    # adsasd    
    report_dataframe: #dasdasd
---



Report start at **2018-12-03 16:00:00**

<img src="/shared/test_christian/Stations_Data/Data/DQC_Processed_Data/LTER/DQC_Reports/LTER_Report_20181203_files/figure-html/unnamed-chunk-2-1.png" width="672" />




<!-- <!-- aggiungere parte tabella che per ogni file ho la flag_df --> -->

<!-- ```{r echo=FALSE, warning=FALSE} -->
<!-- report_dataframe -->
<!-- ff = as.data.frame(final_dataframe,stringsAsFactors = F) -->

<!-- for(i in 3:17){ -->
<!--   ff[,i] = as.numeric(ff[,i]) -->
<!-- } -->


<!-- final_2 = as.data.frame(final_dataframe[,1:17],stringsAsFactors = F) -->

<!-- final_2$Status[ff$Status == "Already analyzed"] = "B"  -->
<!-- final_2$Status[ff$Status == "Analyzed with errors"] = "R"  -->
<!-- final_2$Status[ff$Status == "Analyzed and write output" & !all(ff[3,17] == 0)] = "Y"  # ff[3,11] ?????? -->
<!-- final_2$Status[ff$Status == "Analyzed and write output" & all(ff[3,17] == 0)] = "G"   # ff[3,11] ?????? -->
<!-- final_2$Status[ff$Status == "Not analyzed"] = NA -->

<!-- final_2$flag_empty[ff$flag_empty == 1] = "R"  -->
<!-- final_2$flag_empty[ff$flag_empty == 0] = "G" -->

<!-- final_2$flag_logger_number[ff$flag_logger_number == 1] = "R" -->
<!-- final_2$flag_logger_number[ff$flag_logger_number == 0] = "G" -->

<!-- final_2$flag_error_df[ff$flag_error_df == 1] = "R" -->
<!-- final_2$flag_error_df[ff$flag_error_df == -1] = "R"  -->
<!-- final_2$flag_error_df[ff$flag_error_df == 0] = "G" -->

<!-- final_2$flag_date[ff$flag_date == 1] = "R"  -->
<!-- final_2$flag_date[ff$flag_date == 0] = "G" -->

<!-- final_2$flag_duplicates_rows[ff$flag_duplicates_rows == 1] = "Y"  -->
<!-- final_2$flag_duplicates_rows[ff$flag_duplicates_rows == 0] = "G" -->

<!-- final_2$flag_overlap[ff$flag_overlap == 1] = "R"  -->
<!-- final_2$flag_overlap[ff$flag_overlap == 0] = "G" -->

<!-- final_2$flag_missing_records[ff$flag_missing_records == 1] = "R"  -->
<!-- final_2$flag_missing_records[ff$flag_missing_records == 0] = "G" -->
<!-- final_2$flag_missing_records[ff$flag_missing_records == 50] = "GR" -->

<!-- final_2$flag_missing_dates[ff$flag_missing_dates == 1] = "Y"  -->
<!-- final_2$flag_missing_dates[ff$flag_missing_dates == 0] = "G" -->

<!-- final_2$flag_range_variable_to_set[ff$flag_range_variable_to_set == 1] = "Y"  -->
<!-- final_2$flag_range_variable_to_set[ff$flag_range_variable_to_set == 0] = "G" -->

<!-- final_2$flag_range_variable_new[ff$flag_range_variable_new == 1] = "Y"  -->
<!-- final_2$flag_range_variable_new[ff$flag_range_variable_new == 0] = "G" -->

<!-- final_2$flag_out_of_range[ff$flag_out_of_range == 1] = "Y"  -->
<!-- final_2$flag_out_of_range[ff$flag_out_of_range == 0] = "G" -->

<!-- final_2$flag_new_duplicates_rows[ff$flag_new_duplicates_rows == 1] = "Y"  -->
<!-- final_2$flag_new_duplicates_rows[ff$flag_new_duplicates_rows == 0] = "G" -->

<!-- final_2$flag_new_overlap[ff$flag_new_overlap == 1] = "R"  -->
<!-- final_2$flag_new_overlap[ff$flag_new_overlap == 0] = "G" -->

<!-- final_2$flag_new_missing_dates[ff$flag_new_missing_dates == 1] = "Y"  -->
<!-- final_2$flag_new_missing_dates[ff$flag_new_missing_dates == 0] = "G" -->

<!-- final_2$flag_missing_records_new[ff$flag_missing_records_new == 1] = "R"  -->
<!-- final_2$flag_missing_records_new[ff$flag_missing_records_new == 0] = "G" -->
<!-- final_2$flag_missing_records_new[ff$flag_missing_records_new == 50] = "GR" -->

<!-- colnames(final_2) = c("Station", -->
<!--                       "Status", -->
<!--                       "File is empty", -->
<!--                       "Logger number", -->
<!--                       "Structure issues", -->
<!--                       "Date issues", -->
<!--                       "Duplicated rows", -->
<!--                       "Overlap", -->
<!--                       "Missing records", -->
<!--                       "Missing dates", -->
<!--                       "Range to update: old variables ", -->
<!--                       "Range to update: new variable", -->
<!--                       "Out of range", -->
<!--                       "NEW Duplicated rows", -->
<!--                       "NEW Overlap", -->
<!--                       "NEW Missing dates", -->
<!--                       "NEW Missing records") -->



<!-- # missing_color = data.frame( rep("R", times= nrow(final_2)), rep("Y", times= nrow(final_2)),rep("G", times= nrow(final_2)),rep(NA, times= nrow(final_2))) -->
<!-- # colnames(missing_color) = c("Red", "Yellow", "Green", "NaN") -->

<!-- # final_2 = cbind(final_2[,1:2],message,final_2[,3:11] ) -->

<!-- ff2 = ff[,c(1:17)] -->
<!-- colnames(ff2)  = colnames(final_2) -->
<!-- melt_final = melt(final_2,id.vars = "Station") -->
<!-- melt_ff2 = melt(ff2[,-2],id.vars = "Station") -->


<!-- message = data.frame(final_dataframe[,1], rep("Status", times = nrow(final_dataframe)), final_dataframe [,2]) -->
<!-- colnames(message) = colnames(melt_final) -->

<!-- mycolors = c("lightblue","red", "yellow","green","grey","white") -->
<!-- names(mycolors) = c("B", "R", "Y", "G","GR", "NA") -->
<!-- # colScale <- scale_fill_manual(name = "mycolors",values =  mycolors, drop = FALSE) -->
<!-- colScale <- scale_fill_manual(name = "",values = mycolors, -->
<!--                               limits = c("B","R", "Y", "G", "GR", "NA"), -->
<!--                               labels = c("Already processed!","Fatal Error!", "Warning!", "OK!","Check disabled!", "Not Evaluated!"), -->
<!--                               drop = FALSE) -->


<!-- level_station = levels(factor(melt_final$Station)) -->


<!-- ggplot(melt_final, aes(x = variable, y = Station))+ -->
<!--   theme(axis.text.x = element_text(angle = 90, hjust = 0, vjust =0), -->
<!--         panel.background = element_rect(fill = "white", colour = "grey"), -->
<!--         panel.border = element_rect(fill = "NA", colour = "white"))+ -->
<!--   geom_tile(aes(fill = value),colour = "black")+ -->
<!--   # geom_text(data = melt_ff2,aes(label = value ))+ -->
<!--   colScale+ -->
<!--   scale_y_discrete(limits = rev(level_station))+ -->
<!--   scale_x_discrete(position = "top") -->



<!-- #   geom_text(data = message, aes(label = value), hjust=0, size = 2) -->


<!-- ``` -->


Station      Report 
-----------  -------
B1           ---    
B2           ---    
B3           ---    
F06fon2400   ---    
I1           ---    
I3           ---    
M1           ---    
M2           ---    
M3           ---    



Report end at **2018-12-03 16:01:00**
