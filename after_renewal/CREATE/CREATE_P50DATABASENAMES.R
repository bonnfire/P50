## CREATE P50 DATABASE NAMES
shipments <- list("Meyer" = WFU_Meyer_excel_orig_test_df, 
                  "Richards" = WFU_Jerry_excel_orig_test_df,
                  "Chen" = WFU_Chen_excel_orig_test_df)
shipments_p50_df <- shipments %>% lapply(., function(x){
  x$cohort <- ifelse(grepl("#", x$cohort), stringr::str_match(x$cohort, "#(\\d+).*?")[,2], x$cohort)
  x$cohort <- ifelse(nchar(x$cohort) > 1, x$cohort, gsub('([[:digit:]]{1})$', '0\\1', x$cohort)) # add leading zeroes when necessary
  x$litternumber = as.numeric(x$litternumber)
  x$littersize = as.numeric(x$littersize)
  return(x)
}) %>% rbindlist(shipments, id = "p50", fill = T)
