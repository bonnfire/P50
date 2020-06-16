## UPLOAD TO DB
library(RPostgreSQL)

# set up the connection
drv <- dbDriver("PostgreSQL")
con <- dbConnect(drv, user='postgres', password='postgres', dbname='PalmerLab_Datasets')

## write table
dbWriteTable(con, c("p50_paul_meyer","wfu_master_table"), value = WFU_Meyer_excel_orig_test_df, row.names = FALSE)
# dbExecute(con,"ALTER TABLE u01_olivier_george_cocaine.olivier_rewards ADD PRIMARY KEY(rfid,exp)")
dbWriteTable(con, c("p50_jerry_richards","wfu_master_table"), value = WFU_Jerry_excel_orig_test_df, row.names = FALSE)
dbWriteTable(con, c("p50_hao_chen","wfu_master_table"), value = WFU_Chen_excel_orig_test_df, row.names = FALSE)

