#### EXTRACT EXCEL DATA




# Load libraries
library(tidyverse) #loads dplyr, tidyr, ggplot2, purrr, etc
require(mgsub)
library(readxl)
library(lubridate)
library(openxlsx)
library(stringr)
library(data.table)
library(janitor)
library(magrittr)



u01.importxlsx <- function(xlname){
  path_sheetnames <- excel_sheets(xlname)
  df <- lapply(excel_sheets(path = xlname), read_excel, path = xlname)
  names(df) <- path_sheetnames
  return(df)
} 


uniform.var.names.testingu01 <- function(df){
  lapply(seq_along(df), function(i) {
    if(grepl("Parent", names(df[[i]])) %>% any()){
      names(df[[i]])[1:2] <- df[[i]][1,][1:2] %>% as.character()
      df[[i]] <- df[[i]][-1, ]
    }
    names(df[[i]]) <- mgsub::mgsub(names(df[[i]]),
                                   c(" |\\.", "#", "Transponder ID", "Date of Wean|Wean Date","Animal", "Shipping|Ship", "Dams"),
                                   c("", "Number", "RFID", "DOW","LabAnimal", "Shipment", "Dames"))
    # names(df[[i]]) <- mgsub::mgsub(names(df[[i]]),
    #                                c("DateofShipment", "LabAnimalID"), 
    #                                c("ShipmentDate", "LabAnimalNumber"))
    names(df[[i]]) <- mgsub::mgsub(names(df[[i]]),
                                   c("DateofShipment", "LabAnimalNumber"), 
                                   c("ShipmentDate", "LabAnimalID")) # actually keep the column named lab animal number
    names(df[[i]]) <- tolower(names(df[[i]]))
    df[[i]]
  })
}




################# MISC 
setwd("~/Dropbox (Palmer Lab)/Palmer Lab/Bonnie Lin/P50")
# 1/31 OKSANA PAUL #2 SHIPPING SHEET 
## NY (he's at the University at Buffalo)

NY_excel_orig <- u01.importxlsx("Paul #2 shipping sheet.xlsx")
NY_excel_orig_test <- uniform.var.names.testingu01(NY_excel_orig)[[1]]
NY_excel_orig_test %<>% 
  rename("dow" = "datewean",
         "shipmentdate" = "dateshipment",
         "dames" = "dam",
         "sires" = "sire") %>% 
  mutate(rfid = toupper(rfid))

# because of inconsistent date types, remove from olivier dataframe
# WFU_OlivierOxycodone_test[[6]] <- rbind(WFU_OlivierOxycodone_test[[6]], WFU_OlivierOxycodone_test[[7]])
# WFU_OlivierOxycodone_test[[7]] <- NULL # turned into comment 1/31 uncertain about why this is here


# make shipment box uniform (? needed - waiting for confirmation)

# check id values 
idcols <- c("labanimalid", "accessid", "rfid")

uniquevarcount <- sapply(NY_excel_orig_test[idcols], function(x){
  unique(x) %>% length()})
writeLines(c(paste("for", names(uniquevarcount), "the number of unique values (observed count) is", uniquevarcount, "and number of rows (expected count) is", nrow(NY_excel_orig_test))))
NY_excel_orig_test %>% dplyr::filter(rfid != "PILOT") %>% get_dupes(rfid)
NY_excel_orig_test %>% dplyr::filter(rfid != "PILOT") %>% select(rfid) %>% unique %>% dim

# change coat colors
NY_excel_orig_test$coatcolor <- gsub("([A-Z]+)(HOOD)", "\\1 \\2", mgsub::mgsub(NY_excel_orig_test$coatcolor, 
                                                                               c("BRN|[B|b]rown", "BLK|[B|b]lack", "HHOD|[H|h]ood|[H|h]hod", "[A|a]lbino"), 
                                                                               c("BROWN", "BLACK", "HOOD", "ALBINO"))) 


# # add age of shipment and check consistency (* Note concern if animal was shipped older than 65 days)
NY_excel_orig_test %>% mutate(dob = replace(dob, dob == lubridate::ymd("2020-12-31"), lubridate::ymd("2019-12-31"))) %>% mutate(shipmentage = as.numeric(shipmentdate - dob) %>% round) %>% select(shipmentage) %>% summary
## waiting for confirmation: will add variable in once replacement is verified 

# # add age of wean and check consistency (* Note concern if animal was weaned older than 25 days)
NY_excel_orig_test %>% mutate(dob = replace(dob, dob == lubridate::ymd("2020-12-31"), lubridate::ymd("2019-12-31"))) %>% mutate(weanage = as.numeric(dow - dob) %>% round) %>% select(weanage) %>% summary
## waiting for confirmation: will add variable in once replacement is verified 

## check # of same sex siblings (diff litter)
NY_excel_orig_test %>% janitor::get_dupes(sires, dames, sex) # if scrubs is not important, this code works, otherwise, add is.na(comment)

## check # of same sex littermates (same litter)
NY_excel_orig_test %>% janitor::get_dupes(sires, dames, litternumber, sex) #12 pairs

## check number of same sex rats in each rack and get number of rat sexes in each rack
NY_excel_orig_test %>% 
  group_by(rack) %>% count(sex) %>% ungroup() %>% janitor::get_dupes(rack)
NY_excel_orig_test %>% group_by(rack) %>% count(sex) %>% ungroup() %>% select(n) %>% table



setwd("~/Dropbox (Palmer Lab)/Palmer Lab/Bonnie Lin/P50")
# 11/18 OKSANA JERRY #1 SHIPPING SHEET 
Jerry_excel_orig_1 <- u01.importxlsx("Jerry #1 Shipping sheet.xlsx")
Jerry_excel_orig_1_test <- uniform.var.names.testingu01(Jerry_excel_orig_1)[[1]]





# 2/7 OKSANA JERRY #2 SHIPPING SHEET 
# AL O AT BUFFALO?

Jerry_excel_orig <- u01.importxlsx("Jerry #2 Shipping Sheet.xlsx")
Jerry_excel_orig_test <- uniform.var.names.testingu01(Jerry_excel_orig)[[1]]
Jerry_excel_orig_test %<>% 
  rename("dow" = "datewean",
         "shipmentdate" = "dateshipment",
         "dames" = "dam",
         "sires" = "sire") %<>% 
  mutate(rfid = toupper(rfid),
         finalcheck = toupper(finalcheck))  
any(Jerry_excel_orig_test$rfid != Jerry_excel_orig_test$finalcheck) # we want FALSE
# if the above check if false, then you can drop finalcheck
Jerry_excel_orig_test %<>% 
  select(-finalcheck)
grep("1DCD1\\d+\\D?\\d?", Jerry_excel_orig_test$rfid, invert = T, value = T)



# make shipment box uniform (? needed - waiting for confirmation)

# check id values 
uniquevarcount <- sapply(Jerry_excel_orig_test[idcols], function(x){
  unique(x) %>% length()})
writeLines(c(paste("for", names(uniquevarcount), "the number of unique values (observed count) is", uniquevarcount, "and number of rows (expected count) is", nrow(Jerry_excel_orig_test))))

# change coat colors
Jerry_excel_orig_test$coatcolor <- gsub("([A-Z]+)(HOOD)", "\\1 \\2", mgsub::mgsub(Jerry_excel_orig_test$coatcolor, 
                                                                                  c("BRN|[B|b]rown", "BLK|[B|b]lack", "HHOD|[H|h]ood|[H|h]hod", "[A|a]lbino"), 
                                                                                  c("BROWN", "BLACK", "HOOD", "ALBINO"))) 


# # add age of shipment and check consistency (* Note concern if animal was shipped older than 65 days)
Jerry_excel_orig_test %>% mutate(dob = replace(dob, dob == lubridate::ymd("2020-12-31"), lubridate::ymd("2019-12-31"))) %>% mutate(shipmentage = as.numeric(shipmentdate - dob) %>% round) %>% select(shipmentage) %>% summary

# # add age of wean and check consistency (* Note concern if animal was weaned older than 25 days)
Jerry_excel_orig_test %>% mutate(dob = replace(dob, dob == lubridate::ymd("2020-12-31"), lubridate::ymd("2019-12-31"))) %>% mutate(weanage = as.numeric(dow - dob) %>% round) %>% select(weanage) %>% summary
## waiting for confirmation: will add variable in once replacement is verified 

## check # of same sex siblings (diff litter)
Jerry_excel_orig_test %>% janitor::get_dupes(sires, dames, sex) # if scrubs is not important, this code works, otherwise, add is.na(comment)

## check # of same sex littermates (same litter)
Jerry_excel_orig_test %>% janitor::get_dupes(sires, dames, litternumber, sex) #12 pairs

## check number of same sex rats in each rack and get number of rat sexes in each rack
Jerry_excel_orig_test %>% 
  group_by(rack) %>% count(sex) %>% ungroup() %>% janitor::get_dupes(rack)
Jerry_excel_orig_test %>% group_by(rack) %>% count(sex) %>% ungroup() %>% select(n) %>% table

