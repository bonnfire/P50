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



uniform.date.testingu01 <- function(df){
  lapply(seq_along(df), function(i) {
    datecols <- c("dob", "dow", "shipmentdate")
    datefunction <- function(x){
      if(all(nchar(x)==5)){
        convertToDate(x)
      } 
      else if(all(grepl("^\\d{4}-", x))){
        lubridate::ymd(x)
      }
      else if(all(grepl("^\\d{2}-", x))){
        lubridate::mdy(x)
      }
      else
        paste0(x, "INVALID_CHECK")
    }
    df[[i]] <- df[[i]] %>% 
      mutate_at(.vars = vars(datecols), .funs = datefunction)
    return(df[[i]])
  })
} # function should be used for other cases (testing)


unique.values.length.by.col <- function(df, var){
  lapply(df, function(df){
    uniquevarcount <- sapply(df[var], function(x){
      unique(x) %>% length()})
    print(paste("for", var, "the number of unique values (observed count) is", uniquevarcount, "and number of rows (expected count) is", nrow(df)))})
} # function should be used for identification variables, and for other cases (testing)


########################################################################################
###
### PAUL MEYER (RESEARCH PROJECT 1)
###
########################################################################################
setwd("~/Dropbox (Palmer Lab)/Palmer Lab/Bonnie Lin/P50/WFU_ShipmentSheets")
## NY (he's at the University at Buffalo)
WFU_Meyer_excel_orig <- lapply(list.files(path = ".", pattern = "Paul"), function(x){
  x <- u01.importxlsx(x)
  x <- x[[1]] %>% 
    mutate_all(as.character) %>% 
    mutate_all(toupper)
  return(x)
})

WFU_Meyer_excel_orig_test <- WFU_Meyer_excel_orig %>% 
  uniform.var.names.testingu01() %>% 
  lapply(., function(x){
    x <- x %>% 
      rename("dow" = "datewean",
             "shipmentdate" = "dateshipment",
             "dames" = "dam",
             "sires" = "sire")
    return(x)
  })

# change date type
WFU_Meyer_excel_orig_test[[1]] <- WFU_Meyer_excel_orig_test[[1]] %>% 
  mutate(dow = replace(dow, dow == "10/31/20169", as.numeric(as.Date('2019-10-31')-as.Date(0, origin="1899-12-30", tz='UTC'))))
WFU_Meyer_excel_orig_test <- uniform.date.testingu01(WFU_Meyer_excel_orig_test)


# check id values
idcols <- c("labanimalid", "accessid", "rfid")
unique.values.length.by.col(WFU_Meyer_excel_orig_test, idcols)
## extract pilot 
WFU_Meyer_pilot <- lapply(WFU_Meyer_excel_orig_test, function(x){
  x <- x %>% 
    subset(rfid == "PILOT")
  return(x)
})
names(WFU_Meyer_pilot) <- paste0("C", str_pad(readr::parse_number(list.files(path = ".", pattern = "Paul")), 2, side = "left", pad = "0"))
WFU_Meyer_pilot <- WFU_Meyer_pilot %>% rbindlist(fill = T, idcol = "cohort") # replace list object to prevent creation of another dataframe object
## remove pilot from df
WFU_Meyer_excel_orig_test <- lapply(WFU_Meyer_excel_orig_test, function(x){
  x <- x %>% 
    subset(rfid != "PILOT")
  return(x)
})
unique.values.length.by.col(WFU_Meyer_excel_orig_test, idcols)

# %>% dplyr::filter(rfid != "PILOT") %>% get_dupes(rfid) # use to find inconsistencies

# turn into df for the rest of the basic QC 
names(WFU_Meyer_excel_orig_test) <- paste0("C", str_pad(readr::parse_number(list.files(path = ".", pattern = "Paul")), 2, side = "left", pad = "0"))

WFU_Meyer_excel_orig_test_df <- WFU_Meyer_excel_orig_test %>% 
  rbindlist(idcol = "cohort", fill = T) %>% 
  as.data.frame()

# change `16` column name to comment
WFU_Meyer_excel_orig_test_df <- WFU_Meyer_excel_orig_test_df %>% 
  rename("comment"="16")

# make coat colors uniform
WFU_Meyer_excel_orig_test_df$coatcolor <- gsub("([A-Z]+)(HOOD)", "\\1 \\2", mgsub::mgsub(WFU_Meyer_excel_orig_test_df$coatcolor, 
                                                                               c("BRN|[B|b]rown", "BLK|[B|b]lack", "HHOD|[H|h]ood|[H|h]hod", "[A|a]lbino"), 
                                                                               c("BROWN", "BLACK", "HOOD", "ALBINO"))) 

# # add age of shipment and check consistency (* Note concern if animal was shipped older than 65 days)
WFU_Meyer_excel_orig_test_df <- WFU_Meyer_excel_orig_test_df %>% mutate(dob = replace(dob, dob == lubridate::ymd("2020-12-31"), lubridate::ymd("2019-12-31"))) %>% 
  mutate(shipmentage = as.numeric(shipmentdate - dob) %>% round) 
WFU_Meyer_excel_orig_test_df %>% group_by(cohort) %>% summarize(min_ship = min(shipmentage),
                                                         med_ship = median(shipmentage), 
                                                         mean_ship = mean(shipmentage), 
                                                         max_ship = max(shipmentage),
                                                         CONCERN_OVER65 = sum(shipmentage > 65))

# # add age of wean and check consistency (* Note concern if animal was weaned older than 27 days or younger than 18 days)
WFU_Meyer_excel_orig_test_df <- WFU_Meyer_excel_orig_test_df %>% 
  mutate(weanage = as.numeric(dow - dob) %>% round) 
WFU_Meyer_excel_orig_test_df %>% group_by(cohort) %>% summarize(min_wean = min(weanage),
                                                         med_wean = median(weanage), 
                                                         mean_wean = mean(weanage), 
                                                         max_wean = max(weanage),
                                                         CONCERN_OVER27 = sum(weanage > 27),
                                                         CONCERN_UNDER18 = sum(weanage < 18)) 


## check ID consistency
WFU_Meyer_excel_orig_test_df %>% subset(!grepl("1DCD(\\d|\\D){4}", rfid)) %>% nrow()


## check # of same sex siblings (diff litter)
WFU_Meyer_excel_orig_test_df %>% janitor::get_dupes(sires, dames, sex) # if scrubs is not important, this code works, otherwise, add is.na(comment)

## check # of same sex littermates (same litter)
WFU_Meyer_excel_orig_test_df %>% janitor::get_dupes(sires, dames, litternumber, sex) #12 pairs

## check number of same sex rats in each rack and get number of rat sexes in each rack
WFU_Meyer_excel_orig_test_df %>% 
  group_by(rack) %>% count(sex) %>% ungroup() %>% janitor::get_dupes(rack)
WFU_Meyer_excel_orig_test_df %>% group_by(rack) %>% count(sex) %>% ungroup() %>% select(n) %>% table









########################################################################################
###
### JERRY
###
########################################################################################
setwd("~/Dropbox (Palmer Lab)/Palmer Lab/Bonnie Lin/P50/WFU_ShipmentSheets")
## NY (also at the University at Buffalo)
Jerry_excel_orig <- lapply(list.files(path = ".", pattern = "Jerry", ignore.case = T), function(x){
  x <- u01.importxlsx(x)
  x <- x[[1]] %>% 
    mutate_all(as.character) %>% 
    mutate_all(toupper)
  return(x)
})

Jerry_excel_orig_test <- Jerry_excel_orig %>% 
  uniform.var.names.testingu01() %>% 
  lapply(., function(x){
    x <- x %>% 
      rename("dow" = "datewean",
             "shipmentdate" = "dateshipment",
             "dames" = "dam",
             "sires" = "sire")
    x <- x[!duplicated(as.list(x))]
    return(x)
  })

# quick scan
Jerry_excel_orig_test %>% lapply(., sapply, unique)

# change date type
Jerry_excel_orig_test <- uniform.date.testingu01(Jerry_excel_orig_test)


# check id values
idcols <- c("labanimalid", "accessid", "rfid")
unique.values.length.by.col(Jerry_excel_orig_test, idcols)
## extract pilot (none as of cohort 2)
Jerry_pilot <- lapply(Jerry_excel_orig_test, function(x){
  x <- x %>% 
    subset(rfid == "PILOT")
  return(x)
})
names(Jerry_pilot) <- paste0("C", str_pad(readr::parse_number(list.files(path = ".", pattern = "Paul")), 2, side = "left", pad = "0"))
Jerry_pilot <- Jerry_pilot %>% rbindlist(fill = T, idcol = "cohort") # replace list object to prevent creation of another dataframe object
## remove pilot from df
Jerry_excel_orig_test <- lapply(Jerry_excel_orig_test, function(x){
  x <- x %>% 
    subset(rfid != "PILOT")
  return(x)
})
unique.values.length.by.col(Jerry_excel_orig_test, idcols)

# %>% dplyr::filter(rfid != "PILOT") %>% get_dupes(rfid) # use to find inconsistencies

# turn into df for the rest of the basic QC 
names(Jerry_excel_orig_test) <- paste0("C", str_pad(readr::parse_number(list.files(path = ".", pattern = "Paul")), 2, side = "left", pad = "0"))

Jerry_excel_orig_test_df <- Jerry_excel_orig_test %>% 
  rbindlist(idcol = "cohort", fill = T) %>% 
  as.data.frame()

# make coat colors uniform
Jerry_excel_orig_test_df$coatcolor <- gsub("([A-Z]+)(HOOD)", "\\1 \\2", mgsub::mgsub(Jerry_excel_orig_test_df$coatcolor, 
                                                                                  c("BRN|[B|b]rown", "BLK|[B|b]lack", "HHOD|[H|h]ood|[H|h]hod", "[A|a]lbino"), 
                                                                                  c("BROWN", "BLACK", "HOOD", "ALBINO"))) 

# # add age of shipment and check consistency (* Note concern if animal was shipped older than 65 days)
Jerry_excel_orig_test_df <- Jerry_excel_orig_test_df %>% mutate(dob = replace(dob, dob == lubridate::ymd("2020-12-31"), lubridate::ymd("2019-12-31"))) %>% 
  mutate(shipmentage = as.numeric(shipmentdate - dob) %>% round) 
Jerry_excel_orig_test_df %>% group_by(cohort) %>% summarize(min_ship = min(shipmentage),
                                                         med_ship = median(shipmentage), 
                                                         mean_ship = mean(shipmentage), 
                                                         max_ship = max(shipmentage),
                                                         CONCERN_OVER65 = sum(shipmentage > 65))

# # add age of wean and check consistency (* Note concern if animal was weaned older than 27 days or younger than 18 days)
Jerry_excel_orig_test_df <- Jerry_excel_orig_test_df %>% 
  mutate(weanage = as.numeric(dow - dob) %>% round) 
Jerry_excel_orig_test_df %>% group_by(cohort) %>% summarize(min_wean = min(weanage),
                                                         med_wean = median(weanage), 
                                                         mean_wean = mean(weanage), 
                                                         max_wean = max(weanage),
                                                         CONCERN_OVER27 = sum(weanage > 27),
                                                         CONCERN_UNDER18 = sum(weanage < 18)) 


## check ID consistency
Jerry_excel_orig_test_df %>% subset(!grepl("1DCD(\\d|\\D){4}", rfid)) %>% nrow()


## check # of same sex siblings (diff litter)
Jerry_excel_orig_test_df %>% janitor::get_dupes(sires, dames, sex) # if scrubs is not important, this code works, otherwise, add is.na(comment)

## check # of same sex littermates (same litter)
Jerry_excel_orig_test_df %>% janitor::get_dupes(sires, dames, litternumber, sex) #12 pairs

## check number of same sex rats in each rack and get number of rat sexes in each rack
Jerry_excel_orig_test_df %>% 
  group_by(rack) %>% count(sex) %>% ungroup() %>% janitor::get_dupes(rack)
Jerry_excel_orig_test_df %>% group_by(rack) %>% count(sex) %>% ungroup() %>% select(n) %>% table




########################################################################################
###
### CHEN
###
########################################################################################
setwd("~/Dropbox (Palmer Lab)/Palmer Lab/Bonnie Lin/P50/WFU_ShipmentSheets")
## U of Memphis (TN)
Chen_excel_orig <- lapply(list.files(path = ".", pattern = "Chen", ignore.case = T), function(x){
  x <- u01.importxlsx(x)
  x <- x[[2]] %>% 
    mutate_all(as.character) %>% 
    mutate_all(toupper)
  return(x)
})

Chen_excel_orig_test <- Chen_excel_orig %>% 
  uniform.var.names.testingu01() %>% 
  lapply(., function(x){
    x <- x %>% 
      rename("dow" = "datewean",
             "shipmentdate" = "dateshipment",
             "dames" = "dam",
             "sires" = "sire")
    x <- x[!duplicated(as.list(x))]
    return(x)
  })


## COHORT 1 -- 30 RATS USED AS PILOTS, REMOVE AND REPLACE ID'S 
## extract pilot (none as of cohort 2)
C01_pilot_rfid <- read.csv("RFID_Oxycodone_HS_pilot.csv", header = F) %>% unlist() %>% as.character() %>% toupper()
Chen_pilot <- lapply(Chen_excel_orig_test, function(x){
  x <- x %>% 
    subset(rfid == "PILOT"|rfid%in%C01_pilot_rfid)
  return(x)
})
names(Chen_pilot) <- paste0("C", str_pad(readr::parse_number(list.files(path = ".", pattern = "Chen")), 2, side = "left", pad = "0"))
Chen_pilot <- Chen_pilot %>% rbindlist(fill = T, idcol = "cohort") %>% mutate(pilot = "PILOT") # replace list object to prevent creation of another dataframe object
## remove pilot from df
Chen_excel_orig_test <- lapply(Chen_excel_orig_test, function(x){
  x <- x %>% 
    subset(!rfid %in% Chen_pilot$rfid)
  return(x)
})
 
# quick scan
Chen_excel_orig_test %>% lapply(., sapply, unique)

# change date type
Chen_excel_orig_test <- uniform.date.testingu01(Chen_excel_orig_test)


# check id values
idcols <- c("labanimalid", "accessid", "rfid")
unique.values.length.by.col(Chen_excel_orig_test, idcols)

# turn into df for the rest of the basic QC 
names(Chen_excel_orig_test) <- paste0("C", str_pad(readr::parse_number(list.files(path = ".", pattern = "Chen")), 2, side = "left", pad = "0"))

Chen_excel_orig_test_df <- Chen_excel_orig_test %>% 
  rbindlist(idcol = "cohort", fill = T) %>% 
  as.data.frame()

# make coat colors uniform
Chen_excel_orig_test_df$coatcolor <- gsub("([A-Z]+)(HOOD)", "\\1 \\2", mgsub::mgsub(Chen_excel_orig_test_df$coatcolor, 
                                                                                     c("BRN|[B|b]rown", "BLK|[B|b]lack", "HHOD|[H|h]ood|[H|h]hod", "[A|a]lbino"), 
                                                                                     c("BROWN", "BLACK", "HOOD", "ALBINO"))) 

# # add age of shipment and check consistency (* Note concern if animal was shipped older than 65 days)
Chen_excel_orig_test_df <- Chen_excel_orig_test_df %>% 
  mutate(shipmentage = as.numeric(shipmentdate - dob) %>% round) 
Chen_excel_orig_test_df %>% group_by(cohort) %>% summarize(min_ship = min(shipmentage),
                                                            med_ship = median(shipmentage), 
                                                            mean_ship = mean(shipmentage), 
                                                            max_ship = max(shipmentage),
                                                            CONCERN_OVER65 = sum(shipmentage > 65))

# # add age of wean and check consistency (* Note concern if animal was weaned older than 27 days or younger than 18 days)
Chen_excel_orig_test_df <- Chen_excel_orig_test_df %>% 
  mutate(weanage = as.numeric(dow - dob) %>% round) 
Chen_excel_orig_test_df %>% group_by(cohort) %>% summarize(min_wean = min(weanage),
                                                            med_wean = median(weanage), 
                                                            mean_wean = mean(weanage), 
                                                            max_wean = max(weanage),
                                                            CONCERN_OVER27 = sum(weanage > 27),
                                                            CONCERN_UNDER18 = sum(weanage < 18)) 


## check ID consistency
Chen_excel_orig_test_df %>% subset(nchar(rfid) != 8|grepl("[[:punct:]]", rfid)) %>% nrow()


## check # of same sex siblings (diff litter)
Chen_excel_orig_test_df %>% janitor::get_dupes(sires, dames, sex) 

## check # of same sex littermates (same litter)
Chen_excel_orig_test_df %>% janitor::get_dupes(sires, dames, litternumber, sex) #12 pairs

## check number of same sex rats in each rack and get number of rat sexes in each rack
Chen_excel_orig_test_df %>% 
  group_by(rack) %>% count(sex) %>% ungroup() %>% janitor::get_dupes(rack)
Chen_excel_orig_test_df %>% group_by(rack) %>% count(sex) %>% ungroup() %>% select(n) %>% table
