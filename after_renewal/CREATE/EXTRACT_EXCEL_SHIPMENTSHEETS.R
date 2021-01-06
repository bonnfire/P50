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



## functions by cohort 
#### important functions
# use this function to make variables uniform once the variables are in the names()
uniform.var.names.cohort <- function(df){
  names(df) <- mgsub::mgsub(names(df),
                            c(" |\\.", "#", "Transponder ID", "Date of Wean|Wean Date|Date Wean","Animal", "Shipping|Ship", "Dams|Dam", "Sire"),
                            c("", "Number", "RFID", "DOW","LabAnimal", "Shipment", "Dames", "Sires"))
  
  names(df) <- mgsub::mgsub(names(df),
                            c("DateofShipment|DateShipment", "LabAnimalNumber"), 
                            c("ShipmentDate", "LabAnimalID")) # actually keep the column named lab animal number
  names(df) <- tolower(names(df))
  return(df)
}

# use this function to get rid of columns that are all na's or contain redundant information
remove.irr.columns <- function(df){
  df <- df[ , colSums(is.na(df)) == 0]  # remove columns with any na's, checked for no na rows 
  df <- df %>% 
    select(-matches("age|last"))  # remove age in day columns or last 5 digit columns
  return(df)
}

# use this function to make all coat colors unifrom
uniform.coatcolors.df <- function(df){
  df$coatcolor <- toupper(df$coatcolor)  
  df$coatcolor <- gsub("([A-Z]+)(HOOD)", "\\1 \\2", mgsub::mgsub(df$coatcolor, 
                                                                 c("BRN|[B|b]rown", "BLK|[B|b]lack", "HHOD|[H|h]ood|[H|h]hod", "[A|a]lbino"), 
                                                                 c("BROWN", "BLACK", "HOOD", "ALBINO"))) 
  return(df)
}


## add shipment age and weaning age AND QC 
add.age.qc <- function(df){
  date_vars <- c("dob", "dow", "shipmentdate")
  datefunction <- function(x){
    if(lubridate::is.POSIXct(x) == F & all(grepl("^\\d{5}$", x))){
      as.POSIXct(as.numeric(x) * (60*60*24), origin="1899-12-30", tz="UTC", format="%Y-%m-%d")
    } else x
  }
  df <- df %>% 
    mutate_at(vars(one_of(date_vars)), .funs = datefunction)
  
  df <- df %>% 
    mutate(shipmentage = as.numeric(difftime(shipmentdate, dob, units = "days")) %>% round)
  try(if(any(df$shipmentage > 65)) 
    stop("Some animals were too old to ship"))
  
  df <- df %>%
    mutate(weanage = as.numeric(difftime(dow, dob, units = "days")) %>% round) 
  try(if(any(df$weanage > 25))
    stop("Some animals were too old to wean"))    
  
  return(df)
}

## QC for duplicated or incorrectly formatted rfids, genetic diversity in siblings 
id.qc <- function(df){
  ## check if any rfid are duplicated
  suppressMessages(janitor::get_dupes(df, rfid))
  
  ## check for duplicated accessid and labanimalid
  suppressMessages(janitor::get_dupes(df, accessid))
  suppressMessages(janitor::get_dupes(df, labanimalid))
  
  # check if any rfid's are not in the usual 15 character format
  
  try(if(any(!grepl("1DCD(\\d|\\D){4}", df$rfid)))
    stop("Inconsistent rfid formatting"))
  
  ## check # of same sex siblings (diff litter)
  try(if(nrow(df %>% janitor::get_dupes(sires, dames, sex)) != 1)
    stop("Same sex siblings in different litter"))
  
  ## check # of same sex littermates (same litter)
  try(if(nrow(df %>% janitor::get_dupes(sires, dames, litternumber,sex)) != 1)
    stop("Same sex siblings in same litter"))
}

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

# rename columns and remove all na columns
WFU_Meyer_excel_orig_test <- WFU_Meyer_excel_orig %>% 
  uniform.var.names.testingu01() %>% 
  lapply(., function(x){
    x <- x %>% 
      rename("dow" = "datewean",
             "shipmentdate" = "dateshipment",
             "dames" = "dam",
             "sires" = "sire")
    x <- x[, colSums(is.na(x)) < nrow(x)]
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
  rename("comment"="16") %>% 
  mutate(resolution = NA) # add resolution column

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


## add cohort 3 to sample_tracking.sample_metadata

# add C04 - Meyer
meyer_04_wfu_metadata <- u01.importxlsx("~/Dropbox (Palmer Lab)/Palmer Lab/Bonnie Lin/P50/WFU_ShipmentSheets/Paul #4 Shipping sheet.xlsx")$`Paul` %>%
  mutate(cohort = "C04") %>%
  uniform.var.names.cohort %>%
  remove.irr.columns %>%
  uniform.coatcolors.df %>%
  add.age.qc
meyer_04_wfu_metadata %>% id.qc
# add comments
# meyer_04_wfu_metadata <- meyer_04_wfu_metadata %>%
#   mutate(comments = "NA", resolution = "NA") %>%
#   select(cohort, sires, dames, labanimalid, accessid, sex, rfid, dob, dow, shipmentdate, litternumber, littersize, coatcolor, earpunch, rack, shipmentbox, shipmentage, weanage, comments, resolution) # to match the wfu sql in db

# add C05 - Meyer
meyer_05_wfu_metadata <- u01.importxlsx("~/Dropbox (Palmer Lab)/Palmer Lab/Bonnie Lin/P50/WFU_ShipmentSheets/Paul #5 Shipping Sheet.xlsx")$`WORKSHEET` %>%
  mutate(cohort = "C05") %>%
  uniform.var.names.cohort %>%
  remove.irr.columns %>%
  uniform.coatcolors.df %>%
  add.age.qc
meyer_05_wfu_metadata %>% id.qc ## XX COME BACK TO THIS ONE 01/05/2020
# add comments
# meyer_05_wfu_metadata <- meyer_05_wfu_metadata %>%
#   mutate(comments = "NA", resolution = "NA") %>%
#   select(cohort, sires, dames, labanimalid, accessid, sex, rfid, dob, dow, shipmentdate, litternumber, littersize, coatcolor, earpunch, rack, shipmentbox, shipmentage, weanage, comments, resolution) # to match the wfu sql in db







########################################################################################
###
### JERRY RICHARDS (AND PAUL MEYERS- RESEARCH PROJECT 2)
###
########################################################################################
setwd("~/Dropbox (Palmer Lab)/Palmer Lab/Bonnie Lin/P50/WFU_ShipmentSheets")
## NY (also at the University at Buffalo)
WFU_Jerry_excel_orig <- lapply(list.files(path = ".", pattern = "Jerry", ignore.case = T), function(x){
  x <- u01.importxlsx(x)
  x <- x[[1]] %>% 
    mutate_all(as.character) %>% 
    mutate_all(toupper)
  return(x)
})

WFU_Jerry_excel_orig_test <- WFU_Jerry_excel_orig %>% 
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
WFU_Jerry_excel_orig_test %>% lapply(., sapply, unique)

# change date type
WFU_Jerry_excel_orig_test <- uniform.date.testingu01(WFU_Jerry_excel_orig_test)


# check id values
idcols <- c("labanimalid", "accessid", "rfid")
unique.values.length.by.col(WFU_Jerry_excel_orig_test, idcols)
## extract pilot (none)
WFU_Jerry_pilot <- lapply(WFU_Jerry_excel_orig_test, function(x){
  x <- x %>% 
    subset(rfid == "PILOT")
  return(x)
})
names(WFU_Jerry_pilot) <- paste0("C", str_pad(readr::parse_number(list.files(path = ".", pattern = "Paul")), 2, side = "left", pad = "0"))
WFU_Jerry_pilot <- WFU_Jerry_pilot %>% rbindlist(fill = T, idcol = "cohort") # replace list object to prevent creation of another dataframe object
## remove pilot from df
WFU_Jerry_excel_orig_test <- lapply(WFU_Jerry_excel_orig_test, function(x){
  x <- x %>% 
    subset(rfid != "PILOT")
  return(x)
})
unique.values.length.by.col(WFU_Jerry_excel_orig_test, idcols)

# %>% dplyr::filter(rfid != "PILOT") %>% get_dupes(rfid) # use to find inconsistencies

## change names of columns
WFU_Jerry_excel_orig_test[[1]] <- WFU_Jerry_excel_orig_test[[1]] %>% 
  rename("shipmentbox"="shipmentcrate",
         "comment" = "notes")
WFU_Jerry_excel_orig_test <- mapply(cbind, WFU_Jerry_excel_orig_test, resolution = NA) 


# turn into df for the rest of the basic QC 
names(WFU_Jerry_excel_orig_test) <- paste0("C", str_pad(readr::parse_number(list.files(path = ".", pattern = "Paul")), 2, side = "left", pad = "0"))

WFU_Jerry_excel_orig_test_df <- WFU_Jerry_excel_orig_test %>% 
  rbindlist(idcol = "cohort", fill = T) %>% 
  as.data.frame()

# make coat colors uniform
WFU_Jerry_excel_orig_test_df$coatcolor <- gsub("([A-Z]+)(HOOD)", "\\1 \\2", mgsub::mgsub(WFU_Jerry_excel_orig_test_df$coatcolor, 
                                                                                  c("BRN|[B|b]rown", "BLK|[B|b]lack", "HHOD|[H|h]ood|[H|h]hod", "[A|a]lbino"), 
                                                                                  c("BROWN", "BLACK", "HOOD", "ALBINO"))) 

# # add age of shipment and check consistency (* Note concern if animal was shipped older than 65 days)
WFU_Jerry_excel_orig_test_df <- WFU_Jerry_excel_orig_test_df %>% mutate(dob = replace(dob, dob == lubridate::ymd("2020-12-31"), lubridate::ymd("2019-12-31"))) %>% 
  mutate(shipmentage = as.numeric(shipmentdate - dob) %>% round) 
WFU_Jerry_excel_orig_test_df %>% group_by(cohort) %>% summarize(min_ship = min(shipmentage),
                                                         med_ship = median(shipmentage), 
                                                         mean_ship = mean(shipmentage), 
                                                         max_ship = max(shipmentage),
                                                         CONCERN_OVER65 = sum(shipmentage > 65))

# # add age of wean and check consistency (* Note concern if animal was weaned older than 27 days or younger than 18 days)
WFU_Jerry_excel_orig_test_df <- WFU_Jerry_excel_orig_test_df %>% 
  mutate(weanage = as.numeric(dow - dob) %>% round) 
WFU_Jerry_excel_orig_test_df %>% group_by(cohort) %>% summarize(min_wean = min(weanage),
                                                         med_wean = median(weanage), 
                                                         mean_wean = mean(weanage), 
                                                         max_wean = max(weanage),
                                                         CONCERN_OVER27 = sum(weanage > 27),
                                                         CONCERN_UNDER18 = sum(weanage < 18)) 

  
## check ID consistency
WFU_Jerry_excel_orig_test_df %>% subset(!grepl("1DCD(\\d|\\D){4}", rfid)) %>% nrow()
  

## check # of same sex siblings (diff litter)
WFU_Jerry_excel_orig_test_df %>% janitor::get_dupes(sires, dames, sex) # if scrubs is not important, this code works, otherwise, add is.na(comment)

## check # of same sex littermates (same litter)
WFU_Jerry_excel_orig_test_df %>% janitor::get_dupes(sires, dames, litternumber, sex) #12 pairs

## check number of same sex rats in each rack and get number of rat sexes in each rack
WFU_Jerry_excel_orig_test_df %>% 
  group_by(rack) %>% count(sex) %>% ungroup() %>% janitor::get_dupes(rack)
WFU_Jerry_excel_orig_test_df %>% group_by(rack) %>% count(sex) %>% ungroup() %>% select(n) %>% table


# prep cohorts 01-03 for database
WFU_Jerry_excel_orig_test_df <- WFU_Jerry_excel_orig_test_df %>% 
  mutate_at(vars(one_of("dob", "dow", "shipmentdate")), as.Date) %>%
  mutate_at(vars(matches("litter|age|shipmentbox")), as.numeric) %>%
  rename("comments" = "comment") %>%
  select(cohort, sires, dames, labanimalid, accessid, sex, rfid, dob, dow, shipmentdate, litternumber, littersize, coatcolor, earpunch, rack, shipmentbox, shipmentage, weanage, comments, resolution) # to match the wfu sql in db

# add C04 - Jerry
jerry_04_wfu_metadata <- u01.importxlsx("~/Dropbox (Palmer Lab)/Palmer Lab/Bonnie Lin/P50/WFU_ShipmentSheets/Jerry #4 Shipping Sheet.xlsx")$`Jerry` %>% 
  mutate(cohort = "C04") %>% 
  uniform.var.names.cohort %>%
  remove.irr.columns %>% 
  uniform.coatcolors.df %>% 
  add.age.qc %>% 
  mutate(rfid = toupper(rfid))
jerry_04_wfu_metadata %>% id.qc
# add comments 
jerry_04_wfu_metadata <- jerry_04_wfu_metadata %>% 
  select(-rfid) %>% #from id.qc
  rename("rfid" = "finalcheck") %>% 
  mutate(comments = "NA", resolution = "NA") %>%
  mutate(comments = replace(comments, rfid == "1DCD195B", "RETRANSPONDERED CHIP IS IN MIDDLE OF THE BACK -- might have double transponders, but one should be invalid")) %>% 
  select(cohort, sires, dames, labanimalid, accessid, sex, rfid, dob, dow, shipmentdate, litternumber, littersize, coatcolor, earpunch, rack, shipmentbox, shipmentage, weanage, comments, resolution) # to match the wfu sql in db





########################################################################################
###
### HAO CHEN (RESEARCH PROJECT 3)
###
########################################################################################
setwd("~/Dropbox (Palmer Lab)/Palmer Lab/Bonnie Lin/P50/WFU_ShipmentSheets")
## U of Memphis (TN)
WFU_Chen_excel_orig <- lapply(list.files(path = ".", pattern = "Chen", ignore.case = T), function(x){
  x <- u01.importxlsx(x)
  x <- x[[2]] %>% 
    mutate_all(as.character) %>% 
    mutate_all(toupper)
  return(x)
})

WFU_Chen_excel_orig_test <- WFU_Chen_excel_orig %>% 
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
WFU_Chen_pilot <- lapply(WFU_Chen_excel_orig_test, function(x){
  x <- x %>% 
    subset(rfid == "PILOT"|rfid%in%C01_pilot_rfid)
  return(x)
})
names(WFU_Chen_pilot) <- paste0("C", str_pad(readr::parse_number(list.files(path = ".", pattern = "Chen")), 2, side = "left", pad = "0"))
WFU_Chen_pilot <- WFU_Chen_pilot %>% rbindlist(fill = T, idcol = "cohort") %>% mutate(pilot = "PILOT") # replace list object to prevent creation of another dataframe object
## remove pilot from df
WFU_Chen_excel_orig_test <- lapply(WFU_Chen_excel_orig_test, function(x){
  x <- x %>% 
    subset(!rfid %in% WFU_Chen_pilot$rfid)
  return(x)
})
 
# quick scan
WFU_Chen_excel_orig_test %>% lapply(., sapply, unique)

# change date type
WFU_Chen_excel_orig_test <- uniform.date.testingu01(WFU_Chen_excel_orig_test)


# check id values
idcols <- c("labanimalid", "accessid", "rfid")
unique.values.length.by.col(WFU_Chen_excel_orig_test, idcols)


## add comment and resolution column
WFU_Chen_excel_orig_test <- lapply(WFU_Chen_excel_orig_test, cbind, comment = NA, resolution = NA) 


# turn into df for the rest of the basic QC 
names(WFU_Chen_excel_orig_test) <- paste0("C", str_pad(readr::parse_number(list.files(path = ".", pattern = "Chen")), 2, side = "left", pad = "0"))

WFU_Chen_excel_orig_test_df <- WFU_Chen_excel_orig_test %>% 
  rbindlist(idcol = "cohort", fill = T) %>% 
  as.data.frame()

# make coat colors uniform
WFU_Chen_excel_orig_test_df$coatcolor <- gsub("([A-Z]+)(HOOD)", "\\1 \\2", mgsub::mgsub(WFU_Chen_excel_orig_test_df$coatcolor, 
                                                                                     c("BRN|[B|b]rown", "BLK|[B|b]lack", "HHOD|[H|h]ood|[H|h]hod", "[A|a]lbino"), 
                                                                                     c("BROWN", "BLACK", "HOOD", "ALBINO"))) 

# # add age of shipment and check consistency (* Note concern if animal was shipped older than 65 days)
WFU_Chen_excel_orig_test_df <- WFU_Chen_excel_orig_test_df %>% 
  mutate(shipmentage = as.numeric(shipmentdate - dob) %>% round) 
WFU_Chen_excel_orig_test_df %>% group_by(cohort) %>% summarize(min_ship = min(shipmentage),
                                                            med_ship = median(shipmentage), 
                                                            mean_ship = mean(shipmentage), 
                                                            max_ship = max(shipmentage),
                                                            CONCERN_OVER65 = sum(shipmentage > 65))

# # add age of wean and check consistency (* Note concern if animal was weaned older than 27 days or younger than 18 days)
WFU_Chen_excel_orig_test_df <- WFU_Chen_excel_orig_test_df %>% 
  mutate(weanage = as.numeric(dow - dob) %>% round) 
WFU_Chen_excel_orig_test_df %>% group_by(cohort) %>% summarize(min_wean = min(weanage),
                                                            med_wean = median(weanage), 
                                                            mean_wean = mean(weanage), 
                                                            max_wean = max(weanage),
                                                            CONCERN_OVER27 = sum(weanage > 27),
                                                            CONCERN_UNDER18 = sum(weanage < 18)) 



## check ID consistency
WFU_Chen_excel_orig_test_df %>% subset(nchar(rfid) != 8|grepl("[[:punct:]]", rfid)) %>% nrow()


## check # of same sex siblings (diff litter)
WFU_Chen_excel_orig_test_df %>% janitor::get_dupes(sires, dames, sex) 

## check # of same sex littermates (same litter)
WFU_Chen_excel_orig_test_df %>% janitor::get_dupes(sires, dames, litternumber, sex) #12 pairs

## check number of same sex rats in each rack and get number of rat sexes in each rack
WFU_Chen_excel_orig_test_df %>% 
  group_by(rack) %>% count(sex) %>% ungroup() %>% janitor::get_dupes(rack)
WFU_Chen_excel_orig_test_df %>% group_by(rack) %>% count(sex) %>% ungroup() %>% select(n) %>% table



# prep cohorts 01-02 for database
WFU_Chen_excel_orig_test_df <- WFU_Chen_excel_orig_test_df %>% 
  mutate_at(vars(one_of("dob", "dow", "shipmentdate")), as.Date) %>%
  mutate_at(vars(matches("litter|age|shipmentbox")), as.numeric) %>%
  rename("comments" = "comment") %>%
  select(cohort, sires, dames, labanimalid, accessid, sex, rfid, dob, dow, shipmentdate, litternumber, littersize, coatcolor, earpunch, rack, shipmentbox, shipmentage, weanage, comments, resolution) # to match the wfu sql in db

# add C03 - Chen
chen_03_wfu_metadata <- u01.importxlsx("~/Dropbox (Palmer Lab)/Palmer Lab/Bonnie Lin/P50/WFU_ShipmentSheets/Chen #3 Shipping Sheet.xlsx")$`Chen` %>% 
  mutate(cohort = "C03") %>% 
  uniform.var.names.cohort %>%
  remove.irr.columns %>% 
  uniform.coatcolors.df %>% 
  add.age.qc 
chen_03_wfu_metadata %>% id.qc
# add comments 
chen_03_wfu_metadata <- chen_03_wfu_metadata %>% 
  mutate(rfid = toupper(rfid)) %>% #from id.qc
  mutate(comments = "NA", resolution = "NA") %>%
  select(cohort, sires, dames, labanimalid, accessid, sex, rfid, dob, dow, shipmentdate, litternumber, littersize, coatcolor, earpunch, rack, shipmentbox, shipmentage, weanage, comments, resolution) # to match the wfu sql in db


