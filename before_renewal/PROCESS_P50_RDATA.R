## load the 6170 animals that have been genotyped
load("~/Dropbox (Palmer Lab)/Palmer Lab/Bonnie Lin/github/P50/before_renewal/old_p50_gbs_n6170.RData")


# load Apurva P50_metadata.R object renamed as old_p50_metadata_n6990.RData
db_cols<-c('rfid','sex','coatcolor','project_name','organism','strain','comments')

riptide_control<-c('0007D60648','0007D2CD05','000792A2B8','0007899771','000792A09B','00079FD00D','0007D601AF','0007D610E9','000792A391','000792A25F','0007D2D146','0007D2DFF4','000792A5C9','000789993E','000792994E','0007929A91','0007D30D95','0007D3138C','0007A01406','0007899868','000792A393','00079FB269','00078997D8','0007899897','000792A3A6','000792A3EF','0007929A41','000792A347','0007899825','000792A322','000792A29D','000792A33F','000792A0FB','00078996AC','00078997E1','000792A2A4','00079FFEA7','000792A5ED','000792A6B3','00079FE64A','0007899931','0007A0153B','0007D48080','0007D3D938','0007D60B77','0007D60F60','0007D60ADB','0007D60399','00077E93C6','00077E9E72','00077E8DB1','00077E9095','00077EA00B','00077E8978','00077E828B','00077E922E','00077E8AE2','00077E919B','00077E920D','00077E84EF','00077E91EE','00077E9E2F','00077E9490','00077E9B87','00077E9968','00077E9BF2','00077E784F','00077E8C05','00077E9DE8','00077E99F4','00077E83F8','00077E8FAC','00077E7AC5','00077E7B77','00077E7C0F','00077E7C6D','00077E8288','00077E8410','00077E8481','00077E84C8','00077E89DE','00077E8A7D','00077E8B25','00077E8BA6','00077E8DEE','00077E8F53','00077E8FFC','00077E9059','00077E9086','00077E909A','00077E9152','00077E91A4','00077E9275','00077E92A7','00077E9370','00077E9391')
  
# 12/11/2020
# extract the metadata that Apurva sent through P50
metadata_db <- metadata %>% 
  select(transpnum, sex, color, center) %>% 
  rename("rfid" = "transpnum",
         "coatcolor" = "color",
         "project_name" = "center") %>% 
  mutate(project_name = 
    case_when(
      project_name == "NY" ~ "p50_jerry_richards_2014,p50_paul_meyer_2014",
      project_name == "TN" ~ "p50_hao_chen_2014",
      project_name == "MI" ~ "p50_shelly_flagel_2014",
      TRUE ~ "NA"
    )
  ) %>% 
  mutate(organism = "rat", 
         strain = "Heterogenous stock",
         comments = "NA") %>% 
  mutate(coatcolor = toupper(coatcolor) %>% gsub(" ", "", .)) %>% 
  separate_rows(project_name, sep = ",", convert = FALSE) %>% 
  subset(!rfid %in% riptide_control)

# add the 88 rats that are used for other projects 

rats_88_ids <- read.table("~/Dropbox (Palmer Lab)/Palmer Lab/Bonnie Lin/github/PalmerLab_genotyping/CREATE/final.IDs.txt") %>% 
  rename("rfid" = "V1") %>% 
  mutate(rfid = toupper(rfid))

dan_eyeeqtl <- read.table("~/Dropbox (Palmer Lab)/Palmer Lab/Bonnie Lin/github/PalmerLab_genotyping/CREATE/rat_ids.txt") %>% 
  rename("rfid" = "V1") %>% 
  mutate(rfid = toupper(rfid)) # 53 rats 

ped_map %>% head(3)



metadata_db_other <- metadata_db %>% 
  rbind(metadata_db %>% 
          subset(rfid %in% rats_88_ids$rfid) %>% 
          mutate(project_name = "p50_hao_chen_2014_brain_rnaseq")) %>% 
  rbind(metadata_db %>% 
          subset(rfid %in% rats_88_ids$rfid) %>% 
          mutate(project_name = "u01_gymrek_sebat")) %>% 
  rbind(metadata_db %>%
            subset(rfid %in% dan_eyeeqtl$rfid) %>%
            mutate(project_name = "r01_monica_jablonski")) %>% 
  distinct()

## read in metadata for p50
load("~/Dropbox (Palmer Lab)/Palmer Lab/Bonnie Lin/P50/P50_metadata.RData")
metadata %>% 
  subset(grepl("1761|1779|1790|1749", transpnum))
  

