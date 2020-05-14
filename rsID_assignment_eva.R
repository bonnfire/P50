### 
# 5/1/2020 The current reference genome RN6 will retire soon. By the end of the 2020 year, the new rat genome
# will be uploaded. At that point, we will reanalyze the P50 genome. The current genome has about 5 million
# SNPs. For reanalysis, all of them need rsIDs, but, at the moment, some of them lack rsIDs. In preparation, 
# I will extract the variants from the European Variation Archive (EVA) hosted by the European Bioinformatics
# Institute, use this to filter out the P50 SNPs that already have ID, and request that EVA managers assign 
# rsIDs.


## Use API to extract variant information
# Submit requests
library(tidyverse) ; library(httr) ; library(jsonlite); library(data.table)
library(tidyr) ; library(janitor) ; library(ggplot2) ; library(magrittr)
# taxonomyCode = rnorvegicus
# assemblyCode = 60
paths <- paste0("http://www.ebi.ac.uk/eva/webservices/rest/v1/segments/", 1:20, ":3000000-3100000/variants?species=rnorvegicus_60") # rat has 42 chromosomes
## change to 20 bc there are actually 20 chromosomes in rat
## limitation on the website only allows chromosome location be million base pairs wide
## should be 1 through XX 

## function to reiterate api call
extract_eva_as_df <- function(path){
  request <- GET(url = path)
  response <- content(request, as = "text", encoding = "UTF-8")
  df <- fromJSON(response, flatten = TRUE)$response$result[[1]] 
  return(df)
}


# eva_as_df <- lapply(paths[1], extract_eva_as_df) %>% rbindlist()
eva_as_df <- lapply(paths[1], extract_eva_as_df) %>% rbindlist()

# ## troubleshoot # one api call at a time
# # If the request fails the API will return a non-200 status code
# request <- GET(url = paths)
# request$status_code
# # Parse response
# response <- content(request, as = "text", encoding = "UTF-8")
# # Convert to df 
# df <- fromJSON(response, flatten = TRUE)$response$result[[1]] 



### XXX ### XXX ### XXX ### 
# install.packages("vcfR")
library(vcfR)
setwd("~/Dropbox (Palmer Lab)/Palmer Lab/Bonnie Lin/P50/EVA_Rat/ftp.ebi.ac.uk/pub/databases/eva/rs_releases/release_1/by_assembly/GCA_000001895.4")

# ratrsid_basic <- vcfR::read.vcfR("GCA_000001895.4_current_ids.vcf") # The vcfR object is an S4 class object with three slots containing the metadata, the fixed data and the genotype dat
# ratrsid_basic_df <- vcfR::vcfR2tidy(ratrsid_basic)
# ratrsid_basic %>% class

# identify dupes -- resolved bc these are snv's and can have dupes
chrom_pos_rsid <- data.frame(chromosome = system("sed -n '/#/!p' GCA_000001895.4_current_ids.vcf | cut -f 1,2,3", intern = T)) %>% 
  separate(chromosome, c("chromosome", "position", "id"))
# chrom_pos_rsid_dupes <- chrom_pos_rsid %>% 
#   get_dupes(id) %>%
#   rename("dupe_id" = "dupe_count") %>%
#   group_by(id) %>% 
#   mutate(unique_ch = n_distinct(chromosome),
#          unique_po = n_distinct(position)) %>% 
#   ungroup()
#   # get_dupes(id, chromosome) %>% 
#   # rename("dupe_id_diffchr" = "dupe_count")
# 
# chrom_pos_rsid_dupes %>% subset(unique_ch > 1) %>% dim
# chrom_pos_rsid_dupes %>% distinct(id) %>% dim
# chrom_pos_rsid_dupes %>% subset(unique_po > 1) %>% dim


chrom_pos_rsid_dupes %>% 
  mutate(chromosome = gsub("chr", "", chromosome) %>% 
           factor(levels = c(1:20, "X"))) %>% 
  ggplot(aes(x = chromosome, y = dupe_count)) +
  geom_boxplot()

chrom_pos_rsid_dt <- as.data.table(chrom_pos_rsid)


### get the p50 data / current snps
setwd("~/Dropbox (Palmer Lab)/Palmer Lab/Bonnie Lin/P50/SNPs")
snps_positions_txt <- read.delim("positions.snplist", header = FALSE) 
snps_positions_txt <- snps_positions_txt %>% 
  rename("chromosome" = "V1") %>% 
  separate(chromosome, c("chromosome", "position"))

# what we have compared to what eba has
snps_positions_txt_jn <- snps_positions_txt %>%
  left_join(., chrom_pos_rsid, by = c("chromosome", "position")) # jn = join
snps_positions_txt_jn %<>% 
  mutate(in_eba = ifelse(is.na(id) == T, 0, 1) %>% as.factor) # 0 is not in eba

## prepare vcf file for eba with the 1.6m that we don't have 
snps_freq <- read.delim("freq.vcf", header = TRUE) 
snps_freq_df <- snps_freq %>% 
  mutate(`CHR.............SNP...A1...A2..........MAF..NCHROBS` = gsub("^[[:space:]]+", "", `CHR.............SNP...A1...A2..........MAF..NCHROBS`)) %>% 
  separate(`CHR.............SNP...A1...A2..........MAF..NCHROBS`, str_split(names(.), "[.]+")[[1]], "[[:space:]]+") %>% 
  clean_names() %>% 
  separate(snp, c("snp_chr", "snp_pos"), "[:]") %>% 
  mutate(snp_chr = gsub("chr", "", snp_chr)) %>%
  left_join(., snps_positions_txt_jn %>% 
              mutate(chromosome = gsub("chr", "", chromosome)), 
                     by = c("snp_chr" = "chromosome", "snp_pos" = "position")) %>% 
  dplyr::filter(is.na(in_eba)|in_eba==0)

snps_freq_df %>% dim

# remove redundant columns
if(snps_freq_df %>% subset(chr != snp_chr) %>% nrow() == 0){
  snps_freq_df <- snps_freq_df %>% 
    select(-chr)
} else{
  print("Chr and snp_chr NOT THE SAME")
}
  
snps_freq_df <- snps_freq_df %>% 
  # mutate(INFO = paste0("AC=", nchrobs, ";", "AF=", maf)) %>% 
  mutate(INFO = paste0("AF=", maf)) %>%
  rename("CHROM" = "snp_chr",
         "POS" = "snp_pos",
         "REF" = "a1", 
         "ALT" = "a2",
         "ID" = "id") %>% 
  mutate(ID = replace(ID, in_eba == 0, ".")) %>% 
  select(CHROM, POS, ID, REF, ALT, INFO) # remove the nonmissing allele count for the vcf submission, they only need one
  
save(snps_freq_df, file = "GBSRat_SNPs.vcf.gz")
  