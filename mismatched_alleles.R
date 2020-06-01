library(tidyverse)
library(dplyr)
library(vcfR)

setwd("~/Dropbox (Palmer Lab)/Palmer Lab/Bonnie Lin/P50/SNPs")
## examination of alleles which were found in hs rats and are not present in the reference genome
## could be errors or true variants in hs rats
unassigned_rsid_data %>% head(2)
unassigned_rsid_data %>% subset(REF == ref_allele) %>% nrow
unassigned_rsid_data %>% subset(REF == ref_allele | ALT == ref_allele) %>% nrow
unassigned_rsid_data %>% subset(REF != ref_allele & ALT != ref_allele) %>% nrow
unassigned_rsid_err_vars <- unassigned_rsid_data %>% subset(REF != ref_allele & ALT != ref_allele) 

#(1) look at the frequency distribution (are they all rare?)
# subset the dataframe from before for the subset of 27299 "wrong" alleles
freq_vcf <- read.vcfR( "rsid_unassigned_snps.vcf", verbose = FALSE ) 
freq_df <- as.data.frame((getFIX(freq_vcf, getINFO = T)))
unassigned_rsid_err_vars_freq <-  unassigned_rsid_err_vars %>% left_join(., freq_df %>% mutate_all(as.character) %>% select(ID, INFO), by = "ID") %>% 
  mutate(INFO = parse_number(INFO))
unassigned_rsid_err_vars_freq %>% 
  ggplot(aes(x = INFO)) + 
  geom_histogram(bins = 20) + 
  labs(title = "Frequency distribution of Subset of Unassigned SNPs (Neither REF nor ALT match reference)",
       x = "Allele Frequency")
# if we only have minor alleles, than we will only have frequency from 0 to 0.5, so don't be alarmed if this is the case
#(2) map their positions (are they concentrated in some places or uniformly distributed over the genome?). One way to do this is to calculate distance between neigbouring alleles and plot distribution of thiese distances. They other way is to plot them along the chromosomes.


#(3) check where we got those allele from (which set of variants - Jun Li? HS from 42 strains?) The way to do this is to see how many of those variants overlap with our reference datasets.

