library(dplyr)
library(tidyr)

setwd("~/Dropbox (Palmer Lab)/Palmer Lab/Bonnie Lin/github/P50")
# heritability_data <- read.delim("heritability_traits.txt", header = F) %>%  ## GOT RID OF BECAUSE IT DODESN'T INCLUDE THE EXPERIMENT NAME 
  
heritability_data <- read.delim("h_traits.txt", header = F) %>% 
  as.data.frame() %>% 
  separate(V1, into = c("snp_heritability", "p_val", "trait", "filename"), sep = " ") %>% 
  mutate(filename = stringr::str_match(filename, "/(.*)/")[,2], 
         trait = paste0(filename, "_", trait)) %>% 
  select(-filename) %>% 
  left_join(data.frame(trait = names(raw_traits)[-(1:5)]) %>% 
              mutate(trait = as.character(trait)), ., by = "trait") %>% 
 mutate_at(vars(-one_of("trait")), as.numeric) %>% 
  arrange(snp_heritability)

raw_traits_heritability <- data.frame(trait = names(raw_traits)[-(1:5)])




ssh b2lin@tscc-login2.sdsc.edu
cd /oasis/tscc/scratch/aschitre/round8/unpruned

find . -name "heritability*.txt" -exec awk '$1 ~ /^[0-9]/{print $1, $3, $4, FILENAME}' {} \; > heritability_traits.txt
