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
  # labs(title = "Frequency distribution of Subset of Unassigned SNPs (Neither REF nor ALT match reference)",
  #      x = "Allele Frequency") + 
  labs(title = "Histogram of Allele Frequency in Subset",
       x = "Allele Frequency") +
  theme(text = element_text(size=20),
       panel.grid.major = element_blank(), 
       panel.grid.minor = element_blank())
# if we only have minor alleles, than we will only have frequency from 0 to 0.5, so don't be alarmed if this is the case

#(2) map their positions 
# (are they concentrated in some places or uniformly distributed over the genome?). 
# One way to do this is to calculate distance between neigbouring alleles and plot distribution of these distances. 
unassigned_rsid_err_vars_freq %>% 
  # select()
  mutate_at(vars("POS"), as.numeric) %>% 
  mutate(CHROM = factor(CHROM, levels = 1:20)) %>% 
  group_by(CHROM) %>% 
  mutate(dist = POS - lag(POS)) %>% 
  ungroup() %>% 
  subset(dist < 5000) %>%
  ggplot(aes(x = CHROM, y = dist)) + 
  geom_boxplot() +
  scale_y_continuous(labels = scales::comma,
                     breaks = scales::pretty_breaks(n = 10)) +
  labs(title = "Distance to Neighboring Alleles on Chromosomes of Subset (Dist < 5,000)") +
  theme(text = element_text(size=20),
        plot.title = element_text(size=18),
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank())
  # facet_grid(~CHROM)
    
# The other way is to plot them along the chromosomes.
unassigned_rsid_err_vars_freq %>%
  mutate_at(vars("POS"), as.numeric) %>% 
  mutate(CHROM = factor(CHROM, levels = 1:20)) %>% 
  ggplot(aes(x = CHROM, y = POS)) +
  geom_point(size = 1) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) + 
  scale_y_continuous(labels = scales::comma,
                     breaks = scales::pretty_breaks(n = 10)) +
  # labs(title = "Chromosome Positions of the Subset of Unassigned SNPs (Neither REF nor ALT match reference)") + 
  labs(title = "Positions on Chromosomes of Subset") +
  theme(text = element_text(size=20),
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank())
  


#(3) check where we got those allele from (which set of variants - Jun Li? HS from 42 strains?) The way to do this is to see how many of those variants overlap with our reference datasets.
# might delete these commented lines later
# founder_seq = as.data.frame(do.call(rbind, strsplit(readLines("founder_GT_transposedPED.txt"), split=" {2,10}")))
# paste0("-h ","/oasis/tscc/scratch/aschitre/round8_imputation/reference_files/refs_impute2/42genomes_homozygous_",chr,"QC.ref.impute.hap.gz",newline_command)

# in tscc load module R
impute_legend_files <- list.files(pattern = ".*legend.gz")
genomes42 <- lapply(impute_legend_files, function(x){data.table::fread(paste0("zcat ", "'", x,"'", " | awk \'{print $1}\'"), fill = T)})
genomes42_df <- data.table::rbindlist(genomes42)
save(genomes42_df, file = "chr_pos_all.RData") # transfer from tscc into filezilla
# back here
load("~/Dropbox (Palmer Lab)/Palmer Lab/Bonnie Lin/P50/SNPs/chr_pos_all.RData")
unassigned_rsid_data_genomes42 <- unassigned_rsid_err_vars %>% 
  left_join(., genomes42_df %>% 
  separate(id, into = c("chr", "pos"), sep = ":") %>% 
  mutate(chr = parse_number(chr) %>% as.character) %>% 
  mutate(genomes_42 = "1"), by = c("CHROM" = "chr", 
                                   "POS" = "pos")) %>% 
  mutate(genomes_42 = ifelse(is.na(genomes_42), "0", genomes_42))
  unassigned_rsid_data_genomes42 %>% select(genomes_42) %>% table()

  unassigned_rsid_data_genomes42 %>% subset(genomes_42 == "0") %>% left_join(., unassigned_rsid_err_vars_freq[, c("POS", "INFO")], by = "POS") %>% select(-(one_of("QUAL", "FILTER", "genomes_42")))
  