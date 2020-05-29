library(tidyverse)
library(dplyr)

## examination of alleles which were found in hs rats and are not present in the referecne genome
## could be errors or true variants in hs rats
unassigned_rsid_data %>% head(2)
#(1) look at the frequency distribution (are they all rare?)
# subset the dataframe from before for the subset of 27299 "wrong" alleles
# if we only have minor alleles, than we will only have frequency from 0 to 0.5, so don't be alarmed if this is the case
#(2) map their positions (are they concentrated in some places or uniformly distributed over the genome?). One way to do this is to calculate distance between neigbouring alleles and plot distribution of thiese distances. They other way is to plot them along the chromosomes.


#(3) check where we got those allele from (which set of variants - Jun Li? HS from 42 strains?) The way to do this is to see how many of those variants overlap with our reference datasets.

