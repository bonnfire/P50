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

