library(dplyr)
library(purrr)

#A function to get taxa from a file
getTaxa <- function(afile){
  print(afile)
  one_dataset <- read.csv(paste0("../01_clean_raw_data/", afile), stringsAsFactors = FALSE)
  
  if("Study.ID" %in% names(one_dataset)){
    one_dataset <- one_dataset %>% dplyr::rename(Study = Study.ID) %>%
      select(-Taxon) %>%
      dplyr::rename(Taxon = Species)
  }
  
  reduced_dataset <- one_dataset %>%
    select(Study, Taxon) %>%
    group_by(Study, Taxon) %>%
    slice(1L)
}

#Get a list of all files
data_files <- list.files("../01_clean_raw_data")

#Map across all files to generate a set of taxa
taxa_list <- data_files %>% map(getTaxa)
names(taxa_list) <- data_files

#Fix the taxa in each set to more standard names

#turn that into a single data table
#match this up against the current used set of studies for analysis
