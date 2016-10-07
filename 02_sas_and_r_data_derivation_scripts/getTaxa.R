library(dplyr)
library(tidyr)
library(sp)
library(meowR); data(regions)

#what directory
taxa_dir <- "../01_clean_raw_data/Taxa/"

###Some helpful functions
getRegionalData <- function(Longitude, Latitude){
  require(dplyr)
  
  #Create a spatial Points Data Frame
  pts <- SpatialPoints(cbind(Longitude, Latitude),
                       proj4string=CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"))
  pts <- SpatialPointsDataFrame(pts,
                                data= data.frame(Longitude=Longitude,
                                                 Latitude=Latitude))
  #plot(pts)
  
  #get the regions shapefile
  data(regions)
  
  #extract regional info
  dataRegions <- over(pts, regions) %>%
    dplyr::select(ECOREGION, PROVINCE, REALM)
  
  return(dataRegions)
}



#read in the mined data and get taxa info
taxa_df <- read.csv("../01_clean_raw_data/Mined_Temporal_Data.csv", stringsAsFactors=FALSE) 
taxa_df <- cbind(taxa_df, getRegionalData(taxa_df$Longitude, taxa_df$Latitude)) %>%
  group_by(Study.ID, Species) %>%
  slice(1L) %>%
  ungroup() %>%
  mutate(Study.ID = gsub("_", "", Study.ID)) %>%
  mutate(Species = gsub(" \\+ ", "_ ", Species))%>%
  mutate(Species = gsub(" \\(with L\\. setchellii \\& E\\.a arborea)", 
                        "_ Laminaria setchellii_ Eisenia arborea", Species)) %>%
  mutate(Species = gsub("L\\.", "Laminaria", Species)) %>%
  dplyr::select(Study.ID, ECOREGION, Species) %>%
  #deal with lack of newlines
  separate(Species, c("Species1", "Species2", "Species3", "Species4"), sep="_ ") %>%
  gather(SpeciesType, Species, Species1:Species4) %>%
  select(-SpeciesType) %>%
  filter(!is.na(Species)) %>%
  #filter out a few extra pieces of info
  filter(Species != "Sargassum sp.") %>%
  filter(Species != "Durvillaea antarctica") %>%
  mutate(Species = gsub(" - adults", "", Species))%>%
  mutate(Species = gsub("spp\\.", "sp\\.", Species))

#########
# Function to process taxa files
#########

filter_taxa_file <- function(afile){
  print(afile)
  #load in the file
  cur_taxa <- read.csv(paste0(taxa_dir, afile), stringsAsFactors=FALSE)
  
  
  #get rid of any genus in the species column
  filter_taxa <- cur_taxa %>%
    #in case everything is in species
    mutate(Genus = ifelse(!is.na(Genus), 
                          Genus, 
                          gsub("^([a-z,A-Z]+) (.+)$", "\\1", Species))) %>%
    
    #get rid of extra genus
    mutate(Species = gsub("^([a-z,A-Z]+) (.+)$", "\\2", Species)) %>%
  
  #make sp. into any NAs in the species column
  mutate(Species = ifelse(is.na(Species), "sp.", Species)) %>%
    
  #make sp. into any NAs in the species column
  mutate(Species = paste(Genus, Species, sep=" "))  %>%
    
  #filter out everything except species
  select(Taxon, Species) %>%
  
  #last check for NA.sp
  mutate(Species = ifelse(Species =="NA sp.", Taxon, Species)) %>%
    
  select(Species)
  
  #tack on a study column
  ret <- data.frame(Study.ID = gsub("temporal_data_(.+)[-,_]Taxa.csv", "\\1", afile),
               Species = filter_taxa$Species)
  
  ret
}

#load in all of the taxa files
for(afile in list.files(taxa_dir)){
  
  #filter the taxa file
  
  new_taxa <- filter_taxa_file(afile)
  
  #add regional info
  
  data_file <- read.csv(paste0("../01_clean_raw_data/temporal_data_",new_taxa$Study.ID[1],".csv" ), stringsAsFactors=FALSE) %>%
    group_by(Latitude, Longitude) %>%
    slice(1L) %>%
    ungroup()
    
  regional_data <- getRegionalData(data_file$Longitude, data_file$Latitude) %>%
    group_by(ECOREGION) %>%
    slice(1L) %>%
    ungroup() %>%
    select(ECOREGION)
  
  new_taxa_regional <- expand.grid(Study.ID = new_taxa$Study.ID, 
                   ECOREGION = regional_data$ECOREGION, 
                   Species = new_taxa$Species)
  
  taxa_df <- rbind(taxa_df, new_taxa_regional)
  
  
}


#fix double names
taxa_df$Species <- gsub("(.+) (\\1)$", "\\1 sp.", taxa_df$Species)

#Make anything with only a genus name have sp.
taxa_df$Species <- gsub("^([a-z,A-Z]+)$", "\\1 sp.", taxa_df$Species)

#We aren't using the Konar Aleutians data
taxa_df <- filter(taxa_df, Study.ID != "konar_Aleutians") %>%
  dplyr::arrange(Study.ID)


taxa_df_all <- taxa_df %>%
  group_by(Study.ID, Species) %>%
  slice(1L) %>%
  ungroup()

#OK, now grab the HLM results and match up the Study IDs to get the
#proper studies to include
used_studies <- read.csv("../05_HLM_analysis_code/formatted_data_3points.csv", stringsAsFactors=FALSE) %>%
  group_by(Study) %>%
  slice(1L) %>%
  ungroup() %>%
  select(Study) %>%
  dplyr::rename(Study.ID = Study) 



#Filter duplicates
taxa_df_filt <- taxa_df %>%
  group_by(Study.ID, ECOREGION, Species) %>%
  slice(1L) %>%
  ungroup()

write.csv(taxa_df_filt, "../03_derived_data/taxa.csv", row.names=F)

taxa_df_ecoregion <- taxa_df_filt %>%
  group_by(ECOREGION, Species) %>%
  dplyr::summarise(Studies = paste(unique(Study.ID), collapse="; ")) %>%
  ungroup() 

write.csv(taxa_df_ecoregion, "../03_derived_data/taxa_ecoregion.csv", row.names=F)

#anti_join(used_studies, taxa_df)$Study.ID

###### After hand-editing
library(readxl)
library(dplyr)

taxa <- read_excel("../03_derived_data/good_taxa_ecoregion.xlsx") %>%
  mutate(Species = trimws(Species)) %>%
  mutate(Species = gsub("spp\\.", "sp\\.", Species)) %>% 
  mutate(Species = gsub("cribosum", "clathratum", Species)) %>%
  group_by(ECOREGION, Species) %>%
  dplyr::summarise(Studies = paste(Studies, collapse="; ")) %>%
  ungroup() %>%
  group_by(ECOREGION) %>%
  dplyr::arrange(desc(ECOREGION), desc(Species)) %>%
  dplyr::rename(Ecoregion = ECOREGION)

#make it pretty
cur_val <- taxa$Ecoregion[1]
for(i in 2:nrow(taxa)){
  if(taxa$Ecoregion[i] == cur_val){
    taxa$Ecoregion[i] <- ""
  }else{
    cur_val <- taxa$Ecoregion[i]
  }
}


write.csv(taxa, "../03_derived_data/good_taxa_ecoregion.csv", row.names=FALSE)

