# Cleaning of raw timeseries data for global analyses

Collection of scripts to clean and integrate data from author-provided datasets as well as literature mined data. Data cleaning and unification happens in SAS. Other code is run in R.

###### `importDataFromKira.sas`
Takes the data mined from the literature by K. Krumhansl and co-authors using graphclick and reformats it to same data standard as other sets, as well as implements some taxonomic screening.

###### `Run_temporal_processing.sas`

Cleans all data sets. Filters out non-Laminarian taxa (a few made their way into the clean data), as well as implementing 3m cutoff for data. Also includes code to check and ensure data sets are in compliance with formatting, and that there are no errors (e.g., uneven taxonomic sampling, etc). Outputs a cleaned data file into `03_derived_data` as well as a report spreadsheet and a spreadsheet detailing any data errors.

###### `getRegions.R`

Uses http://github.com/jebyrnes/meowR to add information about the Ecoregion, Province, and Realm of each point using Spalding et al.'s 2007 Marine Ecoregions of the World classification. Creates `CleanDataWithRegions.csv` in `03_derived_data` which should be used for all later analyses.

###### `make_data_for_TRB.R`

Reformats clean and aggregated data to be uploaded to Temperate Reef Base - http://temperatereefbase.imas.utas.edu.au/ and outputs to `03_derived_data`

###### `getTaxa.R`

Filters out the taxonomic data files for each data set and determines the unique taxa sampled in each study. Outputs `good_taxa_ecoregion.csv` to
`03_derived_data`.

###### References
Spalding, M.D., Fox, H.E., Allen, G.R., Davidson, N., Ferdaña, Z.A., Finlayson, M., Halpern, B.S., Jorge, M.A., Lombana, A.L., Lourie, S.A., 2007. Marine ecoregions of the world: a bioregionalization of coastal and shelf areas. Bioscience 57, 573–583.
