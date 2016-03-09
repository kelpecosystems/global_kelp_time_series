
      data WORK.kelp_data                              ;
      *%let _EFIERR_ = 0; /* set the ERROR detection macro variable */
infile "C:\Dropbox\nceas_kelp_climate_2013\temporal_change\clean_datasets\&inName" delimiter = ',' MISSOVER DSD lrecl=1000000 firstobs=2 ;

*informat VAR1 $7. ;
informat Entry best7. ;
informat Study $32. ;
informat Region $32. ;
informat Site $50. ;
informat Depth_m best6. ;
informat Sample_ID $512. ;
informat Sample_Year best6. ;
informat Sample_Month best2. ;
informat Sample_Day best2. ;
informat Latitude best10. ;
informat Longitude best10. ;
informat Taxon $32. ;
informat Fixed_or_Random $10. ;  
informat Sample_Unit_Size_sq_m best4. ; *this is a problem if we are using areal samples;
informat Percent_Cover best8. ;
informat Stipe_Density_num_per_sq_m best8. ;
informat Individual_Density_num_per_sq_m best8. ;
informat Biomass_kg_wet_per_sq_m best8. ;

*format VAR1 $7. ;
format Entry best7. ;
format Study $32. ;
format Region $32. ;
format Site $50. ;
format Depth_m best6. ;
format Sample_ID $512. ;
format Sample_Year best6. ;
format Sample_Month best2. ;
format Sample_Day best2. ;
format Latitude best10. ;
format Longitude best10. ;
format Taxon $32. ;
format Fixed_or_Random $10. ;    
format Sample_Unit_Size_sq_m best4. ; *this is a problem if we are using areal samples;
format Percent_Cover best8. ;
format Stipe_Density_num_per_sq_m best8. ;
format Individual_Density_num_per_sq_m best8. ;
format Biomass_kg_wet_per_sq_m best8. ;

input
/*VAR1 $*/
Entry
Study $ 
Region $
Site $
Depth_m
Sample_ID $
Sample_Year
Sample_Month
Sample_Day
Latitude 
Longitude 
Taxon $
Fixed_or_Random $
Sample_Unit_Size_sq_m 
Percent_Cover
Stipe_Density_num_per_sq_m
Individual_Density_num_per_sq_m 
Biomass_kg_wet_per_sq_m 
                  
      ;
      *if _ERROR_ then call symputx('_EFIERR_',1);  /* set ERROR detection macro variable */
      run;
