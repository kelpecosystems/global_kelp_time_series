
      data WORK.kelp_dataT;
      *%let _EFIERR_ = 0; /* set the ERROR detection macro variable */
infile "C:\Dropbox\nceas_kelp_climate_2013\temporal_change\lit_mining\Mined_Temporal_Data_Apr_2016.csv" delimiter = ',' MISSOVER DSD lrecl=1000000 firstobs=2 ;

*informat VAR1 $7. ;
informat Entry best7. ;
informat Study $32. ;
informat Study_ID $32. ;
informat Region $32. ;
informat Site $256. ;
informat Depth_m best6. ;
informat Sample_ID $256. ;
informat Sample_Year best6. ;
informat Sample_Month best2. ;
informat Sample_Day best2. ;
informat Latitude best10. ;
informat Longitude best10. ;
informat Taxon $32. ;
informat Species $256. ;
informat Fixed_or_Random $10. ;  
informat Sample_Unit_Size_sq_m best4. ; *this is a problem if we are using areal samples;
informat Percent_Cover best8. ;
informat Stipe_Density_num_per_sq_m best8. ;
informat Individual_Density_num_per_sq_m best8. ;
informat Biomass_kg_wet_per_sq_m best8. ;
informat Stressor_or_Cause $32. ;
informat Direct_or_Indirect $32. ;


*format VAR1 $7. ;
format Entry best7. ;
format Study $32. ;
format Study_ID $32. ;
format Region $32. ;
format Site $256. ;
format Depth_m best6. ;
format Sample_ID $256. ;
format Sample_Year best6. ;
format Sample_Month best2. ;
format Sample_Day best2. ;
format Latitude best10. ;
format Longitude best10. ;
format Taxon $32. ;
format Species $256. ;
format Fixed_or_Random $10. ;    
format Sample_Unit_Size_sq_m best4. ; *this is a problem if we are using areal samples;
format Percent_Cover best8. ;
format Stipe_Density_num_per_sq_m best8. ;
format Individual_Density_num_per_sq_m best8. ;
format Biomass_kg_wet_per_sq_m best8. ;
format Stressor_or_Cause $32. ;
format Direct_or_Indirect $32. ;

input
/*VAR1 $*/
Entry
Study $ 
Study_ID $ 
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
Species $
Fixed_or_Random $
Sample_Unit_Size_sq_m 
Percent_Cover
Stipe_Density_num_per_sq_m
Individual_Density_num_per_sq_m 
Biomass_kg_wet_per_sq_m 
Stressor_or_Cause $
Direct_or_Indirect $
      ;
      *if _ERROR_ then call symputx('_EFIERR_',1);  /* set ERROR detection macro variable */
      run;


	  data kelp_data;
	  	set kelp_dataT (drop = taxon stressor_or_cause direct_or_indirect);
			taxon = species;
			if sample_ID EQ . then sample_ID= site;
			if sample_Month EQ . then sample_Month = 6;
			if sample_Day EQ . then sample_Day = 15;
			if depth_m = . then depth_m = 9999;
			if sample_unit_size_sq_m EQ . then sample_unit_size_sq_m = 9999;
			*if (study EQ 'Dayton et al. 1999') then sample_month = ceil(sample_Month/3)*3;
			if study_ID NE 'Duggins 1980';
		run;

