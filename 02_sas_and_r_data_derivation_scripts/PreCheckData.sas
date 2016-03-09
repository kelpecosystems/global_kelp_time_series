%let outName = &inName;

%MACRO exportIfRows(dataName=, out=, errorName=, isFatal= );
proc sql noprint; select count(*) into :mvar1-:mvar1 from &dataName; quit;
%if (&mvar1 > 0)%then %do;

data _null_;
NewDirectory=dcreate("&outName",'C:\Dropbox\nceas_kelp_climate_2013\temporal_change\clean_datasets\dataQA\');
put(NewDirectory)(=/);  
run;

proc export data=&dataName
 	outfile="C:\Dropbox\nceas_kelp_climate_2013\temporal_change\clean_datasets\dataQA\&outName\&dataName..csv"
	dbms=csv
	replace;
run;

%end;

%LET dsid=%SYSFUNC(OPEN(&dataName));
%LET nobs=%SYSFUNC(ATTRN(&dsid.,NOBS));
%LET rc=%SYSFUNC(CLOSE(&dsid.));
%IF &Nobs. GT 0 %THEN %DO; 
data Dataset_ErrorsT;
study = "&inName";
study_ID = &setStudyID;
Error_type = "&errorName";
Fatal  = &isFatal;
run;
data Dataset_errors;
set Dataset_errors dataset_errorsT;
run;
%END;

%MEND exportIfRows;

data _missing_info; *are there any rows missing key descriptive data?;
	set kelp_data;
	if missing(site) 
	or missing(Depth_m)
	or missing(Sample_Year)
	or missing(Sample_Month)
	/*or missing(Sample_Day)*/
	or missing(Latitude)
	or missing(Longitude)
	or missing(Taxon)
	or missing(Sample_ID)
	/*or missing(Sample_Unit_Size_sq_m)*/
	or missing(Fixed_or_Random)
	or missing(Study);
run;

data _missing_data;*are there any rows with no data on abundance?;
	set kelp_data; 
	if missing(Percent_Cover)
	and missing(Biomass_kg_wet_per_sq_m)
	and missing(Individual_Density_num_per_sq_m)
	and missing(Stipe_Density_num_per_sq_m);
run;

data kelp_data; *replace missing data with placeholders;
	set kelp_data;
	if missing(sample_day)   then sample_day = 15;
run;

proc sort data = kelp_data; by 	site Depth_m Sample_Year Sample_Month Sample_day Latitude Longitude Taxon Sample_ID; run;

proc means data = kelp_data noprint;
class site Depth_m Sample_Year Sample_Month Sample_day  Latitude Longitude Taxon Sample_ID;
output out = pre_duplicates min(entry) = smallestEntry max(entry) = largestEntry ;
ways 9;
run;

data duplicates;
	set pre_duplicates;
	if _freq_ gt 1;
run;

data _checkDouble;
merge kelp_data pre_duplicates;
by site Depth_m Sample_Year Sample_Month Latitude Longitude Taxon Sample_ID;
if _freq_ gt 1;
run;

proc sort data = kelp_data; by 	site Depth_m Sample_Year Sample_Month  Latitude Longitude Sample_ID; run;

proc means data = kelp_data noprint;
class site Depth_m Sample_Year Sample_Month  Latitude Longitude Sample_ID;
output out = balenced1 mean(Sample_Year) = dummy;
ways 7;
run;

proc sort data = balenced1; by 	site Depth_m Latitude Longitude Sample_ID; run;

proc means data = balenced1 noprint;
class site Depth_m Latitude Longitude Sample_ID;
output out = balenced2 max(_freq_) = max_SP min(_freq_) = min_SP;
ways 5;
run;

data _missing_species;
	set balenced2;
	if max_SP NE min_SP;
run;

proc sort data = balenced1; by 	site Sample_Year Sample_Month; run;

proc means data = balenced1 noprint;
class site Sample_Year Sample_Month;
output out = balenced3 mean(depth_m) = meanDepth;
ways 3;
run;

proc sort data = balenced3; by 	site; run;

proc means data = balenced3 noprint;
class site ;
output out = balenced4 min(meanDepth)=minDepth max(meanDepth)=maxDepth min(_freq_)=minN max(_freq_)=maxN;
ways 1;
run;

%exportIfRows(dataName=_missing_info, out=&outName, errorName= missing_info, isFatal=1);
%exportIfRows(dataName=_missing_data, out=&outName, errorName= No_kelp_data, isFatal=1);
%exportIfRows(dataName=_checkDouble, out=&outName, errorName= Duplicate rows, isFatal=1);


/* moved this error check to processKelpTemporal, makes more sense to chech for changes in species based on period not month
%exportIfRows(dataName=_missing_species, out=&outName, errorName= Change_in_species_sampled, isFatal=1);*/
