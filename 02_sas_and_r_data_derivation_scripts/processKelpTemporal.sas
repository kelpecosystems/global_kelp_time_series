%MACRO logErrors(dataName=, errorName= , isFatal= );
%LET dsid=%SYSFUNC(OPEN(&dataName));
%LET nobs=%SYSFUNC(ATTRN(&dsid.,NOBS));
%LET rc=%SYSFUNC(CLOSE(&dsid.));
%IF &Nobs. GT 0 %THEN %DO; 
data Dataset_ErrorsT;
study = "&inName";
study_ID = &setStudyID;
Error_type = "&errorName";
Fatal  = &isFatal;;
run;
data Dataset_errors;
set Dataset_errors dataset_errorsT;
run;
%END;
%MEND logErrors;

data kelp_data1; *replace missing data with placeholders;
set kelp_data;
	if missing(sample_day)   then sample_day = 15;
	if missing(sample_month) then sample_month = 6;
	kelpPresent = 0;
	if (percent_cover GT 0) or (individual_Density_num_per_sq_m GT 0) or (biomass_KG_wet_per_sq_m GT 0) or (Stipe_Density_num_per_sq_m GT 0) 
	then kelpPresent = 1;
	if missing(percent_cover) and missing(individual_Density_num_per_sq_m) and missing(biomass_KG_wet_per_sq_m ) and missing(Stipe_Density_num_per_sq_m) 
	then kelpPresent = .;
run;

*  add study name to list;
********************************************************************;
proc means data = kelp_data1 noprint; 
class study;
output out = study_listT max(kelpPresent) = kelpPresent;
ways  1;
run;
data study_listT; set study_listT(keep = study); 
study_ID = &setStudyID;
run;
data study_list; set study_List study_ListT; run; 

*come up with trajectory ids ;
*********************************************************************************************;
proc sort data = kelp_data1; by study Site latitude longitude; run;

proc means data = kelp_data1 noprint;
class study Site;
output out = traj_2 mean(kelpPresent latitude longitude) =  kelpPresent latitude longitude
min(latitude longitude)  = minLat minLon
max(latitude longitude)  = maxLat maxLon
;
ways 2;
run;

data _bad_lat_lon(keep = study site errorType maxLat minLat maxLon minLon);
set traj_2;
if ((maxLat - minLat) GT 1) or ((maxLat - minLat) GT 1);
errorType = 'site has 2 locations';
run;

%exportIfRows(dataName=_bad_lat_lon, out=&outName, errorName= site_has_two_locations, isFatal=1);

data traj_3(keep = study Site trajectory_ID); 
set traj_2;
retain trajectory_ID 0;
trajectory_ID = trajectory_ID + 1;
run;

proc sort data = kelp_data1; by study Site; run;

data kelp_data_Traj; 
merge kelp_data1 traj_3;
by study Site;
sasdate=mdy(sample_month,sample_day,sample_year);
format sasdate mmddyy10.;
run;

*Define sampling events (periods) ;
*********************************************************************************************;

proc sort data = kelp_data_traj; by study Site trajectory_ID sasdate; run;

data kelp_data_periods; 
set kelp_data_traj;
retain period 0;
retain lastDate;
retain lastTraj -99;
if abs(lastDate - sasDate)>&gapCT then period = period + 1;
if lastTraj NE trajectory_ID then period = 1;
lastDate = SASDATE;
lastTraj = trajectory_ID;
run;


*check that sampling events are not too long ;
*********************************************************************************************;
proc sort data = kelp_data_periods;  by study Site trajectory_ID period sasdate sample_ID; run;

proc means data = kelp_data_periods noprint;
class study Site trajectory_ID period;
output out = check_periods 
mean(sasDate) = SASdate max(sasDate) = maxDate min(sasDate) = mindate ;
ways  4;
run;

data F3; set check_periods ; *(keep = study errorType);
periodLength = abs(maxDate-minDate);
if abs(maxDate-minDate) gt 60;
errorType = 'Sampling_events_poorly_defined';
if Study NE 'PISCO_subtidal';
run;


%exportIfRows(dataName=F3, out=&outName, errorName= Sampling_events_poorly_defined, isFatal=1);
*%logErrors(dataName=F3, errorName= Sampling_events_poorly_defined, isFatal= 1);

data errors(keep = study errortype); set errors f3; run;

* average each taxon within a sample_ID (to deal with cases where a site was sampled twice within a 45 day period);

proc sort data = kelp_data_periods; by study Site  trajectory_ID period sample_ID taxon; run;

proc means data = kelp_data_periods noprint; *sum all kelp taxa by sample ID; 
class study Site  trajectory_ID period sample_ID taxon;
output out = kelp_data_periods2
mean(Latitude) = Latitude 
mean(Longitude ) = Longitude 
mean(sample_unit_size_sq_m) = sample_unit_size_sq_m
max(kelpPresent) = kelpPresent  
max(depth_M) = max_Depth 
mean(depth_M) = mean_Depth 
min(depth_M) = min_Depth 
mean(sasDate) = sasDate
mean(biomass_KG_wet_per_sq_m Stipe_Density_num_per_sq_m individual_Density_num_per_sq_m percent_cover)
= biomass_KG_wet_per_sq_m Stipe_Density_num_per_sq_m individual_Density_num_per_sq_m percent_cover;
ways  6;
run;


*make sure one sample ID does not have different number of species sampled (rows of data) at different periods;
*********************************************************************************************;
proc sort data = kelp_data_periods2; by study Site trajectory_ID sample_ID period; run;

proc means data = kelp_data_periods2 noprint;
class study Site trajectory_ID sample_ID period;
output out = check_sampling  max(kelpPresent) = kelpPresent;
ways 5;
run;

proc means data = check_sampling noprint;
class study Site trajectory_ID sample_ID;
output out = check_sampling2  max(_freq_) = max_sampling min(_freq_) = min_sampling;
ways 4;
run;

data _change_In_Species_Sampled;
set check_sampling2;
if (study NE 'Dayton et al. 1999') or (sample_ID NE 'Point Loma') or min_sampling NE 1;
if max_sampling NE min_sampling;
errorType = '_change_In_Species_Sampled';
run;

*%logErrors(dataName=_change_In_Species_Sampled, errorName= _change_In_Species_Sampled, isFatal= 1);
%exportIfRows(dataName=_change_In_Species_Sampled, out=&outName, errorName= _change_In_Species_Sampled, isFatal=1);

data errors(keep = study errortype); set errors check_sampling2; run;

* would filter taxa here if neccesary;

* Sum kelp across taxa to the sample_ID level;

proc sort data = kelp_data_periods2; by study Site trajectory_ID period sample_ID; run;

proc means data = kelp_data_periods2 noprint; *sum all kelp taxa by sample ID; 
class study Site  trajectory_ID period sample_ID;
output out = clean_data2 
mean(Latitude) = Latitude 
mean(Longitude ) = Longitude 
mean(sample_unit_size_sq_m) = sample_unit_size_sq_m
max(kelpPresent) = kelpPresent  
max(max_Depth) = max_Depth  
mean(mean_Depth) = mean_Depth 
min(min_Depth ) = min_Depth 
mean(sasDate) = sasDate
sum(biomass_KG_wet_per_sq_m Stipe_Density_num_per_sq_m individual_Density_num_per_sq_m percent_cover)
= biomass_KG_wet_per_sq_m Stipe_Density_num_per_sq_m individual_Density_num_per_sq_m percent_cover;
ways  5;
run;

data clean_data3;
set clean_data2;
if biomass_KG_wet_per_sq_m GE 0 then 			has_BM = 1; else has_BM = 0;
if Stipe_Density_num_per_sq_m GE 0 then 		has_SD = 1; else has_SD = 0;
if individual_Density_num_per_sq_m GE 0 then 	has_ID = 1; else has_ID = 0;
if percent_cover GE 0 then 						has_PC = 1; else has_PC = 0;
if (study NE 'Dayton et al. 1999') or (sample_ID NE 'Point Loma') or _freq_ NE 1; * throw out the cases at point loma where dayton reported one species but not the other;

if Min_depth GE 3; *cut intertidal and near intertidal data;
run;

*sum to site scale;
*********************************************************************************************;
proc sort data = clean_data3; by study Site  trajectory_ID period sasdate; run;

proc means data = clean_data3 noprint;
class study Site trajectory_ID period; 
output out =clean_data4
mean(Latitude) = Latitude 
mean(Longitude ) = Longitude 
mean(sample_unit_size_sq_m) = sample_unit_size_sq_m
mean(sasdate) = sasDate 
max(max_Depth) = max_Depth  
mean(mean_Depth ) = mean_Depth 
min(min_Depth ) = min_Depth
mean(biomass_KG_wet_per_sq_m Stipe_Density_num_per_sq_m individual_Density_num_per_sq_m percent_cover)
= biomass_KG_wet_per_sq_m Stipe_Density_num_per_sq_m individual_per_sq_m percent_cover
std(biomass_KG_wet_per_sq_m Stipe_Density_num_per_sq_m individual_Density_num_per_sq_m percent_cover)
= biomass_KG_wet_per_sq_m_STD Stipe_Density_num_per_sq_m_STD individual_per_sq_m_STD percent_cover_STD
max(has_BM has_SD has_ID has_PC) = has_BM has_SD has_ID has_PC
;
ways  4;
run;

*check sampling at the site scale;
********************************************************************************************;
proc means data = clean_data4 noprint;
class study Site latitude longitude trajectory_ID; 
output out =site_sampling
max(_freq_) = max_sampling 
min(_freq_ ) = min_sampling
 ;
ways  5;
run;

data _change_In_sample_IDs;
set site_sampling;
if max_sampling NE min_sampling;
errorType = '_change_In_sample_IDs';

*if study NE "Coyle Cove";*nova scotia studies with variabe sampling;
*if study NE "Paddy's Head";*nova scotia studies with variabe sampling;
*if study NE "Sandy Cove";*nova scotia studies with variabe sampling;
*if study NE "Lodge";*nova scotia studies with variabe sampling;
*if study NE "Southern_Chile";
*if study NE "KelpCover_Witman_GOM";
*if study NE "isla_natividad";
*if study NE "PISCO_oregon";

if study NE "SNI";
if study NE "Nova_Scotia";
if study NE "New_Zealand_Hauraki_Gulf";
if study NE "Northern_New_Zealand";
if study NE "Haida_Gwaii";
if study NE "witman_gom";
if study NE "Konar_Beaufort_Sea";
if study NE "Estes_NPRB";
if study NE "PISCO_subtidal";
if study NE "Northern_Chile";
if study NE "KelpCover_Temperate_Australia";
if study NE "KelpCover_Australia_Victoria";
if study NE "Edwards_baja";
if study NE "sbc_lter_longterm_community";
if study NE "steneck_gom";
if study NE "Northern_Ireland_Summary";
if study NE "REBENT_Brittany_NW_France";

if study NE "SA_Gansbaai_kelp_data";



run;

%exportIfRows(dataName=_change_In_sample_IDs, out=&outName, errorName= _change_In_sample_IDs, isFatal=1);


*arrange for output;
*********************************************************************************************;
* setp 3. retain last sample for each metric, calculate the delta and the elapsed time.
* step 4.  ;

data clean_data5;
set clean_data4;
study_ID = &SetStudyID;
year = year(sasDate);
rawDate = sasDate * 1;
sample_size = _FREQ_;
;*taxa = -1placeholder taxa not being used;
*if kelp GE 0;
run;

proc sql;
create table clean_data6 as
select study_ID, study, site, latitude, longitude, max_depth, mean_Depth, min_Depth, trajectory_ID, sample_size, sample_unit_size_sq_m, year, period, rawdate, sasdate,
biomass_KG_wet_per_sq_m, Stipe_Density_num_per_sq_m, individual_per_sq_m, percent_cover, biomass_KG_wet_per_sq_m_STD,  Stipe_Density_num_per_sq_m_STD,  individual_per_sq_m_STD,  percent_cover_STD, 
has_BM, has_SD, has_ID, has_PC
from clean_data5;
run;
quit;
