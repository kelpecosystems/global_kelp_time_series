proc datasets library=work kill noprint; run; quit;

%MACRO import_check_process;
%include 'C:\work - analysis\NCEAS Kelp\Unified_temporal\importDataFromR.sas' /lrecl=100000; 
%include 'C:\work - analysis\NCEAS Kelp\Unified_temporal\preCheckData.sas' /lrecl=100000; 
%include 'C:\work - analysis\NCEAS Kelp\Unified_temporal\processKelpTemporal.sas' /lrecl=100000; 
/*%include 'C:\work - analysis\NCEAS Kelp\Unified_temporal\processKelpTemporal_minimal_QA.sas' /lrecl=100000; this is broken becasuse does not use the variable time? */
data kelpOut; set clean_data6 kelpOut; run;
%MEND import_check_process;

DATA Dataset_errors;
	informat Study $64. ; informat Study_ID 8. ;informat error_Type $32. ; informat Fatal 8.                 ;
   INPUT Study $ study_ID error_Type $ fatal;
   DATALINES;  
dummyDummyDummydummyDummyDummy -1 dummyDummyDummydummyDummyDummy -1
; RUN;

data dataset_errors; set dataSet_errors; if study eq 'impossible_string'; run; * make a clean errors file;
DATA errors;
	informat Study $64. ; informat error_Type $32. ;
   INPUT Study $ error_Type $;
   DATALINES;  
dummyDummyDummydummyDummyDummy dummyDummyDummydummyDummyDummy 
; RUN;

data errors; set errors; if study eq 'impossible_string'; run; * make a clean errors file;
DATA study_list;
	informat Study $64. ; informat Study_ID 8.;
   INPUT Study $ Study_ID;
   DATALINES;  
dummyDummyDummydummyDummyDummy -1 
; RUN;

data study_list; set Study_list; if study eq 'impossible_string'; run; * make a clean errors file;

DATA kelpOut;
   DATALINES;
;RUN;

%let setStudyID =  1;  %let gapCT = 45; %let inName = temporal_data_VancouverIsland-JaneWatson.csv; %import_check_process; *%include 'C:\work - analysis\NCEAS Kelp\Unified_temporal\check_errors_and_process.sas' /lrecl=100000; 
%let setStudyID =  2;  %let gapCT = 45; %let inName = temporal_data_SNI.csv; %import_check_process; *%include 'C:\work - analysis\NCEAS Kelp\Unified_temporal\check_errors_and_process.sas' /lrecl=100000; 
%let setStudyID =  3;  %let gapCT = 45; %let inName = temporal_data_Witman_GOM.csv; %import_check_process; *%include 'C:\work - analysis\NCEAS Kelp\Unified_temporal\check_errors_and_process.sas' /lrecl=100000; 
%let setStudyID =  4;  %let gapCT = 45; %let inName = temporal_data_Konar_Beaufort_Sea.csv; %import_check_process; *%include 'C:\work - analysis\NCEAS Kelp\Unified_temporal\check_errors_and_process.sas' /lrecl=100000; 
%let setStudyID =  5;  %let gapCT = 45; %let inName = temporal_data_Channel_Islands_National_Park.csv; %import_check_process; *%include 'C:\work - analysis\NCEAS Kelp\Unified_temporal\check_errors_and_process.sas' /lrecl=100000; 
%let setStudyID =  6;  %let gapCT = 45; %let inName = temporal_data_SA_Gansbaai_kelp_data.csv; %import_check_process; *%include 'C:\work - analysis\NCEAS Kelp\Unified_temporal\check_errors_and_process.sas' /lrecl=100000; 
%let setStudyID =  7;  %let gapCT = 45; %let inName = temporal_data_Central_Chile_Fondecyt.csv; %import_check_process; *%include 'C:\work - analysis\NCEAS Kelp\Unified_temporal\check_errors_and_process.sas' /lrecl=100000; 
%let setStudyID =  8;  %let gapCT = 65; %let inName = temporal_data_PISCO_subtidal.csv; %import_check_process; *%include 'C:\work - analysis\NCEAS Kelp\Unified_temporal\check_errors_and_process.sas' /lrecl=100000; 
%let setStudyID =  9;  %let gapCT = 20; %let inName = temporal_data_KelpAbundance_NovaScotia.csv; %import_check_process; *%include 'C:\work - analysis\NCEAS Kelp\Unified_temporal\check_errors_and_process.sas' /lrecl=100000; 
%let setStudyID =  10; %let gapCT = 45; %let inName = temporal_data_sbc_lter_longterm_community.csv; %import_check_process; *%include 'C:\work - analysis\NCEAS Kelp\Unified_temporal\check_errors_and_process.sas' /lrecl=100000; 
%let setStudyID =  11; %let gapCT = 45; %let inName = temporal_data_KelpCover_Temperate_Australia_Laminariales.csv; %import_check_process; *%include 'C:\work - analysis\NCEAS Kelp\Unified_temporal\check_errors_and_process.sas' /lrecl=100000; 
%let setStudyID =  12; %let gapCT = 45; %let inName = temporal_data_Estes_NPRB.csv; %import_check_process; *%include 'C:\work - analysis\NCEAS Kelp\Unified_temporal\check_errors_and_process.sas' /lrecl=100000; 

%let setStudyID =  13; %let gapCT = 20; %let inName = Mined_Temporal_Data.csv;/*%import_check_process;%include 'C:\work - analysis\NCEAS Kelp\Unified_temporal\check_errors_and_process.sas' /lrecl=100000; */
%include 'C:\work - analysis\NCEAS Kelp\Unified_temporal\importDataFromKira.sas' /lrecl=100000; 
*%include 'C:\work - analysis\NCEAS Kelp\Unified_temporal\processKelpTemporal_minimal_QA.sas' /lrecl=100000; /**/
%include 'C:\work - analysis\NCEAS Kelp\Unified_temporal\preCheckData.sas' /lrecl=100000; 
%include 'C:\work - analysis\NCEAS Kelp\Unified_temporal\processKelpTemporal.sas' /lrecl=100000; /**/
data kelpOut; set clean_data6 kelpOut; run;

%let setStudyID =  14; %let gapCT = 45; %let inName = temporal_data_kelp_isla_natividad.csv; %import_check_process; *%include 'C:\work - analysis\NCEAS Kelp\Unified_temporal\check_errors_and_process.sas' /lrecl=100000; 
%let setStudyID =  15; %let gapCT = 45; %let inName = temporal_data_New_Zealand_Hauraki_Gulf.csv;; %import_check_process; *%include 'C:\work - analysis\NCEAS Kelp\Unified_temporal\check_errors_and_process.sas' /lrecl=100000; 
%let setStudyID =  16; %let gapCT = 45; %let inName = temporal_data_Northern_New_Zealand.csv; %import_check_process; *%include 'C:\work - analysis\NCEAS Kelp\Unified_temporal\check_errors_and_process.sas' /lrecl=100000; 
%let setStudyID =  17; %let gapCT = 45; %let inName = temporal_data_REBENT_Brittany_NW_France.csv; %import_check_process; *%include 'C:\work - analysis\NCEAS Kelp\Unified_temporal\check_errors_and_process.sas' /lrecl=100000; 
%let setStudyID =  18; %let gapCT = 45; %let inName = temporal_data_Australia_WA_Wernberg.csv; %import_check_process; *%include 'C:\work - analysis\NCEAS Kelp\Unified_temporal\check_errors_and_process.sas' /lrecl=100000; 
%let setStudyID =  19; %let gapCT = 45; %let inName = temporal_data_PISCO_oregon.csv; %import_check_process; *%include 'C:\work - analysis\NCEAS Kelp\Unified_temporal\check_errors_and_process.sas' /lrecl=100000; 
%let setStudyID =  20; %let gapCT = 45; %let inName = temporal_data_Norway_monitoring_kelp_data.csv; %import_check_process; *%include 'C:\work - analysis\NCEAS Kelp\Unified_temporal\check_errors_and_process.sas' /lrecl=100000; 
%let setStudyID =  21; %let gapCT = 31; %let inName = temporal_data_Northern_Chile.csv; %import_check_process; *%include 'C:\work - analysis\NCEAS Kelp\Unified_temporal\check_errors_and_process.sas' /lrecl=100000; 
%let setStudyID =  22; %let gapCT = 20; %let inName = temporal_data_Northern_Ireland_Summary.csv; %import_check_process; *%include 'C:\work - analysis\NCEAS Kelp\Unified_temporal\check_errors_and_process.sas' /lrecl=100000; 
%let setStudyID =  23; %let gapCT = 45; %let inName = temporal_data_Haida_Gwaii.csv; %import_check_process; *%include 'C:\work - analysis\NCEAS Kelp\Unified_temporal\check_errors_and_process.sas' /lrecl=100000; 
%let setStudyID =  24; %let gapCT = 4; %let inName = temporal_data_Southern_Chile.csv; %import_check_process; *%include 'C:\work - analysis\NCEAS Kelp\Unified_temporal\check_errors_and_process.sas' /lrecl=100000; 
%let setStudyID =  25; %let gapCT = 45; %let inName = temporal_data_Edwards_baja.csv; %import_check_process; /**/*%include 'C:\work - analysis\NCEAS Kelp\Unified_temporal\check_errors_and_process.sas' /lrecl=100000; 
%let setStudyID =  26; %let gapCT = 45; %let inName = temporal_data_KelpCover_Australia_Victoria.csv; %import_check_process; *%include 'C:\work - analysis\NCEAS Kelp\Unified_temporal\check_errors_and_process.sas' /lrecl=100000; 

%let setStudyID =  27; %let gapCT = 45; %let inName = temporal_data_autralia_connell_canopyLoss.csv; %import_check_process; /**/*%include 'C:\work - analysis\NCEAS Kelp\Unified_temporal\check_errors_and_process.sas' /lrecl=100000; 
%let setStudyID =  28; %let gapCT = 45; %let inName = temporal_data_steneck_gom.csv; %import_check_process; *%include 'C:\work - analysis\NCEAS Kelp\Unified_temporal\check_errors_and_process.sas' /lrecl=100000; 

*adding more 6/30/2015;
%let setStudyID =  29; %let gapCT = 45; %let inName = temporal_data_konar_GulfOfAlaska.csv; %import_check_process; *%include 'C:\work - analysis\NCEAS Kelp\Unified_temporal\check_errors_and_process.sas' /lrecl=100000; 

*added more 1/19/16;

%let setStudyID =  30; %let gapCT = 45; %let inName = temporal_data_norway_inner_coast_norderhaug.csv; %import_check_process; *%include 'C:\work - analysis\NCEAS Kelp\Unified_temporal\check_errors_and_process.sas' /lrecl=100000; 
%let setStudyID =  31; %let gapCT = 45; %let inName = temporal_data_norway_moy_christie_with_Åsen_2006.csv; %import_check_process; *%include 'C:\work - analysis\NCEAS Kelp\Unified_temporal\check_errors_and_process.sas' /lrecl=100000; 




* set cleared errors fatal tp 0;

*********************************************;

proc sort data = dataset_errors; by study_ID; run;
proc means data = dataset_errors noprint;
class study_ID;
output out=Fatal_studies(drop = _type_ _freq_) max(fatal) = fatal;
ways 1;
run;

proc sort data = kelpOut; by study_ID; run;
data kelpOut2; 
merge kelpOut fatal_studies;
by study_ID;
run; 

data kelpOut3; set kelpOut2; if fatal NE 1; run;
proc export data=WORK.kelpOut3 outfile="C:\work - analysis\NCEAS Kelp\CleanData.csv" 
dbms = csv replace;run;

data good_studies;
merge study_list fatal_studies;
by study_ID;
if fatal NE 1;
run; 

*check focal data prevalance;
proc means data = kelpOut4 noprint;
output out = dataTypes sum(has_BM has_SD has_ID has_PC) = has_BM has_SD has_ID has_PC;
run;


data kelpOut4;
set kelpout3;
retain LSI .;
retain LTI .;
retain LRD .;
retain LFK .;
if study_ID GT 0;

*assign focal units based on prioraty (higher is lower prioraty in this list);
if has_PC EQ 1 then do; focalKelp = percent_cover; 				focalKelp_STD = percent_cover_STD; 				focalUnit = 1; end;
if has_ID EQ 1 then do; focalKelp = individual_per_sq_m ; 		focalKelp_STD = individual_per_sq_m_STD ; 		focalUnit = 2; end;
if has_SD EQ 1 then do; focalKelp = Stipe_Density_num_per_sq_m; focalKelp_STD = Stipe_Density_num_per_sq_m_STD;	focalUnit = 3;  end; 
if has_BM EQ 1 then do; focalKelp = biomass_KG_wet_per_sq_m;	focalKelp_STD = biomass_KG_wet_per_sq_m_STD;		focalUnit = 4;end;

*force Konar beaufort sea and others with incomplete datasets to use complete focal units;
if study_ID EQ 4 then do;  							focalKelp = biomass_KG_wet_per_sq_m;	focalKelp_STD = biomass_KG_wet_per_sq_m_STD;	focalUnit = 4;end;
if (study_ID EQ 9 and trajectory_ID EQ 4) then do; 	focalKelp = biomass_KG_wet_per_sq_m;	focalKelp_STD = biomass_KG_wet_per_sq_m_STD;	focalUnit = 4;end;

if (study_ID EQ 9 and trajectory_ID EQ 6) then do; focalKelp = biomass_KG_wet_per_sq_m;	focalKelp_STD = biomass_KG_wet_per_sq_m_STD;	focalUnit = 4;end;
if (study_ID EQ 9 and trajectory_ID EQ 7) then do; focalKelp = individual_per_sq_m ; 		focalKelp_STD = individual_per_sq_m_STD ; 		focalUnit = 2; end;


if (study_ID EQ 9 and trajectory_ID EQ 10) then do; focalKelp = biomass_KG_wet_per_sq_m;	focalKelp_STD = biomass_KG_wet_per_sq_m_STD;	focalUnit = 4;end;




/*if (study_ID EQ 13 and trajectory_ID EQ 51) then do; focalKelp = biomass_KG_wet_per_sq_m;	focalKelp_STD = biomass_KG_wet_per_sq_m_STD;	focalUnit = 4;end;
if (study_ID EQ 13 and trajectory_ID EQ 53) then do; focalKelp = biomass_KG_wet_per_sq_m;	focalKelp_STD = biomass_KG_wet_per_sq_m_STD;	focalUnit = 4;end;
if (study_ID EQ 13 and trajectory_ID EQ 70) then do; focalKelp = individual_per_sq_m ; 		focalKelp_STD = individual_per_sq_m_STD ; 		focalUnit = 2;end;
if (study_ID EQ 13 and trajectory_ID EQ 71) then do; focalKelp = individual_per_sq_m ; 		focalKelp_STD = individual_per_sq_m_STD ; 		focalUnit = 2;end;
*/

*cut trajectories without consistant sampling;
/*if (study_ID EQ 9 and trajectory_ID EQ 9) then do; focalKelp = .;	focalKelp_STD = .;		focalUnit = .;end;*/

*if this is the same trajectory and study ID and there is focal data;
if (LSI EQ study_ID) and (LTI EQ trajectory_ID) and (focalKelp GE 0) then elapsed = rawDate - LRD; else elapsed = .;

if 
(elapsed GT 0)  /*if there is a previous point in the trajectory to compare it to*/
then do;
if (focalKelp GT 0 and LFK GT 0) then ROC = log(focalKelp/LFK)/elapsed*365.25; /*if something to something calculate annualuized ROC*/
if (focalKelp EQ 0 and LFK EQ 0) then ROC = .; /*if it is  a zero to zero step, we do not calculate ROC*/
if (focalKelp GT 0 and LFK EQ 0) then ROC =  9999; /*if it is  a something to zero step, ROC is arbitraraly small*/
if (focalKelp EQ 0 and LFK GT 0) then ROC = -9999; /*if it is  a zero to something step, ROC is arbitraraly large*/
end;

LSI = study_ID;
LTI = trajectory_ID;
LRD = rawDate;
LFK = focalKelp;

run; 

data kelpout5;
set kelpout4(drop = LSI  LTI LRD LFK has_BM has_SD has_ID has_PC fatal);
run;



proc export data=WORK.kelpOut5 outfile="C:\Dropbox\nceas_kelp_climate_2013\temporal_change\clean_datasets\dataQA\CleanData.csv" 
dbms = csv replace;run;

proc export data=WORK.good_studies outfile="C:\Dropbox\nceas_kelp_climate_2013\temporal_change\clean_datasets\dataQA\clean_studies.csv" 
dbms = csv replace;run;

proc export data=WORK.dataset_errors outfile="C:\Dropbox\nceas_kelp_climate_2013\temporal_change\clean_datasets\dataQA\errors.csv" 
dbms = csv replace;run;

proc sort data = kelpout5; by study_ID trajectory_ID; run;
proc means data = kelpout5 noprint;
class study_ID trajectory_ID; 
output out = test_kelp 
min(mean_Depth ) = Min_D 
max(mean_depth )= max_D
min(focalUnit) = min_unit
max(focalUnit) = max_unit;
ways 2;
run;

data test_Kelp2; *should have zero observations;
set test_kelp;
if min_Unit NE max_unit;
run;
data test_Kelp3; 
set test_kelp;
if ((max_D-min_D)/min_D GT 1) or min_Unit NE max_unit;
run;


data analyze1;
set kelpout5;
absROC = abs(ROC);
latDeg = round(latitude,1.0);
lonDeg = round(longitude,1.0);
if latitude GT 0;
*if ROC LT 0;
run;

DATA results;
	informat Time 8.2 ; informat _FREQ_ 8.2 ; informat ROC 8.2 ;informat ROC_75 8.2 ;informat ROC_25 8.2 ;
   INPUT Time _FREQ_ ROC  ;
   DATALINES;
   dummyDummyDummydummyDummyDummy dummyDummyDummydummyDummyDummy 
;RUN;

%macro runYEARS();
%do i=1978 %to 2012;
data Q1;
set analyze1;
if year GE (&i);
if (year - elapsed/365.25) LE (&i);
yearLow = (year - elapsed/365.25);
Time = &i;
if (absROC GE 0);
run;

proc means data = Q1 noprint;
class Time;
output out = Q2(drop =  _type_) p50(ROC) = ROC p75(ROC) = ROC_75 p25(ROC) = ROC_25; 
ways 1;
run;

data results;
set results Q2;
run; 
%end;
%mend runYears;

%runYEARS()


DATA resultsSpatial;
	informat Time 8.2 ; informat _FREQ_ 8.2 ; informat ROC 8.2 ;informat ROC_75 8.2 ;informat ROC_25 8.2 ;
   INPUT Time _FREQ_ ROC  ;
   DATALINES;
   dummyDummyDummydummyDummyDummy dummyDummyDummydummyDummyDummy 
;RUN; 


%macro runYEARSspatial();
%do i=1978 %to 2012;
data Q1;
set analyze1;
if year GE (&i);
if (year - elapsed/365.25) LE (&i);
yearLow = (year - elapsed/365.25);
Time = &i;
run;

proc sort data = Q1; by time latDeg lonDeg; run;

proc means data = Q1 noprint;
class Time latDeg lonDeg;
output out = Q2(drop =  _type_) p50(ROC) = ROC p75(ROC) = ROC_75 p25(ROC) = ROC_25 p50(absROC) = absROC; 
ways 3;
run;

data resultsSpatial_2;
set resultsSpatial_2 Q2;
run; 

data  Q2; set Q2; if absROC ge 0; run;

proc means data = Q2 noprint;
class Time;
output out = Q3(drop =  _type_) p50(ROC) = ROC p75(ROC) = ROC_75 p25(ROC) = ROC_25; 
ways 1;
run;

data resultsSpatial;
set resultsSpatial Q3;
run; 
%end;
%mend runYearsspatial;

%runYEARSspatial()
