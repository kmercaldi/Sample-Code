/*==========================================================================================
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

   Program:         01GetCohort.sas
  
   Developer:       Katie Mercaldi
  
   Date:            24Jul2019
  
   Platform:        SAS 9.4 TS1M5 on X94_7PRO Platform (Windows)
  
   Description:     Executes a brief cohort selection to study the impact of comorbities
                    on mortality using Centers for Medicare & Medicaid Services (CMS)
                    2008-2010 Data Entrepreneurs’ Synthetic Public Use File (DE-SynPUF).
                    This program must be run sequentially with these additional
                    programs:
  
                      00GETDATA.SAS
                      01GETCOHORT.SAS <-- Current program
                      02MKVARS.SAS
                      03TABLES.SAS
   
                    Please note that the DE-SynPUF data does not perfectly match the
                    structure of actual CMS data, so some differences in methods are
                    necessary.
  
                    The current program identifies a retrospective cohort of patients
                    meeting the following criteria common to studies using CMS data:
  
                      1. Age at least 65 years old as of 1/1/2008 to exclude patients
                         receiving Medicare benefits soley due to disability
                        
                      2. Continuously enrolled in Medicare Parts A and B from 1/1/2008
                         to 12/31/2008 (baseline period)
   
                      3. Enrolled in a fee-for-service (FFS) plan from 1/1/2008 during
                         baseline and until end of follow-up
   
                      4. Death date on or after 1/1/2009 (start of follow-up) or not
                         recorded
  
   Macros Used:     None

   Input:           2008-2010 CMS synthetic beneficiary data:
                      SASDIR.DE1_0_2008_Beneficiary_Summary_File_Sample_1
                      SASDIR.DE1_0_2009_Beneficiary_Summary_File_Sample_1
                      SASDIR.DE1_0_2010_Beneficiary_Summary_File_Sample_1
  
   Output:          Study attrition and final cohort data:
                      SASDIR.ATTRITION
                      SASDIR.COHORT
  
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
============================================================================================
 Automatically run AUTOEXEC.SAS first if working interactively
============================================================================================*/
data _null_;
     if "%sysfunc(getoption(sysin))" = "" then do;
        fullpath = "%sysget(SAS_EXECFILEPATH)";
        filename = "%sysget(SAS_EXECFILENAME)";
        pathlen = length(fullpath) - length(filename);
        path = substr(fullpath,1,pathlen);
        call execute(cats("%include '",path,"Autoexec.sas';"));
     end;
     run;


**************************
***  START OF PROGRAM  ***
**************************;


*============================================================================================
  Assess baseline cohort selection criteria using the 2008 beneficiary data
 ============================================================================================;
data base (keep = desynpuf_id index_dt base_startdt base_stopdt age age_cat 
                  gender race qual_age qual_enr qual_ffs qual_death);
     set sasdir.de1_0_2008_bene_sample_1 
         (keep = desynpuf_id bene_birth_dt bene_death_dt bene_sex_ident_cd bene_race_cd 
                 bene_hi_cvrage_tot_mons bene_smi_cvrage_tot_mons bene_hmo_cvrage_tot_mons);

     * Input data has already been sorted by ID ;
     by desynpuf_id;
 
     label  age          = "Patient age (in years) as of 1/1/2008"
            age_cat      = "Patient age (in years), categorized"
            gender       = "Gender"
            race         = "Race/ethnicity"
            index_dt     = "Index date"
            base_startdt = "Baseline period start date"
            base_stopdt  = "Baseline period stop date"
            qual_age     = "At least 65 years old as of 1/1/2008"
            qual_enr     = "Continuously enrolled in Medicare Parts A and B from 1/1/2008 to 12/31/2008"
            qual_ffs     = "Enrolled in a fee-for-service (FFS) plan from 1/1/2008 until end of follow-up"
            qual_death   = "Death date on or after 1/1/2009 or not recorded"
            ;
     format age_cat agecat.
            gender gender.
            race race.
            index_dt base_startdt base_stopdt mmddyy10.
            qual_age qual_enr qual_ffs qual_death yesno.
            ;

     * Index date will be 1/1/2009 and baseline period will be 1/1/2008 to 12/31/2010 for 
       all patients (not typical of most studies, which usually have a diagnosis, procedure,
       or medication start date as the index date);
     index_dt     = '01jan2009'd;
     base_startdt = intnx('year',index_dt,-1,'s');;
     base_stopdt  = index_dt - 1;

     * Age in years ;
     age = intck('year',bene_birth_dt,'01jan2008'd);

     * Age category ;
     age_cat = sum(1 * (65 <= age < 70),
                   2 * (70 <= age < 75),
                   3 * (75 <= age < 80),
                   4 * (80 <= age));

     * Gender (1 = male, 2 = female);
     if bene_sex_ident_cd in ("1" "2") then gender = input(bene_sex_ident_cd,2.);
     else gender = 9;

     * Race/ethnicity
 --------------------------------------------------------------------------------------------
       CMS SynPUF codes:       Recode to:
         "1" = White             1 = White
         "2" = Black             2 = Black
         "3" = Others            3 = Hispanic
         "5" = Hispanic          4 = Other/Unknown
 --------------------------------------------------------------------------------------------;
     race = sum(1 * (bene_race_cd = "1"),
                2 * (bene_race_cd = "2"),
                3 * (bene_race_cd = "5"),
                9 * (bene_race_cd in (" " "3")));

*--------------------------------------------------------------------------------------------
     STEP 1: Inclusion criteria: Patient age is at least 65 years old as of 1/1/2008 
 --------------------------------------------------------------------------------------------;
     qual_age = (age >= 65);

*--------------------------------------------------------------------------------------------
     STEP 2: Inclusion criteria: Assess whether the patient was continuously enrolled in 
             Medicare Parts A and B from 1/1/2008 to 12/31/2008
 --------------------------------------------------------------------------------------------;   
     * Note: Enrollment data is far less detailed in the DE-SynPUF data as compared to the 
             actual CMS data with only a single variable (values 0-12) representing the 
             number of months of enrollment during the year. Because of this, the algorithm
             below for identifying continuous enrollment is greatly simplified from a typical
             claims analysis.

     * Patient must have 12 months of Part A (represented by variable BENE_HI_CVRAGE_TOT_MONS) 
       and 12 months of Part B (represented by BENE_SMI_CVRAGE_TOT_MONS) in 2008 ;
     if qual_age  then qual_enr = (bene_hi_cvrage_tot_mons = 12 and 
                                   bene_smi_cvrage_tot_mons = 12);
     
*--------------------------------------------------------------------------------------------
      STEP 3: Enrolled in a fee-for-service (FFS) plan from 1/1/2008 during baseline 
 --------------------------------------------------------------------------------------------;
     * Patient must have 0 months of HMO coverage (BENE_HMO_CVRAGE_TOT_MONS) ;
     if qual_enr then qual_ffs = (bene_hmo_cvrage_tot_mons = 0);

*--------------------------------------------------------------------------------------------
      STEP 4: Death date on or after 1/1/2009 (start of follow-up)
 --------------------------------------------------------------------------------------------;
     * Note: All death dates in the input data are in 2008, so the code 
       "bene_death_dt >= '01jan2009'd" is not strictly needed here ;
     if qual_ffs then qual_death = (bene_death_dt >= index_dt or missing(bene_death_dt));

     run;


*============================================================================================
  Determine duration of follow-up and death outcomes
 ============================================================================================;
%let keepvars = desynpuf_id bene_death_dt bene_hi_cvrage_tot_mons bene_smi_cvrage_tot_mons bene_hmo_cvrage_tot_mons;

data folup (keep = desynpuf_id enr_stopdt ffs_stopdt death_dt folup_startdt folup_stopdt status status_days);
     merge base (keep = desynpuf_id index_dt base_stopdt)
           sasdir.de1_0_2009_bene_sample_1 (keep   = &keepvars
                                            rename = (bene_death_dt=death_dt_2009 
                                                      bene_hi_cvrage_tot_mons=hi_cvrage_tot_mons_2009
                                                      bene_smi_cvrage_tot_mons=smi_cvrage_tot_mons_2009
                                                      bene_hmo_cvrage_tot_mons=hmo_cvrage_tot_mons_2009))
           sasdir.de1_0_2010_bene_sample_1 (keep   = &keepvars 
                                            rename = (bene_death_dt=death_dt_2010 
                                                      bene_hi_cvrage_tot_mons=hi_cvrage_tot_mons_2010
                                                      bene_smi_cvrage_tot_mons=smi_cvrage_tot_mons_2010
                                                      bene_hmo_cvrage_tot_mons=hmo_cvrage_tot_mons_2010));
     by desynpuf_id;

     label  enr_mons_2009 = "Number of months of continuous enrollment in Parts A and B in 2009"
            enr_mons      = "Total number of months of continuous enrollment in Parts A and B"
            ffs_mons_2009 = "Number of months of FFS plan continuous enrollment in 2009"
            ffs_mons      = "Total number of months of FFS plan continuous enrollment"
            enr_stopdt    = "Continuous enrollment in Parts A and B stop date"
            ffs_stopdt    = "FFS plan continuous enrollment stop date"
            death_dt      = "Date of death"
            folup_startdt = "Follow-up start date"
            folup_stopdt  = "Follow-up stop date"
            status        = "Patient status at the end of follow-up"
            status_days   = "Time to event or censoring (in days)"
            ;
     format enr_stopdt ffs_stopdt death_dt folup_startdt folup_stopdt mmddyy10.
            status status.
            ;

     * Define an array to impute zeros for any missing coverage months ;
     array coverage hi_cvrage_tot_mons_: smi_cvrage_tot_mons_: hmo_cvrage_tot_mons_:;

     do i = 1 to dim(coverage);
        coverage{i} = max(coverage{i},0);
     end;

*--------------------------------------------------------------------------------------------
     Note: Since only number of months enrolled is available, it is assumed here that the any 
           value between 1 and 11 is for months at the start of the year. HMO plan enrollment 
           is assumed to be at the end of the year.These would not be a valid assumptions in 
           actual data but will allow for follow-up durations that are not a multiple of 12 
           months for demonstration purposes.
 --------------------------------------------------------------------------------------------;

     * Calculate the number of months of Parts A and B continuous enrollment in 2009. If the 
       patient has complete enrollment in 2009, then add any months with Parts A and B 
       enrollment from 2010 as well. ;
     enr_mons_2009 = min(hi_cvrage_tot_mons_2009,smi_cvrage_tot_mons_2009);
     if enr_mons_2009 = 12 
        then enr_mons = sum(enr_mons_2009,min(hi_cvrage_tot_mons_2010,smi_cvrage_tot_mons_2010));
     else enr_mons = enr_mons_2009;

     * Calculate the number of months of FFS plan (i.e., non-HMO plan) continuous enrollment
       in 2009. If the patient has complete enrollment in 2009, then add any months with FFS 
       plan enrollment from 2010 as well. ;
     ffs_mons_2009 = 12-hmo_cvrage_tot_mons_2009;
     if ffs_mons_2009 = 12 
        then ffs_mons = sum(ffs_mons_2009,12-hmo_cvrage_tot_mons_2009);
     else ffs_mons = ffs_mons_2009;

     * Calculate enrollment end dates based on the number of months of enrollment defined 
       above. Calculate the date of death as the minimum of any recorded dates of death. ;
     enr_stopdt = intnx('month',base_stopdt,enr_mons,'e');
     ffs_stopdt = intnx('month',base_stopdt,ffs_mons,'e');
     if not missing(death_dt_2009) or not missing(death_dt_2010) 
        then death_dt = min(death_dt_2009,death_dt_2010);

*--------------------------------------------------------------------------------------------
     Follow-up dates
 --------------------------------------------------------------------------------------------
      - Start date = index date
      - Stop date  = minimum of: 
          1) end of Parts A and B continuous enrollment
          2) end of FFS plan continuous enrollment
          3) death
 --------------------------------------------------------------------------------------------;
     folup_startdt = index_dt;
     folup_stopdt  = min(enr_stopdt,ffs_stopdt,death_dt);

*--------------------------------------------------------------------------------------------
     Outcome: Death
 --------------------------------------------------------------------------------------------
     Determine whether the patient has a date of death during follow-up. If death date is 
     after follow-up stop date, then set to missing
 --------------------------------------------------------------------------------------------;
     status = (index_dt <= death_dt <= folup_stopdt);
     if status = 0 then death_dt = .;
     status_days = folup_stopdt - folup_startdt + 1;

     run;


*============================================================================================
  Attrition data and final cohort
 ============================================================================================;
data attrition0;
     merge base
           folup
           ;
     by desynpuf_id;
     label  allpt = "Total number of patients assessed"
            include = "Patients included in the final cohort"
            ;
     format allpt include yesno.;

     allpt = 1;

*--------------------------------------------------------------------------------------------
     STEP 4: (Continued) Determine whether the patient has Parts A and B / FFS plan
             enrollment for at least one day of follow-up
 --------------------------------------------------------------------------------------------;
     if folup_stopdt <= base_stopdt and qual_ffs then do;
        qual_ffs = 0;
        qual_death = .;
     end;

     * Set missing values for qualifying variables to zero ;
     array qual qual_:;
     do i = 1 to dim(qual);
        if qual{i} = . then qual{i} = 0;
     end;

     include = (qual_death = 1);

     run;

* Save final attrition and cohort data ;
data sasdir.attrition;
     set attrition0 (keep = desynpuf_id allpt qual_: include);
     run;

data sasdir.cohort (drop = include);
     set attrition0 (drop = enr_stopdt ffs_stopdt qual_: 
                     where = (include=1));
     label allpt = "All Patients";
     format allpt all.;
     run;

title 'Contents of attrition data set';
proc contents data=sasdir.attrition;
     run;

title 'Contents of final cohort data set';
proc contents data=sasdir.cohort;
     run;


*============================================================================================
  Run data checks and print reports for review
 ============================================================================================;
title 'Inclusion criteria frequencies';
proc freq data=sasdir.attrition;
     tables qual_age * qual_enr * qual_ffs * qual_death * include / list missing;
     run;

title 'Demographic and outcome variable frequencies';
proc freq data=sasdir.cohort;
     tables age_cat gender race status / list;
     run;

proc sql noprint;
     * Age group check ;
     create table agecheck as
     select min(age_cat) as age_cat label="Age category" format=agecat.,
            min(age) as min_age label="Minimum age in years",
            max(age) as max_age label="Maximum age in years"
     from sasdir.cohort
     group by age_cat;

     * Study date checks ;
     create table indexcheck as
     select min(index_dt) as min_index_dt label="Minimum index date" format=mmddyy10.,
            max(index_dt) as max_index_dt label="Maximum index date" format=mmddyy10.
     from sasdir.cohort;

     create table basecheck as
     select min(base_startdt) as min_base_startdt label="Minimum baseline start date" format=mmddyy10.,
            max(base_startdt) as max_base_startdt label="Maximum baseline start date" format=mmddyy10.,
            min(base_stopdt) as min_base_stopdt label="Minimum baseline stop date" format=mmddyy10.,
            max(base_stopdt) as max_base_stopdt label="Maximum baseline stop date" format=mmddyy10.
     from sasdir.cohort;

     create table folupcheck as
     select min(folup_startdt) as min_folup_startdt label="Minimum follow-up start date" format=mmddyy10.,
            max(folup_startdt) as max_folup_startdt label="Maximum follow-up start date" format=mmddyy10.,
            min(folup_stopdt) as min_folup_stopdt label="Minimum follow-up stop date" format=mmddyy10.,
            max(folup_stopdt) as max_folup_stopdt label="Maximum follow-up stop date" format=mmddyy10.
     from sasdir.cohort;

     create table deathcheck as
     select min(death_dt) as min_death_dt label="Minimum death date" format=mmddyy10.,
            max(death_dt) as max_death_dt label="Maximum death date" format=mmddyy10.,
            min(folup_stopdt-death_dt) as min_diff label="Minimum days between end of follow-up and death date",
            max(folup_stopdt-death_dt) as max_diff label="Maximum days between end of follow-up and death date",
            min(status_days) as min_days label="Minimum time to event or censoring",
            max(status_days) as max_days label="Maximum time to event or censoring"
     from sasdir.cohort (where = (death_dt > .));

     quit;
     

* Print data check reports ;
title 'Minimum and maximum ages in each group should be within category limits';
proc print data=agecheck noobs label;
     run;
     
title 'Minimum and maximum index date should be 1/1/2009';
proc print data=indexcheck noobs label;
     run;
     
title1 'Minimum and maximum baseline start date should be 1/1/2008';
title2 'Minimum and maximum baseline stop date should be 12/31/2008';
proc print data=basecheck noobs label;
     run;  
 
title1 'Minimum and maximum follow-up start date should be 1/1/2009';
title2 'Follow-up stop date should be between 1/1/2009 and 12/31/2010';
proc print data=folupcheck noobs label;
     run;
 
title1 'Non-missing death dates should be 1/1/2009 and 12/31/2010';
title2 'There should be no difference between death date and the end of follow-up';
proc print data=deathcheck noobs label;
     run;
     

*============================================================================================
  Delete intermediate and data checking files
 ============================================================================================;
proc datasets nolist;
     delete base
            folup
            attrition0
            agecheck 
            indexcheck 
            basecheck 
            folupcheck 
            deathcheck
            ;
     run;
     quit;


************************
***  END OF PROGRAM  ***
************************;
