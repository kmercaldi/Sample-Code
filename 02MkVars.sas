/*==========================================================================================
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

   Program:         02MkVars.sas
  
   Developer:       Katie Mercaldi
  
   Date:            24Jul2019
  
   Platform:        SAS 9.4 TS1M5 on X94_7PRO Platform (Windows)
  
   Description:     Creates patient-level using Centers for Medicare & Medicaid Services
                    (CMS) 2008-2010 Data Entrepreneurs’ Synthetic Public Use File
                    (DE-SynPUF). This program must be run sequentially with these
                    additional programs:
  
                      00GETDATA.SAS
                      01GETCOHORT.SAS
                      02MKVARS.SAS <-- Current program
                      03TABLES.SAS
   
                    Please note that the DE-SynPUF data does not perfectly match the
                    structure of actual CMS data, so some differences in methods are
                    necessary.
  
                    The current program calculates the Charlson Comorbidity Index (CCI)
                    based on the coding algorithm described in:
  
                      Quan et al., "Coding Algorithms for Defining Comorbidities in
                      ICD-9-CM and ICD-10 Administrative Data", Medical Care:43(11), 
                      Nov. 2005 p1130-1139:
  
                    Steps include:
  
                      1. Get the ICD-9 diagnosis codes associated with each condition 
                         of the CCI
  
                      2. Determine presence/absence of the conditions on inpatient
                         and/or outpatient claims in baseline period for each patient
                        
                      3. Calculate the CCI based on the binary comorbidiy variables
                         for each patient
  
   Macros Used:     All macros available in MACROS.SAS. This program uses:
                      %CCIFLAGS.SAS
                      %PULLCLAIMS.SAS
                      %COMPARECLAIMS.SAS
                      %CCIWTS.SAS

   Input:           Study attrition and final cohort data:
                      SASDIR.ATTRITION
                      SASDIR.COHORT
  
                    2008-2010 CMS synthetic inpatient and outpatient claims data:
                      SASDIR.DE1_0_2008_to_2010_Inpatient_Claims_Sample_1
                      SASDIR.DE1_0_2008_to_2010_Outpatient_Claims_Sample_1
  
                    CMS ICD-9-CM code dictionary:
                      SASDIR.ICD9DX
    
   Output:          Patient-level analysis data set:
                      SASDIR.PATVARS
  
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
  STEP 1: Get the ICD-9 diagnosis codes for identifying CCI conditions (based on codes in 
          Quan et al.)
 --------------------------------------------------------------------------------------------
    Note: Here the code search is applied to the code dictionary rather than the actual data 
          so that the codes may be loaded into a HASH table for matching later on -- This is 
          to increase efficiency when running on very large data, such as the CMS 100% Sample 
          Limited Data Sets.
  
          The ICD-9 code dictionary downloaded from CMS does not historical or non-billable 
          codes. If these codes are desired for variable definition, then a more complete 
          codebook is needed or the code search should be directly applied to the data.
 ============================================================================================;

* Get the SAS code containing the ICD-9 codes for each CCI condition used by Quan et al. 
  published with permission by Manitoba Centre for Health Policy at University of Manitoba ;
filename ccisas "&sasdir\cci_sascode.txt";
 
proc http url="&cciurl/ICD9_E_Charlson.sas.txt" method="get" out=ccisas;
     run;

data _null_;
     infile ccisas truncover;
     input sascode $1000.;

     * Replace tabs with spaces ;
     sascode = tranwrd(sascode,'09'x,'    '); 

     * Use SAS code by Quan et al. for defining macro variables identify each of the 17 
       comorbid conditions to calculate CCI:
 --------------------------------------------------------------------------------------------
        - DC1-DC17:   ICD-9 diagnosis codes to identify each condition
        - DIS1-DIS17: Abbreviation for each condition
        - LBL1-LBL17: Full name of each condition to be used in labels
 --------------------------------------------------------------------------------------------;
     if strip(upcase(sascode)) =: '%LET';

     * Run the SAS code to create the macro variables ;
     call execute(sascode); 

     run;

%put _user_;

* Flag CCI conditions in the CMS ICD-9 code dictionary and save for reference ;
data sasdir.cci_codes;
     set sasdir.icd9dx;

     * Flag ICD-9 diagnosis codes associated with each condition ;
     %cciflags(icdvar=code)

     * Keep only codes for CCI conditions ;
     format flag_: yesno.;
     array cci_flags flag_:;
     if sum(of cci_flags{*});

     run;


*============================================================================================
  STEP 2: Determine presence/absence of the conditions on inpatient and/or outpatient claims 
          in baseline period for each patient
 ============================================================================================;

* Get the names of all CCI condition flag variables quoted and comma delimited ;
proc sql noprint;
     select quote(strip(name)), name 
     into :qflags separated by ",", :flags separated by " "
     from dictionary.columns 
     where upcase(libname) = "WORK" and 
           upcase(memname) = "CCI_CODES" and 
           upcase(name) like 'FLAG_%';
     quit;

* Count the number of flag variables and print;
%let nflags = %sysfunc(countw(&flags));
%put FLAG VARIABLES = &flags;
%put NUMBER OF FLAG VARIABLES = &nflags;

*--------------------------------------------------------------------------------------------
  Use macros to select claims with diagnoses of interest for study patients 
  during baseline:
 --------------------------------------------------------------------------------------------
  - %PULLCLAIMS uses HASH tables to select study patients and claims with relevant diagnoses
  - %COMPARECLAIMS uses DATA STEP match merge and direct search of diagnoses for comparison
    of results and performance to HASH methods
 --------------------------------------------------------------------------------------------;
%pullclaims(source=ip) 
%compareclaims(source=ip)

* PROC COMPARE shows equal results for both methods on inpatient claims:
 --------------------------------------------------------------------------------------------
  Performance using hash:
    real time           0.26 seconds
    cpu time            0.23 seconds

  Performance using more traditional methods:
    real time           0.34 seconds
    cpu time            0.32 seconds
 --------------------------------------------------------------------------------------------;

%pullclaims(source=op)
%compareclaims(source=op)

* PROC COMPARE shows equal results for both methods on outpatient claims:
 --------------------------------------------------------------------------------------------
  Performance using hash:
    real time           1.52 seconds
    cpu time            1.17 seconds

  Performance using more traditional methods:
    real time           2.12 seconds
    cpu time            2.12 seconds
 --------------------------------------------------------------------------------------------;

* Combine inpatient and outpatient claims into one data set ;
data cohort_claims;
     set cohort_claims_ip (in=i)
         cohort_claims_op (in=o)
         ;
     by desynpuf_id;

     * Document claim source:
 --------------------------------------------------------------------------------------------
        1 = Inpatient
        2 = Outpatient
 --------------------------------------------------------------------------------------------;
     label claim_source = "Claim source";
     format claim_source clmsrc.;
     claim_source = sum(1*i,2*o);
     run;

* Delete intermediate data sets ;
proc datasets nolist;
     delete cohort_claims_ip
            test_claims_ip
            cohort_claims_op
            test_claims_op
            ;
     run;
     quit;


*============================================================================================
  Determine presence/absence of comorbidities for each patient based on inpatient and 
  outpatient claims
 ============================================================================================;

* Assign weights to CCI conditions using %CCIWTS macro:
 --------------------------------------------------------------------------------------------
   - 6 points: Metastatic cancer and HIV/AIDS
   - 3 points: Moderate to severe liver disease
   - 2 points: Complicated diabetes, renal disease, non-metastatic cancer
   - 1 point:  All other conditions
 --------------------------------------------------------------------------------------------;
%cciwts;


* Merge cohort IDs with inpatient/outpatient claims to ensure variable values for all patients 
  in case any patients had no relevant claims ;
data patvars0 (keep = desynpuf_id cci:);
     merge sasdir.cohort (in=c keep = desynpuf_id)
           cohort_claims (keep = desynpuf_id clm_thru_dt flag_:)
           ;
     by desynpuf_id;

     * Keep all cohort IDs in case any patients have no claims pulled ;
     if c;

     label  %ccilbls
            cci = "CCI score"
            ccicat = "CCI score, categorized"
            ccipred = "CCI score"
            ;
     format cci_: yesno.
            ccicat ccicat.
            ccipred ccifmt.
            ;

     * Retain across observations for each patient ;
     retain cci_:;

     * Define arrays for variables flagging claims, final patient-level variables, and
       weighted CCI scores ;
     array flagvar{&nflags} flag_:;
     array ccivar{&nflags} cci_:;
     array wccivar{&nflags} wcci_:;

     * Reset CCI condition variable to 0 ("No") on the first claim for each patient ;
     if first.desynpuf_id then do i = 1 to dim(ccivar);
        ccivar{i} = 0;
     end;

     * For each condition, check if flagged on the claims and set to "1" if yes ;
     do i = 1 to dim(ccivar);
        ccivar{i} = max(ccivar{i},(flagvar{i} = 1));
     end;

     * On the last patient claim, calculate CCI and output ;
     if last.desynpuf_id then do;

        * If patient has diagnoses for the same disease at different severity levels then 
          only keep the more severe form ;
        if sum(cci_diab_nc,cci_diab_c) = 2 then cci_diab_nc = 0;
        if sum(cci_mildld,cci_msld) = 2 then cci_mildld = 0;
        if sum(cci_cancer,cci_mets) = 2 then cci_cancer = 0;

        * Multiply each condition indicator with the weight defined in %CCIWTS ;
        do i = 1 to dim(ccivar);
           wccivar{i} = ccivar{i} * symgetn(cats("wt",i));
        end;

*--------------------------------------------------------------------------------------------
        STEP 3: Calculate CCI based on component condition variable and weights defined 
                above and output
 --------------------------------------------------------------------------------------------;
        cci = sum(of wccivar{*});
        ccicat = ifn(cci < 3,cci,3);
        ccipred = ifn(cci < 3,1,2);
        output;

     end;

     run;
     

*============================================================================================
  Merge in demographic and any other variables defined at cohort selection and save
 ============================================================================================;
data sasdir.patvars;
     merge sasdir.cohort
           patvars0
           ;
     by desynpuf_id;
     run;

* Contents and frequencies for patient-level data ;
proc contents data=sasdir.patvars;
     run;

proc freq data=sasdir.patvars;
     tables cci: / list missing;
     run;


*============================================================================================
  Clear filerefs and delete intermediate data sets
 ============================================================================================;
filename _all_ clear;

proc datasets nolist;
     delete patvars0;
     run;
     quit;


************************
***  END OF PROGRAM  ***
************************;
