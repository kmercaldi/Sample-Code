/*==========================================================================================
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

   Program:         03Tables.sas                                                         
                                                                                        
   Developer:       Katie Mercaldi                                                       
                                                                                        
   Date:            24Jul2019                                                            
                                                                                        
   Platform:        SAS 9.4 TS1M5 on X94_7PRO Platform (Windows)                         
                                                                                        
   Description:     Creates sample tables using a patient-level data set derived from    
                    Centers for Medicare & Medicaid Services (CMS) 2008-2010 Data        
                    Entrepreneurs’ Synthetic Public Use File (DE-SynPUF). This program   
                    must be run sequentially with these additional programs:             
                                                                                        
                      00GETDATA.SAS                                                      
                      01GETCOHORT.SAS                                                    
                      02MKVARS.SAS                                                       
                      03TABLES.SAS <-- Current program                                   
                                                                                         
                    The current program completes the following steps:                   
                                                                                        
                      1. Creates study attrition table                             
                                                                                        
                      2. Creates a table with descriptive statistics for demographic variables

                      3. Creates a table with descriptive statistics for Charlson Comorbidity 
                         Index (CCI) and individual component conditions of the CCI                                 
                                                                                        
                      4. Runs Cox proportional-hazards regression to predict mortality   
                         using demographic and CCI measures and creates a table of the   
                         results                                                         
                                                                                        
                      5. Puts all study tables into an Excel workbook                                                       
  
   Macros Used:     All macros available in MACROS.SAS. This program uses:
                      %RUNSTATS.SAS
                      %ADDLABEL.SAS
                      %COXPH.SAS
                      %MKREPORT.SAS
                                                                                        
   Input:           Patient-level analysis data set:                                     
                      SASDIR.PATVARS                                                     
                                                                                        
   Output:          Results tables in SAS format:                                        
                      SASDIR.T01                                                         
                      SASDIR.T02                                                         
                      SASDIR.T03                                                         
                      SASDIR.T04                                                         
                                                                                        
                    Excel workbook containing results tables:                            
                      Sample Tables.xlsx                                                 
 
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
  STEP 1: Create study attrition table
 ============================================================================================;

* Get names of variables for cohort qualifying criteria ;
proc sql noprint;
     select name 
     into :qualvars separated by " "
     from sashelp.vcolumn
     where upcase(libname) = "SASDIR" and 
           upcase(memname) = "ATTRITION" and 
           upcase(name) like 'QUAL^_%' escape '^'
     order by varnum;
     quit;

* Use %RUNSTATS macro to calculate statistics for study attrition table ;
%runstats(data    = sasdir.attrition,
          out     = t01,
          coldefs = allpt=1,
          rowvars = allpt &qualvars include
          );

* Reformat the final table and save ;
data sasdir.t01;
     set t01 end = eof;
     label col1 = "Patients Included, n (%)";
     if _n_ = 1 or eof then rowlabel = tranwrd(rowlabel,'A0A0A0A0'x,"");
     run;


*============================================================================================
  STEP 2: Create a table with descriptive statistics for demographic variables
 ============================================================================================;

* Use %RUNSTATS macro to calculate descriptive statistics for the table ;
%runstats(data    = sasdir.patvars,
          out     = sasdir.t02,
          coldefs = allpt=1 status=0 status=1,
          rowvars = age age_cat gender race
          );


*============================================================================================
  STEP 3: Create a table with descriptive statistics for Charlson Comorbidity Index (CCI)
          and individual component conditions of the CCI
 ============================================================================================;

* Get names of CCI condition variables ;
proc sql noprint;
     select name 
     into :ccivars separated by " "
     from sashelp.vcolumn
     where upcase(libname) = "SASDIR" and 
           upcase(memname) = "PATVARS" and 
           upcase(name) like 'CCI^_%' escape '^'
     order by varnum;
     quit;

* Use %RUNSTATS macro to calculate statistics for the table ;
%runstats(data    = sasdir.patvars,
          out     = sasdir.t03,
          coldefs = allpt=1 status=0 status=1,
          rowvars = cci ccicat &ccivars
          );

* Add row label for reporting of individual CCI conditions ;
%addlabel(data=sasdir.t03, rownum=3, label=%bquote(CCI conditions, n (%)));


*============================================================================================
  STEP 4: Run Cox proportional-hazards regression to predict mortality using demographic and 
          CCI measures and create a table of the results
 ============================================================================================;

* Use %COXPH macro to run univariate and multivariate Cox regression models, with CCI <3 or 
  >=3 as the predictor of interest ;
%coxph(data      = sasdir.patvars,
       out       = sasdir.t04,
       outcome   = status,
       time      = status_days,
       predictor = ccipred,
       covlist   = age_cat gender race &ccivars
       );

* Add row label for reporting of individual CCI conditions ;
%addlabel(data=sasdir.t04, rownum=5, label=%bquote(CCI conditions*));


*============================================================================================
  STEP 5: Print all study tables in an Excel workbook
 ============================================================================================;
ods escapechar = "^";
ods listing close;

filename xltables "&sasdir\Sample Tables.xlsx";

ods excel file = xltables 
    options(fittopage = 'no'
            embedded_titles = 'yes'
            embedded_footnotes = 'yes'
            flow = 'tables'
            zoom = '100'
            orientation='Landscape'
            row_repeat = 'header'
            pages_fitheight = '100'
            center_horizontal = 'yes'
            center_vertical = 'no'
            frozen_headers = "yes"
            frozen_rowheaders = "1"
            index = 'yes'
            );

title1 font=arial color=blue height=9pt underlin=1 link="#'Index'!A1"  "Return to Table Index";
title2 "Table 1. Study Attrition";
footnote1 "Percentage is calculated as the number of patients qualifying at each step divided by the total number assessed";
%mkreport(data=sasdir.t01, sheet=Table 1. Attrition, rowhdr=Inclusion Criteria);

title2 "Table 2. Demographic Characteristics";
%mkreport(data=sasdir.t02, sheet=Table 2. Demographics, rowhdr=Demographic Characteristics);

title2 "Table 3. Comorbid Conditions";
footnote1 "Abbreviations: CCI = Charlson Coborbidity Index";
%mkreport(data=sasdir.t03, sheet=Table 3. Comorbid, rowhdr=Comorbid Conditions);

title2 "Table 4. Cox Proportional-Hazards Model for CCI as a Predictor of Mortality";
footnote1 "* Reference category is absence of the condition";
%mkreport(data=sasdir.t04, sheet=Table 4. Cox Model, rowhdr=Predictor);

ods excel close;
ods listing;


************************
***  END OF PROGRAM  ***
************************;
