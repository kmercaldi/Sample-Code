/*=============================================================================================
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

   Program:         Autoexec.sas
  
   Developer:       Katie Mercaldi
  
   Date:            24Jul2019
  
   Platform:        SAS 9.4 TS1M5 on X94_7PRO Platform (Windows)
  
   Description:     Automatically executed code to run at the beginning of each program:
                     - Set desired options
                     - Assign global macro variables
                     - Assign locations for data, formats, and macros
  
   Macros Used:     None
  
   Input:           None
    
   Output:          None

~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
===============================================================================================*/


**************************
***  START OF PROGRAM  ***
**************************;


* Set options ;
options nocenter nofmterr pageno=1 msglevel=i mergenoby=warn minoperator nodate mprint;

* Project global macro variables ;
%symdel cmsurl icdurl cciurl dldir sasdir / nowarn;

%global cmsurl /* URL to access the CMS synthetic data for download                          */
        icdurl /* URL to access the CMS ICD-9-CM code dictionary                             */
        cciurl /* URL to access the SAS code with ICD-9 diagnosis codes for CCI used by Quan */
        dldir  /* Location for saving downloaded zip files                                   */
        sasdir /* Location for saving SAS data                                               */
        ;

%let cmsurl = https://www.cms.gov/Research-Statistics-Data-and-Systems/Downloadable-Public-Use-Files/SynPUFs/Downloads;
%let icdurl = https://www.cms.gov/Medicare/Coding/ICD9ProviderDiagnosticCodes/Downloads;
%let cciurl = http://mchp-appserv.cpe.umanitoba.ca/Upload/SAS;

* Make all files temporary by saving to the WORK directory ;
%let dldir  = %qsysfunc(getoption(work));
%let sasdir = &dldir;

%put _user_;

* Library for SAS data and formats ;
libname sasdir "&sasdir";

* Program location ;
data _null_;
     if "%sysfunc(getoption(sysin))" = "" then do;
        fullpath = "%sysget(SAS_EXECFILEPATH)";
        filename = "%sysget(SAS_EXECFILENAME)";
        pathlen = length(fullpath) - length(filename);
        path = substr(fullpath,1,pathlen);
        call symputx("pgmloc",path,"g");
     end;
     run;

* Project formats and macros ;
%include "&pgmloc\Formats.sas";
%include "&pgmloc\Macros.sas";

* Title for listing output ;
title "Output for Sample Code written by Katie Mercaldi";


************************
***  END OF PROGRAM  ***
************************;
