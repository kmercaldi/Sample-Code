/*==========================================================================================
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

   Program:         00GetData.sas
  
   Developer:       Katie Mercaldi
  
   Date:            24Jul2019
  
   Platform:        SAS 9.4 TS1M5 on X94_7PRO Platform (Windows)
  
   Description:     The Centers for Medicare & Medicaid Services (CMS) offers publicly
                    available synthetic claims data referred to as CMS 2008-2010 Data 
                    Entrepreneurs’ Synthetic Public Use File (DE-SynPUF). The data is
                    cut into 20 different samples, which can be downloaded and combined,
                    if desired, from the CMS website. Additional information on the data
                    source is available at:
  
                    https://www.cms.gov/Research-Statistics-Data-and-Systems/Downloadable-
                    Public-Use-Files/SynPUFs/DE_Syn_PUF.html
  
                    This program is designed to be run sequentially with these
                    additional programs:
  
                      00GETDATA.SAS <-- Current program
                      01GETCOHORT.SAS
                      02MKVARS.SAS
                      03TABLES.SAS
  
                    The first sample will be used for program demonstration. The current
                    program completes the following steps:
  
                      1. Download the zip files from the CMS website containing the
                         synthetic data in CSV format, SAS conversion programs and 
                         ICD-9-CM diagnosis code list in CSV format
  
                      2. Modify and then run the CMS SAS programs to import the CSV data
  
                      3. Convert the ICD-9-CM code list from TXT to SAS7BDAT format
  
   Macros Used:     All macros available in MACROS.SAS. This program uses:
                      %GETZIPFILE.SAS
                      %CMSIMPORT.SAS
  
   Input:           2008-2010 CMS synthetic beneficiary, inpatient and outpatient data
                    in CSV format:
                      DE1_0_2008_Beneficiary_Summary_File_Sample_1.CSV
                      DE1_0_2009_Beneficiary_Summary_File_Sample_1.CSV
                      DE1_0_2010_Beneficiary_Summary_File_Sample_1.CSV
                      DE1_0_2008_to_2010_Inpatient_Claims_Sample_1.CSV
                      DE1_0_2008_to_2010_Outpatient_Claims_Sample_1.CSV

                    Programs written by CMS to convert the CSV files to SAS format:
                      DESynPUF_BENE_READIN_20121211.SAS
                      DESynPUF_IP_READIN_20121211.SAS
                      DESynPUF_OP_READIN_20121211.SAS

                    CMS ICD-9-CM code dictionary in TXT format:
                      CMS32_DESC_LONG_DX.TXT
    
   Output:          2008-2010 CMS synthetic beneficiary, inpatient and outpatient data
                    in SAS format:
                      SASDIR.DE1_0_2008_Beneficiary_Summary_File_Sample_1
                      SASDIR.DE1_0_2009_Beneficiary_Summary_File_Sample_1
                      SASDIR.DE1_0_2010_Beneficiary_Summary_File_Sample_1
                      SASDIR.DE1_0_2008_to_2010_Inpatient_Claims_Sample_1
                      SASDIR.DE1_0_2008_to_2010_Outpatient_Claims_Sample_1
  
                    CMS ICD-9-CM code dictionary in SAS format:
                      SASDIR.ICD9DX

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
  STEP 1. Download the zip files from the CMS website and access needed files
 ============================================================================================;

* CMS synthetic data files in CSV format ;
%getzipfile(fileref=ben2008, zipfile=DE1_0_2008_Beneficiary_Summary_File_Sample_1.zip,  zipmember=DE1_0_2008_Beneficiary_Summary_File_Sample_1.csv)
%getzipfile(fileref=ben2009, zipfile=DE1_0_2009_Beneficiary_Summary_File_Sample_1.zip,  zipmember=DE1_0_2009_Beneficiary_Summary_File_Sample_1.csv)
%getzipfile(fileref=ben2010, zipfile=DE1_0_2010_Beneficiary_Summary_File_Sample_1.zip,  zipmember=DE1_0_2010_Beneficiary_Summary_File_Sample_1.csv)
%getzipfile(fileref=ip,      zipfile=DE1_0_2008_to_2010_Inpatient_Claims_Sample_1.zip,  zipmember=DE1_0_2008_to_2010_Inpatient_Claims_Sample_1.csv)
%getzipfile(fileref=op,      zipfile=DE1_0_2008_to_2010_Outpatient_Claims_Sample_1.zip, zipmember=DE1_0_2008_to_2010_Outpatient_Claims_Sample_1.csv)

* CMS SAS programs to convert the data from CSV to SAS format ;
%getzipfile(fileref=benpgm, zipfile=Bene_SAS.zip,       zipmember=DESynPUF_BENE_READIN_20121211.sas)
%getzipfile(fileref=ippgm,  zipfile=Inpatient_SAS.zip,  zipmember=DESynPUF_IP_READIN_20121211.sas)
%getzipfile(fileref=oppgm,  zipfile=Outpatient_SAS.zip, zipmember=DESynPUF_OP_READIN_20121211.sas)

* CMS ICD-9-CM code dictionary ;
%getzipfile(fileref=icd, url=&icdurl, zipfile=ICD-9-CM-v32-master-descriptions.zip, zipmember=CMS32_DESC_LONG_DX.txt)


*============================================================================================
  STEP 2. The %IMPORT macro modifies the CMS SAS programs to import the CSV data by
          making the below changes and then runs the programs to get the data
 --------------------------------------------------------------------------------------------
           - Assigns the save directory for the converted SAS data

           - Replaces the CMS-provided fileref, which assumes that the CSV data have already 
             been downloaded and extracted from the zip file, with a fileref that uses the 
             ZIP access method to eliminate the need to download/extract the data prior to 
             running the program

           - Sets up to only import the first sample (of 20) by commenting out macro calls to 
             import samples 2-20 and the macro to combine data from different samples
  
           - Creates a temporary file containing the modified SAS code
 ============================================================================================;

* Convert the synthetic beneficiary, inpatient and outpatient data from CSV to SAS format ;
%cmsimport(inref=benpgm, origref=inbene, zipref=%nrstr(ben&y))
%cmsimport(inref=ippgm,  origref=inip,   zipref=ip)
%cmsimport(inref=oppgm,  origref=inop,   zipref=op)

* Turn off compression option set by CMS programs ;
options compress=no;


*============================================================================================
  STEP 3: Convert the ICD-9-CM code list from TXT to SAS7BDAT format
 ============================================================================================;
data sasdir.icd9dx;
     infile icd dlm='09'X dsd truncover;
     input code $5. desc $250.;
     label code = "ICD-9 diagnosis code"
           desc = "Description"
           ;
     run;


*============================================================================================
  Clear filenames and global macro variables
 ============================================================================================;
filename _all_ clear;
%symdel dldir sasdir cmsurl icdurl / nowarn;


************************
***  END OF PROGRAM  ***
************************;
