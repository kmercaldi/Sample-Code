/*==========================================================================================
  ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

   Program          Formats.sas   
 
   Developer:       Katie Mercaldi                                                       

   Date:            24Jul2019                                                            

   Platform:        SAS 9.4 TS1M5 on X94_7PRO Platform (Windows)                         

   Description:     Creates a format library with all study formats

   Macros Used:     None

   Input:           None

   Output:          None

~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
============================================================================================*/


**************************
***  START OF PROGRAM  ***
**************************;


*============================================================================================
  Study formats saved to the SASDIR library
 ============================================================================================;
proc format library=sasdir;

     * Format for all patients column ;
     value all 1 = "All Patients"
               ;

     * Yes/No indicator ;
     value yesno 1 = "Yes"
                 0 = "No"
                 9 = "Unknown"
                 ;

     * Claim source ;
     value clmsrc 1 = "Inpatient"
                  2 = "Outpatient"
                  ;

     * Patient status ;
     value status 0 = "Survived"
                  1 = "Died"
                  ;

     * Age categories ;
     value agecat 1 = "65-69"
                  2 = "70-74"
                  3 = "75-79"
                  4 = ">=80"
                  ;

     * Gender ;
     value gender 1 = "Male"
                  2 = "Female"
                  9 = "Unknown"
                  ;

     * Race ;
     value race 1 = "White"
                2 = "Black"
                3 = "Hispanic"
                9 = "Other/Unknown"
                ;

     * CCI categories ;
     value ccicat 0 = "0"
                  1 = "1"
                  2 = "2"
                  3 = "3 or more"
                  ;

     * CCI binary categories ;
     value ccifmt 1 = "<3"
                  2 = ">=3"
                  ;

     * Percentage format with automatically determined decimal places ;
     picture autopct (round)
             . = ""
             low      - -99.95  = "0,000,000,000,000,009%" (prefix='-')
             -99.95<  -  -0.095 = "09.9%" (prefix='-')
              -0.095< -  -0.01  = "9.99%" (prefix='-')
              -0.01< -   <0     = "-<0.01%" (noedit)
               0                = "0%" (noedit)
               0<    -   <0.01  = "<0.01%" (noedit)
               0.01  -   <0.095 = "9.99%"
               0.095 -  <99.95  = "09.9%" 
              99.95  -  high    = "0,000,000,000,000,009%" (mult=1)
              ;

     * Decimal format with automatically determined decimal places ;
     picture autodec (round)
             . = ""
             low        - -99.95    = "0,000,000,000,000,009" (prefix='-')
             -99.95<    -  -0.095   = "09.9" (prefix='-')
              -0.095<   -  -0.0095  = "9.99" (prefix='-')
              -0.0095<  -  -0.00095 = "9.999" (prefix='-')
              -0.00095< -  -0.0001  = "9.9999" (prefix='-')
              -0.0001<  -  <0       = "-<0.0001" (noedit)
               0                    = "0" (noedit)
               0<       -  <0.0001  = "<0.0001" (noedit)
               0.0001   -  <0.00095 = "9.9999"
               0.00095  -  <0.0095  = "9.999"
               0.0095   -  <0.095   = "9.99"
               0.095    - <99.95    = "09.9"
              99.95     - high      = "0,000,000,000,000,009"
              ;

     * P-value format ;
     picture pvalfmt (round)
             . = ""
             low-<0.0001 = "<0.0001" (noedit)
             0.0001-<0.000995 = "9.9999"
             0.000995-<0.00995 = "9.999"
             0.00995-<0.995 = "9.99"
             0.995-1.0 = "1.00" (noedit)
             ;

     run;


************************
***  END OF PROGRAM  ***
************************;
