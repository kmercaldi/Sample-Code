/*==========================================================================================
  ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

   Program          Macros.sas   
 
   Developer:       Katie Mercaldi                                                       

   Date:            24Jul2019                                                            

   Platform:        SAS 9.4 TS1M5 on X94_7PRO Platform (Windows)                         

   Description:     Contains all the macros needed to run the following programs:
                      00GETDATA.SAS                                                      
                      01GETCOHORT.SAS                                                    
                      02MKVARS.SAS                                                       
                      03TABLES.SAS                              
                                                                                        
                    Typically, each of these macros would be saved as a separate program
                    in folder designated for SAS macros and included as a macro autocall
                    library. Instead, they are combined here to keep the number of files 
                    small and will be compiled with an %INCLUDE statement.

                    Macros in this file:
                      %GETZIPFILE
                      %CMSIMPORT
                      %CCIFLAGS
                      %CCIWTS
                      %CCILBLS
                      %PULLCLAIMS
                      %COMPARECLAIMS
                      %INDENT
                      %ADDLABEL
                      %DESCSTATS
                      %RUNSTATS
                      %COXPH
                      %MKREPORT

   Macros Used:     None

   Input:           None

   Output:          None

~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
============================================================================================*/


************************************
***  START OF MACRO DEFINITIONS  ***
************************************;


*============================================================================================
  %GETZIPFILE: Downloads a zip file from a given URL and access a specific file within
               the zip file
 ============================================================================================;
%macro getzipfile(savedir   = &dldir,  /* Location to save the downloaded data      */
                  fileref   = ,        /* Fileref for the member in the zip file    */
                  url       = &cmsurl, /* URL where the data is downloaded from     */
                  zipfile   = ,        /* Zip file to download ending in .zip       */
                  zipmember =          /* Target file within the zip file (any ext) */
                  );

%*-------------------------------------------------------------------------------------------
       Assign a fileref for the zip file and download from the specified URL, then clearing
       the fileref for future use 
  -------------------------------------------------------------------------------------------;
       filename zipfile "&savedir\&fileref..zip";

       proc http method = 'get' url = "&url/&zipfile" out = zipfile;
            run;

       filename zipfile clear;

%*-------------------------------------------------------------------------------------------
       Use the ZIP access method to open the target file within the zip file 
  -------------------------------------------------------------------------------------------;
       filename &fileref zip "&savedir\&fileref..zip" member="&zipmember";

%mend getzipfile;



*============================================================================================
  %CMSIMPORT: Modifies the SAS programs written by CMS to import the synthetic data import 
              and runs the data conversion from the modified SAS programs
 ============================================================================================;
%macro cmsimport(savedir = &sasdir, /* Location to save the converted SAS data        */
                 inref   = ,        /* Fileref of original CMS SAS program            */
                 origref = ,        /* Fileref for extracted data in original program */
                 zipref  =          /* Fileref for directly accessing zip file member */
                 );

%*-------------------------------------------------------------------------------------------
       Assign temporary file to store the modified SAS code 
  -------------------------------------------------------------------------------------------;
       filename saspgm temp;

%*-------------------------------------------------------------------------------------------
       Get the text of the CMS SAS programs to convert synthetic data from CSV to SAS and
       make the following modifications:
  -------------------------------------------------------------------------------------------
        - Add the NOWARN option to all %SYMDEL statements
        - Assign the save directory
        - Change the raw data fileref to the zip fileref
        - Only import the first sample (of 20) and turn off macro to combine samples
  -------------------------------------------------------------------------------------------;
       data _null_;
            infile &inref truncover;
            input sascode $1000.;

            %* Flag %SYMDEL statements ;
            label flag_symdel = 'Find %SYMDEL statements';
            retain flag_symdel 0;
            if sascode =: '%symdel' then flag_symdel = 1; 

            %* Look for the semicolon to end the %SYMDEL statement, add NOWARN option and reset ;
            if flag_symdel = 1 and find(sascode,';') then do;
               sascode = tranwrd(sascode,';',' / nowarn;');
               flag_symdel = 0;
            end;

            %* Set up program to save data to the directory specified in the macro call ;
            if sascode = 'libname desynpuf " ";' then sascode = 'libname desynpuf "&savedir";';

            %* Replace the CMS-provided fileref with a fileref that uses the ZIP access method to
               eliminate the need to download/extract the data prior to running the program ;
            else if sascode =: 'filename' then sascode = catx(" ","/*",sascode,"*/");
            else if sascode =: 'infile' then sascode = tranwrd(sascode,"&origref","&zipref");

            %* Only import the first sample with sort option enabled and remove macro calls to 
               combine the sample data sets (since only one sample is imported) ;
            else if sascode =: '%desynpuf_' then do;
               if find(sascode,'_readin(filenumber=1,') then 
                  sascode = tranwrd(sascode,"sortds=no)","sortds=yes)/*");
               else if find(sascode,'_combine') then sascode = catx(" ","/*",sascode,"*/");
            end;

            %* Create a temporary file containing the modified SAS code ;
            file saspgm;
            put sascode;

            run;

%*-------------------------------------------------------------------------------------------
       Run the modified SAS programs to download and convert data directly from the CMS
       website and clear the SASPGM fileref for future use
  -------------------------------------------------------------------------------------------;
       %include saspgm / source2;
       filename saspgm clear;

%mend cmsimport;



*============================================================================================
  %CCIFLAGS: Creates flag variables for each of the 17 conditions of the CCI
 ============================================================================================;
%macro cciflags(icdvar = /* Name of the ICD-9 diagnosis code variable */);

       %do i = 1 %to 17;

           %* Get variable information for each condition ;
           %let dc = %upcase(&&dc&i);
           %let dis = %lowcase(&&dis&i);
           %let lbl = &&lbl&i;

           %* Assign label and search the ICD-9 dictionary for codes specified by Quan ;
           label flag_&dis = "&lbl flag";
           flag_&dis = (&icdvar in: (&dc));

       %end;

%mend cciflags;



*============================================================================================
  %CCIWTS: Assigns weights to each of the 17 conditions of the CCI as follows:
 --------------------------------------------------------------------------------------------
            - 6 points: Metastatic cancer and HIV/AIDS
            - 3 points: Moderate to severe liver disease
            - 2 points: Complicated diabetes, renal disease, non-metastatic cancer
            - 1 point:  All other conditions
 ============================================================================================;
%macro cciwts();

       %do i = 1 %to 17;

           %global wt&i;

           %* Get condition abbreviation given by Quan ;
           %let dis = %upcase(&&dis&i);

           %* Assign the weight based on the condition ;
           %if &dis in (HIV METS) %then %let wt&i = 6;
           %else %if &dis = MSLD %then %let wt&i = 3;
           %else %if &dis in (DIAB_C RD CANCER) %then %let wt&i = 2;
           %else %let wt&i = 1;

           %put Weight for &dis = &&wt&i;

       %end;

%mend cciwts;



*============================================================================================
  %CCILBLS: Labels patient-level CCI condition variables
 ============================================================================================;
%macro ccilbls();

    %do i = 1 %to 17;
        %let dis = %lowcase(&&dis&i);
        %if %index(&&lbl&i,AIDS) or %index(&&lbl&i,HIV) %then %let lbl = &&lbl&i;
        %else %let lbl = %bquote(%substr(&&lbl&i,1,1)%lowcase(%substr(&&lbl&i,2)));
        cci_&dis = "&lbl"
        wcci_&dis = "CCI weighted score &i: &lbl"
    %end;

%mend ccilbls;



*============================================================================================
  %PULLCLAIMS: Pulls claims for study patients and flag CCI conditions using HASH objects
 ============================================================================================;
%macro pullclaims(source = /* Source of claims (IP or OP) */);

%*-------------------------------------------------------------------------------------------
       Pull relevant claims during the baseline period for study patients using hash objects

       Note: Admitting diagnosis (ADMTNG_ICD9_DGNS_CD) on the inpatient file is not
             assessed, so some additional programming would be needed if it is included in
             variable definitions
  -------------------------------------------------------------------------------------------;
       data cohort_claims_&source (drop = code i temp:);
            set sasdir.de1_0_2008_to_2010_&source._sample_1
                (keep = desynpuf_id clm_thru_dt icd9_dgns_cd: 
                 where = (year(clm_thru_dt)=2008)); 
            by desynpuf_id;
              
            %* Initialize variables from the HASH tables ;
            if 0 then do;
               set sasdir.cohort (keep = desynpuf_id base_startdt base_stopdt);
               set cci_codes (keep = code flag_:);
            end;

            %* Set up the HASH objects ;
            if _n_ = 1 then do;

               %* HSELECT selects claims only for study cohort patient IDs ;
               declare hash hselect(dataset:"sasdir.cohort");
               hselect.defineKey("desynpuf_id");
               hselect.defineData("base_startdt","base_stopdt");
               hselect.defineDone();

               %* HCODES matches on ICD-9 codes ;
               declare hash hcodes(dataset:"sasdir.cci_codes");
               hcodes.defineKey("code");
               hcodes.defineData(&qflags);
               hcodes.defineDone();

            end;  /* of "if _n_ = 1 then do;" */

            %* Keep if the patient ID is found in HASH key list ;
            if hselect.find() = 0;

            %* Only keep claims during baseline period (which is Jan-Dec 2008
               for all patients here, but usually is variable) ;
            if base_startdt <= clm_thru_dt <= base_stopdt;

            %* Set up arrays: 
               DX      = Original diagnoses on the claims
               DXFLAGS = CCI condition flag variables
               TEMPDX  = Placeholder to save matches across all variables in the DX array ;
            array dx icd9_dgns_cd:;
            array dxflags{&nflags} &flags;
            array tempdx{&nflags};

            %* Search all diagnosis code variables ;
            do over dx;

               %* Resent the flags for each data key variable ;
               call missing(of dxflags{*});

               %* Find diagnosis key matches and set condition flags, retaining any previous 
                  matches ;
               if hcodes.find(key:dx) = 0 then do i = 1 to dim(dxflags);
                  tempdx{i} = max(dxflags{i},tempdx{i});
               end;

            end;  /* of "do over dx;" */

            %* Assign the final flag variable based on the temporary value that captured
               any match across the array of data keys ;
            do i = 1 to dim(dxflags);
               dxflags{i} = (tempdx{i} = 1);
            end;

            %* Keep claims with CCI diagnosis codes ;
            if sum(of dxflags{*});

            run;

%mend pullclaims;



*============================================================================================
   %COMPARECLAIMS: Pulls claims using more traditional code search for validation and
                   performance comparison against using HASH objects
 ============================================================================================;
%macro compareclaims(source = /* Source of claims (IP or OP) */);

%*-------------------------------------------------------------------------------------------
       Pull lists of ICD-9 diagnosis codes for each condition
  -------------------------------------------------------------------------------------------;
       proc sql noprint;
            %do i = 1 %to &nflags;
                %let dis = %lowcase(&&dis&i);
                select quote(code) into :icd&i separated by " " from cci_codes
                where flag_&dis = 1;
            %end;
            quit;

%*-------------------------------------------------------------------------------------------
       Pull relevant claims for study patients by selecting cohort IDs via merge(rather 
       than hash select) and searching ICD-9 codes on claims for CCI conditions
  -------------------------------------------------------------------------------------------;
       data test_claims_&source;
            merge cohort (in = c keep = desynpuf_id)
                  sasdir.de1_0_2008_to_2010_&source._sample_1 
                     (in = i 
                      keep = desynpuf_id clm_thru_dt icd9_dgns_cd:
                      where = (year(clm_thru_dt)=2008));
            by desynpuf_id;

            %* Only keep claims for cohort patients ;
            if c and i; 

            label %do i = 1 %to &nflags;
                      %let dis = %lowcase(&&dis&i);
                      flag_&dis = "&&lbl&i flag"
                  %end;
                  ;

            %* Set up arrays:
               DX = Original diagnoses on the claims
               DXFLAGS =  CCI condition flag variables ;
            array dx icd9_dgns_cd:;
            array dxflags{&nflags} &flags;

            %* Search claims for CCI condition codes ;
            do over dx;
               %do i = 1 %to &nflags;
                   %let dis = %lowcase(&&dis&i);
                   flag_&dis = max(flag_&dis,dx in (&&icd&i));
               %end;
            end;

            %* Keep claims with CCI diagnosis codes ;
            format flag_: yesno.;
            if sum(of dxflags{*});

            run;

%*-------------------------------------------------------------------------------------------
     Compare results with HASH methods
  -------------------------------------------------------------------------------------------;
    proc compare data=test_claims_&source compare=cohort_claims_&source;
         run;

%mend compareclaims;



*============================================================================================
   %INDENT: Applies the given number of indentations to the text immediately following
 ============================================================================================;
%macro indent(indent /* Desired levels of indentation */);

       %do indents = 1 %to &indent;
           'A0A0A0A0'x ||
       %end;

%mend indent;



*============================================================================================
   %ADDLABEL: Adds a descriptive row label to a table for a given row
 ============================================================================================;
%macro addlabel(data    = , /* Name of the table data set                                   */
                out     = , /* Name for the output data -- If blank will use DATA parameter */
                rownum  = , /* Row number to add label                                      */
                label   =   /* Text for the label to add                                    */
                );

       %* If no output data set name is given, use the original data set name ;
       %if &out = %str() %then %let out = &data;

       %* Assign special missing characters to order labels ;
       data test;
            set &data (keep = rownum statnum where = (rownum = &rownum));
            by rownum statnum;
            if first.rownum;

            %* Determine the first statistic value for the row -- If a label already exists
               then set the special missing character to one letter before ;
            statchar = strip(lowcase(put(statnum,4.)));
            if anyalpha(statchar) then call symputx("statnum",cats(".",byte(rank(statchar)-1)));

            %* Otherwise use .L to represent the label ;
            else call symputx("statnum",".l");

            run;

       %* Add the label to the table ;
       data &out (drop = i);
            set &data;
            by rownum statnum;
            output;
            array col $ col: pval:;

            %* Create the new label based on the first observation for the row and output ;
            if first.rownum and rownum = &rownum then do;
               statnum = &statnum;
               rowlabel = "&label";
               do i = 1 to dim(col);
                  col{i} = "";
               end;
               output;
            end;
            run;

       proc sort data=&out;
            by rownum statnum;
            run;

%mend addlabel;



*============================================================================================
  %DESCSTATS: Calculates descriptive statistics for these variable types:
 --------------------------------------------------------------------------------------------
               - Continuous:  Mean (SD) and median (range)
               - Categorical: N (%) for all categories
               - Indicator:   N (%) for "Yes" values
 ============================================================================================;
%macro descstats(data   = , /* Name of the analytic data set   */
                 colnum = , /* Numeric order for the column    */
                 coldef = , /* Column definition statement     */
                 collbl = , /* Label to display for the column */
                 rownum = , /* Numeric order for the row       */
                 rowvar = , /* Name of the row variable        */
                 rowlbl = , /* Label to display for the row    */
                 indent = 1 /* Desired indent for row labels   */
                 );

%*-------------------------------------------------------------------------------------------
       Get the row variable format and use to determine what type of statistics to calculate 
  -------------------------------------------------------------------------------------------;
       proc contents data = &data (keep = &rowvar)
                     out = rowfmt (keep = name format formatd 
                                   where = (upcase(name)="%upcase(&rowvar)")) 
                     noprint;
            run;

       %* Save row variable information as macro variables ;
       data _null_;
            set rowfmt;

            %*If format is missing then the variable is numeric so use COMMA format ;
            if format = "" then format = "COMMA";

            %* Save format as macro variable with "." and decimal places if present ;
            if formatd > 0 then call symputx("rowfmt",cats(format,".",formatd));
            else call symputx("rowfmt",cats(format,"."));

            %* For continuous variables use AUTODEC format to automatically determine best 
               number of decimal places for mean and SD (except for DOLLAR formats) ;
            if format in ("COMMA" "DOLLAR") then do;
               call symputx("rowtype","CONTINUOUS");
               if upcase(format) ne "DOLLAR" then call symputx("meanfmt","autodec.");
               else call symputx("meanfmt",cats(format,".",formatd));
            end;

            %* Indicator variables have YESNO format, and all others are considered as 
               categorical ;
            else if format = "YESNO" then call symputx("rowtype","INDICATOR");
            else call symputx("rowtype","CATEGORICAL");

            run;

%*-------------------------------------------------------------------------------------------
     Get number of non-missing observations for this column and row variable -- For
     indicator variables, only look at "Yes" and "No" values (not "Unknown")
  -------------------------------------------------------------------------------------------;
    proc sql noprint;
         select count(*) into :colcnt from &data where &coldef;
         select count(&rowvar) into :rowcnt from &data where &coldef 
         %if &rowtype = INDICATOR %then and &rowvar in (0 1);
         ;
         quit;

%*-------------------------------------------------------------------------------------------
     Calculate descriptive statistics for the row variable according to the row variable
     type (continuous or categorical) and column definition
  -------------------------------------------------------------------------------------------;
    %if &rowtype = CONTINUOUS %then %do;
        proc univariate data=&data noprint;
             where &coldef and &rowvar > .;
             var &rowvar; 
             output out=stats mean=mean std=std median=median min=min max=max;
             run;
    %end;

    %else %do;
        proc freq data=&data noprint;
             where &coldef and 
             %if &rowtype = INDICATOR %then &rowvar in (0 1);
             %else &rowvar > .;
             ;
             tables &rowvar / out=stats0 (keep=&rowvar count percent); 
             run;

        %* Include all possible values of variable ;
        proc format library = sasdir 
                    cntlout = rowvalues0 (keep = fmtname start label 
                                          where = (cats(fmtname,".")="&rowfmt" and 
                                                   label ^in: ("Missing", "Unknown")));
             run;

        data rowvalues;
             set rowvalues0;
             &rowvar = input(start,20.);
             run;

        %* Impute zeros for missing values ;
        data stats;
             merge stats0 (in=s)
                   rowvalues
                   ;
             by &rowvar;

             %* Only report "Yes" values for indicator variables (1 = "Yes") ;
             %if &rowtype = INDICATOR %then if &rowvar = 1;; 

             %* Assign zeros if the value is not present in the data ;
             if ^s then do;
                count = 0;
                percent = 0;
             end;

             run;
    %end;
        
%*-------------------------------------------------------------------------------------------
     Format the output statistics for the table
  -------------------------------------------------------------------------------------------;
    data col&colnum.row&rownum (keep = rownum statnum rowlabel col&colnum);
         set stats;
         label rownum     = "Row Number"
               statnum    = "Row Statistic"
               rowlabel   = "Row Label"
               col&colnum = "&collbl~N = %left(%qsysfunc(putn(&colcnt,comma20.)))"
               ;
         length rowlabel col&colnum $ 200;

         %* Row number ;
         rownum = &rownum;

         %* Formatting for non-indicator (continuous/categorical) variables ;
         %if &rowtype ne INDICATOR %then %do;

             %* Label for row variables (non-indicator) ;
             statnum    = .l;
             rowlabel   = %indent(%eval(&indent-1)) "&rowlbl" 
                          %if &rowtype = CATEGORICAL %then || ", n (%)";;
             col&colnum = "";
             %if &rowtype = CATEGORICAL %then if _n_ = 1 then;
             output;

             %* If the number of observations for the row variable is less than the total N 
                for the column, then report the number evaluated for this measure ;
             %if &colcnt > &rowcnt %then %do;
                 statnum    = 0;
                 rowlabel   = %indent(&indent) "N evaluated";
                 col&colnum = strip(put(&rowcnt,comma20.));
                 output;
             %end;

             %* Continuous variable statistics ;
             %if &rowtype = CONTINUOUS %then %do;

                 %* Mean (SD) ;
                 statnum    = 1;
                 rowlabel   = %indent(&indent) "Mean (SD)";
                 col&colnum = cat(strip(put(mean,&meanfmt))," (",
                                  strip(put(std,&meanfmt)),")");
                 output;

                 %* Median (range) ;
                 statnum    = 2;
                 rowlabel   = %indent(&indent) "Median (range)";
                 col&colnum = cat(strip(put(median,&rowfmt))," (",
                                  strip(put(min,&rowfmt))," - ",
                                  strip(put(max,&rowfmt)),")");
                 output;

             %end;

             %* Categorical variable statistics: N (%) ;
             %else %if &rowtype = CATEGORICAL %then %do;
                 statnum    = &rowvar;
                 rowlabel   = %indent(&indent) label;
                 col&colnum = cat(strip(put(count,comma20.))," (",
                                  strip(put(percent,autopct.)),")");
                 output;
             %end;

         %end; /* of "%if &rowtype ne INDICATOR %then %do;" */

         %* Indicator variable statistics: N (%) with denominator if less than column N ;
         %else %if &rowtype = INDICATOR %then %do;
             statnum  = 1;
             rowlabel = %indent(&indent) "&rowlbl";
             col&colnum = cat(strip(put(count,comma20.)),
                          %if &colcnt > &rowcnt %then " / ",strip(put(&rowcnt,comma20.)),;
                          " (",strip(put(percent,autopct.)),")");
             output;
         %end;

         run;

%*-------------------------------------------------------------------------------------------
     Delete intermediate data sets
  -------------------------------------------------------------------------------------------;
    proc datasets nolist;
         delete stats
                rowfmt
                %if &rowtype in (CATEGORICAL INDICATOR) %then 
                    stats0
                    rowvalues0
                    rowvalues
                    ;;
         run;
         quit;

%mend descstats;



*============================================================================================
  %RUNSTATS: Runs the %DESCSTATS macro for each combination of column definitions and row 
             variables
 ============================================================================================;
%macro runstats(data    = , /* Name of the analytic data set                     */
                out     = , /* Name of the output data set with table statistics */
                coldefs = , /* List of column definitions separated by spaces    */
                rowvars =   /* List of table row variables separated by spaces   */
                );

%*-------------------------------------------------------------------------------------------
       Get the number of column definitions and row variables given
  -------------------------------------------------------------------------------------------;
       %let ncoldefs = %sysfunc(countw(&coldefs)); %put COLUMNS = &ncoldefs;
       %let nrowvars = %sysfunc(countw(&rowvars)); %put ROWS = &ncoldefs;
    
%*-------------------------------------------------------------------------------------------
       Loop through each column definition
  -------------------------------------------------------------------------------------------;
       %do c = 1 %to &ncoldefs;

           %* Extract the column variable name and value of the variable from the column
              definition ;
           %let coldef = %scan(&coldefs,&c); %put COLUMN DEFINITION = &coldef;
           %let colvar = %scan(&coldef,1,=); %put COLUMN VARIABLE = &colvar;
           %let value = %scan(&coldef,-1,=); %put COLUMN VARIABLE VALUE = &value;

           %* Get the column variable format and assign the formatted value as the column
              label ;
           %let dsid = %sysfunc(open(&data,i));
           %let varnum = %sysfunc(varnum(&dsid,&colvar));
           %if &varnum > 0 %then %let colfmt = %sysfunc(varfmt(&dsid,&varnum));
           %let rc = %sysfunc(close(&dsid));
           %let collbl = %left(%qsysfunc(putn(&value,&colfmt)));

%*-------------------------------------------------------------------------------------------
           Loop through each row variable
  -------------------------------------------------------------------------------------------;
           %do r = 1 %to &nrowvars;

               %* Get the row variable label;
               %let rowvar = %scan(&rowvars,&r);
               %let dsid = %sysfunc(open(&data,i));
               %let varnum = %sysfunc(varnum(&dsid,&rowvar));
               %if &varnum > 0 %then %let rowlbl = %qsysfunc(varlabel(&dsid,&varnum));
               %let rc = %sysfunc(close(&dsid));

%*-------------------------------------------------------------------------------------------
               Run the macro to calculate the table statistics for this column/row 
               combination
  -------------------------------------------------------------------------------------------;
               %descstats(data   = &data, 
                          colnum = &c, 
                          coldef = &coldef, 
                          collbl = &collbl,
                          rownum = &r, 
                          rowvar = &rowvar, 
                          rowlbl = &rowlbl
                          );

           %end; /* of "%do r = 1 %to &rowvars;" */

%*-------------------------------------------------------------------------------------------
           Combine all rows for this column 
  -------------------------------------------------------------------------------------------;
           data col&c;
                set %do r = 1 %to &nrowvars;
                        col&c.row&r
                    %end;
                    ;
                run;

       %end;  /* of "%do c = 1 %to &coldefs;" */

%*-------------------------------------------------------------------------------------------
       Combine rows for all columns 
  -------------------------------------------------------------------------------------------;
       data &out;
            merge %do c = 1 %to &ncoldefs;
                      col&c
                  %end;
                  ;
            by rownum statnum rowlabel;
            run;

%*-------------------------------------------------------------------------------------------
       Delete individual row and column data
  -------------------------------------------------------------------------------------------;
       proc datasets nolist;
            delete %do c = 1 %to &ncoldefs;
                       col&c
                       %do r = 1 %to &nrowvars;
                           col&c.row&r
                       %end;
                   %end;
            by rownum statnum rowlabel;
            run;
            quit;

%mend runstats;


*============================================================================================
   %COXPH: Runs univariate Cox proportional-hazards regression and selects all covariates
           that modifies the hazard ratio for the predictor of interest by at least 10%
           for the final multivariate model
 --------------------------------------------------------------------------------------------
     Note: This macro requires the variable used in the parameter PREDICTOR to be
           dichotomous (2-levels) and all variables listed in the parameter COVLIST must be
           categorical or indicator variables
 ============================================================================================;
%macro coxph(data      = ,  /* Name of the analytic data set                             */
             out       = ,  /* Name of the output data set with table statistics         */
             outcome   = ,  /* Name of the outcome variable                              */
             censor    = 0, /* Value used for censoring                                  */
             time      = ,  /* Name for time to event/censoring variable                 */
             predictor = ,  /* Name for the predictor of interest -- Must be dichotomous */
             covlist   =    /* List of covariates to assess -- All must be categorical   */
             );

%*-------------------------------------------------------------------------------------------
       Create a version of the analytic data set with all formats removed to use the first
       unformatted value as the reference value
  -------------------------------------------------------------------------------------------;
       data temp;
            set &data;
            format _all_;
            run;

       %* Construct variable lists and count the number of model variables ;
       %let vars = &predictor &covlist;
       %let nvars = %sysfunc(countw(&vars));
       %let modelvars = &predictor;

%*-------------------------------------------------------------------------------------------
       Run Cox univariate and bivariate models for each predictor/covariate
  -------------------------------------------------------------------------------------------;
       %do i = 1 %to &nvars;
           %let var = %scan(&vars,&i);

           %* Cox univariate model ;
           proc phreg data=temp;  
                class &var / ref=first;
                model &time * &outcome(&censor) = &var / ties=efron rl;
                ods output ParameterEstimates = est0&i (keep = parameter classval0 hazardratio 
                                                               hrlowercl hruppercl probchisq);
                run;

           %* Get the univariate hazard ratio for the main predictor ;
           %if &var = &predictor %then %do;
               proc sql noprint;
                    select hazardratio into :predhr from est0&i;
                    quit;
           %end;

           %* Get the variable label and format ;
           %let dsid = %sysfunc(open(&data,i));
           %let varnum = %sysfunc(varnum(&dsid,&var));
           %if &varnum > 0 %then %do;
               %let varlbl = %sysfunc(varlabel(&dsid,&varnum));
               %let varfmt = %sysfunc(varfmt(&dsid,&varnum));
           %end;
           %let rc = %sysfunc(close(&dsid));

           %* Include all possible values of variable ;
           proc format library = sasdir 
                       cntlout = values0&i (keep  = fmtname start label 
                                            where = (cats(fmtname,".")="&varfmt" and 
                                                     label ^in: ("Missing", "Unknown")));
                run;

           data est&i (drop = classval0);
                set est0&i;
                statnum = input(classval0,20.);
                run;

           data values&i (keep = parameter statnum label);
                set values0&i;
                parameter = "&var";
                statnum = input(start,20.);
                run;

           %* Merge all variable values with model parameter estimates and format for output ;
           data stats&i (keep = parameter rownum statnum rowlabel col1 pval1);
                length rowlabel col1 $ 200
                       parameter pval1 $ 32
                       ;
                merge est&i (in=e)
                      values&i
                      ;
                by parameter statnum;

                label rownum   = "Row Number"
                      statnum  = "Statistic Number"
                      rowlabel = "Row Label"
                      col1     = "Univariate HR (95% CI)"
                      pval1    = "P-value"
                      ;

                %* Row number ;
                rownum = &i;

                %* Only report "Yes" values for indicator variables ;
                %if %upcase(&varfmt) = YESNO. %then %do;
                    if statnum = 1;
                    rowlabel = %indent(1) "&varlbl";
                %end;

                %* For other categorical variables, set the value without an estimate
                   as the reference value ;
                %else %do;
                    rowlabel = %indent(1) strip(label);
                    if ^e then col1 = "1.0 (reference)";
                    else
                %end;

                %* Format the final statistics and output ;
                col1 = cat(strip(put(hazardratio,comma20.2))," (",
                           strip(put(hrlowercl,comma20.2))," - ",
                           strip(put(hruppercl,comma20.2)),")");
                if probchisq > . then pval1 = put(probchisq,pvalfmt.);
                output;

                %* Add a label for non-indicator variables ;
                %if %upcase(&varfmt) ne YESNO. %then %do;
                    if _n_ = 1 then do;
                       statnum = .l;
                       rowlabel = "&varlbl";
                       col1 = "";
                       pval1 = "";
                       output;
                    end;
                %end;

                run;

           proc sort data=stats&i;
                by rownum statnum;
                run;

           %if &var ne &predictor %then %do;

              %* Cox bivariate models for measuring impact of each covariate on the 
                 association between the outcome and key predictor variable ;
              proc phreg data=temp;  
                    class &predictor &var / ref=first;
                    model &time * &outcome(&censor) = &predictor &var / ties=efron rl;
                    ods output ParameterEstimates = bivest&i (keep = parameter hazardratio);
                    run;

               data _null_;
                    set bivest&i (where = (parameter="&predictor"));

                    %* Include the covariate in the final model if it modifies the univariate
                       hazard ratio for the key predictor by at least 10% ;
                    if abs(&predhr-hazardratio) / &predhr >= 0.1 
                       then call symputx("modelvars","&modelvars &var");

                    run;

           %end;
       %end;

       %* Combine results for each of the univariate models for the final table ;
       data stats;
            set %do i = 1 %to &nvars;
                    stats&i
                %end;
                ;
            by rownum statnum;
            run;

%*-------------------------------------------------------------------------------------------
       Final Cox multivariate model
  -------------------------------------------------------------------------------------------;
       proc phreg data=temp;  
            class &modelvars / ref=first;
            model &time * &outcome(&censor) = &modelvars / ties=efron rl;
            ods output ParameterEstimates = est (keep = parameter classval0 hazardratio 
                                                        hrlowercl hruppercl probchisq);
            run;

       %* Format the multivariate model estimates for output ;
       data mvstats;
            length col2 $ 200
                   parameter pval2 $ 32
                   ;
            set est;
            label rownum   = "Row Number"
                  statnum  = "Statistic Number"
                  col2     = "Multivariate HR (95% CI)"
                  pval2    = "P-value"
                  ;
            statnum = input(classval0,20.);
            col2 = cat(strip(put(hazardratio,comma20.2))," (",
                       strip(put(hrlowercl,comma20.2))," - ",
                       strip(put(hruppercl,comma20.2)),")");
            if probchisq > . then pval2 = put(probchisq,pvalfmt.);
            run;

       %* Combine the univariate and multivariate model estimates ;
       proc sort data=mvstats;
            by parameter statnum;
            run;

       proc sort data=stats;
            by parameter statnum;
            run;

       data &out (keep = rownum statnum rowlabel col: pval:);
            merge stats
                  mvstats
                  ;
            by parameter statnum;

            %* Create a quoted list of final model variables to determine which
               rows to mark as reference values ;
            %let qmodelvars = %str();
            %let nmodelvars = %sysfunc(countw(&modelvars));
            %do i = 1 %to &nmodelvars;
                %let modelvar = %scan(&modelvars,&i);
                %let qmodelvars = &qmodelvars "&modelvar";
            %end;
            if parameter in (&qmodelvars) and statnum ne .l and col2 = "" 
               then col2 = "1.0 (reference)";

            run;

%*-------------------------------------------------------------------------------------------
       Final output table
  -------------------------------------------------------------------------------------------;
       proc sort data=&out;
            by rownum statnum;
            run;

%*-------------------------------------------------------------------------------------------
       Delete intermediate data sets
  -------------------------------------------------------------------------------------------;
       proc datasets nolist;
            delete temp
                   %do i = 1 %to &nvars;
                       pval&i
                       est0&i
                       est&i
                       values0&i
                       values&i
                       stats&i
                       bivest&i
                   %end;
                   ;
            run;
            quit;

%mend coxph;



*============================================================================================
   %MKREPORT: Generates PROC REPORT syntax to print tables
 ============================================================================================;
%macro mkreport(odstype = excel, /* ODS output destination -- EXCEL or TAGSET.EXCELXP */
                data    = ,      /* Name of the data set containing the table text    */
                sheet   = ,      /* Excel worksheet name for the table                */
                rowhdr  = ,      /* Column header text for row labels                 */
                boldrow = all    /* Indentation level for bold row labels             */
                );

       %* Separate the directory and data set names ;
       %if %index(&data,.) %then %do;
           %let dir = %upcase(%scan(&data,1,.));
           %let mem = %upcase(%scan(&data,-1,.));
       %end;
       %else %do;
           %let dir = WORK;
           %let mem = %upcase(&data);
       %end;

       %* Replace >= and <= with unicode characters in the row label column ;
       data temp;
            set &data;
            rowlabel = tranwrd(rowlabel,"<=","^{unicode '2264'x}");
            rowlabel = tranwrd(rowlabel,">=","^{unicode '2265'x}");
            run;

%*-------------------------------------------------------------------------------------------
       Get column information 
  -------------------------------------------------------------------------------------------;
       proc sql noprint;

            %* Column names to include in the report ;
            select name into :col1- from sashelp.vcolumn
            where upcase(libname) = "&dir" and upcase(memname) = "&mem" and 
                  (upcase(name) = 'ROWLABEL' or upcase(name) like 'COL%' or 
                   upcase(name) like 'PVAL%')
            order by varnum;
            %let ncol = &sqlobs;

            %* Determine maximum column lengths for row label column, statistics columns 
               and p-value colums ;
            %let clen = 0;
            %let plen = 0;
            %let rowht = 15;
            %do i = 1 %to &ncol;
                %let col = &&col&i;
                select max(length(&col)) into :len&i from &data;
                %if %upcase(&col) = ROWLABEL %then %let rlen = &&len&i;
                %else %if %upcase(%substr(&col,1,3)) = COL 
                    %then %let clen = %sysfunc(max(&clen,&&len&i));
                %else %if %upcase(%substr(&col,1,4)) = PVAL 
                    %then %let plen = %sysfunc(max(&plen,&&len&i));
            %end;
            %* Set maximum length for row label column to 90 characters 
               (additional text will be on next line);
            %if &rlen > 90 %then %do;
                %let rlen = 90;
                %let rowht = 0;
            %end;
            quit;

       %put ROW LABEL LENGTH = &rlen;
       %put STAT LENGTH = &clen;
       %put P LENGTH = &plen;


       %* Set the sheet name and row type based on the ODS destination ;
       ods &odstype options(sheet_name = "&sheet"
                            row_heights = "0,&rowht,0,0,0,0,0"
                            title_footnote_width = "&ncol"
                            );

%*-------------------------------------------------------------------------------------------
       Create the table report
  -------------------------------------------------------------------------------------------;
       proc report data = temp
                   nowindows split = "~"
                   missing
                   style(header) = {font_weight=bold 
                                    font_size=10pt 
                                    just=center 
                                    vjust=m
                                    protectspecialchars=off}
                   style(column) = {font_size=10pt 
                                    just=center 
                                    vjust=m}
                                    ;
            column rownum
                   %do i = 1 %to &ncol;
                       &&col&i
                   %end;
                   c;
                                   
            %* Column definitions ;
            define rownum  / order noprint;
            %do i = 1 %to &ncol;

                %* Assign display, width and allignment for each column ;
                %let col = &&col&i;
                %if %upcase(&col) = ROWLABEL %then %do;
                    define &col / display "&rowhdr" style(column)={cellwidth=&rlen.em just=left};
                %end;
                %else %do;
                    %if %upcase(%substr(&col,1,3)) = COL %then %let len = %sysevalf(&clen*1.5);
                    %else %if %upcase(%substr(&col,1,4)) = PVAL %then %let len = %sysevalf(&plen*1.5);
                    define &col / display style={cellwidth=&len.em};
                %end;

            %end;
            define c / noprint;

            compute c;

               %* Bold all non-indented rows of attrition table ;
               %if %index(%upcase(&sheet),ATTRITION) and 
                   (%upcase(&boldrow) = ALL or &boldrow > 0) %then %do;
                   if index(rowlabel,'A0A0A0A0'x) ne 1 
                      then call define(_row_, "style", "style=[font_weight=bold]");
               %end;

               %* Bold row labels at the requested indent level ;
               if %if %upcase(&boldrow) ne ALL and &boldrow > 0 %then %do;
                      index(rowlabel, %do i = 1 %to &boldrow;
                                         'A0A0A0A0'x %if &i ne &boldrow %then ||;
                                      %end;
                                      ) ne 1 and
                  %end;
                  %if %upcase(&boldrow) = ALL or &boldrow > 0 %then %do;
                      col1 = "" then call define(_row_, "style", "style=[font_weight=bold]");
                  %end;

               %* If using TAGSET.EXCELXP, use TAGATTR to preserve cell formatting ;
               %if %upcase(&odstype) = TAGSET.EXCELXP %then %do;
                   if find(rowlabel,", N") and col1 ne "" then do;
                      %do i = 1 %to &ncol;
                          call define("col&i", "style", "style=[tagattr='format:#,##0']"); 
                      %end;
                   end;
               %end;

            endcomp;
             
            run;

%*-------------------------------------------------------------------------------------------
       Clear all titles and footnotes and delete intermediate data sets
  -------------------------------------------------------------------------------------------;
       proc datasets nolist;
            delete temp;
            run;
            quit;

       title2;
       footnote;

%mend mkreport;


**********************************
***  END OF MACRO DEFINITIONS  ***
**********************************;
