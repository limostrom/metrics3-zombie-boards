/** program reads in data from Execucomp and calculates        **/ 
/** delta, vega, and equity portfolio value for all executives **/


options nolabel nocenter ls=80 errors=2;

libname  common     "c:\lalitha\execucomp\";


proc datasets kill;

*==================================================================;
* READING ANNCOMP DATASET - ANNUAL EXEC-LEVEL AGGREGATE COMPENSATION;
*===================================================================;
* this dataset has information on all execs, all years;
* should have no duplicates by coperol-year;

data anncomp;
  set common.anncomp ;

* data was pulled as of Nov 2011 so we have only few observations for 2011 fiscal in execucomp;

where year<2011;

 coperol=co_per_rol;
 shrown=SHROWN_EXCL_OPTS;
 shrowntotal=SHROWN_TOT;
 opts_unvested_num=OPT_UNEX_UNEXER_NUM;
 opts_unvested_val=OPT_UNEX_UNEXER_EST_VAL;
 opts_vested_num=OPT_UNEX_EXER_NUM;
 opts_vested_val=OPT_UNEX_EXER_EST_VAL;
 opts_exercised_num= OPT_EXER_NUM;

* see chk statements below - shrown_excl_opts is missing for some firms even though
 we have shrown_tot.  But shrown_tot cannot be used even if shrown_excl_opts is missing because 
 when both are there for a record, they are not very correlated;

keep gvkey coperol year execid allothpd allothtot bonus 
eip_unearn_num eip_unearn_val noneq_incent old_datafmt_flag
 OPTION_AWARDS_BLK_VALUE 
OPTION_AWARDS_FV OPTION_AWARDS_NUM OPTION_AWARDS_RPT_VALUE 
OPTION_AWARDS  opts_exercised_num
 opts_vested_val 
 opts_vested_num opts_unvested_val
opts_unvested_num  OTHANN OTHCOMP  
 RSTKGRNT   SALARY   
 SHROWN  shrowntotal  SHRS_VEST_NUM 
 SHRS_VEST_VAL  STOCK_AWARDS_FV  STOCK_AWARDS 
 STOCK_UNVEST_NUM  STOCK_UNVEST_VAL 
  TDC1  TITLEANN ltip ceoann defer_rpt_as_comp_tot;
proc sort nodupkey;
 by coperol year;
run;


proc print data=anncomp (obs=100);
 var coperol year shrown tdc1;
 where coperol=2611;
 run;

/** coperol 16285 is palmisano of ibm - printing this out as a test case;

proc print data=anncomp;
 where coperol=16285;
 var year old_datafmt_flag tdc1 OPTION_AWARDS_BLK_VALUE 
OPTION_AWARDS_FV OPTION_AWARDS_NUM 
  opts_exercised_num opts_vested_val 
 opts_vested_num opts_unvested_val
opts_unvested_num  shrown shrs_vest_num
STOCK_AWARDS_FV   STOCK_UNVEST_NUM  STOCK_UNVEST_VAL ;
 title "summary stats for ibm palmisano";
 run;

**/

/**
* tdc1old is the definition of how execucomp used to calculate tdc1;
* post 2006, execucomp calculates tdc1 using tdc1new (although they continue to call it TDC1);
* these are based on information frm s&p;
* also, we verified that these give same values as tdc1;
* OPTION_AWARDS and STOCK_AWARDS represent the expensed portion of total stock and option
  awards made to the CEO including stock and option awards from previous years (per FAS 123R);
*  we are dropping this going forward as they do not represnet the true annual compensation; 
* manual talks about two additional variables, OPT_UNEX_UNEXER and OPT_UNEX_EXER which represent
 the number of securities underlying the exerciseable and unexercisable options;
* but these variables do not show up in the Execucomp database;


data chk1;
 set anncomp;
 tdc1old=salary+bonus+othann+rstkgrnt+ltip+allothtot
        +option_awards_blk_value;
  tdc1new=salary+bonus+noneq_incent+option_awards_fv 
            + stock_awards_fv + defer_rpt_as_comp_tot + othcomp;
difftdc1new=tdc1new-tdc1;
difftdc1old=tdc1old-tdc1;
run;

 * below, we find that mean, mix, max and median are all zero, which means we have the right formula for tdc1old and tdc1new above;

proc means data=chk1 n mean median min max;
 var difftdc1old difftdc1new;
 title "difftdc1old and difftdc1new should be zero";
run;

**/

/**
* the chk below is to understand the old_datafmt_flag - this should be 0 post 2006;

data chk2;
 set chk1;
 proc sort nodupkey;
  by gvkey year;
 proc sort ;
 by year;
proc means noprint data=chk2;
 by year;
  var old_datafmt_flag ;
  output out=out1 mean= old_datafmt_flag ;
  run;

  proc print data=out1;
  title "all have oldformat=1 upto 2005, 15% have oldformat=1 in 2006, 
  and all have oldformat=0 from 2007";
  run;
**/

*=================================================;
* READING PERSON DATASET - EXEC-LEVEL INFORMATION;
*=================================================;

/** this dataset has information on executive name, gender, and age;
* for our program here we only need the fullname so that we can check against proxy statements;
* if necessary to confirm our understanding ;
* should have no duplicates by execid;**/

data person;
 set common.person;
 keep execid exec_fullname ;
 proc sort nodupkey;
  by execid;
* proc contents ;
* title "information on dataset person";
run;

  proc sql;
   create table anncomp1
    as select *
	from anncomp x left join person y 
	on x.execid=y.execid;

proc sort nodupkey data=anncomp1;
 by coperol year;
 run;

* ===========================;
* READING IN CODIRFIN DATASET;
* ===========================;
* this dataset contains firm level information including aggregate director
* information. So should not have duplicates by year;
* data available from 1992-2010;
* the bs_volatility and bs_yield were used by execucomp to calculate the BS values
 before 2007.  After that they stopped calculating the BS values and started reporting the firm's estimates directly;
* therefore they stopped reporting the bs_yield and the bs_volatility also after 2007;
* we calculate the BS_values for the sake of consistency in the same manner before and after 2006.  We therefore
 calculate the dividend yield and volatility using the same approach that Execucomp adopted before 2007;
* the dividend yield that goes into the BS value is the average of the last three years dividend yield 
 and the volatility that goes into the BS value is based on estimates over 60 month rolling windows - see below;

data codirfin;
 set common.codirfin (keep = gvkey year bs_volatility bs_yield divyield shrsout prccf fyr srcdate);
  execucomp_volatility = bs_volatility;

* given as % but it is used as fraction in formula;
  execucomp_yield=bs_yield/100;

* found a mistake with dividend yield - for 2007, the dividend includes special dividend of $25 
  for Alberto Culver, GVKEY=001239.  The annual report indicates that the dividend yield used for
  B-S (fair value) calculation is 1%. Execucomp reports the dividend yield as 0.99%,0.96%
  and 101%. The last should actually be 0.66% ( =0.165/24.79). Using this, we get the dividend yield
  of 0.82% (average of last 3 years to be used in B-S, which gives us a more realistic calculated
  B_S value of $1,011 (in the ballpark of $920 reported by firm);

  if gvkey="001239" and year=2007 then divyield=0.66;

* dividend yield should be a fraction for B_S calculation;
  divyield=divyield/100;

  keep gvkey year divyield execucomp_volatility execucomp_yield prccf shrsout srcdate fyr;
proc sort nodupkey;
  by gvkey year;
 *proc contents data=common.codirfin;
 *  title "information on dataset codirfin";
 run;


 proc sort data=codirfin;
  by gvkey year;
data codirfin;
 set codirfin;
  lgvkey=lag(gvkey);
  lyear=lag(year)+1;
  l2gvkey=lag2(gvkey);
  l2year=lag2(year)+2;
  ldivyield=lag(divyield);
  l2divyield=lag2(divyield);
  if gvkey^=lgvkey or year^=lyear then ldivyield=.;
  if gvkey^=l2gvkey or year^=l2year then l2divyield=.;
  if divyield^=. and ldivyield^=. and l2divyield^=. 
  then estimated_yield= (divyield+ldivyield+l2divyield)/3;
  if divyield^=. and ldivyield^=. and l2divyield=. 
  then estimated_yield= (divyield+ldivyield)/2;
  if divyield^=. and ldivyield=. and l2divyield=. 
  then estimated_yield= divyield;
  if divyield=. and ldivyield=. and l2divyield=. 
  then estimated_yield=.;
  drop ldivyield l2divyield lgvkey l2gvkey lyear l2year;
run;

/**
proc print data=codirfin;
 where gvkey="006066";
 title "print of codirfin for ibm";
 run;
 **/

* ===========================;
* READING IN COMBINED DATASET;
* ===========================;
* going to read in combined_201111_1_for_wrds. This was taken from 
	combined crsp-compustat data from wrds and has fiscal year end date which we need later (in next step);
* going to merge with codirfin because this dataset has only 1 obs per firm year;

data combined;
 set common.commondata_201111_1(keep=gvkey permno year fybegdt fyenddt);
  where year>=1992;
 proc sort nodupkey;
  by gvkey year;
  run;

  proc means data=combined;
  title "combined crsp-compustat data from wrds";
  run;


proc sql;
 create table codirfin1
   as select *
   from codirfin x left join combined y 
	on  x.gvkey=y.gvkey and x.year=y.year;

* should have no duplicates by gvkey-year;
proc sort nodupkey data=codirfin1;
 by gvkey year;
 run;


* ==========================;
* SUBSET OF ANNCOMP DATASET ;
* ==========================;
/** do not need all of these variables beyond this point - we will only keep part of them and 
them when we are all done we can merge back the rest of the variables;
This makes the program faster;**/

data anncomp2;
 set anncomp1;
keep gvkey coperol year exec_fullname
 old_datafmt_flag option_awards_blk_value
option_awards_num opts_exercised_num
opts_vested_num opts_vested_val 
opts_unvested_num opts_unvested_val shrown;  
run;

 proc sql;
  create table excomp1
   as select *
   from anncomp2 x left join codirfin1 y 
	on x.gvkey=y.gvkey and x.year=y.year;

data excomp2;
 set excomp1;
* We want to make sure we pick up firms that have both execucomp and compustat data;
* about 13,000 obs get dropped at this point;
 if fybegdt=. or fyenddt=. then delete;
 run;


/** at this stage, we still have only 1 obs per executive per firm-year;
* so this should have no duplicates by coperol-year;**/

proc sort nodupkey data=excomp2;
 by coperol year;
 run;


* ===========================;
* READING IN STGRTTAB DATASET;
* ===========================;
* this dataset has information on actual stock and option awards for the fiscal year;
* this is available only from 1992-2006;
* no duplicates by coperol grant number;

data stgrttab;
 set common.stgrttab;
 coperol=co_per_rol;
 keep coperol year blkshval exdate expric grntnum mktpric numsecur;
 proc sort nodupkey;
  by coperol year grntnum;
*proc contents;
*   title "information on dataset stgrttab";
run;

* at htis stage, for each executive per firm-year, we will have as many obs;
* as the number of grants for that executive ;
* so this can have more observations than excomp3;

proc sql;
 create table excomp3
   as select *
   from excomp2 x left join stgrttab y 
	on x.coperol=y.coperol and x.year=y.year;

* should have no duplicates by coperol-year-grntnum;

proc sort nodupkey data=excomp3;
 by coperol year grntnum;
 run;


* ===========================;
* READING IN EX_BLACK DATASET;
* ===========================;
*execucomp used to provide the information on risk-free rate upto 2006 - they discontinuted after that;
* this has data from 1992-2006 which is needed to do black-scholes values;
* only 1 obs for each year - so totally 15 rows;
* we are using this only to check our numbers with execucomp for the period when execucomp used to calculated BS values;
* this is just to give confidence that the methodology we use to compute BS values is correct;
* I am downloading the information from federal reserve website later for our calculation of BS values;

data ex_black;
 set common.ex_black;
* risk free rate is given as a percentage, need it to be a fraction;
 execucomp_rf=risk_free_rate/100;
 keep execucomp_rf year;
 * proc contents;
 *    title "information on dataset ex_black";
  run;

proc sql;
 create table excomp4
   as select *
   from excomp3 x left join ex_black y 
	on  x.year=y.year;


* =============================;
* READING IN VOLATILITY DATASET;
* =============================;
* Here we calculate BS value of options granted using Execucomp 
methodology and see if it matches their values reported;
* execucomp uses divyield and volatility estimates over rolling 3 and 5 year
	periods in B-S calculations;
*  they stop providing this as of 2006, so we need to do it ourselves;
* we do this in common.exec_roll_vol_fyear_201111_1.sas7bdat which uses crsp data to calculate volatilities of monthly stock returns;

data volatility;
 set common.exec_roll_vol_fyear_201111_1;
 where year>=1992;
* we have volatility of monthly returns,
   converting to annual volatility;
estimated_volatility=rollingstd*(12**0.5);
proc sort nodupkey;
 by gvkey year;
run;


* ===========================================;
* READING IN RISK-FREE RATES FROM FED WEBSITE;
* ===========================================;
* getting the risk-free rates based on Treasury securities;
* source: fed reserve website (I downloaded to file in excel fed_website_10yrTnote);
* I download the annual Treasury rates from 1992-2012.  
we use the risk free rate corresponding to the actual maturity;
* note that Execucomp assumes 7 year maturity of options and therefore use 7 year risk-free rate
for grant date valuation;
* note that FED website gives only 1,2,3,5,7 and 10 yr rates, I interpolate the numbers for 4,6,8,9 years;

proc import OUT= riskfree_rate 
   datafile= "c:/lalitha/execucomp/federal_reserve_treasury_rates.xlsx" 
    DBMS=excel REPLACE;
     SHEET="federal_reserve_treasury_rates"; 
     GETNAMES=YES;
run;
* proc print data=riskfree_rate;
* run;

* MERGE RISKFREE RATE DATA WITH VOLATILITY ESTIMATES;

proc sql;
 create table BS_estimates as
  select * 
  from volatility x left join riskfree_rate y
   on x.year=y.year;

data BS_estimates1;
 set BS_estimates;
 * rates are in percentages, need to be in fractions;
  array X 
   oneyr twoyr threeyr fouryr fiveyr sixyr sevenyr eightyr nineyr tenyr;
  do over X;
   X=X/100;
  end;
 keep estimated_volatility   oneyr twoyr threeyr fouryr fiveyr sixyr 
   sevenyr eightyr nineyr tenyr gvkey year nrollingstd;
  run;

/**
proc print data=BS_estimates1 ;
 where gvkey="001078";
 title "check estimates of BS";
run;
**/

proc sort nodupkey data=BS_estimates1;
 by gvkey year;
 run;

 proc sql;
  create table excomp5 as
   select *
    from excomp4 x left join BS_estimates1 y
	 on x.gvkey=y.gvkey and x.year=y.year;

	
*WINSORIZE DATA AS IN EXECUCOMP METHODOLOGY;
*==========================================;
* execucomp winsorizes values of volatility and dividend yield at the 5th and 95th percentile values
	 when calculating B-S values, we follow same procedure;

proc sort data=excomp5;
 by year;
 proc univariate data=excomp5 noprint;
  by year;
  var estimated_yield estimated_volatility;
 output out=out1 pctlpts=5 95 pctlpre=estimated_yield estimated_volatility;

data out1;
 set out1;
keep year estimated_yield5 estimated_yield95 estimated_volatility5 estimated_volatility95;
run;

proc sort data=excomp5;
 by year;
proc sort data=out1;
 by year;
data excomp6a;
 merge excomp5 (in=A) out1 (in=B);
  by year;
   if A;
*  proc print data=excomp6 (obs=10);
*   var gvkey year stimated_volatility5 estimated_volatility95 estimated_yield5 estimated_yield95;
* title "check winsorizing done";
  run;

 data excomp6b;
  set excomp6a;
   if estimated_yield>estimated_yield95 and estimated_yield^=. then estimated_yield=estimated_yield95;
   if estimated_volatility>estimated_volatility95 and estimated_volatility^=. then estimated_volatility=estimated_volatility95;
   if estimated_yield<estimated_yield5 and estimated_yield^=. then estimated_yield=estimated_yield5;
   if estimated_volatility<estimated_volatility5 and estimated_volatility^=. then estimated_volatility=estimated_volatility5;
    drop estimated_volatility5 estimated_volatility95 estimated_yield5 estimated_yield95;
run;

* computing annual volatililities for using when firms have less than 1 year of data;
* using the mean estimated volatility for that sample year as in Execucomp methodology;

 proc sql;
 create table excomp6c as
  select *, mean(estimated_volatility) as mean_vol, mean(estimated_yield) as mean_yield
  from excomp6b
  group by year;

/*
* the datastep below shows that our volatility numbers are slightly higher than the sp1500 numbers
  for the period 1992-2003 for which they show the data on their website;
* our range is from 0.34-0.53 and their range is from 0.31-0.50;
* they do not show the average div yield in their sample, only the 95th percentile values;

  data chk3;
   set excomp6c;
   proc sort nodupkey;
    by year;
  proc print data=chk3;
   var year mean_vol mean_yield;
   title "estimated annual volatilities and yields for sp1500 firms";
   run;
*/

data excomp7;
 set excomp6c;

format exdate mmddyy10.;

* if we have less than 1 year of data, we use the average volatility of S&P1500 firms following Execucomp methodology;
* for this, I need the averages by year - Execucomp reports the numbers they use upto 2003. we use the numbers based on 
 our own estimates for all years (see module above);

if nrollingstd<12 and nrollingstd^=. then estimated_volatility=mean_vol; 

 * same for estimated_volatility - use execucomp volatility upto 2006 if ours is missing, after which they do not report;

if year<=2006 and estimated_volatility=. then estimated_volatility=execucomp_volatility;

* since we are using the execucomp dividend yield numbers to calculate the 3-year rolling
  averages, we can actually do the computation only for the 3rd year that each firm is there on Execucomp;
* to avoid missing values, we use the actual Execucomp 3-year averages till 2006, 
 and use the estimated ones from 2007 onwards;

 if year<=2006 and estimated_yield=. then estimated_yield=execucomp_yield;

/*
* execucomp assumes that all grants are issued on july 1st of grant year;

fiscal 	data  assumed grant date 
year    year   for B-S method
end
=====   ====    ======
Jan-06	2005	Jul-05
Feb-06	2005	Jul-05
Mar-06	2005	Jul-05
Apr-06	2005	Jul-05
May-06	2005	Jul-05

Jun-06	2006	Jul-05

Jul-06	2006	Jul-06
Aug-06	2006	Jul-06
Sep-06	2006	Jul-06
Oct-06	2006	Jul-06
Nov-06	2006	Jul-06
Dec-06	2006	Jul-06
*/
 if fyr=6 then assumed_grantyear=year-1;
 else assumed_grantyear=year;
 assumed_grantdate=mdy(7,1,assumed_grantyear);

 * need to get fiscal year end dates for maturity at fiscal year end;

* two maturity values need to be estimated;
* one for B-S value that goes in the TDC1 (as of grant date) and the other for
 B-S value that goes into to delta (as of fiscal year end);
* this is based on difference between exdate and grantdate and between
 exdate and fiscal year end date respectively;
* execucomp rounds off maturity to nearest whole number;
* WE ROUND OFF IN DATA CHECKS ONLY TO MAKE SURE WE GET OVER A 0.999 CORRELATION WITH
 EXECUCOMP VALUES = GOING FORWARD WE DO NOT ROUND OFF ANYWHERE;
* ALSO IMPORTANT - EXECUOMP has a 70% HAIRCUT ON TIME TO MATURITY;
* THIS IS BECAUSE EXECS TYPICALLY EXERCISE EARLIER;
* CORE AND GUAY DO NOT APPPEAR TO MAKE THIS ASSUMPTION;

* IF WE WISH TO DO A 70% HAIRCUT, DO THE STATEMENTS BELOW;
* maturity_grantdate=0.7*round((exdate-assumed_grantdate)/365,1);
* maturity_yearend=0.7*round((exdate-fyenddt)/365,1);

* IF WE WISH TO USE ACTUAL MATURITY RATHER THAN 70% HAIRCUT, DO THE STATEMENTS BELOW;
 maturity_grantdate=(exdate-assumed_grantdate)/365;
 maturity_yearend=(exdate-fyenddt)/365;

 if coperol=. then delete;

* see note below for dataset chk4;
* zero observations where maturity at grant date is negative;
* but 132 cases where maturity at yearend is negative;
* seems reasonable to assume maturity is very small - cannot take zero because this comes in denominator 
 of delta - assume very small maturity;
 if numsecur^=. and numsecur>0 and 
    maturity_yearend<0 and maturity_yearend^=.
then maturity_yearend=0.001; 
 drop assumed_grantyear assumed_grantdate;
run;

/*
* if the maturity is negative (happens when we do
  fiscal year end), it means that the expiry date is before fiscal year end;
* I find 0 obs that have maturity_grantdate<0;
* there are 132 obs that have maturity_yearend<0;
* appears to be cases where options were given to vest immediately;
* checked two of them in proxy statements (see aaron rents in 2005, exec todd 
 evans, coperol 25896 and J Bryant, Kellogg, coperol 28564;
* so these are coded as maturity=. since we do not know if they
* have been exercised and converted to shares (we will  then be doublecounting delta);
* obviously this check has to be done before the final step in the previous program
 is done, which replaces the negatives with zeros;

 data chk4;
  set excomp7;
  proc sort ;
  by coperol descending year;
 proc print ;
  var coperol year fyr fyenddt exdate maturity_grantdate maturity_yearend;
* where numsecur^=. and numsecur>0 and 
   maturity_grantdate<0  and maturity_grantdate^=.;
  where numsecur^=. and numsecur>0 and 
    maturity_yearend<0 and maturity_yearend^=.;
 format fyenddt mmddyy10.;
 title "maturity is negative";
run;

proc means data=excomp7;
 var exdate fyenddt maturity_grantdate maturity_yearend ;
  where numsecur>0 and numsecur^=.;
 title "out of 127861 obs for which grants were made, 127353 had exdate available";
run;

proc sql;
 create table chk5 as
 select *, count(coperol) as ctcoperol, count(maturity_grantdate) as ctmat_grantdate
 from excomp7
group by coperol, year;

 proc means data=chk5;
  var maturity_grantdate;
  title "checking before we replace missing maturity values";
  run;
*/

*sometimes when there are multiple grants for the same executive in the same year,
data on maturity of one option award is missing even though the other awards in 
that year have data on maturity;
* in these cases I am going to replace the maturity that is missing with the average 
 maturity of the other awards granted to the same executive in the same year;
* number of obs goes from 127355 to 127523 as per check statement above and below;
* this changes the mean from 9.0556 to 9.0559;

proc sql;
 create table excomp8 as
 select *, mean(maturity_grantdate) as avg_mat_grantdate,  mean(maturity_yearend) as avg_mat_yearend
 from excomp7
group by coperol, year;

data excomp9;
 set excomp8;
 if maturity_grantdate=. and avg_mat_grantdate^=. then maturity_grantdate=avg_mat_grantdate;
 if maturity_yearend=. and avg_mat_yearend^=. then maturity_yearend=avg_mat_yearend;
run;

/**
 proc means data=excomp9 n mean min p1 p5 p10 p25 p50 p75 p90 p99 max;
  var maturity_grantdate maturity_yearend;
  title "checking after we replace missing maturity values";
  run;
**/

* we can use either the execucomp values or the estimated (by us) values
of risk-free rate, volatility and divyield;
* remember that the risk free rate depends on the maturity of the option at year end or grant date;

data excomp10;
 set excomp9;
 
* USE THESE NUMBERS IF YOU WANT TO USE PRE-2006 EXECUCOMP VALUES;
* I USE THIS ONLY TO CORRELATE  OUR CALCULATED VALUES WITH EXECUCOMP BS VALUES;
* THIS WILL BE VALID ONLY TILL 2006;

* rfc=execucomp_rf;
* bs_yield=execucomp_yield;
* sigma=execucomp_volatility;


 if round(maturity_yearend,1) =1  then rfc=oneyr;
 if round(maturity_yearend,1) =2  then rfc=twoyr;
 if round(maturity_yearend,1) =3  then rfc=threeyr;
 if round(maturity_yearend,1) =4  then rfc=fouryr;
 if round(maturity_yearend,1) =5  then rfc=fiveyr;
 if round(maturity_yearend,1) =6  then rfc=sixyr;
 if round(maturity_yearend,1) =7  then rfc=sevenyr;
 if round(maturity_yearend,1) =8  then rfc=eightyr;
 if round(maturity_yearend,1) =9  then rfc=nineyr;
 if round(maturity_yearend,1)=10 then rfc=tenyr;

 * sometimes maturity is greater than 10;
 if (maturity_yearend^=. and round(maturity_yearend,1)>10) then rfc=tenyr;

 bs_yield=estimated_yield;
 sigma=estimated_volatility;
 

  sigmasq=sigma*sigma;

 drop execucomp_rf execucomp_yield execucomp_volatility
   estimated_yield estimated_volatility;
run;

/**
proc print data=excomp10;
 var year rfc sigma sigmasq maturity_yearend maturity_grantdate;
 where coperol=16285;
 title "ibm checking recall no data will be there post-2006 on options";
run;
**/

*=========================================;
* CALCULATE INCENTIVES FOR PRE-2006 PERIOD;
*=========================================;

* START WITH SENSITIVITY OF CURRENT YEAR OPTIONS;
*------------------------------------------------;
* FOR THIS YEAR'S GRANT we can use blk_val as the value of grants this year or calulate 
 as per core-guay;
* execucomp uses 'mktprice' which is the market price on the date of issue of the
option to calculate B-S value;
* i do the same for the grant date, but use market price on fiscal year close (prccf) as per
Core and Guay for the fiscal year end price;
* execucomp assumes that maturity is only 0.7* maturity as given by exdate;
* reason - these options are usually exercised earlier;
* as mentioned above, WE DO NOT use same assumption, but this can be changed easily in the program;
* I found the value as per the formula below for Vopts_grantdate, using the execucomp
assumptions and compared it to blk_val, find that the two numbers are very close;

data excomp11;
 set excomp10;

* sometimes, it appears that the numsecur is missing when it is actually zero;
* if we compare our nubmers with the numbers in Execucomp,
  they get more observations of option_awards_num compared to our sumnumsecur;
* to correct for this, i am going to code numsecur=0 if numsecur=.
  and if the other data on option awards is missing;

 if numsecur=. and (exdate=. and expric=. and mktpric=.
   and option_awards_num=0) then numsecur=0;

* has to be done before we throw out the duplicate coperols;
* exercise price;

 Xc = expric;

 * REALIZABLE VALUE AS EXCESS OF S OVER X;
  realizable_value = (prccf-Xc)*numsecur;
  if realizable_value<0 then realizable_value=0;
  if mktpric=. or Xc=. or numsecur=. then realizable_value=.;

* we will calculate two values of Zc, one for B-S value at grant date. This
 is for TDC1. The other is for B-S value at year end. This is for delta and vega;

 Zc_yearend = (log(prccf/Xc)+maturity_yearend*(rfc-bs_yield+sigmasq/2))/(sigma*sqrt(maturity_yearend));
 if maturity_yearend=. then Zc_yearend=.;

 Zc_grantdate= (log(mktpric/Xc)+maturity_grantdate*(rfc-bs_yield+sigmasq/2))/(sigma*sqrt(maturity_grantdate));
 if maturity_grantdate=. then Zc_grantdate=.;


* appendix of core guay paper;

 Sc_yearend = exp(-bs_yield*maturity_yearend)*probnorm(Zc_yearend)*numsecur*prccf/100;
  if Zc_yearend=. then Sc_yearend=.;
* If it is missing, then we are assuming no option grants were made, and hence zero;
 * if numsecur=. or numsecur=0 then Sc_yearend=0;
 if  numsecur=0 then Sc_yearend=0;

* Black Scholes value of options;

  Vc_grantdate = numsecur*(mktpric*exp(-bs_yield* maturity_grantdate)*probnorm(Zc_grantdate)
               -Xc*exp(-rfc* maturity_grantdate)*probnorm(Zc_grantdate-sigma*sqrt( maturity_grantdate)));
  if Zc_grantdate=. then Vc_grantdate=.;

* If it is missing, then we are assuming no option grants were made, and hence zero;
 * if numsecur=. or numsecur=0 then Vc_grantdate=0;
 if numsecur=0 then Vc_grantdate=0;

* introduced for our pay for luck paper, to find the wealth measures;
 
  if Zc_yearend^=. then do;
  Vc_yearend = numsecur*(prccf*exp(-bs_yield*maturity_yearend)*probnorm(Zc_yearend)
               -Xc*exp(-rfc*maturity_yearend)*probnorm(Zc_yearend-sigma*sqrt(maturity_yearend)));
  end;

  * If it is missing, then we are assuming no option grants were made, and hence zero;
 * if numsecur=. or numsecur=0 then Vc_yearend=0;
 if numsecur=0 then Vc_yearend=0;

* sensitivity with respect to a 0.01 change in stock return volatility;
* see core and guay appendix A;
  if Zc_yearend^=. then do;
   Rc_yearend = exp(-bs_yield* maturity_yearend)*PDF('normal',Zc_yearend,0,1)*prccf*sqrt( maturity_yearend)*0.01*numsecur;
  end;
  if Zc_yearend=. or maturity_yearend=. then Rc_yearend=.;

* If it is missing, then we are assuming no option grants were made, and hence zero;
*  if numsecur=. or numsecur=0 then Rc_yearend=0;
 if numsecur=0 then Rc_yearend=0;

 run;

/**
 * note that proc print below is correct only upto 2005;
 * after 2005, ibm is on new reporting format, so reports 2006 onward data in different dataset;
 proc print data=excomp11;
*  where coperol=16285;
  where coperol=6;
   var year rfc bs_yield sigma numsecur maturity_yearend Xc Zc_yearend Sc_yearend Rc_yearend Vc_yearend old_datafmt_flag;
 title "ibm palmisano check of current year option calculation"; 
run;
 **/

* AGGREGATING TRANCHE-LEVEL INFORMATION TO PROVIDE 1 OBS PER EXECUTIVE;
* since each executive can be given more than 1 option grant in 1 year, I need
 to cumulate the value calculated above for each coperol year combination;
 
proc sql;
 create table excomp12
  as select *, sum(Vc_grantdate) as Vopts_grantdate,
     sum(Vc_yearend) as Vopts_yearend,
   sum(numsecur) as sumnumsecur,
    sum(Sc_yearend) as Sopts_grants_yearend,  
     sum(Rc_yearend) as Ropts_grants_yearend, 
          sum(realizable_value) as sumrealizable_value
  from excomp11
  group by coperol,year;
 
/*
* when we do this with execucomp 70% haircut on grant date maturity as well as their measures of rf and
 dividend yield and volatility the difference between the two values has a mean of 2%, median of 0%, 10th and 90th of 0% and 5.5%
  and the correlations between our estimated values and execucomp estimated values are about 0.998;
* no difference in option number total, and the correlation between our sumnumsecur and option_awards_num=1;

* when we do this with execucomp 70% cut and with our measures of rf and
 dividend yield and volatility the difference between the two values has a mean of 11%, median of 7.5%, and 10th and 90th percentiles of 1.3% and 22%
 and the correlations between our estimated values and execucomp estimated values is 0.992  ;

data chk6;
   set excomp12;
   diff_optvalue=abs(option_awards_blk_value-Vopts_grantdate)/option_awards_blk_value;
   diff_optnum=abs(option_awards_num-sumnumsecur)/option_awards_num;
   keep coperol year diff_optvalue exdate maturity_grantdate maturity_yearend
 Vc_grantdate rfc
 Vopts_grantdate sumnumsecur option_awards_blk_value option_awards_num blkshval diff_optnum;
   proc sort nodupkey;
    by coperol year;
run;
proc univariate data=chk6;
 var diff_optvalue diff_optnum;
 where year<=2005;
 title "comparing our values with execucomp";
 run;
proc corr data=chk6;
 var option_awards_blk_value Vopts_grantdate;
title "correlation between our calculated value and execucomp B-S value 
  is over 99.8% using execucomp numbers";
 where year<=2005;
 run;
 
proc corr data=chk6;
 var sumnumsecur  option_awards_num;
title "correlation between our calculated value and reported value in execucomp of total number of options
  is 100% but number of obs different";
 where year<=2005;
 run;

* number of observations between sumnumsecur (our calculated total annual grants
 and option_awards_num (execucomp provided number of total grants) are different for 21653 cases;
* these are cases for which execucomp does not calculate the BS value anyway, i.e., the 
 option_awards_blk_value is missing, so it appears that these are problematic cases anyway;
* so should be ok to ignore them;

 proc print data=chk6 (obs=25);
  where sumnumsecur=. and year<=2005;
   run;

 proc univariate data=chk6;
  var option_awards_num option_awards_blk_value vopts_grantdate;
   where sumnumsecur=. and year<=2005;
   title "option_awards_blk_value is missing when sumnumsecur is missing but option_awards_num is not missing";
   title "there are about 22151 obs that have this problem";
   title " we can treat these as missing or zero, we leave it as missing";
   run;
*/

* at this point, we are going to keep only 1 observation per executive year;
* since we have already added up the values of current year's grants, that 
 is all we need to keep;
* going forward, it should always be 1 obs per executive year;
data excomp13;
  set excomp12;
  if sumnumsecur=0 or sumnumsecur=. then do;
   Sopts_grants_yearend=0;
   Vopts_grantdate=0;
   Ropts_grants_yearend=0;
   sumrealizable_value=0;
   Vopts_yearend=0;
   FVopts_yearend=0;
  end;

* dropping rfc here, recalculating rf below based on matvest and matunvest;
* the only reason to calculate Vopts_grantdate was to compare with execucomp;
* so not keeping this below, not keeping BS values reported by execucomp either;
 keep coperol year Vopts_yearend sumnumsecur Sopts_grants_yearend  
    Ropts_grants_yearend sumrealizable_value sigma sigmasq bs_yield option_awards_num 
   opts_vested_num opts_unvested_num opts_exercised_num
	opts_vested_val opts_unvested_val shrown shrsout old_datafmt_flag avg_mat_yearend prccf
    fybegdt fyenddt oneyr twoyr threeyr fouryr fiveyr sixyr sevenyr eightyr nineyr tenyr exec_fullname;

proc sort nodupkey;
 by coperol year;
run;


* first take lags of number of vested and unvested options;
* we use these below to check, see comments;
proc sort data=excomp13;
 by coperol year;
data excomp13a;
 set excomp13;
 lcoperol=lag(coperol);
 lyear=lag(year)+1;
 lag_opts_vested_num=lag(opts_vested_num);
 lag_opts_unvested_num=lag(opts_unvested_num);
 if coperol^=lcoperol or year^=lyear then do;
  lag_opts_vested_num=. ; lag_opts_unvested_num=.;
 end;
 drop lcoperol lyear ;
run;
/*
* compare total number of options and total value before and after this exercise;
* in general, the number of options at the start of the year (lagged values
* of opts_unvested_num and VESTED NUM) plus additional options granted less 
* options exeercised should be equal to the number of options at the
* end of the year (sum of opts_vested_num and opts_unvested_num);
* i.e., lag(opts_unvested_num)+lag(opts_vested_num)+OPTION_AWARDS_NUM
  = opts_exercised_num + opts_unvested_num + opts_vested_num;
* appears that this is not always true because of stock splits;
* specifically, in 18% of cases, this is not true;
* splits will not affect us as the sensitivties etc depend on both price
* and number of options, both of which are adjusted;
* but as in the case of AMR, ALCOA etc, this may mean that we do not find
* the above equality to hold;
* also may be the case when there are expired options - not sure how to capture that;
* also check to see if any of the vested or unvested num or value is negative;
* should never be, but I find about 1 to 17 obs  have this problem;
* set them to zero in the next step;

data chk7;
  set excomp13a;
   balance = lag_opts_unvested_num + lag_opts_vested_num + option_awards_num -
          (opts_exercised_num + opts_unvested_num + opts_vested_num);
* flag if the balance is greater than 10.  It should be zero, doing 0.010 or 10 options 
to allow for rounding off issues;
     if abs(balance)>0.010 then flag=1; else flag=0;
	 if opts_unvested_num<0 and opts_unvested_num^=. then flag1=1 ; else flag1=0;
	 if opts_unvested_val<0 and opts_unvested_val^=. then flag2=1 ; else flag2=0;
	 if opts_vested_num<0 and opts_vested_num^=. then flag3=1 ; else flag3=0;
	 if opts_vested_val<0 and opts_vested_val^=. then flag4=1 ; else flag4=0;
	run;

 proc freq data=chk7;
  tables flag flag1 flag2 flag3 flag4;
  where year<=2005;
  title "flag should be zero typically and flag1-flag4 should be 0";
  run;
  proc means data=chk7 n mean p5 p10 p25 p50 p75 p90 p95;
   var balance;
   title "balance should be zero, but is not. this is because of splits, expiry etc - see note above";
   run;
*/

*=======================================================;
* NOW COMPUTE SENSITIVITY OF UNVESTED AND VESTED OPTIONS;
*=======================================================;

* FOR STOCK OF OPTION GRANTS, DO THIS;
* first, we need the average number of exercisable options, based on the total
realizable value of exercisable options reported in the proxy;
* the realizable value is the excess of stock over exercise price for all exercisable
options;
* then we subtract this from the stock price, which would give the average exercise price;
* market price used is the average year end market price;
* set time to maturity for unexercisable options as one year less than the time to maturity
for current year options (or 9 if no options in current year);
* do the same thing for exercisable options, but keep the time to maturity as three
years less than the  time to maturity for unexercisable options;
* subtract current years options from unexercised options portfolio to aviod double
counting;

data excomp13b; 
  set excomp13a;
* see chk step above to see if any of the vested or unvested num 
   or value is negative. It should never be, but I find about 10 obs
  have this problem. I set them to zero below;

  if opts_unvested_num<0 and opts_unvested_num^=. then opts_unvested_num=0;
  if opts_unvested_val<0 and opts_unvested_val^=. then opts_unvested_val=0;
  if opts_vested_num<0 and opts_vested_num^=. then opts_vested_num=0;
  if opts_vested_val<0 and opts_vested_val^=. then opts_vested_val=0;

*===============;
* unexercisable;
*===============;
*
* also, if we assume stock options granted in the year are all unexercisable;
* then opt_awards_num should be strictly smaller than OPT_UNEX_UNEXER_NUM;
* not always true however (not true in xxx cases);
* looks like some options immediately vest;
* therefore we cannot assume current years options have long vesting;
* if options issued in current year, then time to maturity is maturity-1, else 9;
* also, the unexercisable option portfolio has to be reduced by the number of 
  options granted that year and the value of options granted that year should be 
  reduced from total realizable value of unexercisable options ;
* need to adjust the unexercisable options by the number granted that year;
* that's because we separately calculate the deltas for these;
* assuming all current year's options are unvested;

 opts_unvested_num_excl_curryear = opts_unvested_num - option_awards_num;

* sumrealizable_value represents the aggregate  value of all the options awarded that year to each exec;
* so it is numsecur*(fiscal year end price - exercise price) aggregated across all stock grants during year;
* by subtracting this from opts_unvested_val, we get the aggregate value of unvested options that were 
  granted in prior years (which = opts_unvested_val_excl_curryear);
* for example, assume that UNVEST_NUM=40, OPTION_AWARDS_NUM=50;
* that means that some of the awards given this year vested immediately;
* first we assume that these were not exercised, then we adjust for possible exercise;
* see notes below;
* in the example here, we reduce the current year's portfolio of unvested options
  to zero, because we are separetly considering the 50 options that were granted;
* the 10 options that were given in the current year are reduced from the stock 
* of vested options;

  opts_unvested_val_excl_curryear = opts_unvested_val-sumrealizable_value;
  opts_vested_num_excl_curryear =  opts_vested_num;
  opts_vested_val_excl_curryear =  opts_vested_val;
  if  opts_unvested_num_excl_curryear^=. and opts_unvested_num_excl_curryear<0 
  then do;
   opts_unvested_num_excl_curryear=0;
   opts_unvested_val_excl_curryear =0;
   opts_vested_num_excl_curryear= opts_vested_num + (opts_unvested_num - option_awards_num);
   opts_vested_val_excl_curryear= opts_vested_val + (opts_unvested_val-sumrealizable_value);
  end;
  if  opts_unvested_val_excl_curryear^=. and opts_unvested_val_excl_curryear<0 
   then  opts_unvested_val_excl_curryear=0;
run;

/*
proc means data=excomp13b n  mean p5 p25 p50 p75 p95;
 var opts_unvested_num_excl_curryear;
 where opts_unvested_num_excl_curryear<0 and opts_unvested_num_excl_curryear^=.;
   title "should be no observations here";
run;
proc means data=excomp13b n  mean p5 p25 p50 p75 p95;
 var opts_unvested_val_excl_curryear;
 where opts_unvested_val_excl_curryear<0 and opts_unvested_val_excl_curryear^=.;
title "should be no observations here";
run;
 proc means data=excomp13b n  mean p5 p25 p50 p75 p95;
 var opts_vested_num_excl_curryear;
 where opts_vested_num_excl_curryear<0 and opts_vested_num_excl_curryear^=.;
title "should be no observations here";
run;
 proc means data=excomp13b n  mean p5 p25 p50 p75 p95;
 var opts_vested_val_excl_curryear;
 where opts_vested_val_excl_curryear<0 and opts_vested_val_excl_curryear^=.;
title "should be no observations here";
run;
*/

* checked that all opts_unvested_num_excl_curryear and opts_unvested_val_excl_curryear
   are now zero or positive;
  
* checked that all opts_unvested_num are now zero or positive;
* some cases (3790 obs) of opts_vested_num are now negative;
* these have to be cases where all or part of current years
* grant was exercisable immediately, and some were actually exercised;
* in this case, we check if overall equality (see earlier comment) holds;
* if not, drop these obs becuase it is not clear what is going on (even from proxy);
* ROUND function below matches at 100th place-that is if the difference is less than 100 options, we consider it ok;
* this is only to make sure that rounding off error is not responsible for failure to match;
* if the equality holds, then we need to replace opts_vested_num_excl_curryear to zero;

data excomp13c;
 set excomp13b;
 if opts_vested_num_excl_curryear<0 and opts_vested_num_excl_curryear^=. then 
  if round((opts_unvested_num +  opts_vested_num),0.1)=round((LAG_opts_unvested_num + LAG_opts_vested_num + OPTION_AWARDS_NUM - opts_exercised_num),0.1) 
   then do;
   opts_vested_num_excl_curryear=0; opts_vested_val_excl_curryear=0;
   DUMMY1=1;
  end; 

* if the equality does not hold, then we need to replace all values to missing;

  if opts_vested_num_excl_curryear<0 and opts_vested_num_excl_curryear^=. then 
  if round((opts_unvested_num +  opts_vested_num),0.1)^=round((LAG_opts_unvested_num + LAG_opts_vested_num + OPTION_AWARDS_NUM - opts_exercised_num),0.1) 
   then do;
     opts_unvested_num_excl_curryear=.; opts_unvested_val_excl_curryear=.;
     opts_vested_num_excl_curryear=.; opts_vested_val_excl_curryear=.;
     DUMMY2=1;
  end;

* after these adjustments, still opts_vested_val_excl_curryear is negative for some cases,
 although for some it is just rounding off error, going to make this zero for everyone;
* may be because the dates/assumptions we use for SUMREALIZABLE_VALUE are slightly 
* different from dates used  by company;

  if opts_vested_val_excl_curryear<0 and opts_vested_val_excl_curryear^=. 
   then opts_vested_val_excl_curryear=0;
run;

 /**
 proc means data=excomp13c ;
 var dummy1 dummy2;
title "checking how many observations had vested_num or vested_val is negative";
run;
**/

data excomp14;
 set excomp13c;
* maturity of unercisable set at 9 years or actual minus 1;
  if option_awards_num^=. and option_awards_num>0 
    then matunvest=avg_mat_yearend-1;
  else matunvest=9;
  if matunvest<0 and matunvest^=. then matunvest=0.001;

* dont make it zero because the Z function uses this in denominator;
*  if matunvest<0 then matunvest=1;

 if round(matunvest,1) <=1 then rfunvest=oneyr;
 if round(matunvest,1) =2  then rfunvest=twoyr;
 if round(matunvest,1) =3  then rfunvest=threeyr;
 if round(matunvest,1) =4  then rfunvest=fouryr;
 if round(matunvest,1) =5  then rfunvest=fiveyr;
 if round(matunvest,1) =6  then rfunvest=sixyr;
 if round(matunvest,1) =7  then rfunvest=sevenyr;
 if round(matunvest,1) =8  then rfunvest=eightyr;
 if round(matunvest,1) =9  then rfunvest=nineyr;
 if round(matunvest,1)=10  then rfunvest=tenyr;

* sometimes maturity is greater than 10;
 if (matunvest^=. and round(matunvest,1)>10) then rfunvest=tenyr;
 
* if Xunvest<0 then presumably the option is deep in the money;
* so we reset the exercise price to be very small;
  if opts_unvested_num_excl_curryear^=0 then 
	  Xunvest=prccf-(opts_unvested_val_excl_curryear/opts_unvested_num_excl_curryear);
  if (Xunvest<0 and Xunvest^=.) then Xunvest=0.01;

  Zunvest = (log(prccf/Xunvest)+  matunvest*(rfunvest-bs_yield+sigmasq/2))/(sigma*sqrt(matunvest));

  if matunvest=. or matunvest<0 then Zunvest=.;
  if opts_unvested_val_excl_curryear=. or opts_unvested_val_excl_curryear<0 
   or opts_unvested_num_excl_curryear<0 or opts_unvested_num_excl_curryear=. then Zunvest=.;

  if opts_unvested_val_excl_curryear=. or opts_unvested_val_excl_curryear<0 
   or opts_unvested_num_excl_curryear<0 or opts_unvested_num_excl_curryear=. then FZunvest=.;

  Sunvest = exp(-bs_yield* matunvest)*probnorm(Zunvest)*(prccf/100)*opts_unvested_num_excl_curryear;

* sensitivity with respect to a 0.01 change in stock return volatility;
* see core and guay appendix A;
  if Zunvest^=. then do;
   Runvest = exp(-bs_yield* matunvest)*PDF('normal',Zunvest,0,1)*prccf*sqrt( matunvest)*0.01*opts_unvested_num_excl_curryear;
  end;
  if Zunvest=. then Runvest=.;

*value of option portfolio ($000);
  Vunvest = opts_unvested_num_excl_curryear*(prccf*exp(-bs_yield* matunvest)*probnorm(Zunvest)
                     -Xunvest*exp(-rfunvest* matunvest)*probnorm(Zunvest-sigma*sqrt( matunvest)));
  if opts_unvested_num_excl_curryear=. or Zunvest=. then Vunvest=.;


*============;
* exercisable;
*============;
* maturity is 3 years less than that of unexercisable - dont make it zero because the Z function uses this in denominator;
  matvest=matunvest-3;
  if matvest<0 and matvest^=. then matvest=0.001;

  Xvest=prccf-(opts_vested_val_excl_curryear/opts_vested_num_excl_curryear);
* if Xvest<0 then presumably the option is deep in the money;
* so we reset the exercise price to be very small;
  if (Xvest<0 and Xvest^=.) then Xvest=0.01;

 if round(matvest,1) <=1 then rfvest=oneyr;
 if round(matvest,1) =2  then rfvest=twoyr;
 if round(matvest,1) =3  then rfvest=threeyr;
 if round(matvest,1) =4  then rfvest=fouryr;
 if round(matvest,1) =5  then rfvest=fiveyr;
 if round(matvest,1) =6  then rfvest=sixyr;
 if round(matvest,1) =7  then rfvest=sevenyr;
 if round(matvest,1) =8  then rfvest=eightyr;
 if round(matvest,1) =9  then rfvest=nineyr;
 if round(matvest,1)=10  then rfvest=tenyr;

* sometimes maturity is greater than 10;
 if (matvest^=. and round(matvest,1)>10) then rfvest=tenyr;

  Zvest = (log(prccf/Xvest)+  matvest*(rfvest-bs_yield+sigmasq/2))/(sigma*sqrt(matvest));
  if matvest=. or matvest<0 then Zvest=.;
  if opts_vested_val_excl_curryear=. or opts_vested_val_excl_curryear<0 
   or opts_vested_num_excl_curryear<0 or opts_vested_num_excl_curryear=. 
   then Zvest=.;

* sensitivity of option to 1 percent change in stock price;
* appendix of core guay paper;

 Svest = exp(-bs_yield* matvest)*probnorm(Zvest)*(prccf/100)*opts_vested_num_excl_curryear;

* sensitivity to stock return volatility;
  if Zvest^=. then do;
   Rvest = exp(-bs_yield* matvest)*PDF('normal',Zvest,0,1)*prccf*sqrt( matvest)*0.01*opts_vested_num_excl_curryear;
  end;
  if Zvest=. then Rvest=.;

  Vvest = opts_vested_num_excl_curryear*(prccf*exp(-bs_yield*matvest)*probnorm(Zvest)
                     -Xvest*exp(-rfvest*matvest)*probnorm(Zvest-sigma*sqrt(matvest)));
  if opts_vested_num_excl_curryear=. or Zvest=. then Vvest=.;


* sensitivity of shareholdings;
 Sshr= shrown*prccf/100;

* remember that shrown is also in 000s;

Vshr = shrown*prccf;

 * sometimes both opts_unvested_val_excl_curryear and opts_unvested_num_excl_curryear are zero;
  * in this case, the Xunvest cannot be computed because opts_unvested_val_excl_curryear/opts_unvested_num_excl_curryear is not defined;
  * for example, for ABBOTT LABS in 1998, the # current year stock option grants was 1944.36, but # unvested options at end of year was
   only 1399.22, so we adjust the  number of unvested excluding current year to be zero, then the number of vested is adjusted downward 
    from the reported value of 1100 to (1100-(1944-1399)) = 555;
  * in this case, we still get Vunvest, Runvest etc. to be missing, so we have to set it equal to zero;

* overall sensitivity;
* we do not want to make delta=. if one of the components is missing; 
 if opts_vested_num_excl_curryear=0 then do;
    Svest=0;
    Rvest=0;
    Vvest=0;
 end;
 if opts_unvested_num_excl_curryear=0 then do;
    Sunvest=0;
    Runvest=0;
    Vunvest=0;
 end;

*execucomp reports restricted stock where possible along with the sharebased holdings;
* so we do not include restricted stock as part of the delta;
* email from execucomp: "For (stock ownership) item we are using the Stock-Based Holdings column 
 which includes the restricted stock.  We have also made corrections to past years.  
 As with much of the Execucomp database, what we collect is largely dependent on how the company reports the data.  
 When possible we will try to include restricted stock with this item, however sometimes the company doesn't 
 provide a concise picture and it's not possible";

  delta = Svest+Sunvest+Sopts_grants_yearend+Sshr;

  Ropt = Rvest + Runvest + Ropts_grants_yearend;

* firm-related wealth;
 Vportfolio_yearend = Vvest+ Vunvest + Vopts_yearend + Vshr;

 
 *IMPORTANT = WE TAKE VARIABLES SUCH AS NUMBER OF VESTED AND UNVESTED FROM ANNCOMP TABLE
AS SUCH THESE ARE AVAILABLE FOR BOTH OLD AND NEW FORMAT DATA BECAUSE ANNCOMP PRESENTS AGGREGATES. 
SO FOR EXAMPLE, WE USE THE DATA VARIABLE OPTS_VESTED_NUM AND OPTS_UNVESTED_NUM FOR THE PORTFOLIO
 OF VESTED AND UNVESTED USING OLD METHOD. BUT THIS IS AVAILABLE POST-2006. ALSO, WE ASSUME
 MATUNVEST=9 IF NO OPTION GRANTS IN CURRENT YEAR. GIVEN THIS, AND OTHER ASSUMPTIONS WE MAKE, WE COULD END
 UP CALCULATING VEGA AND DELTA FOR THE OBSERVATIONS WHICH HAVE DETAILED DATA IN OUTSTANDINGAWARDS TABLE.
 TO MAKE SURE WE DO NOT DO THAT, USE THE FOLLOWING STEP;

 if old_datafmt_flag=0 then do;
    Svest=.; Sunvest=.; Sopts_grants_yearend=.; Sshr=.;
	Ropt=.; Rvest=.;  Runvest=.; Ropts_grants_yearend=.;
	Vportfolio_yearend=.; Vvest=.;  Vunvest=.; Vopts_yearend=.; Vshr=.;
	delta=.; 	 
end;

run;

/**
 proc print data=excomp14;
  where coperol=16285;
*  var Zvest prccf Xvest matvest rfvest bs_yield sigmasq sigma matvest;
  var year bs_yield  matunvest Xunvest matvest Xvest Zvest sumnumsecur
opts_unvested_num opts_unvested_val 
  opts_unvested_num_excl_curryear opts_unvested_val_excl_curryear
    opts_vested_num_excl_curryear opts_vested_val_excl_curryear
      Vopts_yearend 
    Svest Sunvest Sopts_grants_yearend sshr 
    Rvest Runvest Ropts_grants_yearend 
  ropt delta  ;
 title "ibm palmisano check of current year option calculation"; 
run;
**/
/*
proc print data=excomp14 ;
where coperol=16285 and year>1997;
 var year matvest matunvest opts_vested_num_excl_curryear opts_vested_val_excl_curryear opts_unvested_num_excl_curryear opts_unvested_val_excl_curryear Xvest Xunvest 
  Zvest Zunvest Svest Sunvest sumrealizable_value Rvest Runvest Vvest Vunvest; 
title "checking vested and unvested option values for ibm";
 run;
proc print data=excomp14 (obs=100);
 var coperol year delta ropt shrown;
*where coperol=16285 and year>1997;
 where coperol=2611;
 run;
 proc sort data=excomp14;
  by year;
 proc means noprint data=excomp14;
  by year;
var delta ropt  ;
output out=out4 mean = delta  ropt ;
proc print data=out4;
 title "checking means of calculated delta and vega by year";
run;

data chk8;
 set excomp14;
 where (delta=. and ropt^=.);
 proc means;
 var shrown;
  title "appears that when ropt is not missing but delta is missing, it is because share ownership is missing";
  title2 "2 obs only have shrownpc when delta is missing, ropt is not missing";
run;
data chk9;
 set excomp14;
 where (delta^=. and ropt=.);
  title "appears that when delta is not missing, ropt is not missing";
run;
**/

data excomp15;
 set excomp14;
 * dropping these variables because in the next step, we are merging back with full anncomp
 dataset that has these variables already;
 keep coperol year delta ropt Vportfolio_yearend exec_fullname;
  run;

* now merge back the anncomp full dataset = recall that we have been working with a 
	subset to make the program faster;
proc sql;
 create table excomp16 as 
  select *
   from excomp15 x left join anncomp y
    on x.coperol=y.coperol and x.year=y.year;

/**
*===============================;
* READ DATA ON PLAN BASED AWARDS;
*===============================;
* data from 2006 onwards;
* has no duplicates by coperol-year-grntnum;
*  WRDS says that earlier, grantnum referred to the grant number;
* that is, it uniquely identified each option grant award;
* now it identifies any award - stock, option,equity, and non-equity incentive plan awards;
* WE DO NOT USE THIS DATA ANYWHERE IN THIS PROGRAM - SEE NOTES IN MAIN PAPER AS TO WHY;
* WE HAVE A SEPARATE PROGRAM THAT  VALUES THESE AWARDS (EIP_VALUE.SAS);

data planbasedawards;
set common.planbasedawards;
coperol=co_per_rol;
* round off the expric since this is sometimes rounded off in outstanding
 table but not in the planbased table, which creates problems with the merge;
  expric = round(expric,0.01);

keep gvkey coperol year grntnum  
 act_date eq_max eq_targ eq_thres expric fair_value 
 grant_date mktpric non_eq_max non_eq_targ non_eq_thres
 opts_grt shares_grt;
proc sort nodupkey;
 by coperol year grntnum;
*proc contents;
 *  title "information on dataset planbasedawards";
run;

proc print data=planbasedawards;
var coperol year grntnum mktpric expric grant_date
 opts_grt shares_grt;
  where coperol=16285;
  title "plan based awards table for ibm samuel palmisano";
run;

* the planbased awards are options/stocks/other awards during the year;
* if opts_grt>0, we take it as an straight option award;
* if shares_grt>0, we take it as a straight share award;
* we will use B-S value of option grants as on grant date for TDC1 adjustment 
* we do not need this for the delta and vega and wealth as these are determined as on fiscal year end and are
  based on options in the outstanding awards table;
* similarly, for stock grants, we will use the grant date value for TDC1 adjustment;
* For delta, vega and wealth, we use the shrown_excl_opts and price as of fiscal year end;
* if opt_grts=0 and shares_grt=0 , then it is a long term performance award (equity or non-equity);
* within long term performance award, it appears that some firms give options that are 
  earned on meeting specific performance criteria, while most give shares;
* if opts_grt=. and shares_grt=. and non-equity=. but there is an equity award and exercise price given, 
  we assume that it is a performance award (EIP) with options;
* we need to value these using B-S value of the options assuming target level payouts;
* if opts_grt=. and shares_grt=. and non-equity=. and exercise price=. but there is an equity award
  we assume that it is a performance award (EIP) with shares;
* we need to value these using grant date stock price and assuming target level payouts;
* if opts_grt=. and shares_grt=. and equity=. but non-equity is not missing, then 
  we assume that it is a non-equity performance award (NEIP);
* we do not use the target/threshold etc. values of these as
  non-equity awards are given in ANNCOMP (NONEQ_INCENT) based on actual payouts;
* to calculate B-S value as on grant date, we need exdate;
* but this is given only in the outstanding awards table;
* so we try and match the two tables to pull the exdate from the outstanding awards table;
* I am going to create a subset of planbasedawards so that we don't match
on missing  values;
* 190  executibve years appear to have expric missing;
* cannot do anything about this anyway;
* going to indicate whether there is only 1 award per record or multiple awards per record;
* this will affect the fair value calculation - when there are multiple awards per record 
 (see example elsewhere in program, the fair value represents the cumulative fair value
 of all the awards. So we are trying to see how big a problem this is;

 data planbasedawards1;
 attrib granttype length=$14.;
 set planbasedawards;
 if opts_grt=. and shares_grt=. and (eq_max=. and eq_targ=. and eq_thres=.)
   and (non_eq_max^=. or non_eq_targ^=. or non_eq_thres^=.) 
   then granttype = "NEIP ONLY";
 if opts_grt=. and shares_grt^=. and (eq_max=. and eq_targ=. and eq_thres=.)
   and (non_eq_max=. and non_eq_targ=. and non_eq_thres=.) 
   then granttype = "PURE STOCK";
 else if opts_grt^=. and shares_grt=. and (eq_max=. and eq_targ=. and eq_thres=.) 
   and (non_eq_max=. and non_eq_targ=. and non_eq_thres=.) 
   then granttype = "PURE OPTIONS";
 else if opts_grt=. and shares_grt=. and (eq_max^=. or eq_targ^=. or eq_thres^=.) and expric^=. 
   and (non_eq_max=. and non_eq_targ=. and non_eq_thres=.) 
   then granttype = "EIP OPTIONS";
  else if opts_grt=. and shares_grt=. and (eq_max^=. or eq_targ^=. or eq_thres^=.) and expric=.
   and (non_eq_max=. and non_eq_targ=. and non_eq_thres=.) 
   then granttype = "EIP STOCK";
  else if opts_grt=. and shares_grt=. and (eq_max=. and eq_targ=. and eq_thres=.)
   and (non_eq_max=. and non_eq_targ=. and non_eq_thres=.) 
   then granttype = "ALL MISSING";
   else granttype = "MULTIPLE";
 run;
 
 proc print data=planbasedawards1;
 * var coperol year eq_max eq_targ eq_thres expric non_eq_max non_eq_targ non_eq_thres;
 * where opts_grt=. and shares_grt=. and (eq_max^=. or eq_targ^=. or eq_thres^=.) and expric^=.;
 where coperol=11664;
* where coperol=16285;
  run;

 proc freq data=planbasedawards1;
 tables granttype;
  title "frequency of different types of plan based awards";
 run;
**/

*================================;
* READ DATA ON OUTSTANDING AWARDS;
*================================;

* this dataset has individual executive awards outstanding at end of fiscal year;
* should have no duplicates by coperol, award number (outawdnum),and year;
* data only available after 2006;
* appears to have a lot of the same information as in coperol;
data outstandingawards;
 set common.outstandingawards;
 coperol=co_per_rol;
 opts_vested_num = opts_unex_exer;
 opts_unvested_num = opts_unex_unexer;
 opts_unearned_num = opts_unex_unearn ;
* round off the expric since this is sometimes rounded off in outstanding
 talbe but not in the planbased table, which creates problems with the merge;
  expric = round(expric,0.01);
 keep coperol year eip_shrs_unvest_num eip_shrs_unvest_val expric
  opts_vested_num opts_unvested_num opts_unearned_num outawdnum
   shrs_unvest_num shrs_unvest_val exdate;
 proc sort nodupkey;
   by coperol year outawdnum;
proc contents data=common.outstandingawards;
   title "information on dataset outstandingawards";
run;

proc print data=outstandingawards;
 where coperol=16285;
 var year eip_shrs_unvest_num eip_shrs_unvest_val expric
  opts_vested_num opts_unvested_num opts_unearned_num outawdnum
   shrs_unvest_num shrs_unvest_val exdate;
   title "printing ibm post-2006 data on outstanding awards";
   title2 "ibm changed reporting format in 2006 itself";
 run;

proc means data=outstandingawards n mean p1 p25 p50 p75 p99;
 var opts_unearned_num;
 where opts_unearned_num>0 and opts_unearned_num^=.;
run;

* we merge with outstanding awards to find the delta, vega, and value as of fiscal year end;
data excomp13_subset;
 set excomp13;
  keep  coperol year shrown bs_yield fyenddt
    sigmasq sigma prccf  year 
   oneyr twoyr threeyr fouryr fiveyr sixyr sevenyr eightyr nineyr tenyr;
proc sort nodupkey;
 by coperol year;
run;

* now we need to compute the deltas of the portfolio as of year end;
* outstandingawards does not have the data to compute risk-free rate etc,
so we merge these in from excomp12_subset;

proc sql;
 create table outstandingawards1 as
  select *
  from outstandingawards  x left join excomp13_subset y
 on x.coperol=y.coperol and x.year=y.year;

 proc contents data=outstandingawards1;
 run;

 data outstandingawards1a; 
  set outstandingawards1;

*if we want a 70% haircut, we need to apply it here for post-2006;
  maturity_yearend = (exdate-fyenddt)/365;

 if round(maturity_yearend,1) =1  then rfyearend=oneyr;
 if round(maturity_yearend,1) =2  then rfyearend=twoyr;
 if round(maturity_yearend,1) =3  then rfyearend=threeyr;
 if round(maturity_yearend,1) =4  then rfyearend=fouryr;
 if round(maturity_yearend,1) =5  then rfyearend=fiveyr;
 if round(maturity_yearend,1) =6  then rfyearend=sixyr;
 if round(maturity_yearend,1) =7  then rfyearend=sevenyr;
 if round(maturity_yearend,1) =8  then rfyearend=eightyr;
 if round(maturity_yearend,1) =9  then rfyearend=nineyr;
 if round(maturity_yearend,1)=10  then rfyearend=tenyr;

* sometimes maturity is greater than 10;
 if (maturity_yearend^=. and round(maturity_yearend,1)>10) then rfyearend=tenyr;

  Xc = expric;

* calculate B-S values;

 Zc_yearend= (log(prccf/Xc)+ maturity_yearend*(rfyearend-bs_yield+sigmasq/2))/(sigma*sqrt( maturity_yearend));
 if maturity_yearend=. then Zc_yearend=.;


 * computing deltas;
 Sunvest_yearend= exp(-bs_yield*maturity_yearend)*probnorm(Zc_yearend)*opts_unvested_num*prccf/100;
  if Zc_yearend=. then Sunvest_yearend=.;
  if opts_unvested_num=. or opts_unvested_num=0 then Sunvest_yearend=0;

 Svest_yearend= exp(-bs_yield*maturity_yearend)*probnorm(Zc_yearend)*opts_vested_num*prccf/100;
  if Zc_yearend=. then Svest_yearend=.;
  if opts_vested_num=. or opts_vested_num=0 then Svest_yearend=0;

* Black Scholes value of options at yearend;
 Vunvest_yearend= opts_unvested_num*(prccf*exp(-bs_yield* maturity_yearend)*probnorm(Zc_yearend)
    -Xc*exp(-rfyearend*maturity_yearend)*probnorm(Zc_yearend-sigma*sqrt( maturity_yearend)));
  if Zc_yearend=. then Vunvest_yearend=.;
  if opts_unvested_num=. or opts_unvested_num=0 then Vunvest_yearend=0;

 Vvest_yearend= opts_vested_num*(prccf*exp(-bs_yield* maturity_yearend)*probnorm(Zc_yearend)
    -Xc*exp(-rfyearend*maturity_yearend)*probnorm(Zc_yearend-sigma*sqrt( maturity_yearend)));
  if Zc_yearend=. then Vvest_yearend=.;
  if opts_vested_num=. or opts_vested_num=0 then Vvest_yearend=0;

* sensitivity with respect to a 0.01 change in stock return volatility;
* see core and guay appendix A;
  if Zc_yearend^=. then do;
   Runvest_yearend = exp(-bs_yield* maturity_yearend)*PDF('normal',Zc_yearend,0,1)*prccf*sqrt( maturity_yearend)*0.01*opts_unvested_num;
  end;
  if  Zc_yearend=. or maturity_yearend=. then Runvest_yearend=.;
  if opts_unvested_num=. or opts_unvested_num=0 then Runvest_yearend=0;

  if Zc_yearend^=. then do;
   Rvest_yearend = exp(-bs_yield* maturity_yearend)*PDF('normal',Zc_yearend,0,1)*prccf*sqrt( maturity_yearend)*0.01*opts_vested_num;
  end;
  if  Zc_yearend=. or maturity_yearend=. then Rvest_yearend=.;
  if opts_vested_num=. or opts_vested_num=0 then Rvest_yearend=0;
 
 drop oneyr twoyr threeyr fouryr fiveyr sixyr
  sevenyr eightyr nineyr tenyr ;
 run;
/**
 proc print data=outstandingawards1a;
  where coperol=16285 and year>1997;
  var year outawdnum Zc_yearend Sunvest_yearend Runvest_yearend Vunvest_yearend 
    Svest_yearend Rvest_yearend Vvest_yearend maturity_yearend; 
	title "calculating post-2006 numbers for ibm palmisano";
  run;
proc print data=outstandingawards1a;
 where coperol=668 and year=2009 and maturity_yearend^=.;
  var year sigma rfyearend bs_yield maturity_yearend; 
 title "print of BS estimates for Molson Coors";
 title2 "per proxy, company uses 28% volatility, 2.46% risk free rate, 2.29% dividend yield, and 5-7 year maturity";
 run;
 **/

* now aggregate from tranche level to coperol-year level;

proc sql;
 create table outstandingawards2
  as select *, 
    sum(opts_unvested_num) as sumunvested_num_opts,
    sum(opts_vested_num) as sumvested_num_opts,
    sum(Sunvest_yearend) as sumSunvest_yearend,
    sum(Svest_yearend) as sumSvest_yearend,
    sum(Runvest_yearend) as sumRunvest_yearend,
    sum(Rvest_yearend) as sumRvest_yearend,
    sum(Vunvest_yearend) as sumVunvest_yearend,
    sum(Vvest_yearend) as sumVvest_yearend
  from outstandingawards1a
group by coperol,year;


data outstandingawards3;
  set outstandingawards2;
   if sumunvested_num_opts=0 or sumunvested_num_opts=. then do;
     sumSunvest_yearend=0; 
     sumRunvest_yearend=0; 
     sumVunvest_yearend=0; 
   end;
   if sumvested_num_opts=0 then do;
     sumSvest_yearend=0; 
     sumRvest_yearend=0; 
     sumVvest_yearend=0; 
   end;

* vega;
  newRopt = sumRvest_yearend + sumRunvest_yearend;

* sensitivity of shareholdings;
 Sshr= shrown*prccf/100;

* value of shareholdings;
 newVshr= shrown*prccf;
 newVportfolio_yearend =  sumVvest_yearend + sumVunvest_yearend + newVshr;

* delta;
  newdelta = sumSvest_yearend + sumSunvest_yearend + Sshr;

drop opts_vested_num opts_unvested_num shrown;
* at this point, we are going to keep only 1 observation per executive year;
* since we have already added up the values of current year's grants, that 
 is all we need to keep;

* the following sort elimates 290,425 obs;
* these are  executives reporting multiple stock grants outstanding at the end of each year;
proc sort nodupkey ;
 by coperol year;
run;

/**
proc print data=outstandingawards3;
 where coperol=16285;
 var year newdelta newRopt sumSunvest_yearend sumSvest_yearend
     sumRunvest_yearend sumRvest_yearend 
   sumVunvest_yearend sumVvest_yearend 
    newVshr  newVportfolio_yearend;
	title "calculating post-2006 numbers for ibm palmisano";
run;
**/
* MERGING THEM TOGETHER;

proc sql; 
 create table excomp17 as
  select *
  from excomp16 x left join outstandingawards3 y
  on x.coperol=y.coperol and x.year=y.year;
  
data excomp18;
 set excomp17;
 if old_datafmt_flag=0 then do;
   delta=newdelta;
   Ropt = newRopt;
   Vportfolio_yearend = newVportfolio_yearend;
 end;
 firm_related_wealth=Vportfolio_yearend;
 vega=ropt;
 keep coperol year delta vega firm_related_wealth old_datafmt_flag tdc1 gvkey exec_fullname;
* keep gvkey coperol year delta vega firm_related_wealth ;
 proc sort nodupkey;
  by coperol year;
proc means n mean median min max;
title "Final dataset";
 run;

/**
 proc print data=excomp18;
  var coperol year firm_related_wealth;
  where coperol=984;
  title "printing for GE -WELCH to check with Sundaram-Yermack table 1";
   run;

proc sort data=excomp18;
 by year;
 proc means noprint data=excomp18;
  by year;
  var delta vega firm_related_wealth ;
  output out=out1 
  mean=delta vega firm_related_wealth
  n=ndelta nvega  nfirm_related_wealth ;
run;

proc print data=out1;
title "Key variables";
run;
**/
 
data common.deltavega_2013 ;
 set excomp18;
keep gvkey coperol year delta vega firm_related_wealth ;
 run;

