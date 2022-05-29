clear all
set matsize 1000


*Filepath for Lauren's computer: 
cap cd "C:\Users\17036\Dropbox\Personal Document Backup\Booth\First Year\Booth Metrics\Spring\zombie-boards-data"

/*This program generates the datasets used in Huang and Hilary (2018)*/

*============================================================================
*Generate Regression Dataset

/*[LM] Additional governance variables needed for extension:
labylw - limit ability to amend bylaws
supermajor - supermajority required for mergers
charter_amend_votepct - vote % required to amend charter (need to turn this into a variable reflecting supermajorities only)
gparachute - golden parachutes
ppill - poison pills */
global eindex = "cboard labylw lachtr supermajor gparachute ppill"
*============================================================================
	
/*Process and Merge Variables from IRRC governance dataset*/
	use "rawdata/govlegacy19902006.dta",clear /*governance legacy file*/
	keep cn6 year $eindex
	tempfile temp2
	save `temp2',replace
	
	use "rawdata/gov20072015.dta",clear
	gen cn6=substr(cusip,1,6)
	gen supermajor = supermajor_pcnt > 51 // starts at 60%
	keep cn6 year $eindex
	replace cboard=upper(cboard)
	gen I=(cboard=="YES")
	drop cboard
	rename I cboard
	foreach var of varlist ppill gparachute labylw lachtr {
		ren `var' `var'_str
		gen `var' = `var'_str == "YES"
	}
	replace labylw = . if labylw_str == "NA"
	keep cn6 year $eindex
	append using `temp2'

	drop if cn6==""
	duplicates drop
	tempfile gov
	save `gov',replace

	use "dataset_irrc.dta",clear /*processed under ProcessIRRC_SDCCode.do*/
	merge m:1 cn6 year using `gov', keep(1 3) nogen
	replace cboard=0 if cboard==.
	sort gvkey fyear
	by gvkey fyear: egen max=max(cboard)
	drop cboard
	rename max cboard
	save dataset,replace
	

/*Process and Merge Variables from Execucomp*/
	use "rawdata/ceo_Execucomp19922010",clear /*execucomp for CEO*/
	keep if ceoann == "CEO"
	rename year fyear
	sort gvkey fyear	

	rename exec_fullname ceo_fullname
	rename age ceoage
	gen ceotenure = fyear - int(becameceo/10000)
		replace ceotenure = . if ceotenure < 0
	*replace ceotenure=ceotenure-1
	
	keep gvkey fyear execid ceo_fullname shrown_excl_opts ceotenure ceoage /*I_ceofounder*/ tdc1 
	tempfile execucomp
	save `execucomp',replace
		
	use dataset,clear
	merge m:1 gvkey fyear using `execucomp', keep(1 3) nogen
	gen ceopctowned=shrown_excl_opts/(csho*1000)*100
	replace ceopctowned=100 if ceopctowned>100&ceopctowned<.
	replace ceopctowned=0 if ceopctowned==. 
	save dataset,replace

		
/*Process and Merge Number of Segment from Compustat Segment Data*/
	use "rawdata/compustatsegment19762010.dta",clear /*compustat segment file*/
	keep gvkey datadate stype snms sic*
	keep if stype=="BUSSEG"
	drop stype
	destring(gvkey),replace
	duplicates drop
	drop if snms==""
	drop if sics1==.
	compress
	sort gvkey datadate sics1
	by gvkey datadate sics1: keep if _n==1	
	by gvkey datadate: gen nseg=_N
	keep gvkey datadate nseg
	duplicates drop
	sort gvkey datadate
	by gvkey: gen nseg_lag=nseg[_n-1]	
	tempfile nseg
	save `nseg',replace
	
	use dataset,clear
	merge m:1 gvkey datadate using `nseg'
	drop if _merge==2
	drop _merge
	replace nseg_lag=0 if nseg_lag==.
	replace nseg=0 if nseg==.
	save dataset,replace
	
/*Generate Variables from Compustat*/	
	/*use "rawdata/compustat19502012.dta",clear
	keep gvkey fyear gdwl
	duplicates drop
	sort gvkey fyear
	save temp1,replace
	
	use "rawdata/crspcomp19502010_prc0mth.dta",clear /*crsp compustat merged file*/
	merge 1:1 gvkey fyear using "C:\Work\WRDS\original data\compustat19502010_disclosure" /*merge additional compustat variables*/
	drop if _merge==2
	drop _merge
	merge 1:1 gvkey fyear using temp1
	drop if _merge==2
	drop _merge
	*/
	keep gvkey fyear sale gdwl txdb itcb csho prcc_f at dlc dltt ni xido at ceq capxv lt re act lct
	duplicates drop
	sort gvkey fyear
	tsset gvkey fyear	
	replace gdwl=0 if gdwl==.
	replace txdb=0 if txdb==.
	replace itcb=0 if itcb==.	
	gen mkvalt=csho*prcc_f	
	gen salegrowth=(sale-l.sale)/l.sale
	gen tobin=(at+mkvalt-ceq-txdb)/at
	gen tobin_lag = l.tobin
	gen leverage=(dlc+dltt)/at
	gen roa_g=(ni+xido)/l.at 
	gen capex_at=capxv/at
	gen gdwl_at=gdwl/at	
	gen roa_g_lag=l.roa_g
	gen salegrowth_lag=l.salegrowth
	gen leverage_lag=l.leverage
	gen capex_at_lag=l.capex_at
	gen gdwl_at_lag=l.gdwl_at
	gen sale_lag=l.sale	
	gen ln_roa_g_lag=ln(1+roa_g_lag)*100
	gen ln_roa_g=ln(1+roa_g)*100
	gen ln_sale_lag=ln(sale_lag)
	gen debtiss=d.lt/l.lt
	gen equityiss=(d.ceq+d.txdb-d.re)/at
	gen curr=act/lct 
	gen debtiss_lag=l.debtiss
	gen equityiss_lag=l.equityiss
	gen curr_lag=l.curr
	keep gvkey fyear tobin tobin_lag ln_roa_g salegrowth_lag ln_roa_g_lag  gdwl_at_lag leverage_lag  ln_sale_lag capex_at_lag debtiss_lag equityiss_lag curr_lag
	tempfile temp1
	save `temp1',replace

	use dataset,clear
	merge m:1 gvkey fyear using `temp1'
	drop if _merge==2
	drop _merge
	save dataset,replace


/*Process and Merge SDC data*/
	use "sdc_processed",clear /*processed under ProcessIRRC_SDCCode.do*/
	keep acusip fyear dealnumber
	ren acusip cusip // acquiror cusip
	duplicates drop
	sort cusip fyear
	by cusip fyear: gen num_acq=_N
	keep cusip fyear num_acq
	duplicates drop
	gen I_acq=1
	egen tempid = group(cusip) // cusip string so can't be used w tsset
	tsset tempid fyear
	gen num_acq_lag=l.num_acq
	gen I_acq_lag=l.I_acq
	tempfile temp
	save `temp',replace
	
	use dataset,clear
	merge m:1 cusip fyear using `temp'
	drop if _merge==2
	drop _merge
	local list="I_acq num_acq I_acq_lag num_acq_lag"
	foreach var of local list{
		replace `var'=0 if `var'==.
	}
	save dataset,replace
	
/*Merge Firm Age*/	
	/*
	We calculate firm age from the year a firm is founded. 
	Founding year is obtained from three sources:
	a)We start with Jay Ritter’s website
	b)If the information is not available there, we do a manual search from the Capital IQ and Hoover to identify founding year. 
	c)If it is still not available, we use first year when a firm appears in the CRSP database.	
	*/
	
	*-- Lauren's firm age codes ---*
	*import excel "rawdata/IPO2609.xls", clear // punting on this for now, just doing (c)
	use "rawdata/crspcomp19502010_prc0mth.dta", clear
		keep gvkey fyear
		bys gvkey: egen minyr = min(fyear)
		gen firmage = fyear - minyr + 1
		gen ln_firmage = ln(firmage)
		duplicates drop
		tempfile firmage
		save `firmage', replace
	*------------------------------*
	
	use dataset,clear
	merge m:1 gvkey fyear using `firmage', keep(1 3) nogen
	save dataset,replace


/*Merge IPO Spin Off dummy from SDC*/
	/*use dataset,clear
	merge m:1  year cn6 using ipo 
	drop if _merge==2
	drop _merge
	merge m:1 year cn6 using spinoff 
	drop if _merge==2
	drop _merge
	replace I_spinoff_lag=0 if I_spinoff_lag==.
	replace I_ipo_lag=0 if I_ipo_lag==.
	gen I_ipospin_lag=(I_spinoff_lag==1|I_ipo_lag==1)
	save dataset,replace*/
	
/*Merge stock return and volatility*/
	use "rawdata/crsp_ret_date.dta" ,clear
	gen fyear = int(date/10000)
	gen cusip6 = substr(cusip,1,6)
	drop if inlist(ret, "", "C", "B")
	destring ret, replace
	bys cusip6 fyear: egen stdret=sd(ret)
	gen lnret=ln(1+ret)
	bys cusip6 fyear: egen sum=sum(lnret)
	gen ret_fiscal=exp(sum)-1
	keep cusip6 fyear stdret ret_fiscal 
	duplicates drop
	egen tempid = group(cusip6)
	tsset tempid fyear
	gen stdret_lag=l.stdret
	gen ret_fiscal_lag=l.ret_fiscal
	drop tempid
	tempfile temp
	save `temp',replace
	
	use dataset,clear
	gen cusip6 = substr(cusip,1,6)
	merge m:1 cusip6 fyear using `temp'
	drop if _merge==2
	drop _merge	
	gen ln_stdret_lag=ln(stdret_lag)
	replace ln_stdret_lag=0 if stdret_lag==0
	gen lnret_fiscal_lag=ln(1+ret_fiscal_lag)
	save dataset,replace
		
*Winsorizing	
	use dataset,clear
	destring(execid),replace	
	gen sich2=floor(sich/100)
	gen indyear=sich2*fyear

	*impose sample period restriction
	keep if fyear>=1998&fyear<=2010
	
	*define controls
	global control2a="tenure_sd_outsider ceoage ceotenure  ceopctowned  I_ceochair cboard I_boardind num_busy I_interlock num_dir salegrowth_lag ln_roa_g_lag  ln_firmage lnret_fiscal_lag"
	*** Removed I_ceofounder from control2a
	*** Moved cboard out of this global and on its own in the regressions to compare w/ extension
	global control2b="num_acq_lag gdwl_at_lag leverage_lag nseg_lag ln_sale_lag capex_at_lag ln_stdret_lag"
	*** Removed I_ipospin_lag from control2b
	global control="blau_gender blau_ethnic blau_age prop_outexec prop_born1920 prop_born1930 prop_born1940 prop_born1950 prop_born1960 equityiss_lag debtiss_lag curr_lag"
	*** Removed avgoutsider_voting, age_avg_outsider, prop_age70 from control
	
	local list="tobin ln_roa_g"
	foreach var of local list{
		centile `var',c (1 99)
		replace `var'=r(c_1) if `var'<r(c_1)&`var'<.
		replace `var'=r(c_2) if `var'>r(c_2)&`var'<.
	
	}
	foreach var of global control2a{
		centile `var',c (1 99)
		replace `var'=r(c_1) if `var'<r(c_1)&`var'<.
		replace `var'=r(c_2) if `var'>r(c_2)&`var'<.
	}	
	foreach var of global control2b{
		centile `var',c (1 99)
		replace `var'=r(c_1) if `var'<r(c_1)&`var'<.
		replace `var'=r(c_2) if `var'>r(c_2)&`var'<.
	}	
	/*foreach var of global control{
		centile `var',c (1 99)
		replace `var'=r(c_1) if `var'<r(c_1)&`var'<.
		replace `var'=r(c_2) if `var'>r(c_2)&`var'<.
	}	*/
	
*--- Label Variables ---*
lab var tobin "Tobin's Q"
lab var ln_roa_g "ROA"
lab var tobin_lag "Lag Tobin's Q"
lab var ln_roa_g_lag "Lag ROA"
lab var tenure_avg_outsider "Tenure"
lab var tenure2_avg_outsider "Tenure^2"
lab var cboard "Staggered Board"
lab var labylw "Lim. Ab. Amend Bylaws"
lab var supermajor "Supermaj. for Mergers"
lab var lachtr "Lim. Ab. Amend Charter"
lab var gparachute "Golden Parachute"
lab var ppill "Poison Pill"

	save dataset,replace

*====================================================================================
*Replicated Results	
*====================================================================================
est clear

*Baseline Specification
	eststo m1a: xi:areg tobin tenure_avg_outsider tenure2_avg_outsider cboard $control2a  $control2b i.fyear,abs(gvkey) cluster(gvkey)
	*outreg2	using table3.xls,excel drop (_I*) tstat  aster(tstat) bdec(3) tdec(2) replace 	
	eststo m1b: xi:areg ln_roa_g tenure_avg_outsider tenure2_avg_outsider cboard  $control2a  $control2b i.fyear,abs(gvkey) cluster(gvkey)
	*outreg2	using table3.xls,excel drop (_I*) tstat  aster(tstat) bdec(3) tdec(2) append	

*Include Additional Firm-Year Controls	
	eststo m2a: xi:areg tobin tenure_avg_outsider tenure2_avg_outsider cboard  $control2a  $control2b $control  i.fyear,abs(gvkey) cluster(gvkey)
	*outreg2	using table3.xls,excel drop (_I*) tstat  aster(tstat) bdec(3) tdec(2) replace	
	eststo m2b: xi:areg ln_roa_g tenure_avg_outsider tenure2_avg_outsider cboard  $control2a  $control2b $control i.fyear,abs(gvkey) cluster(gvkey)
	*outreg2	using table3.xls,excel drop (_I*) tstat  aster(tstat) bdec(3) tdec(2) append	

	
*Include Firm, Year and CEO Fixed Effects
	eststo m3a: xi: reghdfe tobin tenure_avg_outsider tenure2_avg_outsider cboard   $control2a  $control2b, abs(fyear gvkey execid) cluster(gvkey)
	*outreg2	using table3.xls,excel drop (_I*) tstat  aster(tstat) bdec(3) tdec(2) replace
	eststo m3b: xi: reghdfe ln_roa_g tenure_avg_outsider tenure2_avg_outsider cboard   $control2a  $control2b, abs(fyear gvkey execid) cluster(gvkey)
	*outreg2	using table3.xls,excel drop (_I*) tstat  aster(tstat) bdec(3) tdec(2) append
	
	
esttab m* using table2.tex, label replace booktabs se ///
	keep(tenure_avg_outsider tenure2_avg_outsider cboard ln_roa_g_lag) ///
	title(Replication of Original Paper\label{tab1})
	
*====================================================================================
*Extension Results	
*====================================================================================

est clear

*baseline
eststo m1a: reg tobin tenure_avg_outsider tenure2_avg_outsider $eindex tobin_lag $control2a  $control2b, cluster(gvkey)
eststo m1b: reg ln_roa_g tenure_avg_outsider tenure2_avg_outsider $eindex tobin_lag $control2a  $control2b, cluster(gvkey)

*additional controls
eststo m2a: reg tobin tenure_avg_outsider tenure2_avg_outsider $eindex tobin_lag $control2a  $control2b $control, cluster(gvkey)
eststo m2b: reg ln_roa_g tenure_avg_outsider tenure2_avg_outsider $eindex tobin_lag $control2a  $control2b $control, cluster(gvkey)


esttab m* using table2B.tex, label replace booktabs se ///
	keep( tenure_avg_outsider tenure2_avg_outsider $eindex tobin_lag ln_roa_g_lag) ///
	title(Results from Extension\label{tab2})

	

/*
*Include industry*year joint Fixed Effect	
	use dataset,clear
	xi:reghdfe tobin tenure_avg_outsider tenure2_avg_outsider   $control2a $control2b ,abs(gvkey indyear) cluster(gvkey)
	outreg2	using table3.xls,excel drop (_I*) tstat  aster(tstat) bdec(3) tdec(2) replace 	
	xi:reghdfe ln_roa_g tenure_avg_outsider tenure2_avg_outsider  $control2a $control2b ,abs(gvkey indyear) cluster(gvkey)
	outreg2	using table3.xls,excel drop (_I*) tstat  aster(tstat) bdec(3) tdec(2) append		
	
*No Firm Fixed Effect
	use dataset,clear
	xi: reg tobin tenure_avg_outsider tenure2_avg_outsider   $control2a  $control2b i.fyear, cluster(gvkey)
	outreg2	using table3.xls,excel drop (_I*) tstat  aster(tstat) bdec(3) tdec(2) replace		
	xi: reg ln_roa_g tenure_avg_outsider tenure2_avg_outsider  $control2a  $control2b  i.fyear, cluster(gvkey)
	outreg2	using table3.xls,excel drop (_I*) tstat  aster(tstat) bdec(3) tdec(2) append	
	
*Non-parametric Plot
	use dataset,clear
	foreach var of global control2a {
		drop if `var'==.
	}
	foreach var of global control2b {
		drop if `var'==.
	}
	drop if tobin==.
	xi:areg tobin $control2a $control2b  i.fyear,abs(gvkey) cluster(gvkey)
	predict res_tobin,residual
	
	graph twoway lowess res_tobin  tenure_avg_outsider, mean xlabel(0(2)30) xtitle("Board Tenure") ytitle("Residual Tobin's Q") title("Residual Tobin's Q and Board Tenure")
*/