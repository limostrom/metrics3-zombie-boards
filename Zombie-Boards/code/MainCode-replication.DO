clear all
set matsize 1000
cd "C:\Work\tenure\Program\JAR Code\"

/*This program generates the datasets used in Huang and Hilary (2018)*/

*============================================================================
*Generate Regression Dataset
*============================================================================
	
/*Process and Merge Variables from IRRC governance dataset*/
	use "C:\Work\WRDS\Original data\govlegacy19902006.dta",clear /*governance legacy file*/
	keep cn6 year cboard
	save temp2,replace
	
	use "C:\Work\WRDS\Original data\gov20072015.dta",clear
	gen cn6=substr(cusip,1,6) 
	keep cn6 year cboard
	replace cboard=upper(cboard)
	gen I=(cboard=="YES")
	drop cboard
	rename I cboard
	keep cn6 year cboard
	append using temp2
	drop if cn6==""
	duplicates drop
	save cboard,replace

	use "C:\Work\tenure\Program\JAR Code\dataset_irrc.dta",clear /*processed under ProcessIRRC_SDCCode.do*/
	merge m:1 cn6 year using cboard
	drop if _merge==2
	drop _merge
	replace cboard=0 if cboard==.
	sort gvkey fyear
	by gvkey fyear: egen max=max(cboard)
	drop cboard
	rename max cboard
	save dataset,replace
	

/*Process and Merge Variables from Execucomp*/
	use "c:\work\wrds\Original data\ceo_Execucomp19922010",clear /*execucomp for CEO*/
	rename year fyear
	sort gvkey fyear	

	rename fullname ceo_fullname
	rename age ceoage
	replace ceotenure=ceotenure-1
	
	keep gvkey fyear execid ceo_fullname shrown_excl_opts ceotenure ceoage I_ceofounder tdc1 
	save execucomp,replace
		
	use dataset,clear
	merge m:1 gvkey fyear using execucomp
	drop if _merge==2 
	drop _merge
	gen ceopctowned=shrown_excl_opts/(csho*1000)*100
	replace ceopctowned=100 if ceopctowned>100&ceopctowned<.
	replace ceopctowned=0 if ceopctowned==. 
	save dataset,replace

		
/*Process and Merge Number of Segment from Compustat Segment Data*/
	use "C:\Work\WRDS\Original data\compustatsegment19762010.dta",clear /*compustat segment file*/
	keep gvkey datadate stype snms sic*
	keep if stype=="BUSSEG"
	drop stype
	destring(gvkey),replace
	duplicates drop
	drop if snms==""
	drop if sics1==""
	compress
	sort gvkey datadate sics1
	by gvkey datadate sics1: keep if _n==1	
	by gvkey datadate: gen nseg=_N
	keep gvkey datadate nseg
	duplicates drop
	sort gvkey datadate
	by gvkey: gen nseg_lag=nseg[_n-1]	
	save nseg,replace
	
	use dataset,clear
	merge m:1 gvkey datadate using nseg
	drop if _merge==2
	drop _merge
	replace nseg_lag=0 if nseg_lag==.
	replace nseg=0 if nseg==.
	save dataset,replace
	
/*Generate Variables from Compustat*/	
	use "C:\Work\WRDS\original data\compustat19502012.dta",clear
	keep gvkey fyear gdwl
	duplicates drop
	sort gvkey fyear
	save temp1,replace
	
	use "C:\Work\WRDS\Original data\crspcomp19502010_prc0mth.dta",clear /*crsp compustat merged file*/
	merge 1:1 gvkey fyear using "C:\Work\WRDS\original data\compustat19502010_disclosure" /*merge additional compustat variables*/
	drop if _merge==2
	drop _merge
	merge 1:1 gvkey fyear using temp1
	drop if _merge==2
	drop _merge
	
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
	keep gvkey fyear tobin ln_roa_g salegrowth_lag ln_roa_g_lag  gdwl_at_lag leverage_lag  ln_sale_lag capex_at_lag debtiss_lag equityiss_lag curr_lag
	save temp1,replace

	use dataset,clear
	merge m:1 gvkey fyear using temp1
	drop if _merge==2
	drop _merge
	save dataset,replace


/*Process and Merge SDC data*/
	use "C:\Work\tenure\data\dataset_ma",clear /*processed under ProcessIRRC_SDCCode.do*/
	keep gvkey fyear dealnumber
	duplicates drop
	sort gvkey fyear
	by gvkey fyear: gen num_acq=_N
	keep gvkey fyear num_acq
	duplicates drop
	gen I_acq=1
	tsset gvkey fyear
	gen num_acq_lag=l.num_acq
	gen I_acq_lag=l.I_acq
	save temp,replace
	
	use dataset,clear
	merge m:1 gvkey fyear using temp
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

	use dataset,clear
	merge m:1 gvkey fyear using firmage
	drop if _merge==2
	drop _merge
	save dataset,replace


/*Merge IPO Spin Off dummy from SDC*/
	use dataset,clear
	merge m:1  year cn6 using ipo 
	drop if _merge==2
	drop _merge
	merge m:1 year cn6 using spinoff 
	drop if _merge==2
	drop _merge
	replace I_spinoff_lag=0 if I_spinoff_lag==.
	replace I_ipo_lag=0 if I_ipo_lag==.
	gen I_ipospin_lag=(I_spinoff_lag==1|I_ipo_lag==1)
	save dataset,replace
	
/*Merge stock return and volatility*/
	use "C:\Work\WRDS\Cleaned data\ret_fiscal_date.dta" ,clear
	sort gvkey fyear date
	by gvkey fyear: egen stdret=sd(ret)
	gen lnret=ln(1+ret)
	by gvkey fyear: egen sum=sum(lnret)
	gen ret_fiscal=exp(sum)-1
	keep gvkey fyear stdret ret_fiscal 
	duplicates drop
	tsset gvkey fyear
	gen stdret_lag=l.stdret
	gen ret_fiscal_lag=l.ret_fiscal
	save temp,replace
	
	use dataset,clear
	merge m:1 gvkey fyear using temp
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
	global control2a="tenure_sd_outsider ceoage ceotenure  ceopctowned I_ceofounder I_ceochair cboard I_boardind num_busy I_interlock num_dir salegrowth_lag ln_roa_g_lag lnret_fiscal_lag ln_firmage"		
	global control2b="num_acq_lag gdwl_at_lag leverage_lag nseg_lag ln_sale_lag capex_at_lag ln_stdret_lag I_ipospin_lag"		
	global control="blau_gender blau_ethnic blau_age avgoutsider_voting prop_age70 age_avg_outsider prop_outexec prop_born1920 prop_born1930 prop_born1940 prop_born1950 prop_born1960 equityiss_lag debtiss_lag curr_lag"
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
	foreach var of global control{
		centile `var',c (1 99)
		replace `var'=r(c_1) if `var'<r(c_1)&`var'<.
		replace `var'=r(c_2) if `var'>r(c_2)&`var'<.
	}	
	save dataset,replace
	

*====================================================================================
*Analyses	
*====================================================================================

*Baseline Specification
	use dataset,clear
	xi:areg tobin tenure_avg_outsider tenure2_avg_outsider $control2a  $control2b i.fyear,abs(gvkey) cluster(gvkey)
	outreg2	using table3.xls,excel drop (_I*) tstat  aster(tstat) bdec(3) tdec(2) replace 	
	xi:areg ln_roa_g tenure_avg_outsider tenure2_avg_outsider   $control2a  $control2b i.fyear,abs(gvkey) cluster(gvkey)
	outreg2	using table3.xls,excel drop (_I*) tstat  aster(tstat) bdec(3) tdec(2) append	

*Include Additional Firm-Year Controls	
	use dataset,clear
	xi:areg tobin tenure_avg_outsider tenure2_avg_outsider   $control2a  $control2b $control  i.fyear,abs(gvkey) cluster(gvkey)
	outreg2	using table3.xls,excel drop (_I*) tstat  aster(tstat) bdec(3) tdec(2) replace	
	xi:areg ln_roa_g tenure_avg_outsider tenure2_avg_outsider  $control2a  $control2b $control i.fyear,abs(gvkey) cluster(gvkey)
	outreg2	using table3.xls,excel drop (_I*) tstat  aster(tstat) bdec(3) tdec(2) append	

	
*Include Firm, Year and CEO Fixed Effects
	use dataset,clear
	xi: reghdfe tobin tenure_avg_outsider tenure2_avg_outsider   $control2a  $control2b, abs(fyear gvkey execid) cluster(gvkey)
	outreg2	using table3.xls,excel drop (_I*) tstat  aster(tstat) bdec(3) tdec(2) replace
	xi: reghdfe ln_roa_g tenure_avg_outsider tenure2_avg_outsider   $control2a  $control2b, abs(fyear gvkey execid) cluster(gvkey)
	outreg2	using table3.xls,excel drop (_I*) tstat  aster(tstat) bdec(3) tdec(2) append	
	

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
