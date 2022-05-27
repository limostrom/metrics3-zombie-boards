clear all
set type double
set matsize 800
pause on

*Filepath for Lauren's computer: 
cd "C:\Users\17036\Dropbox\Personal Document Backup\Booth\First Year\Booth Metrics\Spring\zombie-boards-data"

*Filepath for Francisco:
cap cd "/Users/francisco/Dropbox/zombie-boards-data"

/*This program generates the datasets used in Huang and Hilary (2018)*/

*=======================================================================
/*Process IRRC Director dataset*/
*=======================================================================
	use "rawdata/crspcomp19502010_prc0mth.dta", clear
		drop linkprim liid linktype lpermno lpermco linkdt linkenddt
		duplicates drop
		tempfile crspcomp
		save `crspcomp', replace
		keep gvkey cusip fyear conm
		gen cusip6 = substr(cusip,1,6)
		keep cusip6 gvkey fyear conm
		duplicates drop
			drop if cusip6 == "37045V" & gvkey == 17197
		tempfile cusip_gvkeys
		save `cusip_gvkeys', replace
		
	use "rawdata/irrc_director20072010.dta", clear /* IRRC Director dataset */
	/*Some variables changed from 0/1 to Yes/No,
		audit_membership and comp_membership changed to member/chair/empty */
	drop employment_subsidiary
	foreach var of varlist ownless1 female attend_less75_pct otherlink succ_comm ///
			business_transaction charity designated employment_* financial_expert ///
			former_employee_yn interlocking prof_services_yn relative_yn ///
			{
		ren `var' `var'_str
		gen byte `var' = inlist(`var'_str,"Yes","member","chair")
	}
	append using "rawdata/irrc_directorlegacy19962006.dta", force /*IRRC director legacy dataset*/
	egen did = group(fullname)
		gen cusip6 = substr(cusip,1,6)
	ren year fyear
	merge m:1 cusip6 fyear using `cusip_gvkeys', keepus(gvkey) keep(1 3)
	ren fyear year
	
	sort gvkey meetingdate did
	replace outside_public_boards=0 if outside_public_boards==.
	gen I_outsider = primary_employer != name
	gen I_insider = primary_employer == name
	gen I_ind = primary_employer != name
	bys did year: egen n_directorships = count(meetingdate)
	gen I_busy = n_directorships > 3
		drop n_directorships
	gen ceochair = employment_ceo == 1 & employment_chairman == 1
	bys gvkey meetingdate: egen I_ceochair = max(ceochair)
		drop ceochair
	

	/*board tenure*/
	gen tenure=year-dirsince
	sort gvkey meetingdate did		
	gen temp=tenure if I_outsider==1
	by gvkey meetingdate: egen tenure_avg_outsider=mean(temp)
	by gvkey meetingdate: egen tenure_sd_outsider=sd(temp)
	drop temp


	/*Board Size, Independence and Busyness*/
	sort gvkey meetingdate did
	by gvkey meetingdate: gen num_dir=_N
	by gvkey meetingdate: egen num_ind=sum(I_ind)
	by gvkey meetingdate: egen num_busy=sum(I_busy)
	gen I_boardind=1 if num_ind/num_dir>0.5&num_ind<.&num_dir<.
	replace I_boardind=0 if I_boardind==.
	
	/*Board Interlock*/
	sort gvkey meetingdate did
	by gvkey meetingdate: egen max=max(interlocking)
	rename max I_interlock
	replace I_interlock=0 if I_interlock==.
	

	/*Blau Index for Gender*/
	by gvkey meetingdate: egen num_female=sum(female)
	gen prop_female=num_female/num_dir
	gen prop2_female=prop_female*prop_female
	gen prop2_male=(1-prop_female)*(1-prop_female)
	egen sumsq=rowtotal(prop2_female prop2_male),missing
	gen blau_gender=1-sumsq /*Blau Index for Gender*/
	drop sumsq prop2_female prop2_male num_female prop_female
	
	/*Blau Index for ethnicity*/
	replace ethnicity=upper(ethnicity)	
	gen ethnic_black=1 if ethnicity=="B"|ethnicity=="AFRICAN-AMERICAN"
	replace ethnic_black=0 if ethnic_black==.
	gen ethnic_unknown=1 if ethnicity=="U"|ethnicity=="UNKNOWN"|ethnicity=="NULL"|ethnicity==""
	replace ethnic_unknown=0 if ethnic_unknown==.
	gen ethnic_cauc=1 if ethnicity=="C"|ethnicity=="CAUCASIAN"
	replace ethnic_cauc=0 if ethnic_cauc==.
	gen ethnic_hisp=1 if ethnicity=="H"|ethnicity=="HISPANIC"
	replace ethnic_hisp=0 if ethnic_hisp==.
	gen ethnic_asia=1 if ethnicity=="ASIAN"
	replace ethnic_asia=0 if ethnic_asia==.
	gen ethnic_india=1 if ethnicity=="NATIVE AMERICAN"|ethnicity=="NAITVE AMERICAN/ALASKAN NATIVE"
	replace ethnic_india=0 if ethnic_india==.	
	local list="black unknown cauc hisp asia india"
	foreach var of local list{
		by gvkey meetingdate: egen num_`var'=sum(ethnic_`var')
		gen prop_`var'=num_`var'/num_dir
		gen prop2_`var'=prop_`var'*prop_`var'
	}
	egen sumsq=rowtotal(prop2_black prop2_unknown prop2_cauc prop2_hisp prop2_asia prop2_india),missing
	gen blau_ethnic=1-sumsq
	drop sumsq prop2_*
	
	
	/*Blau Index for Age*/
	gen birthyear=year(meetingdate)-age	
	gen born1910=1 if birthyear<1920
	replace born1910=0 if born1910==.	
	gen born1920=1 if birthyear>=1920 &birthyear<1930
	replace born1920=0 if born1920==.	
	gen born1930=1 if birthyear>=1930 &birthyear<1940
	replace born1930=0 if born1930==.	
	gen born1940=1 if birthyear>=1940 &birthyear<1950
	replace born1940=0 if born1940==.	
	gen born1950=1 if birthyear>=1950 &birthyear<1960
	replace born1950=0 if born1950==.
	gen born1960=1 if birthyear>=1960 &birthyear<1970
	replace born1960=0 if born1960==.
	gen born1970=1 if birthyear>=1970 
	replace born1970=0 if born1970==.

	local list="born1910 born1920 born1930 born1940 born1950 born1960 born1970"
	foreach var of local list{
		sort gvkey meetingdate did
		by gvkey meetingdate: egen num_`var'=sum(`var')
		gen prop_`var'=num_`var'/num_dir
		gen prop2_`var'=prop_`var'*prop_`var'
	}
	egen sumsq=rowtotal(prop2_born*),missing
	gen blau_age=1-sumsq /*Blau Index for age*/
	drop sumsq
	drop born1910 born1920 born1930 born1940 born1950 born1960 born1970 num_born1910 num_born1920 num_born1930 num_born1940 num_born1950 num_born1960 num_born1970
	drop prop2_born1910 prop2_born1920 prop2_born1930 prop2_born1940 prop2_born1950 prop2_born1960 prop2_born1970	

	/*%Outside executives*/
	gen I=1 if I_insider~=1&(employment_cfo==1|employment_coo==1|employment_evp==1|employment_svp==1|employment_treasurer==1|employment_vp==1)
	replace I=0 if I==.
	gen Iceo = 1 if I_insider != 1 & employment_ceo==1
	by gvkey meetingdate: egen num_outnonceoexec=sum(I)
	bys gvkey meetingdate: egen num_outceo=sum(Iceo)
	gen prop_outnonceoexec=num_outnonceoexec/num_dir
	gen prop_outceo = num_outceo/num_dir
	*rename prop_ceo prop_outceo // this doesn't work?
	gen prop_outexec=prop_outnonceoexec+prop_outceo
	drop I Iceo
	
	/*Director shareholding*/ /* this variable simply doesn't exist		
	replace non_employee_voting=0 if non_employee_voting==.
	by gvkey meetingdate: egen avgboard_voting=mean(non_employee_voting)
	gen I=non_employee_voting if I_outsider==1
	by gvkey meetingdate: egen avgoutsider_voting=mean(I)
	drop I
	*/
	
	keep gvkey year meetingdate  tenure_sd_outsider tenure_avg_outsider num_dir blau_gender blau_ethnic blau_age /*avgoutsider_voting prop_age70 age_avg_outsider*/ prop_outexec prop_born1920 prop_born1930 prop_born1940 prop_born1950 prop_born1960 I_ceochair I_boardind I_interlock num_busy
	duplicates drop 
	sort gvkey meetingdate
	
	/************ Set up merge file - FAR & LM ********************************
	General idea is that crsp data is annual and directors meetings are more
	so we want a map from the meetings to the right data. So my idea is tp grab
	the nearest prior date to the meeting
	
	When I ran the code below there were shockingly few duplicates and honestly
	nothing that seemed too too bad from a matching standpoint. I ended up just 
	keep the most recently updated observation.
	**************************************************************************/	
	snapshot erase _all
	snapshot save // 1
	* first extract unique gvkey - date pairs
	keep gvkey meetingdate /*year
	gen datadate = meetingdate*/
	/*rename year fyear*/
	duplicates drop
	snapshot save // 2
	
	* now get gvkey datadate pairs
	use "rawdata/crspcomp19502010_prc0mth.dta", clear
	keep gvkey datadate /*linkdt fyear*/
	duplicates drop
	tempfile gvkey_date
	save `gvkey_date', replace
	
	* merge on gvkey and keep mininmum difference 
	snapshot restore 2 
	joinby gvkey using `gvkey_date', unm(both)
		tostring meetingdate, replace
		tostring datadate, replace
		gen mtgdate_dt = date(meetingdate, "YMD")
			format mtgdate_dt %td
		gen datadate_dt = date(datadate, "YMD")
			format datadate_dt %td
		gen diff_meetdate_datadate = mtgdate_dt - datadate_dt
			*replace diff = -1 * diff if diff < 0 // maybe we want closest absolute distance?
			replace diff = . if diff < 0 // maybe we want financials available before the meeting?
			bys gvkey meetingdate: egen min_diff = min(diff)
			keep if diff_meetdate_datadate == min_diff & meetingdate != "."
			drop if diff_meetdate_datadate>720 /*drop if the closest match is >2 years apart */
		destring meetingdate, replace
		destring datadate, replace
	keep gvkey meetingdate datadate diff_meetdate_datadate
	save "meetingdate_datadate_match_LM", replace
			
	/*duplicates tag gvkey datadate, gen(tag)
	sort gvkey datadate linkdt 
	drop if (linkdt<linkdt[_n+1] & tag==1 & tag[_n+1]==1)
	keep gvkey meetingdate 
	save meetingdate_datadate_match_FAR, replace */
	
	snapshot restore 1

	/************ END OF FAR CODE *********************************************/
	
	merge 1:1 gvkey meetingdate using "meetingdate_datadate_match_LM"
		/*match meetingdate to datadate*/
	 * -> They don't actually include this dataset in the replication files so FAR and LM fixed it
	keep if _merge==3
	drop _merge 
	*save temp,replace
	
	merge m:1 gvkey datadate using `crspcomp'
	keep if _merge==3 
	drop _merge
	drop if diff_meetdate_datadate>720 /*drop if closest match is >2 years apart */
	drop if year<1998 /*drop pre-1998 data as dirsince information are largely missing */
	gen tenure2_avg_outsider=tenure_avg_outsider*tenure_avg_outsider
	
	gen cn6 = substr(cusip,1,6)
	save "dataset_irrc.dta",replace
	
	/*We have mannually cleaned the IRRC dataset to fix following issues
	1. The company identifier legacy_pps_id and cn6 is not one to one. manually process to get legacy_pps_id gvkey match
		We mannually checked company name and CEO name at firm-year against Execucomp, Proxy filing and Capital IQ		
	2. director ID for the same director might be changing over time/across firm or same ID could be assigned to different directors. 
		We manually create new did to ensure that each director is assigned an unique ID.				
	3. We Only keep firms with complete dirsince information for every director on board
	*/	
	
*=======================================================================
/*Process SDC dataset
(LM note: their management of this dataset is atrocious) */
*=======================================================================
	
	use "rawdata/sdc_ma19962010.dta",clear /*data from SDC download*/
	/* Dropping because these variables don't actually exist but we also don't need them
	gen acq_hightech=0 if AcquirHighTechCode=="0"
	replace acq_hightech=1 if AcquirHighTechCode~="0"
	drop AcquirHighTechCode
	
	gen target_hightech=0 if TargetHighTechCode=="0"
	replace target_hightech=1 if TargetHighTechCode~="0"
	drop TargetHighTechCode
	
	drop HighTechIndustry AL Outcome
	
	rename TargetPrimarySICCode target_sic
	rename AcquirorPrimarySICCode acq_sic
	rename DealNumber dealnumber 
	rename DateAnnounced anndats
	
	gen target_public=1 if TargetPublicStatus=="Public"
	replace target_public=0 if target_public==.
	
	gen target_private=1 if TargetPublicStatus=="Priv."|TargetPublicStatus=="Sub."
	replace target_private=0 if target_private==.
		
	gen deal_cash=1 if ofCash~=0 & ofCash~=.
	replace deal_cash=0 if deal_cash==.
	
	gen deal_allcash=1 if ofCash==100
	replace deal_allcash=0 if deal_allcash==.

	gen deal_stock=1 if ofStock~=0 &ofStock~=.
	replace deal_stock=0 if deal_stock==.
	
	gen hightech=1 if acq_hightech==1&target_hightech==1
	replace hightech=0 if hightech==.
	*/
	
	ren master_deal_no dealnumber
	format dealnumber %20.0g
	compress
	
*impose constraint following Masulis et al (2007) JF paper
	*1) Acquisition is completed
	
	keep if statuscode=="C" 
	*drop DateWithdrawn Status
	
	*2)control less than 50% prior to transaction and own 100% after transaction
	replace pheld=0 if pheld==.
	drop if pheld>=50 
	
	keep if pctown==100 
		
	*3)Deal value disclosed in SDC is more than 1 Mil
	*keep if ValueofTransactionmil>1 &ValueofTransactionmil~=.
	*Consulted: http://mergers.thomsonib.com/td/DealSearch/help/def.htm
	keep if rankval > 1 & rankval != .
	
	*drop if MAType=="Repurchases"
	drop if repurch == "Yes"
	*drop if MAType=="Self Tenders, Recaps"
	drop if stender == "Yes"
	drop if recap == "Yes"
	
	gen fyear = int(dateeff/10000)
	save "sdc_processed",replace
