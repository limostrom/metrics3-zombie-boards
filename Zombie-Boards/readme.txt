

### Notes About Files:

firmlist-gvkeys.txt - save as firmlist.csv from /HH-Datasheet-and-code/, just a .txt version with no column header so we can upload it on the WRDS form and only download what we actually need.


/data/
	ceo_Execucomp19922010.csv - WRDS/Compustat-Capital IQ/Compustat/Execucomp/Annual Compensation/
		1992 to 2010, upload firmlist-gvkeys.txt for company codes, all variables


	compustatsegment19762010.csv - WRDS/Compustat-Capital IQ/Compustat/Historical Segments/
		1976 to 2010, upload firmlist-gvkeys.txt for company codes

	crspcomp19502010.csv - WRDS/CRSP/Monthly Update/CRSP Compustat Merged/ Fundamentals Annual/
		1950-06 to 2010-12, upload firmlist-gvkeys.txt for company codes, all variables, all other options are the defaults

	gov20072015.csv - WRDS/ISS: Institutional Shareholder Services/Governance/Governance/
		2007 to 2015, all company codes, all variables

	govlegacy19902006.csv - WRDS/ISS: Institutional Shareholder Services/Governance/Governance Legacy/
		1990 to 2006, all company codes, all variables

	irrc_director20072010 - WRDS/ISS: Institutional Shareholder Services/Directors/Directors Legacy/
		2007 to 2010, all company codes, all variables

	irrc_directorlegacy19962006 - WRDS/ISS: Institutional Shareholder Services/Directors/Directors/
		1996 to 2006, all company codes, all variables

	sdc_ma19962010 - WRDS/Thomson-Refinitiv/SDC/Mergers and Acquisitions/
		1996 to 2010, all company codes, all variables


/code/
	MainCode-replication.do

	ProcessIRRC_SDCCode-replication.do
		adapting the codes from the replication folder to work with our downloaded files
		Notes:
			1. They use a variable called did which I can't find and I think is director_detail_id?
			2. Their code uses gvkey but the IRRC data I found only has CUSIP, so I think we need to merge in the gvkeys separately
