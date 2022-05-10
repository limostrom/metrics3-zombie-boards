clear all
pause on

*Filepath for Lauren's computer:
cap cd "C:\Users\17036\Dropbox\Personal Document Backup\Booth\Booth Metrics\Spring\zombie-boards-data"


/* These files were all downloaded as CSVs this just saves them as Stata datasets instead */


local filelist: dir "data" files "*.csv"

	local i = 1
	foreach file of local filelist {
		local len = strlen("`file'")-4
		local newname = substr("`file'",1,`len')
		import delimited "data/`file'", clear
		save "data/`newname'", replace
	}
		