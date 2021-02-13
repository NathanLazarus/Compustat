
/* --------------------------------------
AUTHOR: Pian Shu
THIS VERSION: January, 2019
PURPOSE: CLEAN THE BING SEARCH RESULTS
--------------------------------------- */

program define genchars
args var
gen chars=`var'
foreach x in A B C D E F G H I J ///
			 K L M N O P Q R S T ///
			 U V W X Y Z ///
			 0 1 2 3 4 5 6 7 8 9 " " {
	qui replace chars = subinstr(upper(chars), "`x'","", .)
}
end

****************************************
**** IMPORT THE OUTPUT FROM PYTHON	****
****************************************

*import and reformat
import delimited using "[output from bing search]",clear
rename key1 assignee_std
keep assignee_id link* assignee_std
reshape long link, i(assignee_id) j(order)
duplicates drop
order assignee_id assignee_std

save "${temp}/temp", replace

***************************
****** 	CLEAN URLS	 ******
***************************

use "${temp}/temp", clear

drop if link=="" | link=="ERROR"

*** deal with extremely long links

gen orig_link = link
replace link=substr(link,1,300)
compress

*** remove http://
foreach var of varlist link {
	qui replace `var'=lower(`var')
	qui replace `var' = subinstr(`var',"\","/",.)

	qui replace `var' = subinstr(`var',"http://","",1)
	qui replace `var' = subinstr(`var',"https://","",1)
	qui replace `var' = subinstr(`var',"http:","",1)
	qui replace `var' = subinstr(`var',"https:","",1)
	qui replace `var' = subinstr(`var',"www.ttp:","",1)
	qui replace `var' = subinstr(`var',"website: ","",1)
	qui replace `var' = substr(`var',5,length(`var')) if substr(`var',1,4) == "http"

	qui replace `var' = subinstr(`var',"//","",1) if substr(`var',1,2) == "//"
	qui replace `var' = subinstr(`var',"/","",1) if substr(`var',1,1) == "/"
}
 
gen init=substr(link,1,1) 
genchars init
assert chars==""
drop chars init

*** remove default webpages at the end
split link, p(/) gen(temp) limit(3)

assert !(temp2=="" & temp3!="")
count if temp2=="" & temp3==""
count if temp2!="" & temp3==""

replace temp2 = subinstr(temp2,"_","",.)
replace temp2 = subinstr(temp2,"-","",.)
replace temp2 = subinstr(temp2," ","",.)
replace temp2 = subinstr(temp2,"%20","",.)

*~ look for common variations ~
* egen temp=nvals(assignee_id) if temp3=="", by(temp2)
* tab temp2 if temp>=100 & temp!=. & strpos(temp2,".")>0, sort
* tab temp2 if strpos(temp2,"home.")==0 & strpos(temp2,"home")>0, sort

gen flag=0
foreach x in index indexe indexen index1 index2 ///
			default defaulten ///
			contact contactus contacts contacten contac2 contactinfo ///
			about aboutus about2 ///
			home homepage homeen ///
			company companyprofile companyhistory companyinfo ///
			history ourhistory history2 history3 {
	foreach y in .html .htm .php .asp .aspx .jsp {
		qui replace flag=1 if temp3=="" & temp2=="`x'"
		qui replace flag=1 if temp3=="" & temp2=="`x'" +"`y'"
	}
}

tab flag
replace link=temp1  if flag
drop temp* flag

*** remove variations of www. or regions (two letters, e.g., en/us/uk)
split link, p(.) gen(temp) limit(3)
assert link=="" if temp1==""
count if temp3==""
replace link = subinstr(link,temp1+".","",1) if strpos(temp1,"www") > 0 & temp3!=""
replace link = subinstr(link,temp1+".","",1) if length(temp1)==2 & temp3!=""
drop temp*

gen init=substr(link,1,1) 
genchars init
assert chars==""
drop chars init

*** standardize all other chars to _
foreach var of varlist link {
	foreach x in " " "-" "%20" "%" "&" "'" "(" ")" "?" ///
		"!" "#" "=" "+" ";" "$" "~" ":" "," "@" "[" "]" ///
		"{" "}" "*" "," "`" "|" "^" "_____" "____" "___" "__" {
		qui replace `var' = subinstr(`var',"`x'","_",.) 
	}
	qui replace `var' = subinstr(`var', `"""',  "", .)
}

*** remove all encodings - THIS PART ONLY WORKS ON A MAC
foreach var of varlist link {
	foreach i of numlist 127/255 {
	  qui replace `var'=subinstr(`var', "`=uchar(`i')'", "", .)
	}
}

genchars link
replace chars=subinstr(chars,"/","",.)
replace chars=subinstr(chars,".","",.)
replace chars=subinstr(chars,"_","",.)
tab chars
assert length(chars)==1 if chars!=""
replace link = subinstr(link,chars,"",.) if chars!=""
drop chars

*** remove chars at the end
gen last=substr(link,-1,1)
foreach x in "." "_" "/" {
	replace link = substr(link,1,length(link)-1) if last=="`x'"
}
drop last

gen last=substr(link,-1,1)
foreach x in "." "_" "/" {
	replace link = substr(link,1,length(link)-1) if last=="`x'"
}
drop last

gen last=substr(link,-1,1)
foreach x in "." "_" "/" {
	replace link = substr(link,1,length(link)-1) if last=="`x'"
}
drop last

gen last=substr(link,-1,1)
foreach x in "." "_" "/" {
	replace link = substr(link,1,length(link)-1) if last=="`x'"
}
drop last

gen last=substr(link,-1,1)
foreach x in "." "_" "/" {
	replace link = substr(link,1,length(link)-1) if last=="`x'"
}
drop last

gen last=substr(link,-1,1)
genchars last
assert chars==""
drop last chars

*** check urls are functional
assert strpos(link,".")>0

save "${temp}/temp", replace

*******************************************
****** 	IDENTIFY AGGREGATE LINKS	 ******
*******************************************

/*
This step drops the links from patent search websites (e.g., patentgenius.com). It also drops "aggregate links",
which are webpages that show up as the top search results for distinct firms (e.g., yellow pages that contain multiple firm names). 
We use the distribution of the assignee initials to infer the likelihood of "aggregate links".
*/

use "${temp}/temp", clear

gen initial = substr(assignee_std,1,1)
assert initial!=" "
rename link url

* drop links to patents or assignees
drop if strpos(url,"assignee")>0 | strpos(url,"patent")>0

* calculate proportion of modal matches
bysort url: gen count_assignees = _N
label variable count_assignees "number of assignees which return the url"

bysort url initial: gen count_initial_freq = _N
label variable count_initial_freq "occurrences of each initial"

bysort url: egen count_modal_initial = max(count_initial_freq)
label variable count_modal_initial "occurrences of modal initial"

gen prop_modal_initial = count_modal_initial / count_assignees

egen uniq_init = nvals(initial), by(url)

bys url: gen url_sample=(_n==1)
unique url if url_sample==1

sum prop_modal_init if url_sample, d
sum uniq_init if url_sample, d
tab uniq_init if url_sample

* criteria
drop if prop_modal_init<0.5

drop initial-url_sample
compress

* change storage type
rename url url1
gen url=url1
drop url1

d
label data "cleaned bing links for [assignees or Compustat firms]"
save "[output file]", replace

* 
*
*
erase "${temp}/temp.dta"
