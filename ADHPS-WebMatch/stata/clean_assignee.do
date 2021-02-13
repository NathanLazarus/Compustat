
/* --------------------------------------
AUTHOR: Pian Shu
THIS VERSION: January, 2019
PURPOSE: 
- CLEAN PRIMARY PATENT ASSIGNEE NAMES
	1. Remove punctuations
	2. Standardize entities (following NBER PDP)
	3. Combine single letters  (following NBER PDP)
-ADD ASSIGNEE TYPES  (following NBER PDP)

NOTES: Coding of accents only works on a Mac
--------------------------------------- */

*********************************
****** REMOVE PUNCTUATIONS ******
*********************************

use "[input file that contains assignee names]", clear

gen standard_name=" "+upper(assignee)+" "
list standard_name if strpos(standard, "#")>0

** clean names that end in (THE) or start with THE
replace standard_name=substr(standard_name, 5, .) if substr(standard_name, 1, 5)==" THE "
replace standard_name=substr(standard_name, 1, length(standard_name)-6) if substr(standard_name, -6, 6)=="(THE) "
replace standard_name=substr(standard_name, 1, length(standard_name)-5) if substr(standard_name, -5, 5)=="-OLD "

** strips out all punctuation characters

*** manual
replace standard_name= " INTERSONICS INCORPORATED LEGRAPH COMPANY " if patent=="04763358"
replace standard_name= " CONOPCO INC " if patent=="06726949"
replace standard_name= " ATT INTELLECTUAL PROPERTY LLP " if patent=="07433412"
replace standard_name= " COUNCIL OF SCIENTIFIC & INDUSTRIAL RESEARCH " if patent=="08129369"
replace standard_name=" AAC MICROTEC AB " if  patent=="07755899"
replace standard_name=" VALLOUREC " if  patent=="03942650"
replace standard_name=" CG DORIS " if  patent=="04063430"
replace standard_name=" LETAT FRANCAIS REPRESENTE PAR LE MINISTRE DES PA " if  patent=="04496975"
replace standard_name=" SCR " if  patent=="04612551"
replace standard_name=" ERAMET SLN " if  patent=="04911848"
replace standard_name=" SOLVAY & CIE " if  patent=="05257922"
replace standard_name=" SOCIETE DE PROSPECTION ET DINVENTIONS TECHNIQUES " if  patent=="05339992"
replace standard_name=" NCR CORPORATION " if  patent=="05377198"
replace standard_name=" SOLVAY " if  patent=="05464599"
replace standard_name=" TEKNION FURNITURE SYSTEMS " if  patent=="05537290"
replace standard_name=" TRI STATE CORPORATION " if  patent=="06264523"
replace standard_name=" ALSTOM LTD " if  patent=="06499943"
replace standard_name=" ATLANTIC GMBH " if  patent=="06742772"
replace standard_name=" VETROTECH SAINT GOBAIN AG " if  patent=="06818267"
replace standard_name=" DATA COMM ELECTRONICS INC " if  patent=="07399920"
replace standard_name=" SEIKO EPSON CORPORATION " if  patent=="07812681"
replace standard_name=" ONERA " if  patent=="07864340"
replace standard_name=" IUCF HYU " if  patent=="07891016"
replace standard_name=" TELEFONAKTIEBOLAGET L M ERICSSON PUBL " if  patent=="07908386"
replace standard_name="	INSTITUT FRANCAIS DU PETROLE ALSTHOM ATLANTIQUE " if patent=="04886398"
replace standard_name="	TECHNOLOGY ENABLING COMPANY LLC	" if patent=="07171405"
replace standard_name="	AT&T INTELLECTUAL PROPERTY LLP " if patent=="07433412"
replace standard_name="	UNIVERSITY OF QUEENSLAND " if patent=="07592433"
replace standard_name="	STATE OF OREGON " if patent=="07598061"
replace standard_name="	LOUISIANA TECH RESEARCH FOUNDATION " if patent=="07792770"
replace standard_name="	LOUISIANA TECH RESEARCH FOUNDATION " if patent=="07865954"
replace standard_name="	SAMSUNG ELECTRONICS CO LTD " if patent=="07893465"
replace standard_name="	LOUISIANA TECH RESEARCH FOUNDATION " if patent=="07964409"
replace standard_name="	3DHISTECH KFT " if patent=="08041147"
replace standard_name="	PEKING UNIVERSITY PEOPLES HOSPITAL " if patent=="08110654"
replace standard_name="	LOUISIANA TECH RESEARCH FOUNDATION " if patent=="08127357"
replace standard_name="	IUCF HYU " if patent=="08232955"
replace standard_name="	LOUISIANA TECH RESEARCH FOUNDATION " if patent=="08350570"
replace standard_name="	MELBOURNE HEALTH " if patent=="08367317"
replace standard_name="	BIOSYNEXUS INCORPORATED " if patent=="08372958"
replace standard_name="	RUYAN INVESTMENT LIMITED " if patent=="08490628"
replace standard_name="	LOUISIANA TECH RESEARCH FOUNDATION " if patent=="08736452"
replace standard_name="	LOUISIANA TECH RESEARCH FOUNDATION " if patent=="08764938"
replace standard_name="	LOUISIANA TECH RESEARCH FOUNDATION " if patent=="09000768"
replace standard_name="	HORUS VISION LLC " if patent=="09068794"
replace standard_name="	LOUISIANA TECH RESEARCH FOUNDATION " if patent=="08349131"
replace standard_name=" ESSILOR INTERNATIONAL " if  patent=="08436184"
replace standard_name=" ESSILOR INTERNATIONAL " if  patent=="08690324"
replace standard_name=" ESSILOR INTERNATIONAL " if  patent=="08790104"

*** deal with < >
assert strpos(st,">")==0
count if !(strpos(st,"<")==0)
foreach char in "</PDAT" "<PDAT" "<HIL" "</HIL" "<SB" "</SB" "<BOLD" "</BOLD" "<SP" "</SP" "<ULINE" "</ULINE" "</STEXT" "</ONM" "</NAM" "</HI" "<" {
qui replace standard_name = subinstr(standard_name, "`char'",  "",.)
}
qui replace standard_name = itrim(standard_name)
assert strpos(st,"<")==0

*** deal with & ;
replace standard_name = subinstr(standard_name, "&AMP;", " & ", .)
replace standard_name = subinstr(standard_name, "&PLUS;", " & ", .)
replace standard_name = subinstr(standard_name, "&TIMES;", "X", .)
replace standard_name = subinstr(standard_name, "&COMMAT;", "@", .)
replace standard_name = subinstr(standard_name, "&MDASH;", "-", .)

qui replace standard_name = subinstr(standard_name, "&RDQU ", " ", .)
qui replace standard_name = subinstr(standard_name, "&BULL;", " ", .)
qui replace standard_name = subinstr(standard_name, "&NUM;", "#", .)

qui replace standard_name = subinstr(standard_name, "&OCIRC ;", "O", .)
qui replace standard_name = subinstr(standard_name, "&DGR;", "O", .)
qui replace standard_name = subinstr(standard_name, "&ANGST;", "A", .)
qui replace standard_name = subinstr(standard_name, "&AELIG;", "AE", .)
qui replace standard_name = subinstr(standard_name, "&ARING;", "A", .)
qui replace standard_name = subinstr(standard_name, "&CCEDIL;", "C", .)

foreach x in EXCL EQUALS PRIME STAR QUEST REG TRADE TILDE ///
			LDQUO RDQUO LSQUO RSQUO LSQB RSQB LT GT {
	qui replace standard_name = subinstr(standard_name, "&"+"`x'"+";", "",.)
}

foreach x in STARF MIDDOT CIRCLESOLID PLUSMN MINUS THGR{
	qui replace standard_name = subinstr(standard_name, "&"+"`x'"+";", " ",.)
}

foreach x in A E I O U S N C R Y{
	foreach y in GRAVE UML ACUTE CIRC TILDE SLASH {
		qui replace standard_name = subinstr(standard_name, "&"+"`x'"+"`y'"+";", "`x'",.)
	}
}

assert !(strpos(standard_name,"&") >0 & strpos(standard_name,";") > 0)

*** deal with {}
foreach char in "{UMLAUT OVER (" "{UMLAUT OVER" ///
				"{ACUTE OVER (" "{ACUTE OVER" ///
				"{OVERSCORE (" "{OVERSCORE" ///
				"{DOT OVER (" "{DOT OVER" ///
				"{GRAVE OVER (" "{GRAVE OVER" ///
				"{TILDE OVER (" "{TILDE OVER" ///
				"{HACEK OVER (" "{HACEK OVER""))}" "{HAECK OVER (" ///
				"{CIRCUMFLEX OVER (" ///
				")}" "}" {
qui replace standard_name = subinstr(standard_name, "`char'",  "",.)
}
		
assert (strpos(standard_name,"{") ==0 & strpos(standard_name,"}") == 0)

*** accents *** THIS PART OF PROGRAM ONLY WORKS ON MAC
* asciiplot
foreach var of varlist standard_name {
foreach i of numlist 127/160 {
  assert strpos(`var', "`=uchar(`i')'")==0
}

foreach i of numlist 161/191 {
  qui replace `var'=subinstr(`var', "`=uchar(`i')'", "", .)
}

foreach i of numlist 192/197 {
  qui replace `var'=subinstr(`var', "`=uchar(`i')'", "A", .)
}

foreach i of numlist 198 {
  qui replace `var'=subinstr(`var', "`=uchar(`i')'", "AE", .)
}

foreach i of numlist 199 {
  qui replace `var'=subinstr(`var', "`=uchar(`i')'", "C", .)
}

foreach i of numlist 200/203 {
  qui replace `var'=subinstr(`var', "`=uchar(`i')'", "E", .)
}

foreach i of numlist 204/207 {
  qui replace `var'=subinstr(`var', "`=uchar(`i')'", "I", .)
}

foreach i of numlist 208 {
  qui replace `var'=subinstr(`var', "`=uchar(`i')'", "D", .)
}

foreach i of numlist 209 {
  qui replace `var'=subinstr(`var', "`=uchar(`i')'", "N", .)
}

foreach i of numlist 210/214 {
  qui replace `var'=subinstr(`var', "`=uchar(`i')'", "O", .)
}

foreach i of numlist 215 {
  qui replace `var'=subinstr(`var', "`=uchar(`i')'", "X", .)
}

foreach i of numlist 216 {
  assert strpos(`var', "`=uchar(`i')'")==0
}

foreach i of numlist 217/220 {
  qui replace `var'=subinstr(`var', "`=uchar(`i')'", "U", .)
}

foreach i of numlist 221/223 {
  assert strpos(`var', "`=uchar(`i')'")==0
}

foreach i of numlist 224/229 {
  qui replace `var'=subinstr(`var', "`=uchar(`i')'", "A", .)
}

foreach i of numlist 230 {
  qui replace `var'=subinstr(`var', "`=uchar(`i')'", "AE", .)
}

foreach i of numlist 231 {
  qui replace `var'=subinstr(`var', "`=uchar(`i')'", "C", .)
}

foreach i of numlist 232/235 {
  qui replace `var'=subinstr(`var', "`=uchar(`i')'", "E", .)
}

foreach i of numlist 236/239 {
  qui replace `var'=subinstr(`var', "`=uchar(`i')'", "I", .)
}

foreach i of numlist 240 {
  assert strpos(`var', "`=uchar(`i')'")==0
}

foreach i of numlist 241 {
  qui replace `var'=subinstr(`var', "`=uchar(`i')'", "N", .)
}

foreach i of numlist 242/246 {
  qui replace `var'=subinstr(`var', "`=uchar(`i')'", "O", .)
}

foreach i of numlist 247 {
  assert strpos(`var', "`=uchar(`i')'")==0
}

foreach i of numlist 248 {
  qui replace `var'=subinstr(`var', "`=uchar(`i')'", "O", .)
}

foreach i of numlist 249/252 {
  qui replace `var'=subinstr(`var', "`=uchar(`i')'", "U", .)
}

foreach i of numlist 253 {
  qui replace `var'=subinstr(`var', "`=uchar(`i')'", "Y", .)
}

foreach i of numlist 254 {
  assert strpos(`var', "`=uchar(`i')'")==0
}

foreach i of numlist 255 {
  qui replace `var'=subinstr(`var', "`=uchar(`i')'", "Y", .)
}
}

foreach i in  "Ā" "ā" "Å"{
   qui replace standard_name=subinstr(standard_name, "`i'", "A", .)
}

foreach i in  "ē" "ė" {
   qui replace standard_name=subinstr(standard_name, "`i'", "E", .)
}

foreach i in  "ī" {
   qui replace standard_name=subinstr(standard_name, "`i'", "I", .)
}

foreach i in  "ō" "ő" {
   qui replace standard_name=subinstr(standard_name, "`i'", "O", .)
}

foreach i in  "ū" "ű" {
   qui replace standard_name=subinstr(standard_name, "`i'", "U", .)
}

foreach i in  "ć" "ċ" {
   qui replace standard_name=subinstr(standard_name, "`i'", "C", .)
}

foreach i in  "ń" {
   qui replace standard_name=subinstr(standard_name, "`i'", "N", .)
}

foreach i in  "ŕ" {
   qui replace standard_name=subinstr(standard_name, "`i'", "R", .)
}

foreach i in  "ś" {
   qui replace standard_name=subinstr(standard_name, "`i'", "S", .)
}

foreach i in  "ǵ" {
   qui replace standard_name=subinstr(standard_name, "`i'", "G", .)
}

foreach i in  "˜" "β" "–" "‘" "’" "“" "″" "”" "•" "′" "“" "”" "′" "″" "※" "™" "★""$"{
   qui replace standard_name=subinstr(standard_name, "`i'", "", .)
}

*** deal with common chars
foreach char in "'" "." "#" "!" ///
				"?" "*" "$" "`" "," ///
				"[" "]" ":" ";" {
qui replace standard_name = subinstr(standard_name, "`char'",  "",.)
}

foreach char in "@" "/" "-" "=" "—" "|" "\" {
qui replace standard_name = subinstr(standard_name, "`char'",  " ",.)
}

foreach char in "^" "~" "¶" "%" ","  "''""," {
assert strpos(standard_name, "`char'")==0
}

** deal with () - remove content within
foreach char in "(SOUTH AFRICA)" "(PROPRIETARY)" "(S)" "(SA)" "(SM)" "(UK)" "(US)" ///
				"(PTY)" "(AFRICA)""(ICS)" "(1989)" "(TS-A)" "(ISRAEL)" ///
				"(ARO) (VOLCANI CENTER)""(NIH)" ///
				"(DIENST LANDBOUWKUNDIG ONDERZOEK (DLO)" ///
				"(WOLVERHAMPTION) (LIMITED)" ///
				"(DIV OF GREAT PACIFIC ENTERPRISES (II))" ///
				"(1996 ( LTD" ///
				"((PUBL)" "(UPV EHU)" /// 
				"(GMBH) (CUTEC INSTITUT)" ///
				"(SHENZHEN(" ///
				"(SALES) PROPRIETARY)" "(INVESTMENTS)" ///
				"(IMEC) VZW)" {
qui replace standard_name = subinstr(standard_name, "`char'",  "",.)
}

egen temp=noccur(standard_name),s("(")
assert temp==1 | temp==0
egen temp1=noccur(standard_name),s(")")
assert temp1==1 | temp1==0
tab temp temp1

replace standard_name=substr(standard_name,1,strpos(standard_name,"(")-1)+substr(standard_name,strpos(standard_name,")")+1,length(standard_name)) if temp+temp1==2
replace standard_name=substr(standard_name,1,strpos(standard_name,"(")-1) if temp==1 & temp1==0
replace standard_name = subinstr(standard_name, ")",  "",.) if temp==0 & temp1==1

assert (strpos(standard_name,"(") ==0 & strpos(standard_name,")") == 0)
drop temp temp1

/*** common words with & in it, e.g., AT&T
keep standard
gen count=1
collapse (sum) count, by(standard)
split standard, generate(word)
gen id=_n
reshape long word,i(id) j(order)
keep if strpos(word,"&")>0
drop if word=="&"
gen x=1
collapse (sum) x (firstnm) stan count, by(word)
gen total=x*count
gsort -total 
*** MOST WORDS ARE LEGIT PARTS OF COMPANY WORDS AND SHOULD NOT HAVE SPACES
*/

** Recode all common words for "AND" to &
replace standard_name = subinstr(standard_name, " AND ", " & ", .) if substr(standard_name,1,5)!=" AND "
replace standard_name = subinstr(standard_name, " & AND ", " & ", .) 
replace standard_name = subinstr(standard_name, " ET ", " & ", .) if substr(standard_name,1,4)!=" ET "
replace standard_name = subinstr(standard_name, " & ET ", " & ", .)
replace standard_name = subinstr(standard_name, " UND ", " & ", .) if substr(standard_name,1,5)!=" UND "
replace standard_name = subinstr(standard_name, "+", "&", .)
replace standard_name = itrim(standard_name)

** CONFIRM ACCENTS CLEANED
egen chars=sieve(standard_name), keep(other)
replace chars=subinstr(chars,"&","",.)
assert chars==""
drop chars

gen assignee_std=trim(standard_name)
replace assignee_std=itrim(assignee_std)
label var assignee_std "primary assignee, punctuations removed"

count
count if strpos(assignee_std,asg_name)==0 & strpos(asg_name,assignee_std)==0
drop asg_name

*************************
****** DERWENT STD ******
*************************
assert substr(standard_name,1,1)==" "
assert substr(standard_name,length(standard_name),1)==" "

foreach var of varlist standard_name{
quietly replace `var' = subinstr( `var'," A B "," AB ",1)
quietly replace `var' = subinstr( `var'," A CALIFORNIA CORP "," CORP ",1)
quietly replace `var' = subinstr( `var'," A DELAWARE CORP "," CORP ",1)
quietly replace `var' = subinstr( `var'," AKTIEBOLAGET "," AB ",1)
quietly replace `var' = subinstr( `var'," AKTIEBOLAG "," AB ",1)
quietly replace `var' = subinstr( `var'," ACADEMY "," ACAD ",1)
quietly replace `var' = subinstr( `var'," ACTIEN GESELLSCHAFT "," AG ",1)
quietly replace `var' = subinstr( `var'," ACTIENGESELLSCHAFT "," AG ",1)
quietly replace `var' = subinstr( `var'," AKTIEN GESELLSCHAFT "," AG ",1)
quietly replace `var' = subinstr( `var'," AKTIENGESELLSCHAFT "," AG ",1)
quietly replace `var' = subinstr( `var'," AGRICOLAS "," AGRIC ",1)
quietly replace `var' = subinstr( `var'," AGRICOLA "," AGRIC ",1)
quietly replace `var' = subinstr( `var'," AGRICOLES "," AGRIC ",1)
quietly replace `var' = subinstr( `var'," AGRICOLE "," AGRIC ",1)
quietly replace `var' = subinstr( `var'," AGRICOLI "," AGRIC ",1)
quietly replace `var' = subinstr( `var'," AGRICOLTURE "," AGRIC ",1)
quietly replace `var' = subinstr( `var'," AGRICULTURA "," AGRIC ",1)
quietly replace `var' = subinstr( `var'," AGRICULTURAL "," AGRIC ",1)
quietly replace `var' = subinstr( `var'," AGRICULTURE "," AGRIC ",1)
quietly replace `var' = subinstr( `var'," AKADEMIA "," AKAD ",1)
quietly replace `var' = subinstr( `var'," AKADEMIEI "," AKAD ",1)
quietly replace `var' = subinstr( `var'," AKADEMIE "," AKAD ",1)
quietly replace `var' = subinstr( `var'," AKADEMII "," AKAD ",1)
quietly replace `var' = subinstr( `var'," AKADEMIJA "," AKAD ",1)
quietly replace `var' = subinstr( `var'," AKADEMIYA "," AKAD ",1)
quietly replace `var' = subinstr( `var'," AKADEMIYAKH "," AKAD ",1)
quietly replace `var' = subinstr( `var'," AKADEMIYAM "," AKAD ",1)
quietly replace `var' = subinstr( `var'," AKADEMIYAMI "," AKAD ",1)
quietly replace `var' = subinstr( `var'," AKADEMIYU "," AKAD ",1)
quietly replace `var' = subinstr( `var'," AKADEMI "," AKAD ",1)
quietly replace `var' = subinstr( `var'," ALLGEMEINER "," ALLG ",1)
quietly replace `var' = subinstr( `var'," ALLGEMEINE "," ALLG ",1)
quietly replace `var' = subinstr( `var'," ANTREPRIZA "," ANTR ",1)
quietly replace `var' = subinstr( `var'," APARARII "," APAR ",1)
quietly replace `var' = subinstr( `var'," APARATELOR "," APAR ",1)
quietly replace `var' = subinstr( `var'," APPARATEBAU "," APP ",1)
quietly replace `var' = subinstr( `var'," APPARATUS "," APP ",1)
quietly replace `var' = subinstr( `var'," APPARECHHI "," APP ",1)
quietly replace `var' = subinstr( `var'," APPAREILLAGES "," APP ",1)
quietly replace `var' = subinstr( `var'," APPAREILLAGE "," APP ",1)
quietly replace `var' = subinstr( `var'," APPAREILS "," APP ",1)
quietly replace `var' = subinstr( `var'," APPAREIL "," APP ",1)
quietly replace `var' = subinstr( `var'," APARATE "," APAR ",1)
quietly replace `var' = subinstr( `var'," APPARATE "," APP ",1)
quietly replace `var' = subinstr( `var'," APPLICATIONS "," APPL ",1)
quietly replace `var' = subinstr( `var'," APPLICATION "," APPL ",1)
quietly replace `var' = subinstr( `var'," APPLICAZIONE "," APPL ",1)
quietly replace `var' = subinstr( `var'," APPLICAZIONI "," APPL ",1)
quietly replace `var' = subinstr( `var'," ANPARTSSELSKABET "," APS ",1)
quietly replace `var' = subinstr( `var'," ANPARTSSELSKAB "," APS ",1)
quietly replace `var' = subinstr( `var'," A/S "," AS ",1)
quietly replace `var' = subinstr( `var'," AKTIESELSKABET "," AS ",1)
quietly replace `var' = subinstr( `var'," AKTIESELSKAB "," AS ",1)
quietly replace `var' = subinstr( `var'," ASSOCIACAO "," ASSOC ",1)
quietly replace `var' = subinstr( `var'," ASSOCIATED "," ASSOC ",1)
quietly replace `var' = subinstr( `var'," ASSOCIATES "," ASSOCIATES ",1)
quietly replace `var' = subinstr( `var'," ASSOCIATE "," ASSOCIATES ",1)
quietly replace `var' = subinstr( `var'," ASSOCIATION "," ASSOC ",1)
quietly replace `var' = subinstr( `var'," BETEILIGUNGSGESELLSCHAFT MBH "," BET GMBH ",1)
quietly replace `var' = subinstr( `var'," BETEILIGUNGS GESELLSCHAFT MIT "," BET GMBH ",1)
quietly replace `var' = subinstr( `var'," BETEILIGUNGSGESELLSCHAFT "," BET GES ",1)
quietly replace `var' = subinstr( `var'," BESCHRANKTER HAFTUNG "," BET GMBH ",1)
quietly replace `var' = subinstr( `var'," BROEDERNA "," BRDR ",1)
quietly replace `var' = subinstr( `var'," BROEDRENE "," BRDR ",1)
quietly replace `var' = subinstr( `var'," BRODERNA "," BRDR ",1)
quietly replace `var' = subinstr( `var'," BRODRENE "," BRDR ",1)
quietly replace `var' = subinstr( `var'," BROTHERS "," BROS ",1)
quietly replace `var' = subinstr( `var'," BESLOTEN VENNOOTSCHAP MET "," BV ",1)
quietly replace `var' = subinstr( `var'," BESLOTEN VENNOOTSCHAP "," BV ",1)
quietly replace `var' = subinstr( `var'," BEPERKTE AANSPRAKELIJKHEID "," BV ",1)
quietly replace `var' = subinstr( `var'," CLOSE CORPORATION "," CC ",1)
quietly replace `var' = subinstr( `var'," CENTER "," CENT ",1)
quietly replace `var' = subinstr( `var'," CENTRAAL "," CENT ",1)
quietly replace `var' = subinstr( `var'," CENTRALA "," CENT ",1)
quietly replace `var' = subinstr( `var'," CENTRALES "," CENT ",1)
quietly replace `var' = subinstr( `var'," CENTRALE "," CENT ",1)
quietly replace `var' = subinstr( `var'," CENTRAL "," CENT ",1)
quietly replace `var' = subinstr( `var'," CENTRAUX "," CENT ",1)
quietly replace `var' = subinstr( `var'," CENTRE "," CENT ",1)
quietly replace `var' = subinstr( `var'," CENTRO "," CENT ",1)
quietly replace `var' = subinstr( `var'," CENTRUL "," CENT ",1)
quietly replace `var' = subinstr( `var'," CENTRUM "," CENT ",1)
quietly replace `var' = subinstr( `var'," CERCETARE "," CERC ",1)
quietly replace `var' = subinstr( `var'," CERCETARI "," CERC ",1)
quietly replace `var' = subinstr( `var'," CHEMICALS "," CHEM ",1)
quietly replace `var' = subinstr( `var'," CHEMICAL "," CHEM ",1)
quietly replace `var' = subinstr( `var'," CHEMICKEJ "," CHEM ",1)
quietly replace `var' = subinstr( `var'," CHEMICKE "," CHEM ",1)
quietly replace `var' = subinstr( `var'," CHEMICKYCH "," CHEM ",1)
quietly replace `var' = subinstr( `var'," CHEMICKY "," CHEM ",1)
quietly replace `var' = subinstr( `var'," CHEMICZNE "," CHEM ",1)
quietly replace `var' = subinstr( `var'," CHEMICZNY "," CHEM ",1)
quietly replace `var' = subinstr( `var'," CHEMIE "," CHEM ",1)
quietly replace `var' = subinstr( `var'," CHEMII "," CHEM ",1)
quietly replace `var' = subinstr( `var'," CHEMISCHE "," CHEM ",1)
quietly replace `var' = subinstr( `var'," CHEMISCH "," CHEM ",1)
quietly replace `var' = subinstr( `var'," CHEMISKEJ "," CHEM ",1)
quietly replace `var' = subinstr( `var'," CHEMISTRY "," CHEM ",1)
quietly replace `var' = subinstr( `var'," CHIMICA "," CHIM ",1)
quietly replace `var' = subinstr( `var'," CHIMICE "," CHIM ",1)
quietly replace `var' = subinstr( `var'," CHIMICI "," CHIM ",1)
quietly replace `var' = subinstr( `var'," CHIMICO "," CHIM ",1)
quietly replace `var' = subinstr( `var'," CHIMIC "," CHIM ",1)
quietly replace `var' = subinstr( `var'," CHIMIEI "," CHIM ",1)
quietly replace `var' = subinstr( `var'," CHIMIE "," CHIM ",1)
quietly replace `var' = subinstr( `var'," CHIMIESKOJ "," CHIM ",1)
quietly replace `var' = subinstr( `var'," CHIMII "," CHIM ",1)
quietly replace `var' = subinstr( `var'," CHIMIKO "," CHIM ",1)
quietly replace `var' = subinstr( `var'," CHIMIQUES "," CHIM ",1)
quietly replace `var' = subinstr( `var'," CHIMIQUE "," CHIM ",1)
quietly replace `var' = subinstr( `var'," CHIMIYAKH "," CHIM ",1)
quietly replace `var' = subinstr( `var'," CHIMIYAMI "," CHIM ",1)
quietly replace `var' = subinstr( `var'," CHIMIYAM "," CHIM ",1)
quietly replace `var' = subinstr( `var'," CHIMIYA "," CHIM ",1)
quietly replace `var' = subinstr( `var'," CHIMIYU "," CHIM ",1)
quietly replace `var' = subinstr( `var'," COMPAGNIE FRANCAISE "," CIE FR ",1)
quietly replace `var' = subinstr( `var'," COMPAGNIE GENERALE "," CIE GEN ",1)
quietly replace `var' = subinstr( `var'," COMPAGNIE INDUSTRIALE "," CIE IND ",1)
quietly replace `var' = subinstr( `var'," COMPAGNIE INDUSTRIELLE "," CIE IND ",1)
quietly replace `var' = subinstr( `var'," COMPAGNIE INDUSTRIELLES "," CIE IND ",1)
quietly replace `var' = subinstr( `var'," COMPAGNIE INTERNATIONALE "," CIE INT ",1)
quietly replace `var' = subinstr( `var'," COMPAGNIE NATIONALE "," CIE NAT ",1)
quietly replace `var' = subinstr( `var'," COMPAGNIE PARISIENNE "," CIE PARIS ",1)
quietly replace `var' = subinstr( `var'," COMPAGNIE PARISIENN "," CIE PARIS ",1)
quietly replace `var' = subinstr( `var'," COMPAGNIE PARISIEN "," CIE PARIS ",1)
quietly replace `var' = subinstr( `var'," COMPANIES "," CO ",1)
quietly replace `var' = subinstr( `var'," COMPAGNIA "," CIA ",1)
quietly replace `var' = subinstr( `var'," COMPANHIA "," CIA ",1)
quietly replace `var' = subinstr( `var'," COMPAGNIE "," CIE ",1)
quietly replace `var' = subinstr( `var'," COMPANY "," CO ",1)
quietly replace `var' = subinstr( `var'," COMBINATUL "," COMB ",1)
quietly replace `var' = subinstr( `var'," COMMERCIALE "," COMML ",1)
quietly replace `var' = subinstr( `var'," COMMERCIAL "," COMML ",1)
quietly replace `var' = subinstr( `var'," CONSOLIDATED "," CONSOL ",1)
quietly replace `var' = subinstr( `var'," CONSTRUCCIONES "," CONSTR ",1)
quietly replace `var' = subinstr( `var'," CONSTRUCCIONE "," CONSTR ",1) 
quietly replace `var' = subinstr( `var'," CONSTRUCCION "," CONSTR ",1)
quietly replace `var' = subinstr( `var'," CONSTRUCTIE "," CONSTR ",1)
quietly replace `var' = subinstr( `var'," CONSTRUCTII "," CONSTR ",1)
quietly replace `var' = subinstr( `var'," CONSTRUCTIILOR "," CONSTR ",1)
quietly replace `var' = subinstr( `var'," CONSTRUCTIONS "," CONSTR ",1)
quietly replace `var' = subinstr( `var'," CONSTRUCTION "," CONSTR ",1)
quietly replace `var' = subinstr( `var'," CONSTRUCTORTUL "," CONSTR ",1)
quietly replace `var' = subinstr( `var'," CONSTRUCTORUL "," CONSTR ",1)
quietly replace `var' = subinstr( `var'," CONSTRUCTOR "," CONSTR ",1)
quietly replace `var' = subinstr( `var'," CO OPERATIVES "," COOP ",1)
quietly replace `var' = subinstr( `var'," CO OPERATIVE "," COOP ",1)
quietly replace `var' = subinstr( `var'," COOPERATIEVE "," COOP ",1)
quietly replace `var' = subinstr( `var'," COOPERATIVA "," COOP ",1)
quietly replace `var' = subinstr( `var'," COOPERATIVES "," COOP ",1)
quietly replace `var' = subinstr( `var'," COOPERATIVE "," COOP ",1)
quietly replace `var' = subinstr( `var'," INCORPORATED "," INC ",1)
quietly replace `var' = subinstr( `var'," INCORPORATION "," INC ",1)
quietly replace `var' = subinstr( `var'," CORPORATE "," CORP ",1)
quietly replace `var' = subinstr( `var'," CORPORATION OF AMERICA "," CORP ",1)
quietly replace `var' = subinstr( `var'," CORPORATION "," CORP ",1)
quietly replace `var' = subinstr( `var'," CORPORASTION "," CORP ",1)
quietly replace `var' = subinstr( `var'," CORPORATIOON "," CORP ",1)
quietly replace `var' = subinstr( `var'," COSTRUZIONI "," COSTR ",1)
quietly replace `var' = subinstr( `var'," DEUTSCHEN "," DDR ",1)
quietly replace `var' = subinstr( `var'," DEUTSCHE "," DDR ",1)
quietly replace `var' = subinstr( `var'," DEMOKRATISCHEN REPUBLIK "," DDR ",1)
quietly replace `var' = subinstr( `var'," DEMOKRATISCHE REPUBLIK "," DDR ",1)
quietly replace `var' = subinstr( `var'," DEPARTEMENT "," DEPT ",1)
quietly replace `var' = subinstr( `var'," DEPARTMENT "," DEPT ",1)
quietly replace `var' = subinstr( `var'," DEUTSCHES "," DEUT ",1)
quietly replace `var' = subinstr( `var'," DEUTSCHEN "," DEUT ",1)
quietly replace `var' = subinstr( `var'," DEUTSCHER "," DEUT ",1)
quietly replace `var' = subinstr( `var'," DEUTSCHLAND "," DEUT ",1)
quietly replace `var' = subinstr( `var'," DEUTSCHE "," DEUT ",1)
quietly replace `var' = subinstr( `var'," DEUTSCH "," DEUT ",1)
quietly replace `var' = subinstr( `var'," DEVELOPMENTS "," DEV ",1)
quietly replace `var' = subinstr( `var'," DEVELOPMENT "," DEV ",1)
quietly replace `var' = subinstr( `var'," DEVELOPPEMENTS "," DEV ",1)
quietly replace `var' = subinstr( `var'," DEVELOPPEMENT "," DEV ",1)
quietly replace `var' = subinstr( `var'," DEVELOP "," DEV ",1)
quietly replace `var' = subinstr( `var'," DIVISIONE "," DIV ",1)
quietly replace `var' = subinstr( `var'," DIVISION "," DIV ",1)
quietly replace `var' = subinstr( `var'," ENGINEERING "," ENG ",1)
quietly replace `var' = subinstr( `var'," EQUIPEMENTS "," EQUIP ",1)
quietly replace `var' = subinstr( `var'," EQUIPEMENT "," EQUIP ",1)
quietly replace `var' = subinstr( `var'," EQUIPMENTS "," EQUIP ",1)
quietly replace `var' = subinstr( `var'," EQUIPMENT "," EQUIP ",1)
quietly replace `var' = subinstr( `var'," ESTABLISHMENTS "," ESTAB ",1)
quietly replace `var' = subinstr( `var'," ESTABLISHMENT "," ESTAB ",1)
quietly replace `var' = subinstr( `var'," ESTABLISSEMENTS "," ESTAB ",1)
quietly replace `var' = subinstr( `var'," ESTABLISSEMENT "," ESTAB ",1)
quietly replace `var' = subinstr( `var'," ETABLISSEMENTS "," ETAB ",1)
quietly replace `var' = subinstr( `var'," ETABLISSEMENT "," ETAB ",1)
quietly replace `var' = subinstr( `var'," ETABS "," ETAB ",1)
quietly replace `var' = subinstr( `var'," ETS "," ETAB ",1)
quietly replace `var' = subinstr( `var'," ETUDES "," ETUD ",1)
quietly replace `var' = subinstr( `var'," ETUDE "," ETUD ",1)
quietly replace `var' = subinstr( `var'," EUROPAEISCHEN "," EURO ",1)
quietly replace `var' = subinstr( `var'," EUROPAEISCHES "," EURO ",1)
quietly replace `var' = subinstr( `var'," EUROPAEISCHE "," EURO ",1)
quietly replace `var' = subinstr( `var'," EUROPAISCHEN "," EURO ",1)
quietly replace `var' = subinstr( `var'," EUROPAISCHES "," EURO ",1)
quietly replace `var' = subinstr( `var'," EUROPAISCHE "," EURO ",1)
quietly replace `var' = subinstr( `var'," EUROPEAN "," EURO ",1)
quietly replace `var' = subinstr( `var'," EUROPEENNE "," EURO ",1)
quietly replace `var' = subinstr( `var'," EUROPEEN "," EURO ",1)
quietly replace `var' = subinstr( `var'," EUROPEA "," EURO ",1)
quietly replace `var' = subinstr( `var'," EUROPE "," EURO ",1)
quietly replace `var' = subinstr( `var'," EINGETRAGENER VEREIN "," EV ",1)
quietly replace `var' = subinstr( `var'," EXPLOATERINGS "," EXPL ",1)
quietly replace `var' = subinstr( `var'," EXPLOATERING "," EXPL ",1)
quietly replace `var' = subinstr( `var'," EXPLOITATIE "," EXPL ",1)
quietly replace `var' = subinstr( `var'," EXPLOITATIONS "," EXPL ",1)
quietly replace `var' = subinstr( `var'," EXPLOITATION "," EXPL ",1)
quietly replace `var' = subinstr( `var'," FIRMA "," FA ",1)
quietly replace `var' = subinstr( `var'," FABBRICAZIONI "," FAB ",1)
quietly replace `var' = subinstr( `var'," FABBRICHE "," FAB ",1)
quietly replace `var' = subinstr( `var'," FABRICATIONS "," FAB ",1)
quietly replace `var' = subinstr( `var'," FABRICATION "," FAB ",1)
quietly replace `var' = subinstr( `var'," FABBRICA "," FAB ",1)
quietly replace `var' = subinstr( `var'," FABRICA "," FAB ",1)
quietly replace `var' = subinstr( `var'," FABRIEKEN "," FAB ",1)
quietly replace `var' = subinstr( `var'," FABRIEK "," FAB ",1)
quietly replace `var' = subinstr( `var'," FABRIKER "," FAB ",1)
quietly replace `var' = subinstr( `var'," FABRIK "," FAB ",1)
quietly replace `var' = subinstr( `var'," FABRIQUES "," FAB ",1)
quietly replace `var' = subinstr( `var'," FABRIQUE "," FAB ",1)
quietly replace `var' = subinstr( `var'," FABRIZIO "," FAB ",1)
quietly replace `var' = subinstr( `var'," FABRYKA "," FAB ",1)
quietly replace `var' = subinstr( `var'," FARMACEUTICA "," FARM ",1)
quietly replace `var' = subinstr( `var'," FARMACEUTICE "," FARM ",1)
quietly replace `var' = subinstr( `var'," FARMACEUTICHE "," FARM ",1)
quietly replace `var' = subinstr( `var'," FARMACEUTICI "," FARM ",1)
quietly replace `var' = subinstr( `var'," FARMACEUTICOS "," FARM ",1)
quietly replace `var' = subinstr( `var'," FARMACEUTICO "," FARM ",1)
quietly replace `var' = subinstr( `var'," FARMACEUTISK "," FARM ",1)
quietly replace `var' = subinstr( `var'," FARMACEVTSKIH "," FARM ",1)
quietly replace `var' = subinstr( `var'," FARMACIE "," FARM ",1)
quietly replace `var' = subinstr( `var'," FONDATION "," FOND ",1)
quietly replace `var' = subinstr( `var'," FONDAZIONE "," FOND ",1)
quietly replace `var' = subinstr( `var'," FOUNDATIONS "," FOUND ",1)
quietly replace `var' = subinstr( `var'," FOUNDATION "," FOUND ",1)
quietly replace `var' = subinstr( `var'," FRANCAISE "," FR ",1)
quietly replace `var' = subinstr( `var'," FRANCAIS "," FR ",1)
quietly replace `var' = subinstr( `var'," F LLI "," FRAT ",1)
quietly replace `var' = subinstr( `var'," FLLI "," FRAT ",1)
quietly replace `var' = subinstr( `var'," FRATELLI "," FRAT ",1)
quietly replace `var' = subinstr( `var'," GEBRODERS "," GEBR ",1)
quietly replace `var' = subinstr( `var'," GEBRODER "," GEBR ",1)
quietly replace `var' = subinstr( `var'," GEBROEDERS "," GEBR ",1)
quietly replace `var' = subinstr( `var'," GEBROEDER "," GEBR ",1)
quietly replace `var' = subinstr( `var'," GEBRUDERS "," GEBR ",1)
quietly replace `var' = subinstr( `var'," GEBRUDER "," GEBR ",1)
quietly replace `var' = subinstr( `var'," GEBRUEDERS "," GEBR ",1)
quietly replace `var' = subinstr( `var'," GEBRUEDER "," GEBR ",1)
quietly replace `var' = subinstr( `var'," GEB "," GEBR ",1)
quietly replace `var' = subinstr( `var'," GENERALA "," GEN ",1)
quietly replace `var' = subinstr( `var'," GENERALES "," GEN ",1)
quietly replace `var' = subinstr( `var'," GENERALE "," GEN ",1)
quietly replace `var' = subinstr( `var'," GENERAL "," GEN ",1)
quietly replace `var' = subinstr( `var'," GENERAUX "," GEN ",1)
quietly replace `var' = subinstr( `var'," GESELLSCHAFT "," GES ",1)
quietly replace `var' = subinstr( `var'," GEWERKSCHAFT "," GEW ",1)
quietly replace `var' = subinstr( `var'," GAKKO HOJIN "," GH ",1)
quietly replace `var' = subinstr( `var'," GAKKO HOUJIN "," GH ",1)
quietly replace `var' = subinstr( `var'," GUTEHOFFNUNGSCHUETTE "," GHH ",1)
quietly replace `var' = subinstr( `var'," GUTEHOFFNUNGSCHUTTE "," GHH ",1)
quietly replace `var' = subinstr( `var'," GOMEI GAISHA "," GK ",1)
quietly replace `var' = subinstr( `var'," GOMEI KAISHA "," GK ",1)
quietly replace `var' = subinstr( `var'," GOSHI KAISHA "," GK ",1)
quietly replace `var' = subinstr( `var'," GOUSHI GAISHA "," GK ",1)
quietly replace `var' = subinstr( `var'," GESELLSCHAFT MBH "," GMBH ",1)
quietly replace `var' = subinstr( `var'," GESELLSCHAFT MIT BESCHRANKTER HAFTUNG "," GMBH ",1)
quietly replace `var' = subinstr( `var'," GROUPEMENT "," GRP ",1)
quietly replace `var' = subinstr( `var'," GROUPMENT "," GRP ",1)
quietly replace `var' = subinstr( `var'," HANDELSMAATSCHAPPIJ "," HANDL ",1)
quietly replace `var' = subinstr( `var'," HANDELSMIJ "," HANDL ",1)
quietly replace `var' = subinstr( `var'," HANDELS BOLAGET "," HB ",1)
quietly replace `var' = subinstr( `var'," HANDELSBOLAGET "," HB ",1)
quietly replace `var' = subinstr( `var'," HER MAJESTY THE QUEEN IN RIGHT OF CANADA AS REPRESENTED BY THE MINISTER OF "," CANADA MIN OF ",1)
quietly replace `var' = subinstr( `var'," HER MAJESTY THE QUEEN "," UK ",1)
quietly replace `var' = subinstr( `var'," INDUSTRIAS "," IND ",1)
quietly replace `var' = subinstr( `var'," INDUSTRIALS "," IND ",1)
quietly replace `var' = subinstr( `var'," INDUSTRIAL "," IND ",1)
quietly replace `var' = subinstr( `var'," INDUSTRIALA "," IND ",1)
quietly replace `var' = subinstr( `var'," INDUSTRIALE "," IND ",1)
quietly replace `var' = subinstr( `var'," INDUSTRIALIZARE "," IND ",1)
quietly replace `var' = subinstr( `var'," INDUSTRIALIZAREA "," IND ",1)
quietly replace `var' = subinstr( `var'," INDUSTRIALI "," IND ",1)
quietly replace `var' = subinstr( `var'," INDUSTRIEELE "," IND ",1)
quietly replace `var' = subinstr( `var'," INDUSTRIEI "," IND ",1)
quietly replace `var' = subinstr( `var'," INDUSTRIELS "," IND ",1)
quietly replace `var' = subinstr( `var'," INDUSTRIELLES "," IND ",1)
quietly replace `var' = subinstr( `var'," INDUSTRIELLE "," IND ",1)
quietly replace `var' = subinstr( `var'," INDUSTRIELL "," IND ",1)
quietly replace `var' = subinstr( `var'," INDUSTRIEL "," IND ",1)
quietly replace `var' = subinstr( `var'," INDUSTRIER "," IND ",1)
quietly replace `var' = subinstr( `var'," INDUSTRIES "," IND ",1)
quietly replace `var' = subinstr( `var'," INDUSTRII "," IND ",1)
quietly replace `var' = subinstr( `var'," INDUSTRIJ "," IND ",1)
quietly replace `var' = subinstr( `var'," INDUSTRIYAKH "," IND ",1)
quietly replace `var' = subinstr( `var'," INDUSTRIYAM "," IND ",1)
quietly replace `var' = subinstr( `var'," INDUSTRIYAMI "," IND ",1)
quietly replace `var' = subinstr( `var'," INDUSTRIYA "," IND ",1)
quietly replace `var' = subinstr( `var'," INDUSTRIYU "," IND ",1)
quietly replace `var' = subinstr( `var'," INDUSTRIA "," IND ",1)
quietly replace `var' = subinstr( `var'," INDUSTRIE "," IND ",1)
quietly replace `var' = subinstr( `var'," INDUSTRI "," IND ",1)
quietly replace `var' = subinstr( `var'," INDUSTRY "," IND ",1)
quietly replace `var' = subinstr( `var'," INGENIERIA "," ING ",1)
quietly replace `var' = subinstr( `var'," INGENIER "," ING ",1)
quietly replace `var' = subinstr( `var'," INGENIEURS "," ING ",1)
quietly replace `var' = subinstr( `var'," INGENIEURBUERO "," ING ",1)
quietly replace `var' = subinstr( `var'," INGENIEURBUREAU "," ING ",1)
quietly replace `var' = subinstr( `var'," INGENIEURBURO "," ING ",1)
quietly replace `var' = subinstr( `var'," INGENIEURGESELLSCHAFT "," ING ",1)
quietly replace `var' = subinstr( `var'," INGENIEURSBUREAU "," ING ",1)
quietly replace `var' = subinstr( `var'," INGENIEURTECHNISCHES "," ING ",1)
quietly replace `var' = subinstr( `var'," INGENIEURTECHNISCHE "," ING ",1)
quietly replace `var' = subinstr( `var'," INGENIEUR "," ING ",1)
quietly replace `var' = subinstr( `var'," INGENIOERFIRMAET "," ING ",1)
quietly replace `var' = subinstr( `var'," INGENIORSFIRMAN "," ING ",1)
quietly replace `var' = subinstr( `var'," INGENIORSFIRMA "," ING ",1)
quietly replace `var' = subinstr( `var'," INGENJORSFIRMA "," ING ",1)
quietly replace `var' = subinstr( `var'," INGINERIE "," ING ",1)
quietly replace `var' = subinstr( `var'," INSTITUTE FRANCAISE "," INST FR ",1)
quietly replace `var' = subinstr( `var'," INSTITUT FRANCAIS "," INST FR ",1)
quietly replace `var' = subinstr( `var'," INSTITUTE NATIONALE "," INST NAT ",1)
quietly replace `var' = subinstr( `var'," INSTITUT NATIONAL "," INST NAT ",1)
quietly replace `var' = subinstr( `var'," INSTITUTAMI "," INST ",1)
quietly replace `var' = subinstr( `var'," INSTITUTAMKH "," INST ",1)
quietly replace `var' = subinstr( `var'," INSTITUTAM "," INST ",1)
quietly replace `var' = subinstr( `var'," INSTITUTA "," INST ",1)
quietly replace `var' = subinstr( `var'," INSTITUTES "," INST ",1)
quietly replace `var' = subinstr( `var'," INSTITUTET "," INST ",1)
quietly replace `var' = subinstr( `var'," INSTITUTE "," INST ",1)
quietly replace `var' = subinstr( `var'," INSTITUTOM "," INST ",1)
quietly replace `var' = subinstr( `var'," INSTITUTOV "," INST ",1)
quietly replace `var' = subinstr( `var'," INSTITUTO "," INST ",1)
quietly replace `var' = subinstr( `var'," INSTITUTT "," INST ",1)
quietly replace `var' = subinstr( `var'," INSTITUTUL "," INST ",1)
quietly replace `var' = subinstr( `var'," INSTITUTU "," INST ",1)
quietly replace `var' = subinstr( `var'," INSTITUTY "," INST ",1)
quietly replace `var' = subinstr( `var'," INSTITUT "," INST ",1)
quietly replace `var' = subinstr( `var'," INSTITUUT "," INST ",1)
quietly replace `var' = subinstr( `var'," INSTITZHT "," INST ",1)
quietly replace `var' = subinstr( `var'," INSTYTUT "," INST ",1)
quietly replace `var' = subinstr( `var'," INSINOORITOMISTO "," INSTMSTO ",1)
quietly replace `var' = subinstr( `var'," INSTRUMENTS "," INSTR ",1)
quietly replace `var' = subinstr( `var'," INSTRUMENTATION "," INSTR ",1)
quietly replace `var' = subinstr( `var'," INSTRUMENTE "," INSTR ",1)
quietly replace `var' = subinstr( `var'," INSTRUMENT "," INSTR ",1)
quietly replace `var' = subinstr( `var'," INTERNATL "," INT ",1)
quietly replace `var' = subinstr( `var'," INTERNACIONAL "," INT ",1)
quietly replace `var' = subinstr( `var'," INTERNATIONAL "," INT ",1)
quietly replace `var' = subinstr( `var'," INTERNATIONALEN "," INT ",1)
quietly replace `var' = subinstr( `var'," INTERNATIONALE "," INT ",1)
quietly replace `var' = subinstr( `var'," INTERNATIONAUX "," INT ",1)
quietly replace `var' = subinstr( `var'," INTERNATIONELLA "," INT ",1)
quietly replace `var' = subinstr( `var'," INTERNAZIONALE "," INT ",1)
quietly replace `var' = subinstr( `var'," INTL "," INT ",1)
quietly replace `var' = subinstr( `var'," INTREPRINDEREA "," INTR ",1)
quietly replace `var' = subinstr( `var'," ISTITUTO "," IST ",1)
quietly replace `var' = subinstr( `var'," ITALIANA "," ITAL ",1)
quietly replace `var' = subinstr( `var'," ITALIANE "," ITAL ",1)
quietly replace `var' = subinstr( `var'," ITALIANI "," ITAL ",1)
quietly replace `var' = subinstr( `var'," ITALIANO "," ITAL ",1)
quietly replace `var' = subinstr( `var'," ITALIENNE "," ITAL ",1)
quietly replace `var' = subinstr( `var'," ITALIEN "," ITAL ",1)
quietly replace `var' = subinstr( `var'," ITALIAN "," ITAL ",1)
quietly replace `var' = subinstr( `var'," ITALIA "," ITAL ",1)
quietly replace `var' = subinstr( `var'," ITALI "," ITAL ",1)
quietly replace `var' = subinstr( `var'," ITALO "," ITAL ",1)
quietly replace `var' = subinstr( `var'," ITALY "," ITAL ",1)
quietly replace `var' = subinstr( `var'," JUNIOR "," JR ",1)
quietly replace `var' = subinstr( `var'," KOMMANDIT BOLAG "," KB ",1)
quietly replace `var' = subinstr( `var'," KOMMANDIT BOLAGET "," KB ",1)
quietly replace `var' = subinstr( `var'," KOMMANDITBOLAGET "," KB ",1)
quietly replace `var' = subinstr( `var'," KOMMANDITBOLAG "," KB ",1)
quietly replace `var' = subinstr( `var'," KOMMANDIT GESELLSCHAFT "," KG ",1)
quietly replace `var' = subinstr( `var'," KOMMANDITGESELLSCHAFT "," KG ",1)
quietly replace `var' = subinstr( `var'," KOMMANDIT GESELLSCHAFT AUF AKTIEN "," KGAA ",1)
quietly replace `var' = subinstr( `var'," KOMMANDITGESELLSCHAFT AUF AKTIEN "," KGAA ",1)
quietly replace `var' = subinstr( `var'," KUTATO INTEZETE "," KI ",1)
quietly replace `var' = subinstr( `var'," KUTATO INTEZET "," KI ",1)
quietly replace `var' = subinstr( `var'," KUTATOINTEZETE "," KI ",1)
quietly replace `var' = subinstr( `var'," KUTATOINTEZET "," KI ",1)
quietly replace `var' = subinstr( `var'," KABUSHIKI GAISHA "," KK ",1)
quietly replace `var' = subinstr( `var'," KABUSHIKI KAISHA "," KK ",1)
quietly replace `var' = subinstr( `var'," KABUSHIKI GAISYA "," KK ",1)
quietly replace `var' = subinstr( `var'," KABUSHIKI KAISYA "," KK ",1)
quietly replace `var' = subinstr( `var'," KABUSHIKIGAISHA "," KK ",1)
quietly replace `var' = subinstr( `var'," KABUSHIKIKAISHA "," KK ",1)
quietly replace `var' = subinstr( `var'," KABUSHIKIGAISYA "," KK ",1)
quietly replace `var' = subinstr( `var'," KABUSHIKIKAISYA "," KK ",1)
quietly replace `var' = subinstr( `var'," KOMBINATU "," KOMB ",1)
quietly replace `var' = subinstr( `var'," KOMBINATY "," KOMB ",1)
quietly replace `var' = subinstr( `var'," KOMBINAT "," KOMB ",1)
quietly replace `var' = subinstr( `var'," KONINKLIJKE "," KONINK ",1)
quietly replace `var' = subinstr( `var'," KONCERNOVY PODNIK "," KP ",1)
quietly replace `var' = subinstr( `var'," KUNSTSTOFFTECHNIK "," KUNST ",1)
quietly replace `var' = subinstr( `var'," KUNSTSTOFF "," KUNST ",1)
quietly replace `var' = subinstr( `var'," LABORATOIRES "," LAB ",1)
quietly replace `var' = subinstr( `var'," LABORATOIRE "," LAB ",1)
quietly replace `var' = subinstr( `var'," LABORATOIR "," LAB ",1)
quietly replace `var' = subinstr( `var'," LABORATORIEI "," LAB ",1)
quietly replace `var' = subinstr( `var'," LABORATORIES "," LAB ",1)
quietly replace `var' = subinstr( `var'," LABORATORII "," LAB ",1)
quietly replace `var' = subinstr( `var'," LABORATORIJ "," LAB ",1)
quietly replace `var' = subinstr( `var'," LABORATORIOS "," LAB ",1)
quietly replace `var' = subinstr( `var'," LABORATORIO "," LAB ",1)
quietly replace `var' = subinstr( `var'," LABORATORIUM "," LAB ",1)
quietly replace `var' = subinstr( `var'," LABORATORI "," LAB ",1)
quietly replace `var' = subinstr( `var'," LABORATORY "," LAB ",1)
quietly replace `var' = subinstr( `var'," LABORTORI "," LAB ",1)
quietly replace `var' = subinstr( `var'," LAVORAZA "," LAVORAZ ",1)
quietly replace `var' = subinstr( `var'," LAVORAZIONE "," LAVORAZ ",1)
quietly replace `var' = subinstr( `var'," LAVORAZIONI "," LAVORAZ ",1)
quietly replace `var' = subinstr( `var'," LAVORAZIO "," LAVORAZ ",1)
quietly replace `var' = subinstr( `var'," LAVORAZI "," LAVORAZ ",1)
quietly replace `var' = subinstr( `var'," LIMITED PARTNERSHIP "," LP ",1)
quietly replace `var' = subinstr( `var'," LIMITED "," LTD ",1)
quietly replace `var' = subinstr( `var'," LTD LTEE "," LTD ",1)
quietly replace `var' = subinstr( `var'," MASCHINENVERTRIEB "," MASCH ",1)
quietly replace `var' = subinstr( `var'," MASCHINENBAUANSTALT "," MASCHBAU ",1)
quietly replace `var' = subinstr( `var'," MASCHINENBAU "," MASCHBAU ",1)
quietly replace `var' = subinstr( `var'," MASCHINENFABRIEK "," MASCHFAB ",1)
quietly replace `var' = subinstr( `var'," MASCHINENFABRIKEN "," MASCHFAB ",1)
quietly replace `var' = subinstr( `var'," MASCHINENFABRIK "," MASCHFAB ",1)
quietly replace `var' = subinstr( `var'," MASCHINENFAB "," MASCHFAB ",1)
quietly replace `var' = subinstr( `var'," MASCHINEN "," MASCH ",1)
quietly replace `var' = subinstr( `var'," MASCHIN "," MASCH ",1)
quietly replace `var' = subinstr( `var'," MIT BESCHRANKTER HAFTUNG "," MBH ",1)
quietly replace `var' = subinstr( `var'," MANUFACTURINGS "," MFG ",1)
quietly replace `var' = subinstr( `var'," MANUFACTURING "," MFG ",1)
quietly replace `var' = subinstr( `var'," MANIFATTURAS "," MFR ",1)
quietly replace `var' = subinstr( `var'," MANIFATTURA "," MFR ",1)
quietly replace `var' = subinstr( `var'," MANIFATTURE "," MFR ",1)
quietly replace `var' = subinstr( `var'," MANUFACTURAS "," MFR ",1)
quietly replace `var' = subinstr( `var'," MANUFACTURERS "," MFR ",1)
quietly replace `var' = subinstr( `var'," MANUFACTURER "," MFR ",1)
quietly replace `var' = subinstr( `var'," MANUFACTURES "," MFR ",1)
quietly replace `var' = subinstr( `var'," MANUFACTURE "," MFR ",1)
quietly replace `var' = subinstr( `var'," MANUFATURA "," MFR ",1)
quietly replace `var' = subinstr( `var'," MAATSCHAPPIJ "," MIJ ",1)
quietly replace `var' = subinstr( `var'," MEDICAL "," MED ",1)
quietly replace `var' = subinstr( `var'," MINISTERE "," MIN ",1)
quietly replace `var' = subinstr( `var'," MINISTERIUM "," MIN ",1)
quietly replace `var' = subinstr( `var'," MINISTERO "," MIN ",1)
quietly replace `var' = subinstr( `var'," MINISTERSTVAKH "," MIN ",1)
quietly replace `var' = subinstr( `var'," MINISTERSTVAM "," MIN ",1)
quietly replace `var' = subinstr( `var'," MINISTERSTVAMI "," MIN ",1)
quietly replace `var' = subinstr( `var'," MINISTERSTVA "," MIN ",1)
quietly replace `var' = subinstr( `var'," MINISTERSTVE "," MIN ",1)
quietly replace `var' = subinstr( `var'," MINISTERSTVO "," MIN ",1)
quietly replace `var' = subinstr( `var'," MINISTERSTVOM "," MIN ",1)
quietly replace `var' = subinstr( `var'," MINISTERSTVU "," MIN ",1)
quietly replace `var' = subinstr( `var'," MINISTERSTV "," MIN ",1)
quietly replace `var' = subinstr( `var'," MINISTERSTWO "," MIN ",1)
quietly replace `var' = subinstr( `var'," MINISTERUL "," MIN ",1)
quietly replace `var' = subinstr( `var'," MINISTRE "," MIN ",1)
quietly replace `var' = subinstr( `var'," MINISTRY "," MIN ",1)
quietly replace `var' = subinstr( `var'," MINISTER "," MIN ",1)
quietly replace `var' = subinstr( `var'," MAGYAR TUDOMANYOS AKADEMIA "," MTA ",1)
quietly replace `var' = subinstr( `var'," NATIONAAL "," NAT ",1)
quietly replace `var' = subinstr( `var'," NATIONAL "," NAT ",1)
quietly replace `var' = subinstr( `var'," NATIONALE "," NAT ",1)
quietly replace `var' = subinstr( `var'," NATIONAUX "," NAT ",1)
quietly replace `var' = subinstr( `var'," NATL "," NAT ",1)
quietly replace `var' = subinstr( `var'," NAZIONALE "," NAZ ",1)
quietly replace `var' = subinstr( `var'," NAZIONALI "," NAZ ",1)
quietly replace `var' = subinstr( `var'," NORDDEUTSCH "," NORDDEUT ",1)
quietly replace `var' = subinstr( `var'," NORDDEUTSCHE "," NORDDEUT ",1)
quietly replace `var' = subinstr( `var'," NORDDEUTSCHER "," NORDDEUT ",1)
quietly replace `var' = subinstr( `var'," NORDDEUTSCHES "," NORDDEUT ",1)
quietly replace `var' = subinstr( `var'," NARODNI PODNIK "," NP ",1)
quietly replace `var' = subinstr( `var'," NARODNIJ PODNIK "," NP ",1)
quietly replace `var' = subinstr( `var'," NARODNY PODNIK "," NP ",1)
quietly replace `var' = subinstr( `var'," NAAMLOOSE VENOOTSCHAP "," NV ",1)
quietly replace `var' = subinstr( `var'," NAAMLOZE VENNOOTSCHAP "," NV ",1)
quietly replace `var' = subinstr( `var'," N V "," NV ",1)
quietly replace `var' = subinstr( `var'," OESTERREICHISCHES "," OESTERR ",1)
quietly replace `var' = subinstr( `var'," OESTERREICHISCHE "," OESTERR ",1)
quietly replace `var' = subinstr( `var'," OESTERREICHISCH "," OESTERR ",1)
quietly replace `var' = subinstr( `var'," OESTERREICH "," OESTERR ",1)
quietly replace `var' = subinstr( `var'," OSTERREICHISCHES "," OESTERR ",1)
quietly replace `var' = subinstr( `var'," OSTERREICHISCHE "," OESTERR ",1)
quietly replace `var' = subinstr( `var'," OSTERREICHISCH "," OESTERR ",1)
quietly replace `var' = subinstr( `var'," OSTERREICH "," OESTERR ",1)
quietly replace `var' = subinstr( `var'," OFFICINE MECCANICA "," OFF MEC ",1)
quietly replace `var' = subinstr( `var'," OFFICINE MECCANICHE "," OFF MEC ",1)
quietly replace `var' = subinstr( `var'," OFFICINE NATIONALE "," OFF NAT ",1)
quietly replace `var' = subinstr( `var'," OFFENE HANDELSGESELLSCHAFT "," OHG ",1)
quietly replace `var' = subinstr( `var'," ONTWIKKELINGSBUREAU "," ONTWIK ",1)
quietly replace `var' = subinstr( `var'," ONTWIKKELINGS "," ONTWIK ",1)
quietly replace `var' = subinstr( `var'," OBOROVY PODNIK "," OP ",1)
quietly replace `var' = subinstr( `var'," ORGANISATIE "," ORG ",1)
quietly replace `var' = subinstr( `var'," ORGANISATIONS "," ORG ",1)
quietly replace `var' = subinstr( `var'," ORGANISATION "," ORG ",1)
quietly replace `var' = subinstr( `var'," ORGANIZATIONS "," ORG ",1)
quietly replace `var' = subinstr( `var'," ORGANIZATION "," ORG ",1)
quietly replace `var' = subinstr( `var'," ORGANIZZAZIONE "," ORG ",1)
quietly replace `var' = subinstr( `var'," OSAKEYHTIO "," OY ",1)
quietly replace `var' = subinstr( `var'," PHARMACEUTICALS "," PHARM ",1)
quietly replace `var' = subinstr( `var'," PHARMACEUTICAL "," PHARM ",1)
quietly replace `var' = subinstr( `var'," PHARMACEUTICA "," PHARM ",1)
quietly replace `var' = subinstr( `var'," PHARMACEUTIQUES "," PHARM ",1)
quietly replace `var' = subinstr( `var'," PHARMACEUTIQUE "," PHARM ",1)
quietly replace `var' = subinstr( `var'," PHARMAZEUTIKA "," PHARM ",1)
quietly replace `var' = subinstr( `var'," PHARMAZEUTISCHEN "," PHARM ",1)
quietly replace `var' = subinstr( `var'," PHARMAZEUTISCHE "," PHARM ",1)
quietly replace `var' = subinstr( `var'," PHARMAZEUTISCH "," PHARM ",1)
quietly replace `var' = subinstr( `var'," PHARMAZIE "," PHARM ",1)
quietly replace `var' = subinstr( `var'," PUBLIC LIMITED COMPANY "," PLC ",1)
quietly replace `var' = subinstr( `var'," PRELUCRAREA "," PRELUC ",1)
quietly replace `var' = subinstr( `var'," PRELUCRARE "," PRELUC ",1)
quietly replace `var' = subinstr( `var'," PRODOTTI "," PROD ",1)
quietly replace `var' = subinstr( `var'," PRODUCE "," PROD ",1)
quietly replace `var' = subinstr( `var'," PRODUCTS "," PROD ",1)
quietly replace `var' = subinstr( `var'," PRODUCT "," PROD ",1)
quietly replace `var' = subinstr( `var'," PRODUCTAS "," PROD ",1)
quietly replace `var' = subinstr( `var'," PRODUCTA "," PROD ",1)
quietly replace `var' = subinstr( `var'," PRODUCTIE "," PROD ",1)
quietly replace `var' = subinstr( `var'," PRODUCTOS "," PROD ",1)
quietly replace `var' = subinstr( `var'," PRODUCTO "," PROD ",1)
quietly replace `var' = subinstr( `var'," PRODUCTORES "," PROD ",1)
quietly replace `var' = subinstr( `var'," PRODUIT CHIMIQUES "," PROD CHIM ",1)
quietly replace `var' = subinstr( `var'," PRODUIT CHIMIQUE "," PROD CHIM ",1)
quietly replace `var' = subinstr( `var'," PRODUITS "," PROD ",1)
quietly replace `var' = subinstr( `var'," PRODUIT "," PROD ",1)
quietly replace `var' = subinstr( `var'," PRODUKCJI "," PROD ",1)
quietly replace `var' = subinstr( `var'," PRODUKTER "," PROD ",1)
quietly replace `var' = subinstr( `var'," PRODUKTE "," PROD ",1)
quietly replace `var' = subinstr( `var'," PRODUKT "," PROD ",1)
quietly replace `var' = subinstr( `var'," PRODUSE "," PROD ",1)
quietly replace `var' = subinstr( `var'," PRODUTOS "," PROD ",1)
quietly replace `var' = subinstr( `var'," PRODUCTIONS "," PRODN ",1)
quietly replace `var' = subinstr( `var'," PRODUCTION "," PRODN ",1)
quietly replace `var' = subinstr( `var'," PRODUKTIONS "," PRODN ",1)
quietly replace `var' = subinstr( `var'," PRODUKTION "," PRODN ",1)
quietly replace `var' = subinstr( `var'," PRODUZIONI "," PRODN ",1)
quietly replace `var' = subinstr( `var'," PROIECTARE "," PROI ",1)
quietly replace `var' = subinstr( `var'," PROIECTARI "," PROI ",1)
quietly replace `var' = subinstr( `var'," PRZEDSIEBIOSTWO "," PRZEDSIEB ",1)
quietly replace `var' = subinstr( `var'," PRZEMYSLU "," PRZEYM ",1)
quietly replace `var' = subinstr( `var'," PROPRIETARY "," PTY ",1)
quietly replace `var' = subinstr( `var'," PERSONENVENNOOTSCHAP MET "," PVBA ",1)
quietly replace `var' = subinstr( `var'," BEPERKTE AANSPRAKELIJKHEID "," PVBA ",1)
quietly replace `var' = subinstr( `var'," REALISATIONS "," REAL ",1)
quietly replace `var' = subinstr( `var'," REALISATION "," REAL ",1)
quietly replace `var' = subinstr( `var'," RECHERCHES ET DEVELOPMENTS "," R&D ",1)
quietly replace `var' = subinstr( `var'," RECHERCHE ET DEVELOPMENT "," R&D ",1)
quietly replace `var' = subinstr( `var'," RECHERCHES ET DEVELOPPEMENTS "," R&D ",1)
quietly replace `var' = subinstr( `var'," RECHERCHE ET DEVELOPPEMENT "," R&D ",1)
quietly replace `var' = subinstr( `var'," RESEARCH & DEVELOPMENT "," R&D ",1)
quietly replace `var' = subinstr( `var'," RESEARCH AND DEVELOPMENT "," R&D ",1)
quietly replace `var' = subinstr( `var'," RESEARCH "," RES ",1)
quietly replace `var' = subinstr( `var'," RECHERCHES "," RECH ",1)
quietly replace `var' = subinstr( `var'," RECHERCHE "," RECH ",1)
quietly replace `var' = subinstr( `var'," RIJKSUNIVERSITEIT "," RIJKSUNIV ",1)
quietly replace `var' = subinstr( `var'," SECRETATY "," SECRETARY ",1)
quietly replace `var' = subinstr( `var'," SECRETRY "," SECRETARY ",1)
quietly replace `var' = subinstr( `var'," SECREATRY "," SECRETARY ",1)
quietly replace `var' = subinstr( `var'," SOCIETE ANONYME DITE "," SA ",1)
quietly replace `var' = subinstr( `var'," SOCIEDAD ANONIMA "," SA ",1)
quietly replace `var' = subinstr( `var'," SOCIETE ANONYME "," SA ",1)
quietly replace `var' = subinstr( `var'," SOCIETE A RESPONSABILITE LIMITEE "," SARL ",1)
quietly replace `var' = subinstr( `var'," SOCIETE A RESPONSIBILITE LIMITEE "," SARL ",1)
quietly replace `var' = subinstr( `var'," SOCIETA IN ACCOMANDITA SEMPLICE "," SAS ",1)
quietly replace `var' = subinstr( `var'," SCHWEIZERISCHES "," SCHWEIZ ",1)
quietly replace `var' = subinstr( `var'," SCHWEIZERISCHER "," SCHWEIZ ",1)
quietly replace `var' = subinstr( `var'," SCHWEIZERISCHE "," SCHWEIZ ",1)
quietly replace `var' = subinstr( `var'," SCHWEIZERISCH "," SCHWEIZ ",1)
quietly replace `var' = subinstr( `var'," SCHWEIZER "," SCHWEIZ ",1)
quietly replace `var' = subinstr( `var'," SCIENCES "," SCI ",1)
quietly replace `var' = subinstr( `var'," SCIENCE "," SCI ",1)
quietly replace `var' = subinstr( `var'," SCIENTIFICA "," SCI ",1)
quietly replace `var' = subinstr( `var'," SCIENTIFIC "," SCI ",1)
quietly replace `var' = subinstr( `var'," SCIENTIFIQUES "," SCI ",1)
quietly replace `var' = subinstr( `var'," SCIENTIFIQUE "," SCI ",1)
quietly replace `var' = subinstr( `var'," SHADAN HOJIN "," SH ",1)
quietly replace `var' = subinstr( `var'," SIDERURGICAS "," SIDER ",1)
quietly replace `var' = subinstr( `var'," SIDERURGICA "," SIDER ",1)
quietly replace `var' = subinstr( `var'," SIDERURGIC "," SIDER ",1)
quietly replace `var' = subinstr( `var'," SIDERURGIE "," SIDER ",1)
quietly replace `var' = subinstr( `var'," SIDERURGIQUE "," SIDER ",1)
quietly replace `var' = subinstr( `var'," SOCIETA IN NOME COLLECTIVO "," SNC ",1)
quietly replace `var' = subinstr( `var'," SOCIETE EN NOM COLLECTIF "," SNC ",1)
quietly replace `var' = subinstr( `var'," SOCIETE ALSACIENNE "," SOC ALSAC ",1)
quietly replace `var' = subinstr( `var'," SOCIETE APPLICATION "," SOC APPL ",1)
quietly replace `var' = subinstr( `var'," SOCIETA APPLICAZIONE "," SOC APPL ",1)
quietly replace `var' = subinstr( `var'," SOCIETE AUXILIAIRE "," SOC AUX ",1)
quietly replace `var' = subinstr( `var'," SOCIETE CHIMIQUE "," SOC CHIM ",1)
quietly replace `var' = subinstr( `var'," SOCIEDAD CIVIL "," SOC CIV ",1)
quietly replace `var' = subinstr( `var'," SOCIETE CIVILE "," SOC CIV ",1)
quietly replace `var' = subinstr( `var'," SOCIETE COMMERCIALES "," SOC COMML ",1)
quietly replace `var' = subinstr( `var'," SOCIETE COMMERCIALE "," SOC COMML ",1)
quietly replace `var' = subinstr( `var'," SOCIEDAD ESPANOLA "," SOC ESPAN ",1)
quietly replace `var' = subinstr( `var'," SOCIETE ETUDES "," SOC ETUD ",1)
quietly replace `var' = subinstr( `var'," SOCIETE ETUDE "," SOC ETUD ",1)
quietly replace `var' = subinstr( `var'," SOCIETE EXPLOITATION "," SOC EXPL ",1)
quietly replace `var' = subinstr( `var'," SOCIETE GENERALE "," SOC GEN ",1)
quietly replace `var' = subinstr( `var'," SOCIETE INDUSTRIELLES "," SOC IND ",1)
quietly replace `var' = subinstr( `var'," SOCIETE INDUSTRIELLE "," SOC IND ",1)
quietly replace `var' = subinstr( `var'," SOCIETE MECANIQUES "," SOC MEC ",1)
quietly replace `var' = subinstr( `var'," SOCIETE MECANIQUE "," SOC MEC ",1)
quietly replace `var' = subinstr( `var'," SOCIETE NATIONALE "," SOC NAT ",1)
quietly replace `var' = subinstr( `var'," SOCIETE NOUVELLE "," SOC NOUV ",1)
quietly replace `var' = subinstr( `var'," SOCIETE PARISIENNE "," SOC PARIS ",1)
quietly replace `var' = subinstr( `var'," SOCIETE PARISIENN "," SOC PARIS ",1)
quietly replace `var' = subinstr( `var'," SOCIETE PARISIEN "," SOC PARIS ",1)
quietly replace `var' = subinstr( `var'," SOCIETE TECHNIQUES "," SOC TECH ",1)
quietly replace `var' = subinstr( `var'," SOCIETE TECHNIQUE "," SOC TECH ",1)
quietly replace `var' = subinstr( `var'," SDRUZENI PODNIKU "," SP ",1)
quietly replace `var' = subinstr( `var'," SDRUZENI PODNIK "," SP ",1)
quietly replace `var' = subinstr( `var'," SOCIETA PER AZIONI "," SPA ",1)
quietly replace `var' = subinstr( `var'," SPITALUL "," SPITAL ",1)
quietly replace `var' = subinstr( `var'," SOCIETE PRIVEE A RESPONSABILITE LIMITEE "," SPRL ",1)
quietly replace `var' = subinstr( `var'," SOCIEDAD DE RESPONSABILIDAD LIMITADA "," SRL ",1)
quietly replace `var' = subinstr( `var'," STIINTIFICA "," STIINT ",1)
quietly replace `var' = subinstr( `var'," SUDDEUTSCHES "," SUDDEUT ",1)
quietly replace `var' = subinstr( `var'," SUDDEUTSCHER "," SUDDEUT ",1)
quietly replace `var' = subinstr( `var'," SUDDEUTSCHE "," SUDDEUT ",1)
quietly replace `var' = subinstr( `var'," SUDDEUTSCH "," SUDDEUT ",1)
quietly replace `var' = subinstr( `var'," SOCIEDADE "," SOC ",1)
quietly replace `var' = subinstr( `var'," SOCIEDAD "," SOC ",1)
quietly replace `var' = subinstr( `var'," SOCIETA "," SOC ",1)
quietly replace `var' = subinstr( `var'," SOCIETE "," SOC ",1)
quietly replace `var' = subinstr( `var'," SOCIETY "," SOC ",1)
quietly replace `var' = subinstr( `var'," SA DITE "," SA ",1)
quietly replace `var' = subinstr( `var'," TECHNICAL "," TECH ",1)
quietly replace `var' = subinstr( `var'," TECHNICO "," TECH ",1)
quietly replace `var' = subinstr( `var'," TECHNICZNY "," TECH ",1)
quietly replace `var' = subinstr( `var'," TECHNIKAI "," TECH ",1)
quietly replace `var' = subinstr( `var'," TECHNIKI "," TECH ",1)
quietly replace `var' = subinstr( `var'," TECHNIK "," TECH ",1)
quietly replace `var' = subinstr( `var'," TECHNIQUES "," TECH ",1)
quietly replace `var' = subinstr( `var'," TECHNIQUE "," TECH ",1)
quietly replace `var' = subinstr( `var'," TECHNISCHES "," TECH ",1)
quietly replace `var' = subinstr( `var'," TECHNISCHE "," TECH ",1)
quietly replace `var' = subinstr( `var'," TECHNISCH "," TECH ",1)
quietly replace `var' = subinstr( `var'," TECHNOLOGY "," TECH ",1)
quietly replace `var' = subinstr( `var'," TECHNOLOGIES "," TECH ",1)
quietly replace `var' = subinstr( `var'," TELECOMMUNICATIONS "," TELECOM ",1)
quietly replace `var' = subinstr( `var'," TELECOMMUNICACION "," TELECOM ",1)
quietly replace `var' = subinstr( `var'," TELECOMMUNICATION "," TELECOM ",1)
quietly replace `var' = subinstr( `var'," TELECOMMUNICAZIONI "," TELECOM ",1)
quietly replace `var' = subinstr( `var'," TELECOMUNICAZIONI "," TELECOM ",1)
quietly replace `var' = subinstr( `var'," TRUSTUL "," TRUST ",1)
quietly replace `var' = subinstr( `var'," UNITED KINGDOM "," UK ",1)
quietly replace `var' = subinstr( `var'," SECRETARY OF STATE FOR "," UK SEC FOR ",1)
quietly replace `var' = subinstr( `var'," UNIVERSIDADE "," UNIV ",1)
quietly replace `var' = subinstr( `var'," UNIVERSIDAD "," UNIV ",1)
quietly replace `var' = subinstr( `var'," UNIVERSITA DEGLI STUDI "," UNIV ",1)
quietly replace `var' = subinstr( `var'," UNIVERSITAET "," UNIV ",1)
quietly replace `var' = subinstr( `var'," UNIVERSITAIRE "," UNIV ",1)
quietly replace `var' = subinstr( `var'," UNIVERSITAIR "," UNIV ",1)
quietly replace `var' = subinstr( `var'," UNIVERSITATEA "," UNIV ",1)
quietly replace `var' = subinstr( `var'," UNIVERSITEIT "," UNIV ",1)
quietly replace `var' = subinstr( `var'," UNIVERSITETAMI "," UNIV ",1)
quietly replace `var' = subinstr( `var'," UNIVERSITETAM "," UNIV ",1)
quietly replace `var' = subinstr( `var'," UNIVERSITETE "," UNIV ",1)
quietly replace `var' = subinstr( `var'," UNIVERSITETOM "," UNIV ",1)
quietly replace `var' = subinstr( `var'," UNIVERSITETOV "," UNIV ",1)
quietly replace `var' = subinstr( `var'," UNIVERSITETU "," UNIV ",1)
quietly replace `var' = subinstr( `var'," UNIVERSITETY "," UNIV ",1)
quietly replace `var' = subinstr( `var'," UNIVERSITETA "," UNIV ",1)
quietly replace `var' = subinstr( `var'," UNIVERSITAT "," UNIV ",1)
quietly replace `var' = subinstr( `var'," UNIVERSITET "," UNIV ",1)
quietly replace `var' = subinstr( `var'," UNIVERSITE "," UNIV ",1)
quietly replace `var' = subinstr( `var'," UNIVERSITY "," UNIV ",1)
quietly replace `var' = subinstr( `var'," UNIVERSITA "," UNIV ",1)
quietly replace `var' = subinstr( `var'," UNIWERSYTET "," UNIV ",1)
quietly replace `var' = subinstr( `var'," UNITED STATES OF AMERICA ADMINISTRATOR "," US ADMIN ",1)
quietly replace `var' = subinstr( `var'," UNITED STATES OF AMERICA AS REPRESENTED BY THE ADMINISTRATOR "," US ADMIN ",1)
quietly replace `var' = subinstr( `var'," UNITED STATES OF AMERICA AS REPRESENTED BY THE DEPT "," US DEPT ",1)
quietly replace `var' = subinstr( `var'," UNITED STATES OF AMERICA AS REPRESENTED BY THE UNITED STATES DEPT "," US DEPT ",1)
quietly replace `var' = subinstr( `var'," UNITED STATES OF AMERICAN AS REPRESENTED BY THE UNITED STATES DEPT "," US DEPT ",1)
quietly replace `var' = subinstr( `var'," UNITED STATES GOVERNMENT AS REPRESENTED BY THE SECRETARY OF "," US SEC ",1)
quietly replace `var' = subinstr( `var'," UNITED STATES OF AMERICA REPRESENTED BY THE SECRETARY "," US SEC ",1)
quietly replace `var' = subinstr( `var'," UNITED STATES OF AMERICA AS REPRESENTED BY THE SECRETARY "," US SEC ",1)
quietly replace `var' = subinstr( `var'," UNITED STATES OF AMERICAS AS REPRESENTED BY THE SECRETARY "," US SEC ",1)
quietly replace `var' = subinstr( `var'," UNITES STATES OF AMERICA AS REPRESENTED BY THE SECRETARY "," US SEC ",1)
quietly replace `var' = subinstr( `var'," UNITED STATES OF AMERICA SECRETARY OF "," US SEC ",1)
quietly replace `var' = subinstr( `var'," UNITED STATES OF AMERICA "," USA ",1)
quietly replace `var' = subinstr( `var'," UNITED STATES "," USA ",1)
quietly replace `var' = subinstr( `var'," UTILAJE "," UTIL ",1)
quietly replace `var' = subinstr( `var'," UTILAJ "," UTIL ",1)
quietly replace `var' = subinstr( `var'," UTILISATIONS VOLKSEIGENER BETRIEBE "," VEB ",1)
quietly replace `var' = subinstr( `var'," UTILISATION VOLKSEIGENER BETRIEBE "," VEB ",1)
quietly replace `var' = subinstr( `var'," VEB KOMBINAT "," VEB KOMB ",1)
quietly replace `var' = subinstr( `var'," VEREENIGDE "," VER ",1)
quietly replace `var' = subinstr( `var'," VEREINIGTES VEREINIGUNG "," VER ",1)
quietly replace `var' = subinstr( `var'," VEREINIGTE VEREINIGUNG "," VER ",1)
quietly replace `var' = subinstr( `var'," VEREIN "," VER ",1)
quietly replace `var' = subinstr( `var'," VERENIGING "," VER ",1)
quietly replace `var' = subinstr( `var'," VERWALTUNGEN "," VERW ",1)
quietly replace `var' = subinstr( `var'," VERWALTUNGS "," VERW ",1)
quietly replace `var' = subinstr( `var'," VERWERTUNGS "," VERW ",1)
quietly replace `var' = subinstr( `var'," VERWALTUNGSGESELLSCHAFT "," VERW GES ",1)
quietly replace `var' = subinstr( `var'," VYZK USTAV "," VU ",1)
quietly replace `var' = subinstr( `var'," VYZKUMNY USTAV "," VU ",1)
quietly replace `var' = subinstr( `var'," VYZKUMNYUSTAV "," VU ",1)
quietly replace `var' = subinstr( `var'," VEREINIGUNG VOLKSEIGENER BETRIEBUNG "," VVB ",1)
quietly replace `var' = subinstr( `var'," VYZK VYVOJOVY USTAV "," VVU ",1)
quietly replace `var' = subinstr( `var'," VYZKUMNY VYVOJOVY USTAV "," VVU ",1)
quietly replace `var' = subinstr( `var'," WERKZEUGMASCHINENKOMBINAT "," WERKZ MASCH KOMB ",1)
quietly replace `var' = subinstr( `var'," WERKZEUGMASCHINENFABRIK "," WERKZ MASCHFAB ",1)
quietly replace `var' = subinstr( `var'," WESTDEUTSCHES "," WESTDEUT ",1)
quietly replace `var' = subinstr( `var'," WESTDEUTSCHER "," WESTDEUT ",1)
quietly replace `var' = subinstr( `var'," WESTDEUTSCHE "," WESTDEUT ",1)
quietly replace `var' = subinstr( `var'," WESTDEUTSCH "," WESTDEUT ",1)
quietly replace `var' = subinstr( `var'," WISSENSCHAFTLICHE "," WISS ",1)
quietly replace `var' = subinstr( `var'," WISSENSCHAFTLICHES TECHNISCHES ZENTRUM "," WTZ ",1)
quietly replace `var' = subinstr( `var'," YUGEN KAISHA "," YG YUGEN GAISHA ",1)
quietly replace `var' = subinstr( `var'," YUUGEN GAISHA "," YG YUGEN GAISHA ",1)
quietly replace `var' = subinstr( `var'," YUUGEN KAISHA "," YG YUGEN GAISHA ",1)
quietly replace `var' = subinstr( `var'," YUUGEN KAISYA "," YG YUGEN GAISHA ",1)
quietly replace `var' = subinstr( `var'," ZAVODU "," ZAVOD ",1)
quietly replace `var' = subinstr( `var'," ZAVODY "," ZAVOD ",1)
quietly replace `var' = subinstr( `var'," ZENTRALES "," ZENT ",1)
quietly replace `var' = subinstr( `var'," ZENTRALE "," ZENT ",1)
quietly replace `var' = subinstr( `var'," ZENTRALEN "," ZENT ",1)
quietly replace `var' = subinstr( `var'," ZENTRALNA "," ZENT ",1)
quietly replace `var' = subinstr( `var'," ZENTRUM "," ZENT ",1)
quietly replace `var' = subinstr( `var'," ZENTRALINSTITUT "," ZENT INST ",1)
quietly replace `var' = subinstr( `var'," ZENTRALLABORATORIUM "," ZENT LAB ",1)
quietly replace `var' = subinstr( `var'," ZAIDAN HOJIN "," ZH ",1)
quietly replace `var' = subinstr( `var'," ZAIDAN HOUJIN "," ZH ",1)
quietly replace `var' = subinstr( `var'," LIMITED "," LTD ",1)
quietly replace `var' = subinstr( `var'," LIMITADA "," LTDA ",1)
quietly replace `var' = subinstr( `var'," SECRETARY "," SEC ",1)
}

********************************
****** STANDARDIZE ENTITY ******
********************************
assert substr(standard_name,1,1)==" "
assert substr(standard_name,length(standard_name),1)==" "

** UNITED KINGDOM
replace standard_name = subinstr( standard_name, " PUBLIC LIMITED ", " PLC ", 1)
replace standard_name = subinstr( standard_name, " PUBLIC LIABILITY COMPANY ", " PLC ", 1)
replace standard_name = subinstr( standard_name, " HOLDINGS ", " HLDGS ", 1)
replace standard_name = subinstr( standard_name, " HOLDING ", " HLDGS ", 1)
replace standard_name = subinstr( standard_name, " GREAT BRITAIN ", " GB ", 1)
replace standard_name = subinstr( standard_name, " LTD CO ", " CO LTD ", 1)

** SPANISH
replace standard_name = subinstr( standard_name, " SOC LIMITADA ", " SL ", 1)
replace standard_name = subinstr( standard_name, " SOC EN COMMANDITA ", " SC ", 1)
replace standard_name = subinstr( standard_name, " & CIA ", " CO ", 1)

** ITALIAN
replace standard_name = subinstr( standard_name, " SOC IN ACCOMANDITA PER AZIONI ", " SA ", 1)
replace standard_name = subinstr( standard_name, " SAPA ", " SA ", 1)
replace standard_name = subinstr( standard_name, " SOC A RESPONSABILIT¿ LIMITATA ", " SRL ", 1)

** SWEDISH
replace standard_name = subinstr( standard_name, " HANDELSBOLAG ", " HB  ", 1)

** GERMAN
replace standard_name = subinstr( standard_name, " KOMANDIT GESELLSCHAFT ", " KG ", 1)
replace standard_name = subinstr( standard_name, " KOMANDITGESELLSCHAFT ", " KG ", 1)
replace standard_name = subinstr( standard_name, " EINGETRAGENE GENOSSENSCHAFT ", " EG ", 1)
replace standard_name = subinstr( standard_name, " GENOSSENSCHAFT ", " EG ", 1)
replace standard_name = subinstr( standard_name, " GESELLSCHAFT M B H ", " GMBH ", 1)
replace standard_name = subinstr( standard_name, " OFFENE HANDELS GESELLSCHAFT ", " OHG ", 1)
replace standard_name = subinstr( standard_name, " GESMBH ", " GMBH ", 1)
replace standard_name = subinstr( standard_name, " GESELLSCHAFT BURGERLICHEN RECHTS ", " GBR ", 1)
replace standard_name = subinstr( standard_name, " GESELLSCHAFT ", " GMBH ", 1)
* The following is common format. If conflict assume GMBH & CO KG over GMBH & CO OHG as more common.
replace standard_name = subinstr( standard_name, " GMBH CO KG ", " GMBH & CO KG ", 1)
replace standard_name = subinstr( standard_name, " GMBH COKG ", " GMBH & CO KG ", 1)
replace standard_name = subinstr( standard_name, " GMBH U CO KG ", " GMBH & CO KG ", 1)
replace standard_name = subinstr( standard_name, " GMBH U COKG ", " GMBH & CO KG ", 1)
replace standard_name = subinstr( standard_name, " GMBH U CO ", " GMBH & CO KG ", 1)
replace standard_name = subinstr( standard_name, " GMBH CO ", " GMBH & CO KG ", 1)
replace standard_name = subinstr( standard_name, " AG CO KG ", " AG & CO KG ", 1)
replace standard_name = subinstr( standard_name, " AG COKG ", " AG & CO KG ", 1)
replace standard_name = subinstr( standard_name, " AG U CO KG ", " AG & CO KG ", 1)
replace standard_name = subinstr( standard_name, " AG U COKG ", " AG & CO KG ", 1)
replace standard_name = subinstr( standard_name, " AG U CO ", " AG & CO KG ", 1)
replace standard_name = subinstr( standard_name, " AG CO ", " AG & CO KG ", 1)
replace standard_name = subinstr( standard_name, " GMBH CO OHG ", " GMBH &CO OHG ", 1)
replace standard_name = subinstr( standard_name, " GMBH COOHG ", " GMBH & CO OHG ", 1)
replace standard_name = subinstr( standard_name, " GMBH U CO OHG ", " GMBH & CO OHG ", 1)
replace standard_name = subinstr( standard_name, " GMBH U COOHG ", " GMBH & CO OHG ", 1)
replace standard_name = subinstr( standard_name, " AG CO OHG ", " AG & CO OHG ", 1)
replace standard_name = subinstr( standard_name, " AG COOHG ", " AG & CO OHG ", 1)
replace standard_name = subinstr( standard_name, " AG U CO OHG ", " AG & CO OHG ", 1)
replace standard_name = subinstr( standard_name, " AG U COOHG ", " AG & CO OHG ", 1)

** FRENCH and BELGIAN
replace standard_name = subinstr( standard_name, " SOCIETE ANONYME SIMPLIFIEE ", " SAS ", 1)
replace standard_name = subinstr( standard_name, " SOC ANONYME ", " SA ", 1)
replace standard_name = subinstr( standard_name, " STE ANONYME ", " SA ", 1)
replace standard_name = subinstr( standard_name, " SARL UNIPERSONNELLE ", " SARLU ", 1)
replace standard_name = subinstr( standard_name, " SOC PAR ACTIONS SIMPLIFIEES ", " SAS ", 1)
replace standard_name = subinstr( standard_name, " SAS UNIPERSONNELLE ", " SASU ", 1)
replace standard_name = subinstr( standard_name, " ENTREPRISE UNIPERSONNELLE A RESPONSABILITE LIMITEE ", " EURL ", 1)
replace standard_name = subinstr( standard_name, " SOCIETE CIVILE IMMOBILIERE ", " SCI ", 1)
replace standard_name = subinstr( standard_name, " GROUPEMENT D INTERET ECONOMIQUE ", " GIE ", 1)
replace standard_name = subinstr( standard_name, " SOCIETE EN PARTICIPATION ", " SP ", 1)
replace standard_name = subinstr( standard_name, " SOCIETE EN COMMANDITE SIMPLE ", " SCS ", 1)
replace standard_name = subinstr( standard_name, " ANONYME DITE ", " SA ", 1)
replace standard_name = subinstr( standard_name, " SOC DITE ", " SA ", 1)
replace standard_name = subinstr( standard_name, " & CIE ", " CO ", 1)

** BELGIAN
** Note: the Belgians use a lot of French endings, so handle as above.
** Also, they use NV (belgian) and SA (french) interchangably, so standardise to SA

replace standard_name = subinstr( standard_name, " BV BEPERKTE AANSPRAKELIJKHEID ", " BVBA ", 1)
replace standard_name = subinstr( standard_name, " COMMANDITAIRE VENNOOTSCHAP OP AANDELEN ", " CVA ", 1)
replace standard_name = subinstr( standard_name, " GEWONE COMMANDITAIRE VENNOOTSCHAP ", " GCV ", 1)
replace standard_name = subinstr( standard_name, " SOCIETE EN COMMANDITE PAR ACTIONS ", " SCA ", 1)

* Change to French language equivalents where appropriate
* Don't do this for now
*replace standard_name = subinstr( standard_name, " GCV ", " SCS ", 1)
*replace standard_name = subinstr( standard_name, " NV ", " SA ", 1)
*replace standard_name = subinstr( standard_name, " BVBA ", " SPRL ", 1)

** DENMARK
* Usually danish identifiers have a slash (eg. A/S or K/S), but these will have been removed with all
* other punctuation earlier (so just use AS or KS).
replace standard_name = subinstr( standard_name, " ANDELSSELSKABET ", " AMBA ", 1)
replace standard_name = subinstr( standard_name, " ANDELSSELSKAB ", " AMBA ", 1)
replace standard_name = subinstr( standard_name, " INTERESSENTSKABET ", " IS ", 1)
replace standard_name = subinstr( standard_name, " INTERESSENTSKAB ", " IS ", 1)
replace standard_name = subinstr( standard_name, " KOMMANDITAKTIESELSKABET ", " KAS ", 1)
replace standard_name = subinstr( standard_name, " KOMMANDITAKTIESELSKAB ", " KAS ", 1)
replace standard_name = subinstr( standard_name, " KOMMANDITSELSKABET ", " KS ", 1)
replace standard_name = subinstr( standard_name, " KOMMANDITSELSKAB ", " KS ", 1)

** NORWAY
replace standard_name = subinstr( standard_name, " ANDELSLAGET ", " AL ", 1)
replace standard_name = subinstr( standard_name, " ANDELSLAG ", " AL ", 1)
replace standard_name = subinstr( standard_name, " ANSVARLIG SELSKAPET ", " ANS ", 1)
replace standard_name = subinstr( standard_name, " ANSVARLIG SELSKAP ", " ANS ", 1)
replace standard_name = subinstr( standard_name, " AKSJESELSKAPET ", " AS ", 1)
replace standard_name = subinstr( standard_name, " AKSJESELSKAP ", " AS ", 1)
replace standard_name = subinstr( standard_name, " ALLMENNAKSJESELSKAPET ", " ASA ", 1)
replace standard_name = subinstr( standard_name, " ALLMENNAKSJESELSKAP ", " ASA ", 1)
replace standard_name = subinstr( standard_name, " SELSKAP MED DELT ANSAR ", " DA ", 1)
replace standard_name = subinstr( standard_name, " KOMMANDITTSELSKAPET ", " KS ", 1)
replace standard_name = subinstr( standard_name, " KOMMANDITTSELSKAP ", " KS ", 1)

** NETHERLANDS
replace standard_name = subinstr( standard_name, " COMMANDITAIRE VENNOOTSCHAP ", " CV ", 1)
replace standard_name = subinstr( standard_name, " COMMANDITAIRE VENNOOTSCHAP OP ANDELEN ", " CVOA ", 1)
replace standard_name = subinstr( standard_name, " VENNOOTSCHAP ONDER FIRMA ", " VOF ", 1)

** FINLAND
replace standard_name = subinstr( standard_name, " PUBLIKT AKTIEBOLAG ", " APB ", 1)
replace standard_name = subinstr( standard_name, " KOMMANDIITTIYHTIO ", " KY ", 1)
replace standard_name = subinstr( standard_name, " JULKINEN OSAKEYHTIO ", " OYJ ", 1)

** POLAND
replace standard_name = subinstr( standard_name, " SPOLKA AKCYJNA ", " SA ", 1) 
replace standard_name = subinstr( standard_name, " SPOLKA PRAWA CYWILNEGO ", " SC ", 1)
replace standard_name = subinstr( standard_name, " SPOLKA KOMANDYTOWA ", " SK ", 1)
replace standard_name = subinstr( standard_name, " SPOLKA Z OGRANICZONA ODPOWIEDZIALNOSCIA ", " SPZOO ", 1)
replace standard_name = subinstr( standard_name, " SP Z OO ", " SPZOO ", 1)
replace standard_name = subinstr( standard_name, " SPZ OO ", " SPZOO ", 1)
replace standard_name = subinstr( standard_name, " SP ZOO ", " SPZOO ", 1)

** GREECE
replace standard_name = subinstr( standard_name, " ANONYMOS ETAIRIA ", " AE ", 1)
replace standard_name = subinstr( standard_name, " ETERRORRYTHMOS ", " EE ", 1)
replace standard_name = subinstr( standard_name, " ETAIRIA PERIORISMENIS EVTHINIS ", " EPE ", 1)
replace standard_name = subinstr( standard_name, " OMORRYTHMOS ", " OE ", 1)

** CZECH REPUBLIC
replace standard_name = subinstr( standard_name, " AKCIOVA SPOLECNOST ", " AS ", 1)
replace standard_name = subinstr( standard_name, " KOMANDITNI SPOLECNOST ", " KS ", 1)
replace standard_name = subinstr( standard_name, " SPOLECNOST S RUCENIM OMEZENYM ", " SRO ", 1)
replace standard_name = subinstr( standard_name, " VEREJNA OBCHODNI SPOLECNOST ", " VOS ", 1) 
                
** BULGARIA
replace standard_name = subinstr( standard_name, " AKTIONIERNO DRUSHESTWO ", " AD ", 1)
replace standard_name = subinstr( standard_name, " KOMANDITNO DRUSHESTWO ", " KD ", 1)
replace standard_name = subinstr( standard_name, " KOMANDITNO DRUSHESTWO S AKZII ", " KDA ", 1)
replace standard_name = subinstr( standard_name, " DRUSHESTWO S ORGRANITSCHENA OTGOWORNOST ", " OCD ", 1)

********************************
****** STANDARDIZE ABBREV ******
********************************

*** FOR INSTANCE, "A B C INC" -> "ABC INC"

gen str90 outname = " "

* remove quote characters
replace standard_name = subinstr( standard_name, `"""',  "", 30)

local i = 1
while `i' <= _N {
	
	* do next name
	local j = 1
	local len = wordcount(standard_name[`i']) 

	* do next word	
	while `j' <= `len' {
		
		local outword = word( standard_name[`i'], `j')
		if  strlen("`outword'")~=1 | strlen(word( standard_name[`i'],`j'+1))!=1  {
			local outword = "`outword'"+" "
		}
		qui replace outname = outname + "`outword'" in `i'

		local j = `j' + 1
	}
	local i = `i' + 1
}

count if standard_name!=outname
replace standard_name = outname if standard_name!=outname
drop outname


rename standard_name assignee_derwent

replace assignee_derwent=trim(assignee_derwent)
replace assignee_derwent=itrim(assignee_derwent)
label var assignee_derwent "primary assignee derwent std following NBER PDP"


********************************
****** 	ASSIGNEE TYPE	  ******
********************************
replace assignee_std=" "+assignee_std+" "

*** CORPORATE ***
gen asg_corp=0
qui replace asg_corp = 1 if strpos(assignee_std," & BRO ")>0
qui replace asg_corp = 1 if strpos(assignee_std," & BROTHER ")>0
qui replace asg_corp = 1 if strpos(assignee_std," & C ")>0
qui replace asg_corp = 1 if strpos(assignee_std," & CIE ")>0
qui replace asg_corp = 1 if strpos(assignee_std," & CO ")>0
qui replace asg_corp = 1 if strpos(assignee_std," & FILS ")>0
qui replace asg_corp = 1 if strpos(assignee_std," & PARTNER ")>0
qui replace asg_corp = 1 if strpos(assignee_std," & SOEHNE ")>0
qui replace asg_corp = 1 if strpos(assignee_std," & SOHN ")>0
qui replace asg_corp = 1 if strpos(assignee_std," & SON ")>0
qui replace asg_corp = 1 if strpos(assignee_std," & SONS ")>0
qui replace asg_corp = 1 if strpos(assignee_std," & ZN ")>0
qui replace asg_corp = 1 if strpos(assignee_std," & ZONEN ")>0
qui replace asg_corp = 1 if strpos(assignee_std," A ")>0
qui replace asg_corp = 1 if strpos(assignee_std," A G ")>0
qui replace asg_corp = 1 if strpos(assignee_std," A RL ")>0
qui replace asg_corp = 1 if strpos(assignee_std," A S ")>0
qui replace asg_corp = 1 if strpos(assignee_std," AANSPRAKELIJKHEID ")>0
qui replace asg_corp = 1 if strpos(assignee_std," AB ")>0
qui replace asg_corp = 1 if strpos(assignee_std," ACTIEN GESELLSCHAFT ")>0
qui replace asg_corp = 1 if strpos(assignee_std," ACTIENGESELLSCHAFT ")>0
qui replace asg_corp = 1 if strpos(assignee_std," AD ")>0
qui replace asg_corp = 1 if strpos(assignee_std," ADVIESBUREAU ")>0
qui replace asg_corp = 1 if strpos(assignee_std," AE ")>0
qui replace asg_corp = 1 if strpos(assignee_std," AG ")>0
qui replace asg_corp = 1 if strpos(assignee_std," AG & CO ")>0
qui replace asg_corp = 1 if strpos(assignee_std," AGG ")>0
qui replace asg_corp = 1 if strpos(assignee_std," AGSA ")>0
qui replace asg_corp = 1 if strpos(assignee_std," AK TIEBOLAGET ")>0
qui replace asg_corp = 1 if strpos(assignee_std," AKIEBOLAG ")>0
qui replace asg_corp = 1 if strpos(assignee_std," AKIEBOLG ")>0
qui replace asg_corp = 1 if strpos(assignee_std," AKIENGESELLSCHAFT ")>0
qui replace asg_corp = 1 if strpos(assignee_std," AKITENGESELLSCHAFT ")>0
qui replace asg_corp = 1 if strpos(assignee_std," AKITIEBOLAG ")>0
qui replace asg_corp = 1 if strpos(assignee_std," AKLIENGISELLSCHAFT ")>0
qui replace asg_corp = 1 if strpos(assignee_std," AKSJESELSKAP ")>0
qui replace asg_corp = 1 if strpos(assignee_std," AKSJESELSKAPET ")>0
qui replace asg_corp = 1 if strpos(assignee_std," AKSTIEBOLAGET ")>0
qui replace asg_corp = 1 if strpos(assignee_std," AKTAINGESELLSCHAFT ")>0
qui replace asg_corp = 1 if strpos(assignee_std," AKTEIBOLAG ")>0
qui replace asg_corp = 1 if strpos(assignee_std," AKTEINGESELLSCHAFT ")>0
qui replace asg_corp = 1 if strpos(assignee_std," AKTIBOLAG ")>0
qui replace asg_corp = 1 if strpos(assignee_std," AKTIE BOLAGET ")>0
qui replace asg_corp = 1 if strpos(assignee_std," AKTIEBDAG ")>0
qui replace asg_corp = 1 if strpos(assignee_std," AKTIEBLOAG ")>0
qui replace asg_corp = 1 if strpos(assignee_std," AKTIEBOALG ")>0
qui replace asg_corp = 1 if strpos(assignee_std," AKTIEBOALGET ")>0
qui replace asg_corp = 1 if strpos(assignee_std," AKTIEBOCAG ")>0
qui replace asg_corp = 1 if strpos(assignee_std," AKTIEBOLAC ")>0
qui replace asg_corp = 1 if strpos(assignee_std," AKTIEBOLAF ")>0
qui replace asg_corp = 1 if strpos(assignee_std," AKTIEBOLAG ")>0
qui replace asg_corp = 1 if strpos(assignee_std," AKTIEBOLAGET ")>0
qui replace asg_corp = 1 if strpos(assignee_std," AKTIEBOLAQ ")>0
qui replace asg_corp = 1 if strpos(assignee_std," AKTIEBOLOG ")>0
qui replace asg_corp = 1 if strpos(assignee_std," AKTIEGBOLAG ")>0
qui replace asg_corp = 1 if strpos(assignee_std," AKTIEGESELLSCHAFT ")>0
qui replace asg_corp = 1 if strpos(assignee_std," AKTIEGOLAGET ")>0
qui replace asg_corp = 1 if strpos(assignee_std," AKTIELBOLAG ")>0
qui replace asg_corp = 1 if strpos(assignee_std," AKTIEN ")>0
qui replace asg_corp = 1 if strpos(assignee_std," AKTIEN GESELLSCHAFT ")>0
qui replace asg_corp = 1 if strpos(assignee_std," AKTIENBOLAG ")>0
qui replace asg_corp = 1 if strpos(assignee_std," AKTIENBOLAGET ")>0
qui replace asg_corp = 1 if strpos(assignee_std," AKTIENEGESELLSCHAFT ")>0
qui replace asg_corp = 1 if strpos(assignee_std," AKTIENEGSELLSCHAFT ")>0
qui replace asg_corp = 1 if strpos(assignee_std," AKTIENGEGESELLSCHAFT ")>0
qui replace asg_corp = 1 if strpos(assignee_std," AKTIENGELLSCHAFT ")>0
qui replace asg_corp = 1 if strpos(assignee_std," AKTIENGESCELLSCHAFT ")>0
qui replace asg_corp = 1 if strpos(assignee_std," AKTIENGESELL SCHAFT ")>0
qui replace asg_corp = 1 if strpos(assignee_std," AKTIENGESELLCHAFT ")>0
qui replace asg_corp = 1 if strpos(assignee_std," AKTIENGESELLESCHAFT ")>0
qui replace asg_corp = 1 if strpos(assignee_std," AKTIENGESELLESHAFT ")>0
qui replace asg_corp = 1 if strpos(assignee_std," AKTIENGESELLS ")>0
qui replace asg_corp = 1 if strpos(assignee_std," AKTIENGESELLSCAFT ")>0
qui replace asg_corp = 1 if strpos(assignee_std," AKTIENGESELLSCGAFT ")>0
qui replace asg_corp = 1 if strpos(assignee_std," AKTIENGESELLSCHAFT ")>0
qui replace asg_corp = 1 if strpos(assignee_std," AKTIENGESELLSCHART ")>0
qui replace asg_corp = 1 if strpos(assignee_std," AKTIENGESELLSCHATT ")>0
qui replace asg_corp = 1 if strpos(assignee_std," AKTIENGESELLSCHGT ")>0
qui replace asg_corp = 1 if strpos(assignee_std," AKTIENGESELLSCHRAFT ")>0
qui replace asg_corp = 1 if strpos(assignee_std," AKTIENGESELLSHAFT ")>0
qui replace asg_corp = 1 if strpos(assignee_std," AKTIENGESELLSHAT ")>0
qui replace asg_corp = 1 if strpos(assignee_std," AKTIENGESELLSHCAFT ")>0
qui replace asg_corp = 1 if strpos(assignee_std," AKTIENGESELSCHAFT ")>0
qui replace asg_corp = 1 if strpos(assignee_std," AKTIENGESESCHAFT ")>0
qui replace asg_corp = 1 if strpos(assignee_std," AKTIENGESILLSCHAFT ")>0
qui replace asg_corp = 1 if strpos(assignee_std," AKTIENGESLLSCHAFT ")>0
qui replace asg_corp = 1 if strpos(assignee_std," AKTIENGESSELLSCHAFT ")>0
qui replace asg_corp = 1 if strpos(assignee_std," AKTIENGESSELSCHAFT ")>0
qui replace asg_corp = 1 if strpos(assignee_std," AKTIENGSELLSCHAFT ")>0
qui replace asg_corp = 1 if strpos(assignee_std," AKTIENGTESELLSCHAFT ")>0
qui replace asg_corp = 1 if strpos(assignee_std," AKTIENRESELLSCHAFT ")>0
qui replace asg_corp = 1 if strpos(assignee_std," AKTIESELSKAB ")>0
qui replace asg_corp = 1 if strpos(assignee_std," AKTIESELSKABET ")>0
qui replace asg_corp = 1 if strpos(assignee_std," AKTINGESELLSCHAFT ")>0
qui replace asg_corp = 1 if strpos(assignee_std," AKTSIONERNAYA KOMPANIA ")>0
qui replace asg_corp = 1 if strpos(assignee_std," AKTSIONERNO ")>0
qui replace asg_corp = 1 if strpos(assignee_std," AKTSIONERNOE OBCHESTVO ")>0
qui replace asg_corp = 1 if strpos(assignee_std," AKTSIONERNOE OBSCHEDTVO ")>0
qui replace asg_corp = 1 if strpos(assignee_std," AKTSIONERNOE OBSCNESTVO ")>0
qui replace asg_corp = 1 if strpos(assignee_std," AKTSIONERNOE OBSHESTVO ")>0
qui replace asg_corp = 1 if strpos(assignee_std," AKTSIONERNOE OSBCHESTVO ")>0
qui replace asg_corp = 1 if strpos(assignee_std," AKTSIONERNOEOBSCHESTVO ")>0
qui replace asg_corp = 1 if strpos(assignee_std," ALTIENGESELLSCHAFT ")>0
qui replace asg_corp = 1 if strpos(assignee_std," AMBA ")>0
qui replace asg_corp = 1 if strpos(assignee_std," AND SONS ")>0
qui replace asg_corp = 1 if strpos(assignee_std," ANDELSSELSKABET ")>0
qui replace asg_corp = 1 if strpos(assignee_std," ANLAGENGESELLSCHAFT ")>0
qui replace asg_corp = 1 if strpos(assignee_std," APPARATEBAU ")>0
qui replace asg_corp = 1 if strpos(assignee_std," APPERATEBAU ")>0
qui replace asg_corp = 1 if strpos(assignee_std," ARL ")>0
qui replace asg_corp = 1 if strpos(assignee_std," AS ")>0
qui replace asg_corp = 1 if strpos(assignee_std," ASA ")>0
qui replace asg_corp = 1 if strpos(assignee_std," ASKTIENGESELLSCHAFT ")>0
qui replace asg_corp = 1 if strpos(assignee_std," ASOCIADOS ")>0
qui replace asg_corp = 1 if strpos(assignee_std," ASSCOIATES ")>0
qui replace asg_corp = 1 if strpos(assignee_std," ASSOCIADOS ")>0
qui replace asg_corp = 1 if strpos(assignee_std," ASSOCIATE ")>0
qui replace asg_corp = 1 if strpos(assignee_std," ASSOCIATED ")>0
qui replace asg_corp = 1 if strpos(assignee_std," ASSOCIATES ")>0
qui replace asg_corp = 1 if strpos(assignee_std," ASSOCIATI ")>0
qui replace asg_corp = 1 if strpos(assignee_std," ASSOCIATO ")>0
qui replace asg_corp = 1 if strpos(assignee_std," ASSOCIES ")>0
qui replace asg_corp = 1 if strpos(assignee_std," ASSSOCIATES ")>0
qui replace asg_corp = 1 if strpos(assignee_std," ATELIER ")>0
qui replace asg_corp = 1 if strpos(assignee_std," ATELIERS ")>0
qui replace asg_corp = 1 if strpos(assignee_std," ATIBOLAG ")>0
qui replace asg_corp = 1 if strpos(assignee_std," ATKIEBOLAG ")>0
qui replace asg_corp = 1 if strpos(assignee_std," ATKIENGESELLSCHAFT ")>0
qui replace asg_corp = 1 if strpos(assignee_std," AVV ")>0
qui replace asg_corp = 1 if strpos(assignee_std," B ")>0
qui replace asg_corp = 1 if strpos(assignee_std," BANK ")>0
qui replace asg_corp = 1 if strpos(assignee_std," BANQUE ")>0
qui replace asg_corp = 1 if strpos(assignee_std," BEDRIJF ")>0
qui replace asg_corp = 1 if strpos(assignee_std," BEDRIJVEN ")>0
qui replace asg_corp = 1 if strpos(assignee_std," BEPERK ")>0
qui replace asg_corp = 1 if strpos(assignee_std," BEPERKTE AANSPREEKLIJKHEID ")>0
qui replace asg_corp = 1 if strpos(assignee_std," BESCHRAENKTER HAFTUNG ")>0
qui replace asg_corp = 1 if strpos(assignee_std," BESCHRANKTER ")>0
qui replace asg_corp = 1 if strpos(assignee_std," BESCHRANKTER HAFTUNG ")>0
qui replace asg_corp = 1 if strpos(assignee_std," BESLOTENGENOOTSCHAP ")>0
qui replace asg_corp = 1 if strpos(assignee_std," BESLOTENVENNOOTSCHAP ")>0
qui replace asg_corp = 1 if strpos(assignee_std," BETRIEBE ")>0
qui replace asg_corp = 1 if strpos(assignee_std," BMBH ")>0
qui replace asg_corp = 1 if strpos(assignee_std," BRANDS ")>0
qui replace asg_corp = 1 if strpos(assignee_std," BROS ")>0
qui replace asg_corp = 1 if strpos(assignee_std," BUSINESS ")>0
qui replace asg_corp = 1 if strpos(assignee_std," BV ")>0
qui replace asg_corp = 1 if strpos(assignee_std," BV: ")>0
qui replace asg_corp = 1 if strpos(assignee_std," BV? ")>0
qui replace asg_corp = 1 if strpos(assignee_std," BVBA ")>0
qui replace asg_corp = 1 if strpos(assignee_std," BVBASPRL ")>0
qui replace asg_corp = 1 if strpos(assignee_std," BVIO ")>0
qui replace asg_corp = 1 if strpos(assignee_std," BVSA ")>0
qui replace asg_corp = 1 if strpos(assignee_std," C{OVERSCORE O}RP ")>0
qui replace asg_corp = 1 if strpos(assignee_std," CAMPAGNIE ")>0
qui replace asg_corp = 1 if strpos(assignee_std," CAMPANY ")>0
qui replace asg_corp = 1 if strpos(assignee_std," CC ")>0
qui replace asg_corp = 1 if strpos(assignee_std," CIE ")>0
qui replace asg_corp = 1 if strpos(assignee_std," CMOPANY ")>0
qui replace asg_corp = 1 if strpos(assignee_std," CO ")>0
qui replace asg_corp = 1 if strpos(assignee_std," CO OPERATIVE ")>0
qui replace asg_corp = 1 if strpos(assignee_std," CO OPERATIVES ")>0
qui replace asg_corp = 1 if strpos(assignee_std," CO: ")>0
qui replace asg_corp = 1 if strpos(assignee_std," COFP ")>0
qui replace asg_corp = 1 if strpos(assignee_std," COIRPORATION ")>0
qui replace asg_corp = 1 if strpos(assignee_std," COMANY ")>0
qui replace asg_corp = 1 if strpos(assignee_std," COMAPANY ")>0
qui replace asg_corp = 1 if strpos(assignee_std," COMERCIAL ")>0
qui replace asg_corp = 1 if strpos(assignee_std," COMERCIO ")>0
qui replace asg_corp = 1 if strpos(assignee_std," COMMANDITE SIMPLE ")>0
qui replace asg_corp = 1 if strpos(assignee_std," COMMERCIALE ")>0
qui replace asg_corp = 1 if strpos(assignee_std," COMMERCIALISATIONS ")>0
qui replace asg_corp = 1 if strpos(assignee_std," COMNPANY ")>0
qui replace asg_corp = 1 if strpos(assignee_std," COMP ")>0
qui replace asg_corp = 1 if strpos(assignee_std," COMPAGNE ")>0
qui replace asg_corp = 1 if strpos(assignee_std," COMPAGNI ")>0
qui replace asg_corp = 1 if strpos(assignee_std," COMPAGNIE ")>0
qui replace asg_corp = 1 if strpos(assignee_std," COMPAGNIN ")>0
qui replace asg_corp = 1 if strpos(assignee_std," COMPAGNY ")>0
qui replace asg_corp = 1 if strpos(assignee_std," COMPAIGNIE ")>0
qui replace asg_corp = 1 if strpos(assignee_std," COMPAMY ")>0
qui replace asg_corp = 1 if strpos(assignee_std," COMPANAY ")>0
qui replace asg_corp = 1 if strpos(assignee_std," COMPANH ")>0
qui replace asg_corp = 1 if strpos(assignee_std," COMPANHIA ")>0
qui replace asg_corp = 1 if strpos(assignee_std," COMPANIA ")>0
qui replace asg_corp = 1 if strpos(assignee_std," COMPANIE ")>0
qui replace asg_corp = 1 if strpos(assignee_std," COMPANIES ")>0
qui replace asg_corp = 1 if strpos(assignee_std," COMPANY ")>0
qui replace asg_corp = 1 if strpos(assignee_std," COMPAY ")>0
qui replace asg_corp = 1 if strpos(assignee_std," COMPNAY ")>0
qui replace asg_corp = 1 if strpos(assignee_std," COMAPNY ")>0
qui replace asg_corp = 1 if strpos(assignee_std," COMPNY ")>0
qui replace asg_corp = 1 if strpos(assignee_std," COMPORATION ")>0
qui replace asg_corp = 1 if strpos(assignee_std," CONSORTILE PER AZIONE ")>0
qui replace asg_corp = 1 if strpos(assignee_std," CONSORZIO ")>0
qui replace asg_corp = 1 if strpos(assignee_std," CONSTRUCTIONS ")>0
qui replace asg_corp = 1 if strpos(assignee_std," CONSULTING ")>0
qui replace asg_corp = 1 if strpos(assignee_std," CONZORZIO ")>0
qui replace asg_corp = 1 if strpos(assignee_std," COOEPERATIE ")>0
qui replace asg_corp = 1 if strpos(assignee_std," COOEPERATIEVE ")>0
qui replace asg_corp = 1 if strpos(assignee_std," COOEPERATIEVE VERENIGING ")>0
qui replace asg_corp = 1 if strpos(assignee_std," COOEPERATIEVE VERKOOP ")>0
qui replace asg_corp = 1 if strpos(assignee_std," COOP ")>0
qui replace asg_corp = 1 if strpos(assignee_std," COOP A RL ")>0
qui replace asg_corp = 1 if strpos(assignee_std," COOPERATIE ")>0
qui replace asg_corp = 1 if strpos(assignee_std," COOPERATIEVE ")>0
qui replace asg_corp = 1 if strpos(assignee_std," COOPERATIEVE VENOOTSCHAP ")>0
qui replace asg_corp = 1 if strpos(assignee_std," COOPERATION ")>0
qui replace asg_corp = 1 if strpos(assignee_std," COOPERATIVA AGICOLA ")>0
qui replace asg_corp = 1 if strpos(assignee_std," COOPERATIVA LIMITADA ")>0
qui replace asg_corp = 1 if strpos(assignee_std," COOPERATIVA PER AZIONI ")>0
qui replace asg_corp = 1 if strpos(assignee_std," COORPORATION ")>0
qui replace asg_corp = 1 if strpos(assignee_std," COPANY ")>0
qui replace asg_corp = 1 if strpos(assignee_std," COPORATION ")>0
qui replace asg_corp = 1 if strpos(assignee_std," COPR ")>0
qui replace asg_corp = 1 if strpos(assignee_std," COPRORATION ")>0
qui replace asg_corp = 1 if strpos(assignee_std," COPRPORATION ")>0
qui replace asg_corp = 1 if strpos(assignee_std," COROPORTION ")>0
qui replace asg_corp = 1 if strpos(assignee_std," COROPRATION ")>0
qui replace asg_corp = 1 if strpos(assignee_std," COROPROATION ")>0
qui replace asg_corp = 1 if strpos(assignee_std," CORORATION ")>0
qui replace asg_corp = 1 if strpos(assignee_std," CORP ")>0
qui replace asg_corp = 1 if strpos(assignee_std," CORPARATION ")>0
qui replace asg_corp = 1 if strpos(assignee_std," CORPERATION ")>0
qui replace asg_corp = 1 if strpos(assignee_std," CORPFORATION ")>0
qui replace asg_corp = 1 if strpos(assignee_std," CORPN ")>0
qui replace asg_corp = 1 if strpos(assignee_std," CORPO ")>0
qui replace asg_corp = 1 if strpos(assignee_std," CORPOARTION ")>0
qui replace asg_corp = 1 if strpos(assignee_std," CORPOATAION ")>0
qui replace asg_corp = 1 if strpos(assignee_std," CORPOATION ")>0
qui replace asg_corp = 1 if strpos(assignee_std," CORPOIRATION ")>0
qui replace asg_corp = 1 if strpos(assignee_std," CORPOORATION ")>0
qui replace asg_corp = 1 if strpos(assignee_std," CORPOPRATION ")>0
qui replace asg_corp = 1 if strpos(assignee_std," CORPORAATION ")>0
qui replace asg_corp = 1 if strpos(assignee_std," CORPORACION ")>0
qui replace asg_corp = 1 if strpos(assignee_std," CORPORAION ")>0
qui replace asg_corp = 1 if strpos(assignee_std," CORPORAITON ")>0
qui replace asg_corp = 1 if strpos(assignee_std," CORPORARION ")>0
qui replace asg_corp = 1 if strpos(assignee_std," CORPORARTION ")>0
qui replace asg_corp = 1 if strpos(assignee_std," CORPORATAION ")>0
qui replace asg_corp = 1 if strpos(assignee_std," CORPORATE ")>0
qui replace asg_corp = 1 if strpos(assignee_std," CORPORATED ")>0
qui replace asg_corp = 1 if strpos(assignee_std," CORPORATI ")>0
qui replace asg_corp = 1 if strpos(assignee_std," CORPORATIION ")>0
qui replace asg_corp = 1 if strpos(assignee_std," CORPORATIN ")>0
qui replace asg_corp = 1 if strpos(assignee_std," CORPORATINO ")>0
qui replace asg_corp = 1 if strpos(assignee_std," CORPORATINON ")>0
qui replace asg_corp = 1 if strpos(assignee_std," CORPORATIO ")>0
qui replace asg_corp = 1 if strpos(assignee_std," CORPORATIOIN ")>0
qui replace asg_corp = 1 if strpos(assignee_std," CORPORATIOLN ")>0
qui replace asg_corp = 1 if strpos(assignee_std," CORPORATIOM ")>0
qui replace asg_corp = 1 if strpos(assignee_std," CORPORATION ")>0
qui replace asg_corp = 1 if strpos(assignee_std," CORPORATIOPN ")>0
qui replace asg_corp = 1 if strpos(assignee_std," CORPORATITON ")>0
qui replace asg_corp = 1 if strpos(assignee_std," CORPORATOIN ")>0
qui replace asg_corp = 1 if strpos(assignee_std," CORPORDATION ")>0
qui replace asg_corp = 1 if strpos(assignee_std," CORPORQTION ")>0
qui replace asg_corp = 1 if strpos(assignee_std," CORPORTAION ")>0
qui replace asg_corp = 1 if strpos(assignee_std," CORPORTATION ")>0
qui replace asg_corp = 1 if strpos(assignee_std," CORPORTION ")>0
qui replace asg_corp = 1 if strpos(assignee_std," CORPPORATION ")>0
qui replace asg_corp = 1 if strpos(assignee_std," CORPRATION ")>0
qui replace asg_corp = 1 if strpos(assignee_std," CORPROATION ")>0
qui replace asg_corp = 1 if strpos(assignee_std," CORPRORATION ")>0
qui replace asg_corp = 1 if strpos(assignee_std," CROP ")>0
qui replace asg_corp = 1 if strpos(assignee_std," CROPORATION ")>0
qui replace asg_corp = 1 if strpos(assignee_std," CRPORATION ")>0
qui replace asg_corp = 1 if strpos(assignee_std," CV ")>0
qui replace asg_corp = 1 if strpos(assignee_std," D ENTERPRISES ")>0
qui replace asg_corp = 1 if strpos(assignee_std," D ENTREPRISE ")>0
qui replace asg_corp = 1 if strpos(assignee_std," D O O ")>0
qui replace asg_corp = 1 if strpos(assignee_std," DíENTREPRISE ")>0
qui replace asg_corp = 1 if strpos(assignee_std," DD ")>0
qui replace asg_corp = 1 if strpos(assignee_std," DEVELOP ")>0
qui replace asg_corp = 1 if strpos(assignee_std," DEVELOPPEMENT ")>0
qui replace asg_corp = 1 if strpos(assignee_std," DEVELOPPEMENTS ")>0
qui replace asg_corp = 1 if strpos(assignee_std," DOING BUSINESS ")>0
qui replace asg_corp = 1 if strpos(assignee_std," DOO ")>0
qui replace asg_corp = 1 if strpos(assignee_std," DORPORATION ")>0
qui replace asg_corp = 1 if strpos(assignee_std," EDMS ")>0
qui replace asg_corp = 1 if strpos(assignee_std," EG ")>0
qui replace asg_corp = 1 if strpos(assignee_std," ELECTRONIQUE ")>0
qui replace asg_corp = 1 if strpos(assignee_std," EN ZN ")>0
qui replace asg_corp = 1 if strpos(assignee_std," EN ZONEN ")>0
qui replace asg_corp = 1 if strpos(assignee_std," ENGINEERING ")>0
qui replace asg_corp = 1 if strpos(assignee_std," ENGINEERS ")>0
qui replace asg_corp = 1 if strpos(assignee_std," ENGINES ")>0
qui replace asg_corp = 1 if strpos(assignee_std," ENNOBLISSEMENT ")>0
qui replace asg_corp = 1 if strpos(assignee_std," ENTERPRISE ")>0
qui replace asg_corp = 1 if strpos(assignee_std," ENTRE PRISES ")>0
qui replace asg_corp = 1 if strpos(assignee_std," ENTREPOSE ")>0
qui replace asg_corp = 1 if strpos(assignee_std," ENTREPRISE ")>0
qui replace asg_corp = 1 if strpos(assignee_std," ENTREPRISES ")>0
qui replace asg_corp = 1 if strpos(assignee_std," EQUIP ")>0
qui replace asg_corp = 1 if strpos(assignee_std," EQUIPAMENTOS ")>0
qui replace asg_corp = 1 if strpos(assignee_std," EQUIPEMENT ")>0
qui replace asg_corp = 1 if strpos(assignee_std," EQUIPEMENTS ")>0
qui replace asg_corp = 1 if strpos(assignee_std," EQUIPMENT ")>0
qui replace asg_corp = 1 if strpos(assignee_std," EST ")>0
qui replace asg_corp = 1 if strpos(assignee_std," ESTABILSSEMENTS ")>0
qui replace asg_corp = 1 if strpos(assignee_std," ESTABLISHMENT ")>0
qui replace asg_corp = 1 if strpos(assignee_std," ESTABLISSEMENT ")>0
qui replace asg_corp = 1 if strpos(assignee_std," ESTABLISSEMENTS ")>0
qui replace asg_corp = 1 if strpos(assignee_std," ESTABLISSMENTS ")>0
qui replace asg_corp = 1 if strpos(assignee_std," ET FILS ")>0
qui replace asg_corp = 1 if strpos(assignee_std," ETABLISSEMENT ")>0
qui replace asg_corp = 1 if strpos(assignee_std," ETABLISSMENTS ")>0
qui replace asg_corp = 1 if strpos(assignee_std," ETS ")>0
qui replace asg_corp = 1 if strpos(assignee_std," FABRIC ")>0
qui replace asg_corp = 1 if strpos(assignee_std," FABRICA ")>0
qui replace asg_corp = 1 if strpos(assignee_std," FABRICATION ")>0
qui replace asg_corp = 1 if strpos(assignee_std," FABRICATIONS ")>0
qui replace asg_corp = 1 if strpos(assignee_std," FABRICS ")>0
qui replace asg_corp = 1 if strpos(assignee_std," FABRIEKEN ")>0
qui replace asg_corp = 1 if strpos(assignee_std," FABRIK ")>0
qui replace asg_corp = 1 if strpos(assignee_std," FABRIQUE ")>0
qui replace asg_corp = 1 if strpos(assignee_std," FABRYKA ")>0
qui replace asg_corp = 1 if strpos(assignee_std," FACTORY ")>0
qui replace asg_corp = 1 if strpos(assignee_std," FEDERATED ")>0
qui replace asg_corp = 1 if strpos(assignee_std," FILM ")>0
qui replace asg_corp = 1 if strpos(assignee_std," FINANCIERE ")>0
qui replace asg_corp = 1 if strpos(assignee_std," FIRM ")>0
qui replace asg_corp = 1 if strpos(assignee_std," FIRMA ")>0
qui replace asg_corp = 1 if strpos(assignee_std," GBMH ")>0
qui replace asg_corp = 1 if strpos(assignee_std," GBR ")>0
qui replace asg_corp = 1 if strpos(assignee_std," GEBR ")>0
qui replace asg_corp = 1 if strpos(assignee_std," GEBROEDERS ")>0
qui replace asg_corp = 1 if strpos(assignee_std," GEBRUEDER ")>0
qui replace asg_corp = 1 if strpos(assignee_std," GENERALE POUR LES TECHNIQUES NOUVELLE ")>0
qui replace asg_corp = 1 if strpos(assignee_std," GENOSSENSCHAFT ")>0
qui replace asg_corp = 1 if strpos(assignee_std," GES M B H ")>0
qui replace asg_corp = 1 if strpos(assignee_std," GES MB H ")>0
qui replace asg_corp = 1 if strpos(assignee_std," GES MBH ")>0
qui replace asg_corp = 1 if strpos(assignee_std," GES MHH ")>0
qui replace asg_corp = 1 if strpos(assignee_std," GESELLSCHAFT ")>0
qui replace asg_corp = 1 if strpos(assignee_std," GESELLSCHAFT M B ")>0
qui replace asg_corp = 1 if strpos(assignee_std," GESELLSCHAFT MB H ")>0
qui replace asg_corp = 1 if strpos(assignee_std," GESELLSCHAFT MBH ")>0
qui replace asg_corp = 1 if strpos(assignee_std," GESELLSCHAFT MGH ")>0
qui replace asg_corp = 1 if strpos(assignee_std," GESELLSCHAFT MIT ")>0
qui replace asg_corp = 1 if strpos(assignee_std," GESELLSCHAFT MIT BESCHRANKTER ")>0
qui replace asg_corp = 1 if strpos(assignee_std," GESELLSCHAFT MIT BESCHRANKTER HAFT ")>0
qui replace asg_corp = 1 if strpos(assignee_std," GESELLSCHAFTMIT BESCHRANKTER ")>0
qui replace asg_corp = 1 if strpos(assignee_std," GESMBH ")>0
qui replace asg_corp = 1 if strpos(assignee_std," GES ")>0
qui replace asg_corp = 1 if strpos(assignee_std," GESSELLSCHAFT MIT BESCHRAENKTER HAUFTUNG ")>0
qui replace asg_corp = 1 if strpos(assignee_std," GIE ")>0
qui replace asg_corp = 1 if strpos(assignee_std," GMBA ")>0
qui replace asg_corp = 1 if strpos(assignee_std," GMBB ")>0
qui replace asg_corp = 1 if strpos(assignee_std," GMBG ")>0
qui replace asg_corp = 1 if strpos(assignee_std," GMBH ")>0
qui replace asg_corp = 1 if strpos(assignee_std," GMHB ")>0
qui replace asg_corp = 1 if strpos(assignee_std," GNBH ")>0
qui replace asg_corp = 1 if strpos(assignee_std," GORPORATION ")>0
qui replace asg_corp = 1 if strpos(assignee_std," GROEP ")>0
qui replace asg_corp = 1 if strpos(assignee_std," GROUP ")>0
qui replace asg_corp = 1 if strpos(assignee_std," GROUPEMENT D ENTREPRISES ")>0
qui replace asg_corp = 1 if strpos(assignee_std," H ")>0
qui replace asg_corp = 1 if strpos(assignee_std," HAFRUNG ")>0
qui replace asg_corp = 1 if strpos(assignee_std," HANDEL ")>0
qui replace asg_corp = 1 if strpos(assignee_std," HANDELABOLAGET ")>0
qui replace asg_corp = 1 if strpos(assignee_std," HANDELEND ONDER ")>0
qui replace asg_corp = 1 if strpos(assignee_std," HANDELORGANISATION ")>0
qui replace asg_corp = 1 if strpos(assignee_std," HANDELS ")>0
qui replace asg_corp = 1 if strpos(assignee_std," HANDELSBOLAG ")>0
qui replace asg_corp = 1 if strpos(assignee_std," HANDELSBOLAGET ")>0
qui replace asg_corp = 1 if strpos(assignee_std," HANDELSGESELLSCHAFT ")>0
qui replace asg_corp = 1 if strpos(assignee_std," HANDESBOLAG ")>0
qui replace asg_corp = 1 if strpos(assignee_std," HATFUNG ")>0
qui replace asg_corp = 1 if strpos(assignee_std," HB ")>0
qui replace asg_corp = 1 if strpos(assignee_std," HF ")>0
qui replace asg_corp = 1 if strpos(assignee_std," HOLDINGS ")>0
qui replace asg_corp = 1 if strpos(assignee_std," INC ")>0
qui replace asg_corp = 1 if strpos(assignee_std," INC: ")>0
qui replace asg_corp = 1 if strpos(assignee_std," INCOPORATED ")>0
qui replace asg_corp = 1 if strpos(assignee_std," INCORORATED ")>0
qui replace asg_corp = 1 if strpos(assignee_std," INCORPARATED ")>0
qui replace asg_corp = 1 if strpos(assignee_std," INCORPATED ")>0
qui replace asg_corp = 1 if strpos(assignee_std," INCORPORATE ")>0
qui replace asg_corp = 1 if strpos(assignee_std," INCORPORATED ")>0
qui replace asg_corp = 1 if strpos(assignee_std," INCORPORORATED ")>0
qui replace asg_corp = 1 if strpos(assignee_std," INCORPORTED ")>0
qui replace asg_corp = 1 if strpos(assignee_std," INCORPOTATED ")>0
qui replace asg_corp = 1 if strpos(assignee_std," INCORPRATED ")>0
qui replace asg_corp = 1 if strpos(assignee_std," INCORPRORATED ")>0
qui replace asg_corp = 1 if strpos(assignee_std," INCROPORATED ")>0
qui replace asg_corp = 1 if strpos(assignee_std," INDISTRIES ")>0
qui replace asg_corp = 1 if strpos(assignee_std," INDUSRTIES ")>0
qui replace asg_corp = 1 if strpos(assignee_std," INDUSTRI ")>0
qui replace asg_corp = 1 if strpos(assignee_std," INDUSTRIA ")>0
qui replace asg_corp = 1 if strpos(assignee_std," INDUSTRIAL ")>0
qui replace asg_corp = 1 if strpos(assignee_std," INDUSTRIAL COP ")>0
qui replace asg_corp = 1 if strpos(assignee_std," INDUSTRIALNA ")>0
qui replace asg_corp = 1 if strpos(assignee_std," INDUSTRIAS ")>0
qui replace asg_corp = 1 if strpos(assignee_std," INDUSTRIE ")>0
qui replace asg_corp = 1 if strpos(assignee_std," INDUSTRIES ")>0
qui replace asg_corp = 1 if strpos(assignee_std," INDUSTRIJA ")>0
qui replace asg_corp = 1 if strpos(assignee_std," INDUSTRIJSKO ")>0
qui replace asg_corp = 1 if strpos(assignee_std," INGENIEURBUERO ")>0
qui replace asg_corp = 1 if strpos(assignee_std," INGENIEURBURO ")>0
qui replace asg_corp = 1 if strpos(assignee_std," INGENIEURGESELLSCHAFT ")>0
qui replace asg_corp = 1 if strpos(assignee_std," INGENIEURSBUERO ")>0
qui replace asg_corp = 1 if strpos(assignee_std," INGENIEURSBUREAU ")>0
qui replace asg_corp = 1 if strpos(assignee_std," INGENIOERSBYRA ")>0
qui replace asg_corp = 1 if strpos(assignee_std," INGENJOERSFIRMA ")>0
qui replace asg_corp = 1 if strpos(assignee_std," INGENJOERSFIRMAN ")>0
qui replace asg_corp = 1 if strpos(assignee_std," INORPORATED ")>0
qui replace asg_corp = 1 if strpos(assignee_std," INT ")>0
qui replace asg_corp = 1 if strpos(assignee_std," INT L ")>0
qui replace asg_corp = 1 if strpos(assignee_std," INTERNAITONAL ")>0
qui replace asg_corp = 1 if strpos(assignee_std," INTERNATIONAL ")>0
qui replace asg_corp = 1 if strpos(assignee_std," INTERNATIONAL BUSINESS ")>0
qui replace asg_corp = 1 if strpos(assignee_std," INTERNATIONALE ")>0
qui replace asg_corp = 1 if strpos(assignee_std," INTERNATIONAUX ")>0
qui replace asg_corp = 1 if strpos(assignee_std," INTERNTIONAL ")>0
qui replace asg_corp = 1 if strpos(assignee_std," INTL ")>0
qui replace asg_corp = 1 if strpos(assignee_std," INUDSTRIE ")>0
qui replace asg_corp = 1 if strpos(assignee_std," INVESTMENT ")>0
qui replace asg_corp = 1 if strpos(assignee_std," IS ")>0
qui replace asg_corp = 1 if strpos(assignee_std," JOINTVENTURE ")>0
qui replace asg_corp = 1 if strpos(assignee_std," K G ")>0
qui replace asg_corp = 1 if strpos(assignee_std," K K ")>0
qui replace asg_corp = 1 if strpos(assignee_std," KABAUSHIKI KAISHA ")>0
qui replace asg_corp = 1 if strpos(assignee_std," KABISHIKI KAISHA ")>0
qui replace asg_corp = 1 if strpos(assignee_std," KABSUHIKI ")>0
qui replace asg_corp = 1 if strpos(assignee_std," KABUSHI KIKAISHA ")>0
qui replace asg_corp = 1 if strpos(assignee_std," KABUSHIBI KAISHA ")>0
qui replace asg_corp = 1 if strpos(assignee_std," KABUSHIKAISHA ")>0
qui replace asg_corp = 1 if strpos(assignee_std," KABUSHIKI ")>0
qui replace asg_corp = 1 if strpos(assignee_std," KABUSHIKKAISHA ")>0
qui replace asg_corp = 1 if strpos(assignee_std," KABUSHIKU KASISHA ")>0
qui replace asg_corp = 1 if strpos(assignee_std," KABUSHKIKI KAISHI ")>0
qui replace asg_corp = 1 if strpos(assignee_std," KABUSIKI ")>0
qui replace asg_corp = 1 if strpos(assignee_std," KABUSIKI KAISHA ")>0
qui replace asg_corp = 1 if strpos(assignee_std," KABUSIKI KAISYA ")>0
qui replace asg_corp = 1 if strpos(assignee_std," KABUSIKIKAISHA ")>0
qui replace asg_corp = 1 if strpos(assignee_std," KAGUSHIKI KAISHA ")>0
qui replace asg_corp = 1 if strpos(assignee_std," KAUSHIKI KAISHA ")>0
qui replace asg_corp = 1 if strpos(assignee_std," KAISHA ")>0
qui replace asg_corp = 1 if strpos(assignee_std," KAISYA ")>0
qui replace asg_corp = 1 if strpos(assignee_std," KABAUSHIKI GAISHA ")>0
qui replace asg_corp = 1 if strpos(assignee_std," KABISHIKI GAISHA ")>0
qui replace asg_corp = 1 if strpos(assignee_std," KABUSHI KIGAISHA ")>0
qui replace asg_corp = 1 if strpos(assignee_std," KABUSHIBI GAISHA ")>0
qui replace asg_corp = 1 if strpos(assignee_std," KABUSHIGAISHA ")>0
qui replace asg_corp = 1 if strpos(assignee_std," KABUSHIKGAISHA ")>0
qui replace asg_corp = 1 if strpos(assignee_std," KABUSHIKU GASISHA ")>0
qui replace asg_corp = 1 if strpos(assignee_std," KABUSHKIKI GAISHI ")>0
qui replace asg_corp = 1 if strpos(assignee_std," KABUSIKI GAISHA ")>0
qui replace asg_corp = 1 if strpos(assignee_std," KABUSIKI GAISYA ")>0
qui replace asg_corp = 1 if strpos(assignee_std," KABUSIKIGAISHA ")>0
qui replace asg_corp = 1 if strpos(assignee_std," KAGUSHIKI GAISHA ")>0
qui replace asg_corp = 1 if strpos(assignee_std," KAUSHIKI GAISHA ")>0
qui replace asg_corp = 1 if strpos(assignee_std," GAISHA ")>0
qui replace asg_corp = 1 if strpos(assignee_std," GAISYA ")>0
qui replace asg_corp = 1 if strpos(assignee_std," KB ")>0
qui replace asg_corp = 1 if strpos(assignee_std," KB KY ")>0
qui replace asg_corp = 1 if strpos(assignee_std," KFT ")>0
qui replace asg_corp = 1 if strpos(assignee_std," KG ")>0
qui replace asg_corp = 1 if strpos(assignee_std," KGAA ")>0
qui replace asg_corp = 1 if strpos(assignee_std," KK ")>0
qui replace asg_corp = 1 if strpos(assignee_std," KOM GES ")>0
qui replace asg_corp = 1 if strpos(assignee_std," KOMM GES ")>0
qui replace asg_corp = 1 if strpos(assignee_std," KOMMANDITBOLAG ")>0
qui replace asg_corp = 1 if strpos(assignee_std," KOMMANDITBOLAGET ")>0
qui replace asg_corp = 1 if strpos(assignee_std," KOMMANDITGESELLSCHAFT ")>0
qui replace asg_corp = 1 if strpos(assignee_std," KONSTRUKTIONEN ")>0
qui replace asg_corp = 1 if strpos(assignee_std," KOOPERATIVE ")>0
qui replace asg_corp = 1 if strpos(assignee_std," KS ")>0
qui replace asg_corp = 1 if strpos(assignee_std," KUBUSHIKI KAISHA ")>0
qui replace asg_corp = 1 if strpos(assignee_std," KY ")>0
qui replace asg_corp = 1 if strpos(assignee_std," L ")>0
qui replace asg_corp = 1 if strpos(assignee_std," L C ")>0
qui replace asg_corp = 1 if strpos(assignee_std," L L C ")>0
qui replace asg_corp = 1 if strpos(assignee_std," L P ")>0
qui replace asg_corp = 1 if strpos(assignee_std," LAB ")>0
qui replace asg_corp = 1 if strpos(assignee_std," LABARATOIRE ")>0
qui replace asg_corp = 1 if strpos(assignee_std," LABO ")>0
qui replace asg_corp = 1 if strpos(assignee_std," LABORATOIRE ")>0
qui replace asg_corp = 1 if strpos(assignee_std," LABORATOIRES ")>0
qui replace asg_corp = 1 if strpos(assignee_std," LABORATORI ")>0
qui replace asg_corp = 1 if strpos(assignee_std," LABORATORIA ")>0
qui replace asg_corp = 1 if strpos(assignee_std," LABORATORIE ")>0
qui replace asg_corp = 1 if strpos(assignee_std," LABORATORIES ")>0
qui replace asg_corp = 1 if strpos(assignee_std," LABORATORIET ")>0
qui replace asg_corp = 1 if strpos(assignee_std," LABORATORIUM ")>0
qui replace asg_corp = 1 if strpos(assignee_std," LABORATORY ")>0
qui replace asg_corp = 1 if strpos(assignee_std," LABRATIORIES ")>0
qui replace asg_corp = 1 if strpos(assignee_std," LABS ")>0
qui replace asg_corp = 1 if strpos(assignee_std," LC ")>0
qui replace asg_corp = 1 if strpos(assignee_std," LCC ")>0
qui replace asg_corp = 1 if strpos(assignee_std," LDA ")>0
qui replace asg_corp = 1 if strpos(assignee_std," LDT ")>0
qui replace asg_corp = 1 if strpos(assignee_std," LIIMITED ")>0
qui replace asg_corp = 1 if strpos(assignee_std," LIMIDADA ")>0
qui replace asg_corp = 1 if strpos(assignee_std," LIMINTED ")>0
qui replace asg_corp = 1 if strpos(assignee_std," LIMITADA ")>0
qui replace asg_corp = 1 if strpos(assignee_std," LIMITADO ")>0
qui replace asg_corp = 1 if strpos(assignee_std," LIMITATA ")>0
qui replace asg_corp = 1 if strpos(assignee_std," LIMITE ")>0
qui replace asg_corp = 1 if strpos(assignee_std," LIMITED ")>0
qui replace asg_corp = 1 if strpos(assignee_std," LIMITEE ")>0
qui replace asg_corp = 1 if strpos(assignee_std," LIMTED ")>0
qui replace asg_corp = 1 if strpos(assignee_std," LINITED ")>0
qui replace asg_corp = 1 if strpos(assignee_std," LITD ")>0
qui replace asg_corp = 1 if strpos(assignee_std," LLC ")>0
qui replace asg_corp = 1 if strpos(assignee_std," LLLC ")>0
qui replace asg_corp = 1 if strpos(assignee_std," LLLP ")>0
qui replace asg_corp = 1 if strpos(assignee_std," LLP ")>0
qui replace asg_corp = 1 if strpos(assignee_std," LMITED ")>0
qui replace asg_corp = 1 if strpos(assignee_std," LP ")>0
qui replace asg_corp = 1 if strpos(assignee_std," LT EE ")>0
qui replace asg_corp = 1 if strpos(assignee_std," LTA ")>0
qui replace asg_corp = 1 if strpos(assignee_std," LTC ")>0
qui replace asg_corp = 1 if strpos(assignee_std," LTD ")>0
qui replace asg_corp = 1 if strpos(assignee_std," LTD: ")>0
qui replace asg_corp = 1 if strpos(assignee_std," LTDA ")>0
qui replace asg_corp = 1 if strpos(assignee_std," LTDS ")>0
qui replace asg_corp = 1 if strpos(assignee_std," LTEE ")>0
qui replace asg_corp = 1 if strpos(assignee_std," LTEE; ")>0
qui replace asg_corp = 1 if strpos(assignee_std," LTS ")>0
qui replace asg_corp = 1 if strpos(assignee_std," MAATSCHAPPIJ ")>0
qui replace asg_corp = 1 if strpos(assignee_std," MANUFACTURE ")>0
qui replace asg_corp = 1 if strpos(assignee_std," MANUFACTURE D ARTICLES ")>0
qui replace asg_corp = 1 if strpos(assignee_std," MANUFACTURE DE ")>0
qui replace asg_corp = 1 if strpos(assignee_std," MANUFACTURING ")>0
qui replace asg_corp = 1 if strpos(assignee_std," MARKETING ")>0
qui replace asg_corp = 1 if strpos(assignee_std," MASCHINENBAU ")>0
qui replace asg_corp = 1 if strpos(assignee_std," MASCHINENFABRIK ")>0
qui replace asg_corp = 1 if strpos(assignee_std," MBH ")>0
qui replace asg_corp = 1 if strpos(assignee_std," MBH & CO ")>0
qui replace asg_corp = 1 if strpos(assignee_std," MERCHANDISING ")>0
qui replace asg_corp = 1 if strpos(assignee_std," MET BEPERKTE ")>0
qui replace asg_corp = 1 if strpos(assignee_std," MFG ")>0
qui replace asg_corp = 1 if strpos(assignee_std," N A ")>0
qui replace asg_corp = 1 if strpos(assignee_std," N V ")>0
qui replace asg_corp = 1 if strpos(assignee_std," NA ")>0
qui replace asg_corp = 1 if strpos(assignee_std," NAAMLOSE ")>0
qui replace asg_corp = 1 if strpos(assignee_std," NAAMLOZE ")>0
qui replace asg_corp = 1 if strpos(assignee_std," NAAMLOZE VENNOOTSCAP ")>0
qui replace asg_corp = 1 if strpos(assignee_std," NAAMLOZE VENNOOTSHCAP ")>0
qui replace asg_corp = 1 if strpos(assignee_std," NAAMLOZEVENNOOTSCHAP ")>0
qui replace asg_corp = 1 if strpos(assignee_std," NAUCHNO PRIOZVODSTVENNAYA FIRMA ")>0
qui replace asg_corp = 1 if strpos(assignee_std," NAUCHNO PRIOZVODSTVENNOE OBIEDINENIE ")>0
qui replace asg_corp = 1 if strpos(assignee_std," NAUCHNO PRIOZVODSTVENNY KOOPERATIV ")>0
qui replace asg_corp = 1 if strpos(assignee_std," NAUCHNO PROIZVODSTVENNOE ")>0
qui replace asg_corp = 1 if strpos(assignee_std," NAUCHNO PROIZVODSTVENNOE OBJEDINENIE ")>0
qui replace asg_corp = 1 if strpos(assignee_std," NAUCHNO TEKHNICHESKY KOOPERATIV ")>0
qui replace asg_corp = 1 if strpos(assignee_std," NAUCHNO TEKHNICHESKYKKOOPERATIV ")>0
qui replace asg_corp = 1 if strpos(assignee_std," NAUCHNO TEKHNOLOGICHESKOE ")>0
qui replace asg_corp = 1 if strpos(assignee_std," NAUCHNO TEKHNOLOGICHESKOEPREDPRIYATIE ")>0
qui replace asg_corp = 1 if strpos(assignee_std," NAUCHNOPRIOZVODSTVENNOE ")>0
qui replace asg_corp = 1 if strpos(assignee_std," NAUCHNOPROIZVODSTVENNOE ")>0
qui replace asg_corp = 1 if strpos(assignee_std," NAUCHNOTEKHNICHESKYKKOOPERATIV ")>0
qui replace asg_corp = 1 if strpos(assignee_std," NAUCHNOTEKNICHESKY ")>0
qui replace asg_corp = 1 if strpos(assignee_std," NV ")>0
qui replace asg_corp = 1 if strpos(assignee_std," NV SA ")>0
qui replace asg_corp = 1 if strpos(assignee_std," NV: ")>0
qui replace asg_corp = 1 if strpos(assignee_std," NVSA ")>0
qui replace asg_corp = 1 if strpos(assignee_std," OBIDINENIE ")>0
qui replace asg_corp = 1 if strpos(assignee_std," OBIED ")>0
qui replace asg_corp = 1 if strpos(assignee_std," OBSCHESRYO ")>0
qui replace asg_corp = 1 if strpos(assignee_std," OBSCHESTVO & OGRANICHENNOI OTVETSTVENNOSTJU ")>0
qui replace asg_corp = 1 if strpos(assignee_std," OBSCHESTVO & ORGANICHENNOI OTVETSTVENNOSTIJU ")>0
qui replace asg_corp = 1 if strpos(assignee_std," OBSCHESTVO C ")>0
qui replace asg_corp = 1 if strpos(assignee_std," OBSCHESTVO S ")>0
qui replace asg_corp = 1 if strpos(assignee_std," OBSCHESTVO S OGRANICHENNOI ")>0
qui replace asg_corp = 1 if strpos(assignee_std," OBSCHESTVO S OGRANICHENNOI OTVETSTVEN NOSTJU ")>0
qui replace asg_corp = 1 if strpos(assignee_std," OBSCHESTVO S OGRANICHENNOI OTVETSTVENNOSTIJU ")>0
qui replace asg_corp = 1 if strpos(assignee_std," OBSCHESTVO S OGRANICHENNOI OTVETSTVENNPSTJU ")>0
qui replace asg_corp = 1 if strpos(assignee_std," OBSCHESTVO S OGRANICHENNOY OTVETSTVENNOSTJU ")>0
qui replace asg_corp = 1 if strpos(assignee_std," OBSCHESTVO S OGRANICHENOI ")>0
qui replace asg_corp = 1 if strpos(assignee_std," OBSCHESTVO S ORGANICHENNOI OTVETSTVENNOSTIJU ")>0
qui replace asg_corp = 1 if strpos(assignee_std," OBSCHESTVO S ORGANICHENNOI OTVETSTVENNOSTJU ")>0
qui replace asg_corp = 1 if strpos(assignee_std," OBSHESTVO S ")>0
qui replace asg_corp = 1 if strpos(assignee_std," OBSHESTVO S OGRANNICHENNOJ ")>0
qui replace asg_corp = 1 if strpos(assignee_std," OBSHESTVO S ORGANICHENNOI OTVETSTVENNOSTIJU ")>0
qui replace asg_corp = 1 if strpos(assignee_std," OBSHESTVO S ORGANICHENNOI OTVETSTVENNOSTJU ")>0
qui replace asg_corp = 1 if strpos(assignee_std," OCTROOIBUREAU ")>0
qui replace asg_corp = 1 if strpos(assignee_std," OGRANICHENNOI OTVETSTVENNOSTIJU ")>0
qui replace asg_corp = 1 if strpos(assignee_std," OGRANICHENNOI OTVETSTVENNOSTIJU FIRMA ")>0
qui replace asg_corp = 1 if strpos(assignee_std," OGRANICHENNOI OTVETSTVENNOSTJU ")>0
qui replace asg_corp = 1 if strpos(assignee_std," OGRANICHENNOY OTVETSTVENNOSTYU ")>0
qui replace asg_corp = 1 if strpos(assignee_std," OHG ")>0
qui replace asg_corp = 1 if strpos(assignee_std," ONDERNEMING ")>0
qui replace asg_corp = 1 if strpos(assignee_std," OTVETCTVENNOSTJU ")>0
qui replace asg_corp = 1 if strpos(assignee_std," OTVETSTVENNOSTIJU ")>0
qui replace asg_corp = 1 if strpos(assignee_std," OTVETSTVENNOSTJU ")>0
qui replace asg_corp = 1 if strpos(assignee_std," OTVETSTVENNOSTOU ")>0
qui replace asg_corp = 1 if strpos(assignee_std," OTVETSTVENNOSTYU ")>0
qui replace asg_corp = 1 if strpos(assignee_std," OY ")>0
qui replace asg_corp = 1 if strpos(assignee_std," OYABLTD ")>0
qui replace asg_corp = 1 if strpos(assignee_std," OYG ")>0
qui replace asg_corp = 1 if strpos(assignee_std," OYI ")>0
qui replace asg_corp = 1 if strpos(assignee_std," OYJ ")>0
qui replace asg_corp = 1 if strpos(assignee_std," OYL ")>0
qui replace asg_corp = 1 if strpos(assignee_std," P ")>0
qui replace asg_corp = 1 if strpos(assignee_std," P C ")>0
qui replace asg_corp = 1 if strpos(assignee_std," P L C ")>0
qui replace asg_corp = 1 if strpos(assignee_std," PARNERSHIP ")>0
qui replace asg_corp = 1 if strpos(assignee_std," PARNTERSHIP ")>0
qui replace asg_corp = 1 if strpos(assignee_std," PARTNER ")>0
qui replace asg_corp = 1 if strpos(assignee_std," PARTNERS ")>0
qui replace asg_corp = 1 if strpos(assignee_std," PARTNERSHIP ")>0
qui replace asg_corp = 1 if strpos(assignee_std," PATENT OFFICE ")>0
qui replace asg_corp = 1 if strpos(assignee_std," PATENTVERWALTUNGS GESELLSCHAFT MBH ")>0
qui replace asg_corp = 1 if strpos(assignee_std," PATENTVERWALTUNGSGESELLSCHAFT ")>0
qui replace asg_corp = 1 if strpos(assignee_std," PATENTVERWERTUNGSGESELLSCHAFT ")>0
qui replace asg_corp = 1 if strpos(assignee_std," PATNERSHIP ")>0
qui replace asg_corp = 1 if strpos(assignee_std," PC ")>0
qui replace asg_corp = 1 if strpos(assignee_std," PER AZIONA ")>0
qui replace asg_corp = 1 if strpos(assignee_std," PERSONENVENNOOTSCHAP MET BE PERKTE AANSPRAKELIJKHEID ")>0
qui replace asg_corp = 1 if strpos(assignee_std," PHARM ")>0
qui replace asg_corp = 1 if strpos(assignee_std," PHARMACEUTICA ")>0
qui replace asg_corp = 1 if strpos(assignee_std," PHARMACEUTICAL ")>0
qui replace asg_corp = 1 if strpos(assignee_std," PHARMACEUTICALS ")>0
qui replace asg_corp = 1 if strpos(assignee_std," PHARMACEUTIQUE ")>0
qui replace asg_corp = 1 if strpos(assignee_std," PHARMACIA ")>0
qui replace asg_corp = 1 if strpos(assignee_std," PHARMACIE ")>0
qui replace asg_corp = 1 if strpos(assignee_std," PHARMACUETICALS ")>0
qui replace asg_corp = 1 if strpos(assignee_std," PLANTS ")>0
qui replace asg_corp = 1 if strpos(assignee_std," PLC ")>0
qui replace asg_corp = 1 if strpos(assignee_std," PREDPRIVATIE ")>0
qui replace asg_corp = 1 if strpos(assignee_std," PREDPRIYATIE ")>0
qui replace asg_corp = 1 if strpos(assignee_std," PREPRIVATIE ")>0
qui replace asg_corp = 1 if strpos(assignee_std," PRODUCE ")>0
qui replace asg_corp = 1 if strpos(assignee_std," PRODUCT ")>0
qui replace asg_corp = 1 if strpos(assignee_std," PRODUCTEURS ")>0
qui replace asg_corp = 1 if strpos(assignee_std," PRODUCTION ")>0
qui replace asg_corp = 1 if strpos(assignee_std," PRODUCTIONS ")>0
qui replace asg_corp = 1 if strpos(assignee_std," PRODUCTIQUE ")>0
qui replace asg_corp = 1 if strpos(assignee_std," PRODUCTS ")>0
qui replace asg_corp = 1 if strpos(assignee_std," PRODUITS ")>0
qui replace asg_corp = 1 if strpos(assignee_std," PRODUKTE ")>0
qui replace asg_corp = 1 if strpos(assignee_std," PRODUKTER ")>0
qui replace asg_corp = 1 if strpos(assignee_std," PRODUKTION ")>0
qui replace asg_corp = 1 if strpos(assignee_std," PRODUKTIONSGESELLSCHAFT ")>0
qui replace asg_corp = 1 if strpos(assignee_std," PRODUKTUTVECKLING ")>0
qui replace asg_corp = 1 if strpos(assignee_std," PRODURA ")>0
qui replace asg_corp = 1 if strpos(assignee_std," PRODUTIS ")>0
qui replace asg_corp = 1 if strpos(assignee_std," PROIZVODSTENNOE OBIEDINENIE ")>0
qui replace asg_corp = 1 if strpos(assignee_std," PROIZVODSTVENNOE ")>0
qui replace asg_corp = 1 if strpos(assignee_std," PROIZVODSTVENNOE OBIEDINENIE ")>0
qui replace asg_corp = 1 if strpos(assignee_std," PTY ")>0
qui replace asg_corp = 1 if strpos(assignee_std," PTY LIM ")>0
qui replace asg_corp = 1 if strpos(assignee_std," PTYLTD ")>0
qui replace asg_corp = 1 if strpos(assignee_std," PUBLISHING ")>0
qui replace asg_corp = 1 if strpos(assignee_std," PVBA ")>0
qui replace asg_corp = 1 if strpos(assignee_std," RECHERCHES ")>0
qui replace asg_corp = 1 if strpos(assignee_std," RESPONSABILITA LIMITATA ")>0
qui replace asg_corp = 1 if strpos(assignee_std," RESPONSABILITAí LIMITATA ")>0
qui replace asg_corp = 1 if strpos(assignee_std," RESPONSABILITE LIMITE ")>0
qui replace asg_corp = 1 if strpos(assignee_std," RO ")>0
qui replace asg_corp = 1 if strpos(assignee_std," RT ")>0
qui replace asg_corp = 1 if strpos(assignee_std," S A ")>0
qui replace asg_corp = 1 if strpos(assignee_std," S A R L ")>0
qui replace asg_corp = 1 if strpos(assignee_std," S A RL ")>0
qui replace asg_corp = 1 if strpos(assignee_std," S COOP ")>0
qui replace asg_corp = 1 if strpos(assignee_std," S COOP LTDA ")>0
qui replace asg_corp = 1 if strpos(assignee_std," S NC ")>0
qui replace asg_corp = 1 if strpos(assignee_std," S OGRANICHENNOI OTVETSTVENNEST ")>0
qui replace asg_corp = 1 if strpos(assignee_std," S P A ")>0
qui replace asg_corp = 1 if strpos(assignee_std," S PA ")>0
qui replace asg_corp = 1 if strpos(assignee_std," S R L ")>0
qui replace asg_corp = 1 if strpos(assignee_std," S RL ")>0
qui replace asg_corp = 1 if strpos(assignee_std," S S ")>0
qui replace asg_corp = 1 if strpos(assignee_std," SA ")>0
qui replace asg_corp = 1 if strpos(assignee_std," SA A RL ")>0
qui replace asg_corp = 1 if strpos(assignee_std," SA RL ")>0
qui replace asg_corp = 1 if strpos(assignee_std," SA: ")>0
qui replace asg_corp = 1 if strpos(assignee_std," SAAG ")>0
qui replace asg_corp = 1 if strpos(assignee_std," SAARL ")>0
qui replace asg_corp = 1 if strpos(assignee_std," SALES ")>0
qui replace asg_corp = 1 if strpos(assignee_std," SANV ")>0
qui replace asg_corp = 1 if strpos(assignee_std," SARL ")>0
qui replace asg_corp = 1 if strpos(assignee_std," SARL: ")>0
qui replace asg_corp = 1 if strpos(assignee_std," SAS ")>0
qui replace asg_corp = 1 if strpos(assignee_std," SC ")>0
qui replace asg_corp = 1 if strpos(assignee_std," SCA ")>0
qui replace asg_corp = 1 if strpos(assignee_std," SCARL ")>0
qui replace asg_corp = 1 if strpos(assignee_std," SCIETE ANONYME ")>0
qui replace asg_corp = 1 if strpos(assignee_std," SCOOP ")>0
qui replace asg_corp = 1 if strpos(assignee_std," SCPA ")>0
qui replace asg_corp = 1 if strpos(assignee_std," SCRAS ")>0
qui replace asg_corp = 1 if strpos(assignee_std," SCRL ")>0
qui replace asg_corp = 1 if strpos(assignee_std," SEMPLICE ")>0
qui replace asg_corp = 1 if strpos(assignee_std," SERIVICES ")>0
qui replace asg_corp = 1 if strpos(assignee_std," SERVICE ")>0
qui replace asg_corp = 1 if strpos(assignee_std," SERVICES ")>0
qui replace asg_corp = 1 if strpos(assignee_std," SHOP ")>0
qui replace asg_corp = 1 if strpos(assignee_std," SIMPLIFIEE ")>0
qui replace asg_corp = 1 if strpos(assignee_std," SL ")>0
qui replace asg_corp = 1 if strpos(assignee_std," SNC ")>0
qui replace asg_corp = 1 if strpos(assignee_std," SOC ")>0
qui replace asg_corp = 1 if strpos(assignee_std," SOC ARL ")>0
qui replace asg_corp = 1 if strpos(assignee_std," SOC COOOP ARL ")>0
qui replace asg_corp = 1 if strpos(assignee_std," SOC COOP A RESP LIM ")>0
qui replace asg_corp = 1 if strpos(assignee_std," SOC COOP A RL ")>0
qui replace asg_corp = 1 if strpos(assignee_std," SOC COOP R L ")>0
qui replace asg_corp = 1 if strpos(assignee_std," SOC COOP RL ")>0
qui replace asg_corp = 1 if strpos(assignee_std," SOC IND COMM ")>0
qui replace asg_corp = 1 if strpos(assignee_std," SOC RL ")>0
qui replace asg_corp = 1 if strpos(assignee_std," SOCCOOP ARL ")>0
qui replace asg_corp = 1 if strpos(assignee_std," SOCCOOPARL ")>0
qui replace asg_corp = 1 if strpos(assignee_std," SOCIEDAD ")>0
qui replace asg_corp = 1 if strpos(assignee_std," SOCIEDAD ANONIMA ")>0
qui replace asg_corp = 1 if strpos(assignee_std," SOCIEDAD ANONIMYA ")>0
qui replace asg_corp = 1 if strpos(assignee_std," SOCIEDAD INDUSTRIAL ")>0
qui replace asg_corp = 1 if strpos(assignee_std," SOCIEDAD LIMITADA ")>0
qui replace asg_corp = 1 if strpos(assignee_std," SOCIEDADE LIMITADA ")>0
qui replace asg_corp = 1 if strpos(assignee_std," SOCIET CIVILE ")>0
qui replace asg_corp = 1 if strpos(assignee_std," SOCIETA ")>0
qui replace asg_corp = 1 if strpos(assignee_std," SOCIETA A ")>0
qui replace asg_corp = 1 if strpos(assignee_std," SOCIETA A RESPONSABILITA LIMITATA ")>0
qui replace asg_corp = 1 if strpos(assignee_std," SOCIETA ANONIMA ")>0
qui replace asg_corp = 1 if strpos(assignee_std," SOCIETA CONSORTILE ")>0
qui replace asg_corp = 1 if strpos(assignee_std," SOCIETA CONSORTILE A RESPONSABILITA ")>0
qui replace asg_corp = 1 if strpos(assignee_std," SOCIETA CONSORTILE ARL ")>0
qui replace asg_corp = 1 if strpos(assignee_std," SOCIETA CONSORTILE PER AZION ")>0
qui replace asg_corp = 1 if strpos(assignee_std," SOCIETA CONSORTILE PER AZIONI ")>0
qui replace asg_corp = 1 if strpos(assignee_std," SOCIETA COOPERATIVA ")>0
qui replace asg_corp = 1 if strpos(assignee_std," SOCIETA COOPERATIVA A ")>0
qui replace asg_corp = 1 if strpos(assignee_std," SOCIETA IN ACCOMANDITA ")>0
qui replace asg_corp = 1 if strpos(assignee_std," SOCIETA IN ACCOMANDITA SEMPLICE ")>0
qui replace asg_corp = 1 if strpos(assignee_std," SOCIETA IN NOME COLLETTIVO ")>0
qui replace asg_corp = 1 if strpos(assignee_std," SOCIETA INDUSTRIA ")>0
qui replace asg_corp = 1 if strpos(assignee_std," SOCIETA PER AXIONI ")>0
qui replace asg_corp = 1 if strpos(assignee_std," SOCIETA PER AZINOI ")>0
qui replace asg_corp = 1 if strpos(assignee_std," SOCIETA PER AZINONI ")>0
qui replace asg_corp = 1 if strpos(assignee_std," SOCIETA PER AZIONI ")>0
qui replace asg_corp = 1 if strpos(assignee_std," SOCIETA PER AZIONI: ")>0
qui replace asg_corp = 1 if strpos(assignee_std," SOCIETA PER L INDUSTRIA ")>0
qui replace asg_corp = 1 if strpos(assignee_std," SOCIETA PERAZIONI ")>0
qui replace asg_corp = 1 if strpos(assignee_std," SOCIETAPERAZIONI ")>0
qui replace asg_corp = 1 if strpos(assignee_std," SOCIETE ")>0
qui replace asg_corp = 1 if strpos(assignee_std," SOCIETE A ")>0
qui replace asg_corp = 1 if strpos(assignee_std," SOCIETE A RESPONSABILITE ")>0
qui replace asg_corp = 1 if strpos(assignee_std," SOCIETE A RESPONSABILITE DITE ")>0
qui replace asg_corp = 1 if strpos(assignee_std," SOCIETE A RESPONSABILITEE ")>0
qui replace asg_corp = 1 if strpos(assignee_std," SOCIETE ANANYME ")>0
qui replace asg_corp = 1 if strpos(assignee_std," SOCIETE ANNOYME ")>0
qui replace asg_corp = 1 if strpos(assignee_std," SOCIETE ANOMYME ")>0
qui replace asg_corp = 1 if strpos(assignee_std," SOCIETE ANOMYNE ")>0
qui replace asg_corp = 1 if strpos(assignee_std," SOCIETE ANONVME ")>0
qui replace asg_corp = 1 if strpos(assignee_std," SOCIETE ANONYM ")>0
qui replace asg_corp = 1 if strpos(assignee_std," SOCIETE ANONYME ")>0
qui replace asg_corp = 1 if strpos(assignee_std," SOCIETE ANOYME ")>0
qui replace asg_corp = 1 if strpos(assignee_std," SOCIETE CHIMIQUE ")>0
qui replace asg_corp = 1 if strpos(assignee_std," SOCIETE CIVILE ")>0
qui replace asg_corp = 1 if strpos(assignee_std," SOCIETE COOPERATIVE ")>0
qui replace asg_corp = 1 if strpos(assignee_std," SOCIETE D APPLICATIONS GENERALES ")>0
qui replace asg_corp = 1 if strpos(assignee_std," SOCIETE D APPLICATIONS MECANIQUES ")>0
qui replace asg_corp = 1 if strpos(assignee_std," SOCIETE D EQUIPEMENT ")>0
qui replace asg_corp = 1 if strpos(assignee_std," SOCIETE D ETUDE ET DE CONSTRUCTION ")>0
qui replace asg_corp = 1 if strpos(assignee_std," SOCIETE D ETUDE ET DE RECHERCHE EN VENTILATION ")>0
qui replace asg_corp = 1 if strpos(assignee_std," SOCIETE D ETUDES ET ")>0
qui replace asg_corp = 1 if strpos(assignee_std," SOCIETE D ETUDES TECHNIQUES ET D ENTREPRISES ")>0
qui replace asg_corp = 1 if strpos(assignee_std," SOCIETE DE ")>0
qui replace asg_corp = 1 if strpos(assignee_std," SOCIETE DE CONSEILS DE RECHERCHES ET D APPLICATIONS ")>0
qui replace asg_corp = 1 if strpos(assignee_std," SOCIETE DE CONSTRUCTIO ")>0
qui replace asg_corp = 1 if strpos(assignee_std," SOCIETE DE FABRICAITON ")>0
qui replace asg_corp = 1 if strpos(assignee_std," SOCIETE DE FABRICATION ")>0
qui replace asg_corp = 1 if strpos(assignee_std," SOCIETE DE PRODUCTION ET DE ")>0
qui replace asg_corp = 1 if strpos(assignee_std," SOCIETE DES TRANSPORTS ")>0
qui replace asg_corp = 1 if strpos(assignee_std," SOCIETE DITE ")>0
qui replace asg_corp = 1 if strpos(assignee_std," SOCIETE DITE : ")>0
qui replace asg_corp = 1 if strpos(assignee_std," SOCIETE DITE: ")>0
qui replace asg_corp = 1 if strpos(assignee_std," SOCIETE EN ")>0
qui replace asg_corp = 1 if strpos(assignee_std," SOCIETE EN COMMANDITE ")>0
qui replace asg_corp = 1 if strpos(assignee_std," SOCIETE EN COMMANDITE ENREGISTREE ")>0
qui replace asg_corp = 1 if strpos(assignee_std," SOCIETE EN NOM COLLECTIF ")>0
qui replace asg_corp = 1 if strpos(assignee_std," SOCIETE ETUDES ET ")>0
qui replace asg_corp = 1 if strpos(assignee_std," SOCIETE ETUDES ET DEVELOPPEMENTS ")>0
qui replace asg_corp = 1 if strpos(assignee_std," SOCIETE GENERALE POUR LES ")>0
qui replace asg_corp = 1 if strpos(assignee_std," SOCIETE GENERALE POUR LES TECHNIQUES NOVELLES ")>0
qui replace asg_corp = 1 if strpos(assignee_std," SOCIETE METALLURGIQUE ")>0
qui replace asg_corp = 1 if strpos(assignee_std," SOCIETE NOUVELLE ")>0
qui replace asg_corp = 1 if strpos(assignee_std," SOCIETE PAR ACTIONS ")>0
qui replace asg_corp = 1 if strpos(assignee_std," SOCIETE PAR ACTIONS SIMPLIFEE ")>0
qui replace asg_corp = 1 if strpos(assignee_std," SOCIETE PAR ACTIONS SIMPLIFIEE ")>0
qui replace asg_corp = 1 if strpos(assignee_std," SOCIETE TECHNIQUE D APPLICATION ET DE RECHERCHE ")>0
qui replace asg_corp = 1 if strpos(assignee_std," SOCIETE TECHNIQUE DE PULVERISATION ")>0
qui replace asg_corp = 1 if strpos(assignee_std," SOCIETEANONYME ")>0
qui replace asg_corp = 1 if strpos(assignee_std," SOCIETEDITE ")>0
qui replace asg_corp = 1 if strpos(assignee_std," SOCIETEINDUSTRIELLE ")>0
qui replace asg_corp = 1 if strpos(assignee_std," SOCRL ")>0
qui replace asg_corp = 1 if strpos(assignee_std," SOEHNE ")>0
qui replace asg_corp = 1 if strpos(assignee_std," SOGRANICHENNOI OTVETSTVENNOSTJU ")>0
qui replace asg_corp = 1 if strpos(assignee_std," SOHN ")>0
qui replace asg_corp = 1 if strpos(assignee_std," SOHNE ")>0
qui replace asg_corp = 1 if strpos(assignee_std," SONNER ")>0
qui replace asg_corp = 1 if strpos(assignee_std," SP ")>0
qui replace asg_corp = 1 if strpos(assignee_std," SP A ")>0
qui replace asg_corp = 1 if strpos(assignee_std," SP Z OO ")>0
qui replace asg_corp = 1 if strpos(assignee_std," SP ZOO ")>0
qui replace asg_corp = 1 if strpos(assignee_std," SPA ")>0
qui replace asg_corp = 1 if strpos(assignee_std," SPOKAZOO ")>0
qui replace asg_corp = 1 if strpos(assignee_std," SPOL ")>0
qui replace asg_corp = 1 if strpos(assignee_std," SPOL S R O ")>0
qui replace asg_corp = 1 if strpos(assignee_std," SPOL S RO ")>0
qui replace asg_corp = 1 if strpos(assignee_std," SPOL SRO ")>0
qui replace asg_corp = 1 if strpos(assignee_std," SPOLECNOST SRO ")>0
qui replace asg_corp = 1 if strpos(assignee_std," SPOLKA Z OO ")>0
qui replace asg_corp = 1 if strpos(assignee_std," SPOLKA ZOO ")>0
qui replace asg_corp = 1 if strpos(assignee_std," SPOLS RO ")>0
qui replace asg_corp = 1 if strpos(assignee_std," SPOLSRO ")>0
qui replace asg_corp = 1 if strpos(assignee_std," SPRL ")>0
qui replace asg_corp = 1 if strpos(assignee_std," SPZ OO ")>0
qui replace asg_corp = 1 if strpos(assignee_std," SPZOO ")>0
qui replace asg_corp = 1 if strpos(assignee_std," SR ")>0
qui replace asg_corp = 1 if strpos(assignee_std," SR L ")>0
qui replace asg_corp = 1 if strpos(assignee_std," SR1 ")>0
qui replace asg_corp = 1 if strpos(assignee_std," SRI ")>0
qui replace asg_corp = 1 if strpos(assignee_std," SRL ")>0
qui replace asg_corp = 1 if strpos(assignee_std," SRO ")>0
qui replace asg_corp = 1 if strpos(assignee_std," SﬂRL ")>0
qui replace asg_corp = 1 if strpos(assignee_std," SURL ")>0
qui replace asg_corp = 1 if strpos(assignee_std," TEAM ")>0
qui replace asg_corp = 1 if strpos(assignee_std," TECHNIQUES NOUVELLE ")>0
qui replace asg_corp = 1 if strpos(assignee_std," TECHNOLOGIES ")>0
qui replace asg_corp = 1 if strpos(assignee_std," THE FIRM ")>0
qui replace asg_corp = 1 if strpos(assignee_std," TOHO BUSINESS ")>0
qui replace asg_corp = 1 if strpos(assignee_std," TOVARISCHESIVO S OGRANICHENNOI OIVETSIVENNOSTIJU ")>0
qui replace asg_corp = 1 if strpos(assignee_std," TOVARISCHESTVO ")>0
qui replace asg_corp = 1 if strpos(assignee_std," TOVARISCHESTVO S OGRANICHENNOI ")>0
qui replace asg_corp = 1 if strpos(assignee_std," TOVARISCHESTVO S OGRANICHENNOI OTVETSTVENNOSTJU ")>0
qui replace asg_corp = 1 if strpos(assignee_std," TOVARISCHESTVO S OGRANICHENNOI OTVETSVENNOSTJU ")>0
qui replace asg_corp = 1 if strpos(assignee_std," TOVARISCHESTVO S ORGANICHENNOI OTVETSTVENNOSTJU ")>0
qui replace asg_corp = 1 if strpos(assignee_std," TOVARISCHETSTVO S ORGANICHENNOI ")>0
qui replace asg_corp = 1 if strpos(assignee_std," TRADING ")>0
qui replace asg_corp = 1 if strpos(assignee_std," TRADING AS ")>0
qui replace asg_corp = 1 if strpos(assignee_std," TRADING UNDER ")>0
qui replace asg_corp = 1 if strpos(assignee_std," UGINE ")>0
qui replace asg_corp = 1 if strpos(assignee_std," UNTERNEHMEN ")>0
qui replace asg_corp = 1 if strpos(assignee_std," USA ")>0
qui replace asg_corp = 1 if strpos(assignee_std," USINES ")>0
qui replace asg_corp = 1 if strpos(assignee_std," VAKMANSCHAP ")>0
qui replace asg_corp = 1 if strpos(assignee_std," VENNOOTSCHAP ")>0
qui replace asg_corp = 1 if strpos(assignee_std," VENNOOTSCHAP ONDER FIRMA: ")>0
qui replace asg_corp = 1 if strpos(assignee_std," VENNOOTSHAP ")>0
qui replace asg_corp = 1 if strpos(assignee_std," VENNOTSCHAP ")>0
qui replace asg_corp = 1 if strpos(assignee_std," VENOOTSCHAP ")>0
qui replace asg_corp = 1 if strpos(assignee_std," VENTURE ")>0
qui replace asg_corp = 1 if strpos(assignee_std," VERARBEITUNG ")>0
qui replace asg_corp = 1 if strpos(assignee_std," VERKOOP ")>0
qui replace asg_corp = 1 if strpos(assignee_std," VERSICHERUNGSBUERO ")>0
qui replace asg_corp = 1 if strpos(assignee_std," VERTRIEBSGESELLSCHAFT ")>0
qui replace asg_corp = 1 if strpos(assignee_std," VOF ")>0
qui replace asg_corp = 1 if strpos(assignee_std," WERK ")>0
qui replace asg_corp = 1 if strpos(assignee_std," WERKE ")>0
qui replace asg_corp = 1 if strpos(assignee_std," WERKEN ")>0
qui replace asg_corp = 1 if strpos(assignee_std," WERKHUIZEN ")>0
qui replace asg_corp = 1 if strpos(assignee_std," WERKS ")>0
qui replace asg_corp = 1 if strpos(assignee_std," WERKSTAETTE ")>0
qui replace asg_corp = 1 if strpos(assignee_std," WERKSTATT ")>0
qui replace asg_corp = 1 if strpos(assignee_std," WERKZEUGBAU ")>0
qui replace asg_corp = 1 if strpos(assignee_std," WINKEL ")>0
qui replace asg_corp = 1 if strpos(assignee_std," WORKS ")>0
qui replace asg_corp = 1 if strpos(assignee_std," YUGEN KAISHA ")>0
qui replace asg_corp = 1 if strpos(assignee_std," YUGENKAISHA ")>0
qui replace asg_corp = 1 if strpos(assignee_std," YUUGEN KAISHA ")>0
qui replace asg_corp = 1 if strpos(assignee_std," YUUGENKAISHA ")>0
qui replace asg_corp = 1 if strpos(assignee_std," ZOO ")>0

*** NON CORPORATE ***

** Government
gen asg_gov=0
qui replace asg_gov = 1 if strpos(assignee_std, " NAT RES COUNCIL ")>0 
qui replace asg_gov = 1 if strpos(assignee_std, " NAT RES INST ")>0 
qui replace asg_gov = 1 if strpos(assignee_std, " NAT SCI COUNCIL ")>0 
qui replace asg_gov = 1 if strpos(assignee_std, " NAT SCI INST ")>0 
qui replace asg_gov = 1 if strpos(assignee_std," AGENCY ")>0
qui replace asg_gov = 1 if strpos(assignee_std," STATE OF ")>0

qui replace asg_gov = 1 if strpos(assignee_std," US ADMIN ")>0
qui replace asg_gov = 1 if strpos(assignee_std," ADMINISTRATOR ")>0
qui replace asg_gov = 1 if strpos(assignee_std," COMMISSIONER OF PATENTS ")>0
qui replace asg_gov = 1 if strpos(assignee_std," US DEPT ")>0
qui replace asg_gov = 1 if strpos(assignee_std," US SEC ")>0

qui replace asg_gov = 1 if strpos(assignee_std, " UK SEC FOR ")>0 
qui replace asg_gov = 1 if strpos(assignee_std, " UK ")>0 & !asg_corp
qui replace asg_gov = 1 if strpos(assignee_std, " COMMONWEALTH ")>0 
qui replace asg_gov = 1 if strpos(assignee_std, " MIN OF ")>0 
qui replace asg_gov = 1 if strpos(assignee_std, " MIN FOR ")>0 

qui replace asg_gov = 1 if strpos(assignee_std, " LETAT FR REPRESENTE ")>0
qui replace asg_gov = 1 if strpos(assignee_std, " LA POSTE ")>0

qui replace asg_gov = 1 if strpos(assignee_std," ADMINISTRATOR ")>0
qui replace asg_gov = 1 if strpos(assignee_std," AGENCE ")>0
qui replace asg_gov = 1 if strpos(assignee_std," AGENCY ")>0
qui replace asg_gov = 1 if strpos(assignee_std," AMMINISTRAZIONE ")>0
qui replace asg_gov = 1 if strpos(assignee_std," AMMINISTRAZIONE ")>0
qui replace asg_gov = 1 if strpos(assignee_std," AUTHORITY ")>0
qui replace asg_gov = 1 if strpos(assignee_std," BOTANICAL GARDEN ")>0
qui replace asg_gov = 1 if strpos(assignee_std," BUNDESANSTALT ")>0
qui replace asg_gov = 1 if strpos(assignee_std," BUNDESREPUBLIK ")>0
qui replace asg_gov = 1 if strpos(assignee_std," CHAMBRE ")>0
qui replace asg_gov = 1 if strpos(assignee_std," CITY ")>0 & !asg_corp
qui replace asg_gov = 1 if strpos(assignee_std," COMISSARIAT ")>0
qui replace asg_gov = 1 if strpos(assignee_std," COMMISARIAT ")>0
qui replace asg_gov = 1 if strpos(assignee_std," COMMISSARAIT ")>0
qui replace asg_gov = 1 if strpos(assignee_std," COMMISSARAT ")>0
qui replace asg_gov = 1 if strpos(assignee_std," COMMISSARIAT ")>0
qui replace asg_gov = 1 if strpos(assignee_std," COMMISSARIET ")>0
qui replace asg_gov = 1 if strpos(assignee_std," COMMISSION ")>0
qui replace asg_gov = 1 if strpos(assignee_std," COMMISSRIAT ")>0
qui replace asg_gov = 1 if strpos(assignee_std," COMMONWEALTH ")>0
qui replace asg_gov = 1 if strpos(assignee_std," COMMUNAUTE ")>0
qui replace asg_gov = 1 if strpos(assignee_std," CONFEDERATED TRIBES ")>0
qui replace asg_gov = 1 if strpos(assignee_std," COOUNCIL OF ")>0
qui replace asg_gov = 1 if strpos(assignee_std," COUCIL OF ")>0
qui replace asg_gov = 1 if strpos(assignee_std," COUNCIL ")>0
qui replace asg_gov = 1 if strpos(assignee_std," COUNSEL OF ")>0
qui replace asg_gov = 1 if strpos(assignee_std," COUNTY ")>0
qui replace asg_gov = 1 if strpos(assignee_std," DEN PRAESIDENTEN ")>0
qui replace asg_gov = 1 if strpos(assignee_std," DEPARTMENT OF AGRICULTURE ")>0
qui replace asg_gov = 1 if strpos(assignee_std," DETAT ")>0
qui replace asg_gov = 1 if strpos(assignee_std," ETAT ")>0
qui replace asg_gov = 1 if strpos(assignee_std," GERMANY ")>0 & !asg_corp
qui replace asg_gov = 1 if strpos(assignee_std," GEZONDHEIDSDIENST ")>0
qui replace asg_gov = 1 if strpos(assignee_std," GOUVERNEMENT ")>0
qui replace asg_gov = 1 if strpos(assignee_std," GOUVERNMENT ")>0
qui replace asg_gov = 1 if strpos(assignee_std," GOVERNER ")>0
qui replace asg_gov = 1 if strpos(assignee_std," GOVERNMENT ")>0
qui replace asg_gov = 1 if strpos(assignee_std," GOVERNOR ")>0
qui replace asg_gov = 1 if strpos(assignee_std," HER MAJESTY ")>0
qui replace asg_gov = 1 if strpos(assignee_std," KEN ")>0
qui replace asg_gov = 1 if strpos(assignee_std," LETAT ")>0
qui replace asg_gov = 1 if strpos(assignee_std," MINISTER ")>0
qui replace asg_gov = 1 if strpos(assignee_std," MINISTERO ")>0
qui replace asg_gov = 1 if strpos(assignee_std," MINISTRE ")>0
qui replace asg_gov = 1 if strpos(assignee_std," MINISTRI ")>0
qui replace asg_gov = 1 if strpos(assignee_std," MINISTRO ")>0
qui replace asg_gov = 1 if strpos(assignee_std," MINISTRY ")>0
qui replace asg_gov = 1 if strpos(assignee_std," MUNICIPAL UTILITY DISTRICT ")>0
qui replace asg_gov = 1 if strpos(assignee_std," NACIONAL ")>0
qui replace asg_gov = 1 if strpos(assignee_std," NATIONAL ")>0 & !asg_corp
qui replace asg_gov = 1 if strpos(assignee_std," NAZIONALE ")>0
qui replace asg_gov = 1 if strpos(assignee_std," POLICE ")>0
qui replace asg_gov = 1 if strpos(assignee_std," PREFECTURE ")>0
qui replace asg_gov = 1 if strpos(assignee_std," PRESIDENZA DEL CONSIGLIO DEI MINISTRI ")>0
qui replace asg_gov = 1 if strpos(assignee_std," PRESIDENZADEL CONSIGLIO DEL MINISTRI ")>0
qui replace asg_gov = 1 if strpos(assignee_std," REPUBLIC ")>0
qui replace asg_gov = 1 if strpos(assignee_std," RESEARCH COUNCIL ")>0
qui replace asg_gov = 1 if strpos(assignee_std," SECRETARIAT ")>0
qui replace asg_gov = 1 if strpos(assignee_std," SECRETARY ")>0
qui replace asg_gov = 1 if strpos(assignee_std," STAAT ")>0
qui replace asg_gov = 1 if strpos(assignee_std," STADT ")>0
qui replace asg_gov = 1 if strpos(assignee_std," STATE ")>0 & !asg_corp
qui replace asg_gov = 1 if strpos(assignee_std," STATO ")>0
qui replace asg_gov = 1 if strpos(assignee_std," THE QUEEN ")>0
qui replace asg_gov = 1 if strpos(assignee_std," VILLE ")>0

** clean up government departments
qui replace assignee_std = subinstr(assignee_std," SEC OF DEPT OF "," DEPT OF ",1)
qui replace assignee_std = subinstr(assignee_std," SEC OF THE DEPT OF "," DEPT OF ",1)
qui replace asg_gov = 1 if strpos(assignee_std," CNRS ")>0
qui replace asg_gov = 1 if strpos(assignee_std," CENT NAT DE LA RECH ")>0
qui replace asg_gov = 1 if strpos(assignee_std," CENT NAT DETUDES SPATIALES ")>0
qui replace asg_gov = 1 if strpos(assignee_std, " DESY ")>0 

** universities
gen asg_univ=0
qui replace asg_univ = 1 if strpos(assignee_std," ACADEM")>0
qui replace asg_univ = 1 if strpos(assignee_std," ACAD ")>0
qui replace asg_univ = 1 if strpos(assignee_std," AKAD ")>0
qui replace asg_univ = 1 if strpos(assignee_std," COLLEGE ")>0
qui replace asg_univ = 1 if strpos(assignee_std," CURATORS ")>0
qui replace asg_univ = 1 if strpos(assignee_std," ECOLE ")>0
qui replace asg_univ = 1 if strpos(assignee_std," FACULTE ")>0
qui replace asg_univ = 1 if strpos(assignee_std," INST OF TECH ")>0
qui replace asg_univ = 1 if strpos(assignee_std," INST OF TECH")>0
qui replace asg_univ = 1 if strpos(assignee_std," INSTITUTE OF TECHNOLOGY ")>0
qui replace asg_univ = 1 if strpos(assignee_std," INTERNUIVERSITAIR ")>0
qui replace asg_univ = 1 if strpos(assignee_std," INTERUNIVERITAIR ")>0
qui replace asg_univ = 1 if strpos(assignee_std," POLITEC ")>0
qui replace asg_univ = 1 if strpos(assignee_std," POLYTEC ")>0
qui replace asg_univ = 1 if strpos(assignee_std," REGENTS ")>0
qui replace asg_univ = 1 if strpos(assignee_std," RIJKSUNIVERSTTEIT ")>0
qui replace asg_univ = 1 if strpos(assignee_std," SCHOOL ")>0
qui replace asg_univ = 1 if strpos(assignee_std," SCHULE ")>0
qui replace asg_univ = 1 if strpos(assignee_std," SUPERVISORS ")>0
qui replace asg_univ = 1 if strpos(assignee_std," TRUSTEES ")>0
qui replace asg_univ = 1 if strpos(assignee_std," UMIVERSIDAD ")>0
qui replace asg_univ = 1 if strpos(assignee_std," UNIV ")>0
qui replace asg_univ = 1 if strpos(assignee_std," UNIVERISITY ")>0
qui replace asg_univ = 1 if strpos(assignee_std," UNIVERISTY ")>0
qui replace asg_univ = 1 if strpos(assignee_std," UNIVERSATIES ")>0
qui replace asg_univ = 1 if strpos(assignee_std," UNIVERSI")>0
qui replace asg_univ = 1 if strpos(assignee_std," UNIVERSTIA ")>0
qui replace asg_univ = 1 if strpos(assignee_std," UNIVERSTITAT ")>0
qui replace asg_univ = 1 if strpos(assignee_std," UNIVERSTITAET ")>0
qui replace asg_univ = 1 if strpos(assignee_std," UNIVERSTITY ")>0
qui replace asg_univ = 1 if strpos(assignee_std," UNIVERSTIY ")>0
qui replace asg_univ = 1 if strpos(assignee_std," UNIVERSY ")>0
qui replace asg_univ = 1 if strpos(assignee_std," UNIVERZ ")>0
qui replace asg_univ = 1 if strpos(assignee_std," UNVERSITY ")>0
qui replace asg_univ = 1 if strpos(assignee_std," KU LEUVEN ")>0

** Non-profit institutes
gen asg_institute=0
qui replace asg_institute = 1 if strpos(assignee_std," RESEARCH COUNCIL ")!=0
qui replace asg_institute = 1 if strpos(assignee_std," RES COUNCIL ")!=0
qui replace asg_institute = 1 if strpos(assignee_std, " FRAUNHOFER GES ")>0 
qui replace asg_institute = 1 if strpos(assignee_std, " MAX PLANCK GES ")>0 
qui replace asg_institute = 1 if strpos(assignee_std," COUNCIL OF ")>0 & strpos(assignee_std," RES ")>0
qui replace asg_institute = 1 if strpos(assignee_std," ASBL ")>0
qui replace asg_institute = 1 if strpos(assignee_std," ASOCIACION ")>0
*qui replace asg_institute = 1 if strpos(assignee_std," ASSOC ")>0
qui replace asg_institute = 1 if strpos(assignee_std," ASSOCIATION ")>0 & strpos(assignee_std," AUTOMOBILE ASSOCIATION ")==0
qui replace asg_institute = 1 if strpos(assignee_std," ASSOCIAZIONE ")>0
qui replace asg_institute = 1 if strpos(assignee_std," BLOOD ")>0
qui replace asg_institute = 1 if strpos(assignee_std," BLOOD CENTER ")>0
qui replace asg_institute = 1 if strpos(assignee_std," BLOOD SERVICES ")>0
qui replace asg_institute = 1 if strpos(assignee_std," BLOOD TRANSFUSION SERVICE ")>0
qui replace asg_institute = 1 if strpos(assignee_std," CHURCH ")>0 & strpos(assignee_std," CHURCH & DWIGHT ")==0
qui replace asg_institute = 1 if strpos(assignee_std," COOPERATIVE ")>0
qui replace asg_institute = 1 if strpos(assignee_std," E V ")>0
qui replace asg_institute = 1 if strpos(assignee_std," EV ")>0
qui replace asg_institute = 1 if strpos(assignee_std," FEDERATION ")>0
qui replace asg_institute = 1 if strpos(assignee_std," FONDATION ")>0
qui replace asg_institute = 1 if strpos(assignee_std," FONDATIONE ")>0
qui replace asg_institute = 1 if strpos(assignee_std," FOUNDATION ")>0
qui replace asg_institute = 1 if strpos(assignee_std," FOUND ")>0 & asg_univ == 0
qui replace asg_institute = 1 if strpos(assignee_std," FORSKNINGSINSTITUT ")>0
qui replace asg_institute = 1 if strpos(assignee_std," FUNDACAO ")>0
qui replace asg_institute = 1 if strpos(assignee_std," FUNDACIO ")>0
qui replace asg_institute = 1 if strpos(assignee_std," FUNDACION ")>0
qui replace asg_institute = 1 if strpos(assignee_std," FUNDATION ")>0
qui replace asg_institute = 1 if strpos(assignee_std," INDUSTRIAL TECHNOLOGY RESEARCH ")>0
qui replace asg_institute = 1 if strpos(assignee_std," INSITUT ")>0
qui replace asg_institute = 1 if strpos(assignee_std," INSITUTE ")>0
qui replace asg_institute = 1 if strpos(assignee_std," INST ")>0 & asg_univ == 0
qui replace asg_institute = 1 if strpos(assignee_std," INSTIT ")>0 & asg_univ == 0
qui replace asg_institute = 1 if strpos(assignee_std," INSTITUTE ")>0 & asg_univ == 0 & strpos(assignee_std," GENETICS ")==0
qui replace asg_institute = 1 if strpos(assignee_std," INSTYTUT ")>0
qui replace asg_institute = 1 if strpos(assignee_std," INSTYTUT ")>0
qui replace asg_institute = 1 if strpos(assignee_std," INTITUTE ")>0
qui replace asg_institute = 1 if strpos(assignee_std," ISTITUTO ")>0
qui replace asg_institute = 1 if strpos(assignee_std," KENKYUSHO ")>0
qui replace asg_institute = 1 if strpos(assignee_std," MINISTRIES ")>0
qui replace asg_institute = 1 if strpos(assignee_std," SOCIETY ")>0
qui replace asg_institute = 1 if strpos(assignee_std," STICHTING ")>0
qui replace asg_institute = 1 if strpos(assignee_std," STIFTELSE ")>0
qui replace asg_institute = 1 if strpos(assignee_std," STIFTUNG ")>0
qui replace asg_institute = 1 if strpos(assignee_std," TRANSFUSION ")>0
qui replace asg_institute = 1 if strpos(assignee_std," TRANSFUSION SANGUINE ")>0
qui replace asg_institute = 1 if strpos(assignee_std," TRUST ")>0
qui replace asg_institute = 1 if strpos(assignee_std," VERENINING ")>0
qui replace asg_institute = 1 if strpos(assignee_std," VZW ")>0

** GERMANY
* EINGETRAGENER VEREIN. NON PROFIT SOCIETY/ASSOCIATION. 
qui replace asg_institute = 1 if (strpos(assignee_std, " EINGETRAGENER VEREIN ")!=0|strpos(assignee_std," STIFTUNG ")~=0) ///
 &strpos(assignee_std," UNIV ")==0 ///
 &strpos(assignee_std," GMBH ")==0&strpos(assignee_std," KGAA ")==0&strpos(assignee_std," KG ")==0 /// 
 &strpos(assignee_std," AG ")==0&strpos(assignee_std," EG ")==0&strpos(assignee_std," OHG ")==0

** Hospitals
gen asg_hospital=0
qui replace asg_hospital = 1 if strpos(assignee_std," AMTS SYGEHUS ")>0
qui replace asg_hospital = 1 if strpos(assignee_std," AMTSSYGEHUS ")>0
qui replace asg_hospital = 1 if strpos(assignee_std," BOLNHITSA ")>0
qui replace asg_hospital = 1 if strpos(assignee_std," BOLNISN ")>0
qui replace asg_hospital = 1 if strpos(assignee_std," BOLNITSA ")>0
qui replace asg_hospital = 1 if strpos(assignee_std," BOLNYITSA ")>0
qui replace asg_hospital = 1 if strpos(assignee_std," CENTRE ")>0
qui replace asg_hospital = 1 if strpos(assignee_std," CLINIC ")>0
qui replace asg_hospital = 1 if strpos(assignee_std," CLINICA ")>0
qui replace asg_hospital = 1 if strpos(assignee_std," CLINIQUE ")>0
qui replace asg_hospital = 1 if strpos(assignee_std," HAIGLA ")>0
qui replace asg_hospital = 1 if strpos(assignee_std," HOPITAL ")>0
qui replace asg_hospital = 1 if strpos(assignee_std," HOPITAUX ")>0
qui replace asg_hospital = 1 if strpos(assignee_std," HOPSITAL ")>0 
qui replace asg_hospital = 1 if strpos(assignee_std," HOSITAL ")>0
qui replace asg_hospital = 1 if strpos(assignee_std," HOSP ")>0
qui replace asg_hospital = 1 if strpos(assignee_std," HOSPIDAL ")>0
qui replace asg_hospital = 1 if strpos(assignee_std," HOSPITAL ")>0 & (strpos(assignee_std," HOSPITAL PRODUCTS ")==0 & strpos(assignee_std," HOSPITAL SUPPLY ")==0)
qui replace asg_hospital = 1 if strpos(assignee_std," HOSPITALARIO ")>0
qui replace asg_hospital = 1 if strpos(assignee_std," HOSPITALET ")>0
qui replace asg_hospital = 1 if strpos(assignee_std," HOSPITAUX ")>0
qui replace asg_hospital = 1 if strpos(assignee_std," KESKUSSAIRAALA ")>0
qui replace asg_hospital = 1 if strpos(assignee_std," KLIINIK ")>0
qui replace asg_hospital = 1 if strpos(assignee_std," KLINIK ")>0
qui replace asg_hospital = 1 if strpos(assignee_std," KLINIKA ")>0
qui replace asg_hospital = 1 if strpos(assignee_std," KLINIKKA ")>0
qui replace asg_hospital = 1 if strpos(assignee_std," KLINIKUM ")>0
qui replace asg_hospital = 1 if strpos(assignee_std," KORHAZ ")>0
qui replace asg_hospital = 1 if strpos(assignee_std," KRANKENHAUS ")>0
qui replace asg_hospital = 1 if strpos(assignee_std," LHOSPTALET ")>0
qui replace asg_hospital = 1 if strpos(assignee_std," LIGONINE ")>0
*qui replace asg_hospital = 1 if strpos(assignee_std," MEDICAL ")>0
qui replace asg_hospital = 1 if strpos(assignee_std," MEDICAL CENTER ")>0
qui replace asg_hospital = 1 if strpos(assignee_std," NEMOCNICA ")>0
qui replace asg_hospital = 1 if strpos(assignee_std," NEMOCNICE ")>0
qui replace asg_hospital = 1 if strpos(assignee_std," NOSOCOMIO ")>0
qui replace asg_hospital = 1 if strpos(assignee_std," NOSOKOMIO ")>0
qui replace asg_hospital = 1 if strpos(assignee_std," OSPEDALE ")>0
qui replace asg_hospital = 1 if strpos(assignee_std," OSPETALE ")>0
qui replace asg_hospital = 1 if strpos(assignee_std," OSPITALIERI ")>0
qui replace asg_hospital = 1 if strpos(assignee_std," POLICLINICA ")>0
qui replace asg_hospital = 1 if strpos(assignee_std," POLICLINICO ")>0
qui replace asg_hospital = 1 if strpos(assignee_std," POLIKLINIK ")>0
qui replace asg_hospital = 1 if strpos(assignee_std," SAIRAALA ")>0
qui replace asg_hospital = 1 if strpos(assignee_std," SJUKHUS ")>0
qui replace asg_hospital = 1 if strpos(assignee_std," SJUKHUSET ")>0
qui replace asg_hospital = 1 if strpos(assignee_std," SLIMNICA ")>0
qui replace asg_hospital = 1 if strpos(assignee_std," SPITAL ")>0
qui replace asg_hospital = 1 if strpos(assignee_std," STACIONARS ")>0
qui replace asg_hospital = 1 if strpos(assignee_std," STANICA ")>0
qui replace asg_hospital = 1 if strpos(assignee_std," STREDISKO ")>0
qui replace asg_hospital = 1 if strpos(assignee_std," SYGEHUS ")>0
qui replace asg_hospital = 1 if strpos(assignee_std," SYGEHUSET ")>0
qui replace asg_hospital = 1 if strpos(assignee_std," SYKEHUS ")>0
qui replace asg_hospital = 1 if strpos(assignee_std," SZPITAL ")>0
qui replace asg_hospital = 1 if strpos(assignee_std," UNIVERSITAETSKLINIK ")>0
qui replace asg_hospital = 1 if strpos(assignee_std," ZIEKENHUIS ")>0
qui replace asg_hospital = 1 if strpos(assignee_std," CITY OF HOPE ")>0

rename asg_corp asgname_corp
rename asg_gov asgname_gov
rename asg_univ asgname_univ
rename asg_institute asgname_inst
rename asg_hospital asgname_hosp

label var asgname_corp "1 if assignee name is a corporation (PDP method)"
replace assignee_std=itrim(assignee_std)
replace assignee_std=trim(assignee_std)

**********************
****** FINALIZE ******
**********************
unique assignee
unique assignee_std
unique assignee_derwent

egen assignee_id=group(assignee_std)

label var assignee_id "assignee ID based on cleaned assignee name"
order assignee_std assignee_id assignee_derwen asgname_corp-asgname_hosp, after(assignee)
compress

save "[assignee name data]", replace

/*************** ---------------- ****************
		GENERATE SEARCH SAMPLE FOR BING
*************** ---------------- ****************/

keep assignee_id assignee_std
duplicates drop
compress
drop if assignee_std==""
export delimited "[input into python]", replace

