/* --------------------------------------
AUTHOR: Pian Shu
THIS VERSION: January, 2019
PURPOSE: REMOVES PUNCTUATIONS IN COMPUSTAT FIRM NAMES
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

use  "[input file that contains Compustat firm names]", clear

gen standard_name=" "+upper(name)+" "

** clean names that end in (THE) or start with THE
replace standard_name=substr(standard_name, 5, .) if substr(standard_name, 1, 5)==" THE "
replace standard_name=substr(standard_name, 1, length(standard_name)-6)+" " if substr(standard_name, -6, 6)=="(THE) "

** confirm suffix removed
assert strpos(standard_name, " -")==0
assert strpos(standard_name, " -CL")==0

/*
foreach char in "=" "@" "$" "„" "`" "^" "<" ">" "£" "#" "~" "¶" "%" "|" "," "\" "''"","{
di "`char'"
count if strpos(standard_name,"`char'")>0
}
*/

** strips out all punctuation characters

foreach char in "'" ".""!" "?" "*" ","{
	qui replace standard_name = subinstr(standard_name, "`char'",  "",.)
}


foreach char in "/" "-" "\"{
	qui replace standard_name = subinstr(standard_name, "`char'",  " ",.)
}


foreach char in ";" "=" "@" "$" "„" "`" "^" "<" ">" "£" "#" "~" "¶" "%" "|" "," "''""," "{" "}" "[" "]" ":" {
	assert strpos(standard_name, "`char'")==0
}

*** DEAL WITH "(" ")": most contain useful info
replace standard_name = subinstr(standard_name, "(THE)", "", .)

egen temp=noccur(standard_name),s("(")
egen temp1=noccur(standard_name),s(")")
assert temp==1 | temp==0
assert temp==temp1
drop temp temp1

* remove
foreach char in "(" ")" {
replace standard_name = subinstr(standard_name, "`char'",  "",.)
}

** Recode all common words for "AND" to &
replace standard_name = subinstr(standard_name, " AND ", " & ", .) if substr(standard_name,1,5)!=" AND "
replace standard_name = subinstr(standard_name, " ET ", " & ", .) if substr(standard_name,1,4)!=" ET "
replace standard_name = subinstr(standard_name, " UND ", " & ", .) if substr(standard_name,1,5)!=" UND "
replace standard_name = subinstr(standard_name, "+", " PLUS ", .)
replace standard_name = itrim(standard_name)

** CONFIRM ALL CLEAN
genchars standard_name
replace chars=subinstr(chars,"&","",.)
assert chars==""
drop chars

assert strpos(standard_name, " CL A ")==0
assert strpos(standard_name, " CL B ")==0

gen firmname_std=trim(standard_name)
replace firmname_std=itrim(firmname_std)
egen firmname_id=group(firmname_std)

