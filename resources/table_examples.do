clear
set more off

sysuse census, clear
  foreach i of varlist pop-popurban death-divorce {
    replace `i' = `i' / 1000
  }

  
********************************************************************************
																				
				/*LaTeX Summary Statistic Tables*/
				
********************************************************************************

/*Table 1*/
tabstat pop pop65p medage death marriage divorce, c(stat) stat(sum mean sd min max n)

est clear
estpost tabstat pop pop65p medage death marriage divorce, c(stat) stat(sum mean sd min max n)

esttab using "/Users/kevin2/Desktop/tables/table_1.tex", replace cells("sum mean sd min max count") booktabs

/*Table 2*/
esttab using "/Users/kevin2/Desktop/tables/table_2.tex", replace cells("sum mean sd min max count") /// 
booktabs nonumber nomtitle nonote noobs compress label

/*Table 3*/
esttab using "/Users/kevin2/Desktop/tables/table_3.tex", replace /// 
cells("sum(fmt(%8.0fc)) mean(fmt(%8.2fc)) sd(fmt(%8.2fc)) min max count") /// 
booktabs nonumber nomtitle nonote noobs compress label ///
collabels("Sum" "Mean" "SD" "Min" "Max" "Obs")

/*Table 4*/
gen treat=1 if inlist(state, "Louisiana", "Texas", "Arkansas", "Mississippi")
replace treat=0 if treat==.

est clear
estpost tabstat pop pop65p medage death marriage divorce, by(treat) c(stat) stat(mean sd) nototal

esttab using "/Users/kevin2/Desktop/tables/table_4.tex", replace /// 
cells("mean(fmt(%8.2fc))" "sd(par)") /// 
booktabs nonumber nomtitle nonote noobs compress label unstack gap ///
collabels(none) eqlabels("Treatment" "Control")

/*Table 5*/
esttab using "/Users/kevin2/Desktop/tables/table_5.tex", replace ///
refcat(pop "\emph{Demographic}" death "\vspace{0.01in} \\ \emph{Status}", nolabel) ///
cells("mean(fmt(%8.2fc))" "sd(par)") /// 
booktabs nonumber nomtitle nonote noobs compress label unstack gap ///
collabels(none) eqlabels("Treatment" "Control")

/*Table 6*/
esttab using "/Users/kevin2/Desktop/tables/table_6.tex", replace ///
refcat(pop "\emph{Demographic}" death "\vspace{0.01in} \\ \emph{Status}", nolabel) ///
cells("mean(fmt(%8.2fc))" "sd(par)") /// 
booktabs nonumber nomtitle nonote noobs compress label unstack nogaps ///
collabels(none) eqlabels("Treatment" "Control")

/*Table 7*/
count if treat == 1
estadd local treat_N = r(N)
count if treat == 0
estadd local control_N = r(N)

esttab using "/Users/kevin2/Desktop/tables/table_7.tex", replace ///
refcat(pop "\emph{Demographic}" death "\vspace{0.01in} \\ \emph{Status}", nolabel) ///
cells("mean(fmt(%8.2fc))" "sd(par)") /// 
booktabs nonumber nomtitle nonote noobs compress label unstack nogaps ///
collabels(none) eqlabels("Treatment" "Control") /// 
stats(treat_N control_N, layout("@ @") labels("Observations"))

/*Table 8*/
global vars pop pop65p medage death marriage divorce

est clear
estpost ttest $vars, by(treat)

count if treat == 1
estadd local treat_N = r(N)
count if treat == 0
estadd local control_N = r(N)

esttab using "/Users/kevin2/Desktop/tables/table_8.tex", replace ///
refcat(pop "\emph{Demographic}" death "\vspace{0.01in} \\ \emph{Status}", nolabel) ///
cells("mu_1 mu_2 p(star)") /// 
star(* 0.50 ** 0.20 *** 0.10) ///
booktabs nonumber nomtitle nonote noobs compress label nogaps ///
collabels("Treatment" "Control" "\shortstack{p-Value of\\Difference}") /// 
stats(treat_N control_N, layout("@ @") labels("Observations"))

clear

********************************************************************************
																				
				/*Word Summar Statistic Tables*/
				
********************************************************************************

sysuse census, clear
  foreach i of varlist pop-popurban death-divorce {
    replace `i' = `i' / 1000
  }

/*Table 1*/
tabstat pop pop65p medage death marriage divorce, c(stat) stat(sum mean sd min max n)

est clear
estpost tabstat pop pop65p medage death marriage divorce, c(stat) stat(sum mean sd min max n)

esttab using "/Users/kevin2/Desktop/tables/tables.rtf", replace /// 
cells("sum mean sd min max count") title("Table 1: Summary statistics - Basic")

/*Table 2*/
esttab using "/Users/kevin2/Desktop/tables/tables.rtf", append /// 
cells("sum mean sd min max count") title("Table 2: Summary statistics - Basic, Clean") /// 
nonumber nomtitle nonote noobs compress label

/*Table 3*/
esttab using "/Users/kevin2/Desktop/tables/tables.rtf", append /// 
cells("sum(fmt(%8.0fc)) mean(fmt(%8.2fc)) sd(fmt(%8.2fc)) min max count") /// 
title("Table 3: Summary statistics - Basic, Formatted") ///
nonumber nomtitle nonote noobs compress label ///
collabels("Sum" "Mean" "SD" "Min" "Max" "Obs")

/*Table 4*/
gen treat=1 if inlist(state, "Louisiana", "Texas", "Arkansas", "Mississippi")
replace treat=0 if treat==.

est clear
estpost tabstat pop pop65p medage death marriage divorce, by(treat) c(stat) stat(mean sd) nototal

esttab using "/Users/kevin2/Desktop/tables/tables.rtf", append /// 
cells("mean(fmt(%8.2fc))" "sd(par)") ///
title("Table 4: Summary statistics - Treat vs. Control") /// 
nonumber nomtitle nonote noobs compress label unstack gap ///
collabels(none) eqlabels("Treatment" "Control")

/*Table 5*/
esttab using "/Users/kevin2/Desktop/tables/tables.rtf", append ///
refcat(pop "{\i Demographic}" death "\line {\i Status}", nolabel) ///
cells("mean(fmt(%8.2fc))" "sd(par)") ///
title("Table 5: Summary statistics - Treat vs. Control, Grouped") ///  
nonumber nomtitle nonote noobs compress label unstack gap ///
collabels(none) eqlabels("Treatment" "Control")

/*Table 6*/
esttab using "/Users/kevin2/Desktop/tables/tables.rtf", append ///
refcat(pop "{\i Demographic}" death "\line {\i Status}", nolabel) ///
cells("mean(fmt(%8.2fc))" "sd(par)") ///
title("Table 6: Summary statistics - Treat vs. Control, Spaced") ///
nonumber nomtitle nonote noobs compress label unstack nogaps ///
collabels(none) eqlabels("Treatment" "Control")

/*Table 7*/
est clear
estpost tabstat pop pop65p medage death marriage divorce, by(treat) c(stat) stat(mean sd) nototal

count if treat == 1
estadd local treat_N = r(N)
count if treat == 0
estadd local control_N = r(N)

esttab using "/Users/kevin2/Desktop/tables/tables.rtf", append ///
refcat(pop "{\i Demographic}" death "\line {\i Status}", nolabel) ///
cells("mean(fmt(%8.2fc))" "sd(par)") ///
title("Table 7: Summary statistics - Treat vs. Control, Obs") /// 
nonumber nomtitle nonote noobs compress label unstack nogaps ///
collabels(none) eqlabels("Treatment" "Control") /// 
stats(treat_N control_N, layout("@ @") labels("Observations"))

/*Table 8*/
est clear
estpost ttest $vars, by(treat)

count if treat == 1
estadd local treat_N = r(N)
count if treat == 0
estadd local control_N = r(N)

esttab using "/Users/kevin2/Desktop/tables/tables.rtf", append ///
refcat(pop "{\i Demographic}" death "\line {\i Status}", nolabel) ///
cells("mu_1 mu_2 p(star)") /// 
star(* 0.50 ** 0.20 *** 0.10) ///
title("Table 8: Summary statistics - Treat vs. Control, t-test") /// 
nonumber nomtitle nonote noobs compress label nogaps ///
collabels("Treatment" "Control" "p-Value of Difference") /// 
stats(treat_N control_N, layout("@ @") labels("Observations")) ///
addnotes("{\i Notes:} The treatment group includes Louisiana, Texas, Arkansas, and Mississippi. The control group includes all other states." "{\i *p<0.50, **p<0.20, ***p<0.10}")

********************************************************************************
																				
				/*LaTeX Regression Tables*/
				
********************************************************************************

webuse nlswork, clear
xtset idcode year
gen age2      = age^2
gen ttl_exp2  = ttl_exp^2
gen tenure2   = tenure^2
gen black     = (race==2)
lab var age      "Age"
lab var age2     "Age sq."
lab var ttl_exp  "Work experience"
lab var ttl_exp2 "Work experience sq."
lab var tenure   "Job tenure"
lab var tenure2  "Job tenure eq."
lab var not_smsa "Rural"
lab var south    "South"
lab var union    "Union"

/*Table 9*/
est clear
eststo: xtreg ln_w union
eststo: xtreg ln_w union age* ttl_exp* tenure*
eststo: xtreg ln_w union age* ttl_exp* tenure* not_smsa south

esttab using "/Users/kevin2/Desktop/tables/table_9.tex", replace /// 
b(3) se(3) star(* 0.10 ** 0.05 *** 0.01) ///
booktabs nomtitle nonote compress label

/*Table 10*/
est clear
eststo: xtreg ln_w union
estadd local  TE  "No"
estadd local  FE  "No"

eststo: xtreg ln_w union age* ttl_exp* tenure*
estadd local  TE  "No"
estadd local  FE  "No"

eststo: xtreg ln_w union age* ttl_exp* tenure* not_smsa south
estadd local  TE  "No"
estadd local  FE  "No"

eststo: xtreg ln_w union age* ttl_exp* tenure* not_smsa south i.year
estadd local  TE  "Yes"
estadd local  FE  "No"

eststo: xtreg ln_w union age* ttl_exp* tenure* not_smsa south i.year, fe
estadd local  TE  "Yes"
estadd local  FE  "Yes"

esttab using "/Users/kevin2/Desktop/tables/table_10.tex", replace   ///
b(3) se(3) keep(union age* ttl_exp* tenure* not_smsa south) ///
star(* 0.10 ** 0.05 *** 0.01) ///
booktabs nomtitle nonote compress label ///
scalars("TE Year fixed effects" "FE Individual fixed effects")

/*Table 11*/
est clear
eststo: xtreg ln_w union
qui sum `e(depvar)' if e(sample)
estadd scalar Mean= r(mean)
estadd local  TE  "No"
estadd local  FE  "No"

eststo: xtreg ln_w union age* ttl_exp* tenure*
qui sum `e(depvar)' if e(sample)
estadd scalar Mean= r(mean)
estadd local  TE  "No"
estadd local  FE  "No"

eststo: xtreg ln_w union age* ttl_exp* tenure* not_smsa south
qui sum `e(depvar)' if e(sample)
estadd scalar Mean= r(mean)
estadd local  TE  "No"
estadd local  FE  "No"

eststo: xtreg ln_w union age* ttl_exp* tenure* not_smsa south i.year
qui sum `e(depvar)' if e(sample)
estadd scalar Mean= r(mean)
estadd local  TE  "Yes"
estadd local  FE  "No"

eststo: xtreg ln_w union age* ttl_exp* tenure* not_smsa south i.year, fe
qui sum `e(depvar)' if e(sample)
estadd scalar Mean= r(mean)
estadd local  TE  "Yes"
estadd local  FE  "Yes"

esttab using "/Users/kevin2/Desktop/tables/table_11.tex", replace   ///
b(3) se(3) keep(union age* ttl_exp* tenure* not_smsa south) ///
star(* 0.10 ** 0.05 *** 0.01) ///
booktabs nomtitle nonote noobs compress label ///
scalars("TE Year fixed effects" "FE Individual fixed effects" "Mean \vspace{0.01cm} \\ Dep. Var. Mean" "N Observations") sfmt(0 0 2 %8.0fc)

/*Table 12*/
est clear
foreach i of varlist ln_wage hours wks_ue {
	eststo: xtreg `i' union age* ttl_exp* tenure* not_smsa south
	qui sum `e(depvar)' if e(sample)
	estadd scalar Mean= r(mean)
	estadd local  TE  "No"
	estadd local  FE  "No"
	
	eststo: xtreg `i' union age* ttl_exp* tenure* not_smsa south i.year, fe
	qui sum `e(depvar)' if e(sample)
	estadd scalar Mean= r(mean)
	estadd local  TE  "Yes"
	estadd local  FE  "Yes"
}

esttab using "/Users/kevin2/Desktop/tables/table_12.tex", replace   ///
b(3) se(3) keep(union age* ttl_exp* tenure* not_smsa south) ///
star(* 0.10 ** 0.05 *** 0.01) ///
booktabs nomtitle nonote noobs compress label ///
mgroups("Ln(Wages)" "Hours worked" "Weeks unemployed", pattern(1 0 1 0 1 0) /// 
prefix(\multicolumn{@span}{c}{) suffix(}) span erepeat(\cmidrule(lr){@span})) ///
scalars("TE Year fixed effects" "FE Individual fixed effects" "Mean \vspace{0.01cm} \\ Dep. Var. Mean" "N Observations") sfmt(0 0 2 %8.0fc)


********************************************************************************
																				
				/*Word Regression Tables*/
				
********************************************************************************

webuse nlswork, clear
xtset idcode year
gen age2      = age^2
gen ttl_exp2  = ttl_exp^2
gen tenure2   = tenure^2
gen black     = (race==2)
lab var age      "Age"
lab var age2     "Age sq."
lab var ttl_exp  "Work experience"
lab var ttl_exp2 "Work experience sq."
lab var tenure   "Job tenure"
lab var tenure2  "Job tenure eq."
lab var not_smsa "Rural"
lab var south    "South"
lab var union    "Union"

est clear
eststo: xtreg ln_w union
eststo: xtreg ln_w union age* ttl_exp* tenure*
eststo: xtreg ln_w union age* ttl_exp* tenure* not_smsa south

/*Table 9*/
esttab using "/Users/kevin2/Desktop/tables/tables.rtf", append ///
b(3) se(3) star(* 0.10 ** 0.05 *** 0.01) ///
title("Table 9: Regression Table - Basic") /// 
nomtitle nonote compress label ///
addnotes("{\i Notes:} This is an example of a regression table where we estimate three different specifications adding controls as we go." "{\i *p<0.10, **p<0.05, ***p<0.01}")

/*Table 10*/
est clear
eststo: xtreg ln_w union
qui sum `e(depvar)' if e(sample)
estadd scalar Mean= r(mean)
estadd local  TE  "No"
estadd local  FE  "No"

eststo: xtreg ln_w union age* ttl_exp* tenure*
estadd local  TE  "No"
estadd local  FE  "No"

eststo: xtreg ln_w union age* ttl_exp* tenure* not_smsa south
estadd local  TE  "No"
estadd local  FE  "No"

eststo: xtreg ln_w union age* ttl_exp* tenure* not_smsa south i.year
estadd local  TE  "Yes"
estadd local  FE  "No"

eststo: xtreg ln_w union age* ttl_exp* tenure* not_smsa south i.year, fe
estadd local  TE  "Yes"
estadd local  FE  "Yes"

esttab using "/Users/kevin2/Desktop/tables/tables.rtf", append   ///
b(3) se(3) keep(union age* ttl_exp* tenure* not_smsa south) ///
star(* 0.10 ** 0.05 *** 0.01) ///
title("Table 10: Regression Table - FE") /// 
nomtitle nonote compress label ///
scalars("TE Year fixed effects" "FE Individual fixed effects") ///
addnotes("{\i Notes:} This is an example of a regression table where we estimate three different specifications adding controls as we go." "{\i *p<0.10, **p<0.05, ***p<0.01}")

/*Table 11*/
est clear
eststo: xtreg ln_w union
qui sum `e(depvar)' if e(sample)
estadd scalar Mean= r(mean)
estadd local  TE  "No"
estadd local  FE  "No"

eststo: xtreg ln_w union age* ttl_exp* tenure*
qui sum `e(depvar)' if e(sample)
estadd scalar Mean= r(mean)
estadd local  TE  "No"
estadd local  FE  "No"

eststo: xtreg ln_w union age* ttl_exp* tenure* not_smsa south
qui sum `e(depvar)' if e(sample)
estadd scalar Mean= r(mean)
estadd local  TE  "No"
estadd local  FE  "No"

eststo: xtreg ln_w union age* ttl_exp* tenure* not_smsa south i.year
qui sum `e(depvar)' if e(sample)
estadd scalar Mean= r(mean)
estadd local  TE  "Yes"
estadd local  FE  "No"

eststo: xtreg ln_w union age* ttl_exp* tenure* not_smsa south i.year, fe
qui sum `e(depvar)' if e(sample)
estadd scalar Mean= r(mean)
estadd local  TE  "Yes"
estadd local  FE  "Yes"

esttab using "/Users/kevin2/Desktop/tables/tables.rtf", append ///
b(3) se(3) keep(union age* ttl_exp* tenure* not_smsa south) ///
star(* 0.10 ** 0.05 *** 0.01) ///
title("Table 11: Regression Table - FE") /// 
nomtitle nonote noobs compress label ///
scalars("TE Year fixed effects" "FE Individual fixed effects \line" "Mean Dep. Var. Mean" "N Observations") sfmt(0 0 2 %8.0fc) ///
addnotes("{\i Notes:} This is an example of a regression table where we estimate three different specifications adding controls as we go." "{\i *p<0.10, **p<0.05, ***p<0.01}")

/*Table 12*/
est clear
foreach i of varlist ln_wage hours wks_ue {
	eststo: xtreg `i' union age* ttl_exp* tenure* not_smsa south
	qui sum `e(depvar)' if e(sample)
	estadd scalar Mean= r(mean)
	estadd local  TE  "No"
	estadd local  FE  "No"
	
	eststo: xtreg `i' union age* ttl_exp* tenure* not_smsa south i.year, fe
	qui sum `e(depvar)' if e(sample)
	estadd scalar Mean= r(mean)
	estadd local  TE  "Yes"
	estadd local  FE  "Yes"
}

esttab using "/Users/kevin2/Desktop/tables/tables.rtf", append ///
b(3) se(3) keep(union age* ttl_exp* tenure* not_smsa south) ///
star(* 0.10 ** 0.05 *** 0.01) ///
title("Table 12: Regression Table - Grouped") /// 
nomtitle nonote noobs compress label ///
mgroups("Ln(Wages)" "Hours worked" "Weeks unemployed", pattern(1 0 1 0 1 0)) /// 
scalars("TE Year fixed effects" "FE Individual fixed effects \line" "Mean Dep. Var. Mean" "N Observations") sfmt(0 0 2 %8.0fc) ///
addnotes("{\i Notes:} This is an example of a regression table where we estimate three different specifications adding controls as we go." "{\i *p<0.10, **p<0.05, ***p<0.01}")
