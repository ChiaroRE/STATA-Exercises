*cd "C:\

use caschool

label var observat "ID observation"
label var dist_cod "District code"
label var county "Name of the county"
label var district "Name of the school district"
label var gr_span "Grade span of district"
label var enrl_tot "Total enrolments"
label var teachers "Number of teachers"
label var calw_pct "Percent qualyfing for CalWorks (Assistance)"
label var meal_pct "Percent qualyfing for reduced-price lunch"
label var computer "Number of computers"
label var testscr "Average test score (math + reading)"
label var comp_stu "Computers per student"
label var expn_stu "Expenditure per student (in Dollars)"
label var str "Student teacher ratio"
label var avginc "District avaerage income"
label var el_pct "Percentage of english learners"
label var read_scr "Average reading score"
label var math_scr "Average math score"

describe

drop observat read_scr math_scr county district 

describe

*dummy covariates
gen gr_span_dum = .
replace gr_span_dum = 1 if gr_span == "KK-08"
replace gr_span_dum = 0 if gr_span == "KK-06"


reg testscr avginc gr_span_dum enrl_tot teacher calw_pct meal_pct computer comp_stu	expn_stu str el_pct
estat vif

*remove teachers since its student teacher ratio is a function of it. Same for computer. Enrollments seems highly irrelevant and not influence any other. 

reg testscr avginc gr_span_dum calw_pct meal_pct comp_stu expn_stu str el_pct
estat vif  //better vif, acceptable


*last time we selected 
orthpoly avginc, generate (avginc1 avginc2 avginc3 avginc4 avginc5) degree(5)
reg testscr meal_pct el_pct avginc1 gr_span_dum avginc3 expn_stu comp_stu

*noticed possible non linear relationship (as expected from diminishing returns)
graph twoway scatter testscr comp_stu|| lfit testscr comp_stu
orthpoly comp_stu, generate (c1 c2 c3) degree(3)

*from plotting the standardized residuals versus the income I noticed trace of undetected nonlinear relationship

*we try to add 
vselect testscr gr_span_dum enrl_tot teachers calw_pct meal_pct computer comp_stu expn_stu str avginc1 avginc2 avginc3 avginc4 avginc5 el_pct c1 c2 c3 ///
	, best nmodels(1)
	
*choose model 9
reg testscr meal_pct el_pct avginc1 gr_span_dum avginc3 c3 avginc5 expn_stu calw_pct
estat vif
estat ic
cv_kfold, k(5) reps(1000)
cv_kfold, k(10) reps(1000)
cv_kfold, k(20) reps(1000)
cv_regress

*performs better than the last one and the others proposed by best method in terms of estimated SPSE

*Q-Q plot 
predict resstand, rstandard
qnorm resstand , xsize(4.5) xtitle("quantiles of normal distribution", size(medium)) ///
	ytitle("quantiles of residuals", size(medium)) xlab(-2(1)2, labsize(medium)) ///
	ylab(, labsize(medium)) msize(medlarge) mcolor(gs7) msymbol(oh) rlopts(lcolor(black) lwidth(medthick))
	
*studentized residuals
predict rstudent, rstudent
predict scorehat

* compute quantiles 0.005 and 0.995 of the t-distribution with 410 degrees of freedom
gen t995 = invttail(420-9-1, 0.01/2)
gen t005 = -t995

* Plot of studentized residuals versus estimated average score (with t confidence limits)
graph twoway scatter rstudent t995 t005 scorehat if rstudent >= t005 & rstudent <= t995, ///
    msymbol(oh i i) mcolor(blue) msize(medlarge) connect(i l l) clpattern(i l l) lwidth(medthick) ///
    || scatter rstudent t995 t005 scorehat if !(rstudent >= t005 & rstudent <= t995), ///
    msymbol(oh i i) mcolor(red) msize(medlarge) connect(i l l) clpattern(i l l) lwidth(medthick) ///
    , yline(0) xsize(4.5) legend(off) ///
	xtitle("estimated average score", size(medium)) ytitle("studentized residuals", size(medium)) ///
	subtitle("studentized residuals versus estimated average score", size(medium)) ///
	xlab(, labsize(medium)) ylab(, labsize(medium)) 

* Plot of studentized residuals versus income (with t confidence limits)
graph twoway scatter rstudent t995 t005  avginc , yline(0) legend(off) xsize(4.5) xlabel(, ///
	labsize(medium)) msymbol(oh i i) msize(medlarge) mcolor(gs7) ///
	xtitle("average income in 1000$", size(medium)) connect(i l l) clpattern(i l l) ///
	lwidth(. medthick medthick) ytitle("studentized residuals", size(medium)) ///
	subtitle("studentized residuals versus average income", size(medium)) ylab(, labsize(medium)) sort

* Plot of studentized residuals versus comp_stu (with t confidence limits)
graph twoway scatter rstudent t995 t005 comp_stu , yline(0) xlabel(, labsize(medium)) xsize(4.5) ///
	msymbol(oh i i) msize(medlarge) mcolor(gs7) connect(i l l) clpattern(i l l) lwidth(. medthick medthick) ///
	xtitle("computers per student", size(medium)) ytitle("studentized residuals", size(medium)) ///
	subtitle("studentized residuals versus computers per student", size(medium)) legend(off) ylab(, labsize(medium)) sort

* Plot of studentized residuals versus meal_pct (with t confidence limits)
graph twoway scatter rstudent t995 t005 meal_pct, yline(0) xsize(4.5) msymbol(oh i i) msize(medlarge) mcolor(gs7) ///
	connect(i l l) clpattern(i l l) lwidth(. medthick medthick) ///
	xtitle("percentage of people with meal plans", size(medium)) ytitle("studentized residuals", size(medium)) ///
	ylab(, labsize(medium)) subtitle("studentized residuals versus % of people with meal plans", size(medium)) ///
	xlabel(, labsize(medium)) legend(off) sort
	
* Plot of studentized residuals versus exp_stu (with t confidence limits)
graph twoway scatter rstudent t995 t005 expn_stu , yline(0) xlabel(, labsize(medium)) xsize(4.5) ///
	msymbol(oh i i) msize(medlarge) mcolor(gs7) connect(i l l) clpattern(i l l) lwidth(. medthick medthick) ///
	xtitle("expenditure per student", size(medium)) ytitle("studentized residuals", size(medium)) ///
	subtitle("studentized residuals versus expenditure per student", size(medium)) legend(off) ylab(, labsize(medium)) sort

* List outliers (based on studentized residuals)
list rstudent testscr meal_pct el_pct avginc1 gr_span_dum avginc3 c3 avginc5 expn_stu calw_pct if (rstudent > t995 | rstudent < t005)

* compute leverage
predict lev , leverage
* list observations with leverages larger than 2p/n
scalar a = 2*9/420
display a
list lev testscr meal_pct el_pct avginc1 gr_span_dum avginc3 c3 avginc5 expn_stu calw_pct  if lev > a


* compute Cook's distance
predict cook , cooksd
* The threshold is 0.5
list cook testscr meal_pct el_pct avginc1 gr_span_dum avginc3 c3 avginc5 calw_pct expn_stu if cook > 0.5	
gsort -cook
drop in 1/10

reg testscr meal_pct el_pct avginc1 gr_span_dum avginc3 c3 avginc5 calw_pct expn_stu
estat ic
estat vif
cv_kfold, k(5) reps(1000)
cv_kfold, k(10) reps(1000)
cv_kfold, k(20) reps(1000)
cv_regress



