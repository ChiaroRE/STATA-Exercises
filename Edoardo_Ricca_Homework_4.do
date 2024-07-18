*Correlated Covariates
set obs 150
set seed 2000

* Generate variables x1, x3, and u
gen x_1 = runiform()
gen x_3 = runiform()
gen u = runiform()
gen x_2 = x_1 + u
corr(x_1 x_2)

* Generate response variable y a
gen y = rnormal(-1 + 0.3*x_1 + 0.2*x_3 , 0.2)

regress y x_1 x_2 x_3

regress y x_1 x_3

drop y x_1 x_2 x_3 u 



*Application: model selection
set scheme s1mono

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

drop observat dist_cod county district read_scr math_scr 

describe

*scatter plots
graph twoway scatter testscr str , xsize(4.5) msymbol(oh) msize(medlarge) mcolor(gs7) ///
	subtitle("Scatter plot: Test score vs Student teacher ratio", size(medium)) ///
	xtitle("Student teacher ratio", size(medium)) ytitle("Test scores", size(medium))
	
graph twoway scatter testscr comp_stu , xsize(4.5) msymbol(oh) msize(medlarge) mcolor(gs7) ///
	subtitle("Scatter plot: Test score vs computer per students", size(medium)) ///
	xtitle("Computer per students", size(medium)) ytitle("Test scores", size(medium))
	
graph twoway scatter testscr avginc , xsize(4.5) msymbol(oh) msize(medlarge) mcolor(blue) ///
	subtitle("Scatter plot: Test score vs Average district income", size(medium)) ///
	xtitle("Average district income in $", size(medium)) ytitle("Test scores", size(medium))
	
*strong evidence of non-linear relationship for average income
	
graph twoway scatter testscr expn_stu , xsize(4.5) msymbol(oh) msize(medlarge) mcolor(gs7) ///
	subtitle("Scatter plot: Test score vs Expenditure per student", size(medium)) ///
	xtitle("Expenditure per student", size(medium)) ytitle("Test scores", size(medium))
	
graph twoway scatter testscr el_pct , xsize(4.5) msymbol(oh) msize(medlarge) mcolor(gs7) ///
	subtitle("Scatter plot: Test score vs Percentage of english learners", size(medium)) ///
	xtitle("Percentage of english learners", size(medium)) ytitle("Test scores", size(medium))
	
we creat
*dummy covariates
gen gr_span_dum = .
replace gr_span_dum = 1 if gr_span == "KK-08"
replace gr_span_dum = 0 if gr_span == "KK-06"
gen log_inc = log(avginc)

* backward selection with AIC
vselect testscr gr_span_dum enrl_tot teachers calw_pct meal_pct computer comp_stu expn_stu str avginc el_pct ///
	, backward aic
estimate store M1
	
* forward selection with AIC
vselect testscr gr_span_dum enrl_tot teachers calw_pct meal_pct computer comp_stu expn_stu str avginc el_pct ///
	, forward aic
estimate store M2

* backward selection with BIC
vselect testscr gr_span_dum enrl_tot teachers calw_pct meal_pct computer comp_stu expn_stu str avginc el_pct ///
	, backward bic
estimate store M3
	
* forward selection with BIC
vselect testscr gr_span_dum enrl_tot teachers calw_pct meal_pct computer comp_stu expn_stu str avginc el_pct ///
	, forward bic
estimate store M4

estimates stats M*
	
*best model
vselect testscr gr_span_dum enrl_tot teachers calw_pct meal_pct computer comp_stu expn_stu str avginc el_pct ///
	, best nmodels(1)
	
*Considering non linearity of avginc
orthpoly avginc, generate (avginc1 avginc2 avginc3) degree(3)

* backward selection with AIC
vselect testscr gr_span_dum enrl_tot teachers calw_pct meal_pct computer comp_stu expn_stu str avginc1 avginc2 avginc3 el_pct ///
	, backward aic
estimate store M5
	
* forward selection with AIC
vselect testscr gr_span_dum enrl_tot teachers calw_pct meal_pct computer comp_stu expn_stu str avginc1 avginc2 avginc3 el_pct ///
	, forward aic
estimate store M6

* backward selection with BIC
vselect testscr gr_span_dum enrl_tot teachers calw_pct meal_pct computer comp_stu expn_stu str avginc1 avginc2 avginc3 el_pct ///
	, backward bic
estimate store M7
	
* forward selection with BIC
vselect testscr gr_span_dum enrl_tot teachers calw_pct meal_pct computer comp_stu expn_stu str avginc1 avginc2 avginc3 el_pct ///
	, forward bic
estimate store M8

estimates stats M*
	
*best model
vselect testscr gr_span_dum enrl_tot teachers calw_pct meal_pct computer comp_stu expn_stu str avginc1 avginc2 avginc3 el_pct ///
	, best nmodels(1)
	
*verify vif and others
reg  testscr meal_pct el_pct avginc1 gr_span_dum avginc3 calw_pct avginc2 expn_stu comp_stu
estat vif
corr(meal_pct avginc1 gr_span_dum el_pct comp_stu expn_stu)
estimate store M9
estimates stats M*

* plotting the estimates with confidence intervals
coefplot, drop(_cons) xline(0) mlabel format(%9.2f) mlabposition(12) mlabgap(*2)

* Save the estimated coefficients
matrix beta = e(b)
matrix list beta

* Calculate partial residuals and the estimated effects
predict res, residual
gen inceffect = beta[1,4]*avginc1 + beta[1,5]*avginc3
gen partres_avginc = res + inceffect


* Plot of the effect of average income with partial residuals 
graph twoway scatter inceffect partres_avginc avginc , msymbol(i oh) msize(. medlarge) mcolor(. gs7) xsize(4.5) ///
	xtitle("Average income in $", size(medium)) ytitle("effect / partial residuals", size(medium)) ///
	legend(off) subtitle("Effect of average income including partial residuals", size(medium)) ///
	connect(l i) clpattern(l i) lwidth(medthick) xlab(, labsize(medium)) ylab(, labsize(medium)) sort

*cross validation

*best models non linear
*9 parameters
reg  testscr meal_pct el_pct avginc1 gr_span_dum avginc3 calw_pct avginc2 expn_stu comp_stu
cv_kfold, k(5) reps(1000)
cv_kfold, k(10) reps(1000)
cv_kfold, k(20) reps(1000)
cv_regress, 

*8 parameters
reg testscr meal_pct el_pct avginc1 gr_span_dum avginc3 calw_pct expn_stu comp_stu
cv_kfold, k(5) reps(1000)
cv_kfold, k(10) reps(1000)
cv_kfold, k(20) reps(1000)
cv_regress

*7 parameters
reg testscr meal_pct el_pct avginc1 gr_span_dum avginc3 expn_stu comp_stu
cv_kfold, k(5) reps(1000)
cv_kfold, k(10) reps(1000)
cv_kfold, k(20) reps(1000)
cv_regress

*6 parameters 
reg testscr meal_pct el_pct avginc1 gr_span_dum avginc3 comp_stu
cv_kfold, k(5) reps(1000)
cv_kfold, k(10) reps(1000)
cv_kfold, k(20) reps(1000)
cv_regress

*5 parameters 
reg testscr meal_pct el_pct avginc1 gr_span_dum avginc3 
cv_kfold, k(5) reps(1000)
cv_kfold, k(10) reps(1000)
cv_kfold, k(20) reps(1000)
cv_regress



*best model linear
reg testscr meal_pct el_pct avginc gr_span_dum expn_stu comp_stu calw_pct
cv_kfold, k(5) reps(1000)
cv_kfold, k(10) reps(1000)
cv_kfold, k(20) reps(1000)
cv_regress

*chosen linear 
reg testscr meal_pct el_pct avginc gr_span_dum expn_stu comp_stu 
cv_kfold, k(5) reps(1000)
cv_kfold, k(10) reps(1000)
cv_kfold, k(20) reps(1000)
cv_regress


reg testscr log_inc
estat ic

reg testscr avginc
estat ic