*import file
use cps98.dta


*assign label
label variable year "Year of survey"
label variable ahe "Average Hourly Earnings"
label variable bachelor "Possession of Bachelor's Degree"
label variable female "Sex"
label variable age "Age"

*regression of average hourly earnings (AHE) on age (Age), gender (Female), and education (Bachelor)"
est clear
reg ahe age female bachelor
ereturn list
esttab
matrix beta = e(b)
matrix list beta //save estimated coefficients
display (beta[1,1]*26 - beta[1,1]*25)
display (beta[1,1]*34 - beta[1,1]*33)
//both equal as expected to beta[1.1]

*regression of the logarithm average hourly earnings, ln(AHE), on Age, Female, and Bachelor. 
gen log_ahe = ln(ahe)
reg log_ahe age female bachelor
estimates store model1
estat ic

matrix beta = e(b)	// save the estimated coefficients
matrix list beta
display beta[1,1] *  100 //variation in percentage

gen f_age_1 = beta[1,1]*age
predict uhat, res
gen part_res_age_1 = uhat + f_age_1

graph twoway scatter part_res_age_1 f_age_1 age,  sort ///
	m(oh i) c(. l) clpattern(. l) lwidth(. medthick) mlcolor(gs7) msize(medlarge),
	subtitle("effect of age", size(medium)) ///
	xtitle("age", size(medium))   xsize(4.5) legend(off) ///
	ytitle("effect of age / partial residuals", size(medium)) xlab(25(1)26, labsize(medium)) ylab(, labsize(medium)) 

*regression of the logarithm average hourly earnings, ln(AHE), on ln(Age), Female, and Bachelor
gen log_age = ln(age)
reg log_ahe log_age female bachelor
estimates store model2
estat ic

matrix beta = e(b)	// save the estimated coefficients
matrix list beta

display (26 - 25)/25 * 100 //percentage variation in log_age
display (34 - 33)/33 * 100 
display beta[1,1] * (26 - 25)/25 * 100  //percentage variation in log_ahe
display beta[1,1] * (34 - 33)/33 * 100 

gen f_age_2 = beta[1,1]*log_age
drop uhat
predict uhat, res
gen part_res_age_2 = uhat + f_age_2

graph twoway scatter part_res_age_2 f_age_2 log_age,  sort ///
	m(oh i) c(. l) clpattern(. l) lwidth(. medthick) mlcolor(gs7) msize(medlarge),
	subtitle("effect of age", size(medium)) ///
	xtitle("age", size(medium))   xsize(4.5) legend(off) ///
	ytitle("effect of age / partial residuals", size(medium)) xlab(25(1)26, labsize(medium)) ylab(, labsize(medium)) 

*regression of the logarithm average hourly earnings, ln(AHE), on Age, Age2, Female, and Bachelor
gen age2 = age^2
reg log_ahe age age2 female bachelor
estimates store model3
estat ic

matrix beta = e(b)	// save the estimated coefficients
matrix list beta
display (beta[1,1] + beta[1,2]*(26^2 - 25^2)) * 100
display (beta[1,1] + beta[1,2]*(34^2 - 33^2)) * 100

gen f_age_3 = beta[1,1]*age + beta[1,2]*age2
drop uhat
predict uhat, res
gen part_res_age_3 = uhat + f_age_3

graph twoway scatter part_res_age_3 f_age_3 log_age,  sort ///
	m(oh i) c(. l) clpattern(. l) lwidth(. medthick) mlcolor(gs7) msize(medlarge),
	subtitle("effect of age", size(medium)) ///
	xtitle("age", size(medium))   xsize(4.5) legend(off) ///
	ytitle("effect of age / partial residuals", size(medium)) xlab(25(1)26, labsize(medium)) ylab(, labsize(medium)) 


*Model comparison
estimates table model1 model2 model3, stat(r2)
matrix beta = e(b)	// save the estimated coefficients
matrix list beta
estat ic


*Plotting model log-linear
//Males with HS diploma
regress log_ahe age female bachelor if female == 0 & bachelor == 0
predict predicted_log_ahe_1MH, xb
twoway scatter log_ahe age if female == 0 & bachelor == 0 || ///
       line predicted_log_ahe_1MH age if female == 0 & bachelor == 0, ///
       title("Males with HS diploma") ///
       xtitle("Age") ytitle("ln(ahe)") legend(off) xlab(25(1)34, labsize(medium))
//Males with Bs degree 
regress log_ahe age female bachelor if female == 0 & bachelor == 1
predict predicted_log_ahe_1MB, xb
twoway scatter log_ahe age if female == 0 & bachelor == 1 || ///
       line predicted_log_ahe_1MB age if female == 0 & bachelor == 1, ///
       title("Males with Bs Degree") ///
       xtitle("Age") ytitle("ln(ahe)") legend(off) xlab(25(1)34, labsize(medium))
	   
//Females with HS diploma
regress log_ahe age female bachelor if female == 1 & bachelor == 0
predict predicted_log_ahe_1FH, xb
twoway scatter log_ahe age if female == 1 & bachelor == 0 || ///
       line predicted_log_ahe_1FH age if female == 1 & bachelor == 0, ///
       title("Females with a HS Diploma") ///
       xtitle("Age") ytitle("ln(ahe)")
//Females with Bs degree
regress log_ahe age female bachelor if female == 1 & bachelor == 1
predict predicted_log_ahe_1FB, xb
twoway scatter log_ahe age if female == 1 & bachelor == 1 || ///
       line predicted_log_ahe_1FB age if female == 1 & bachelor == 1, ///
       title("Females with a Bs Degree") ///
       xtitle("Age") ytitle("ln(ahe)") legend(off) xlab(25(1)34, labsize(medium))
	   
*Plotting model log-log
//Males with HS dipolma
regress log_ahe log_age female bachelor if female == 0 & bachelor == 0
predict predicted_log_ahe_2MH, xb
twoway scatter log_ahe log_age if female == 0 & bachelor == 0 || ///
       lowess predicted_log_ahe_2MH log_age if female == 0 & bachelor == 0, ///
       title("Males with a HS Diploma") ///
       xtitle("Age") ytitle("ln(ahe)") legend(off)
//Males with Bs degree
regress log_ahe log_age female bachelor if female == 0 & bachelor == 1
predict predicted_log_ahe_2MB, xb
twoway scatter log_ahe log_age if female == 0 & bachelor == 1 || ///
       lowess predicted_log_ahe_2MB log_age if female == 0 & bachelor == 1, ///
       title("Males with a Bs Degree") ///
       xtitle("Age") ytitle("ln(ahe)") legend(off)
	   
//Females with HS diploma
regress log_ahe log_age female bachelor if female == 1 & bachelor == 0
predict predicted_log_ahe_2FH, xb
twoway scatter log_ahe log_age if female == 1 & bachelor == 0 || ///
       lowess predicted_log_ahe_2FH log_age if female == 1 & bachelor == 0, ///
       title("Females with a HS Diploma") ///
       xtitle("Age") ytitle("ln(ahe)")
//Females with Bs degree
regress log_ahe log_age female bachelor if female == 1 & bachelor == 1
predict predicted_log_ahe_2FB, xb
twoway scatter log_ahe log_age if female == 1 & bachelor == 1 || ///
       lowess predicted_log_ahe_2FB log_age if female == 1 & bachelor == 1, ///
	    title("Females with a Bs Degree") ///
       xtitle("Age") ytitle("ln(ahe)") legend(off)
	   
*Plotting model polynomial
//Males with HS diploma
regress log_ahe age age2 female bachelor if female == 0 & bachelor == 0
predict predicted_log_ahe_3MH, xb
twoway scatter log_ahe age if female == 0 & bachelor == 0 || ///
       lowess predicted_log_ahe_3MH age if female == 0 & bachelor == 0, ///
       title("Males with a HS Diploma") ///
       xtitle("Age") ytitle("ln(ahe)") legend(off) xlab(25(1)34)
//Males with Bs degree
regress log_ahe age age2  female bachelor if female == 0 & bachelor == 1
predict predicted_log_ahe_3MB, xb
twoway scatter log_ahe age if female == 0 & bachelor == 1 || ///
       lowess predicted_log_ahe_3MB age if female == 0 & bachelor == 1, ///
       title("Males with a Bs Degree") ///
       xtitle("Age") ytitle("ln(ahe)") legend(off) xlab(25(1)34)
	   
//Females with HS diploma
regress log_ahe age age2  female bachelor if female == 1 & bachelor == 0
predict predicted_log_ahe_3FH, xb
twoway scatter log_ahe log_age if female == 1 & bachelor == 0 || ///
       line predicted_log_ahe_3FH age if female == 1 & bachelor == 0, ///
       title("Age and variation in AHE for Males with a HS Diploma") ///
       xtitle("Age") ytitle("ln(ahe)")
//Females with Bs degree
regress log_ahe age age2  female bachelor if female == 1 & bachelor == 1
predict predicted_log_ahe_3FB, xb
twoway scatter log_ahe age if female == 1 & bachelor == 1 || ///
       lowess predicted_log_ahe_3FB age if female == 1 & bachelor == 1, ///
       title("Females with a Bs Degree") ///
       xtitle("Age") ytitle("ln(ahe)") legend(off) xlab(25(1)34)
	  
*Model with interaction term
gen int_fb = female * bachelor
reg log_ahe age age2 female bachelor int_fb
matrix beta = e(b)	
matrix list beta


gen Alexis_pred = beta[1,6] + beta[1,1]*30 + beta[1,2]*(30^2) + beta[1,3] + beta[1,4] + beta[1,5]
display Alexis_pred
gen Jane_pred = beta[1,6] + beta[1,1]*30 + beta[1,2]*(30^2) + beta[1,3] 
display Jane_pred
display exp(Alexis_pred) - exp(Jane_pred) //difference in dollars

gen Bob_pred = beta[1,6] + beta[1,1]*30 + beta[1,2]*(30^2) + beta[1,4] 
display Bob_pred
gen Jim_pred = beta[1,6] + beta[1,1]*30 + beta[1,2]*(30^2) + beta[1,3] 
display Jim_pred
display exp(Bob_pred) - exp(Jim_pred)

*Effect of age on earning by sex
gen fem_age = female*age
reg log_ahe age age2 female fem_age bachelor
matrix beta = e(b)	
matrix list beta
//Calculate the estimated effects f1(age)
gen f_age_male = beta[1,1]*age + beta[1,2]*age2
gen f_age_female = beta[1,1]*age +beta[1,2]*age2 + beta[1,3] + beta[1,4]*age
gen var_effect = beta[1,3] + beta[1,4]*age

graph twoway connect f_age_male age, sort ///
	m(none none) c(. l) clpattern(solid dash) lwidth(. medthick) mlcolor(gs7) msize(medlarge) ///
	subtitle("effect of age", size(medium)) ///
	xtitle("age", size(medium))   xsize(4.5) legend(off) ///
	ytitle("age effects", size(medium)) xlab(25(1)34, labsize(medium)) ylab(, labsize(medium))|| connect f_age_female age, sort ///
    m(none none) c(. l) clpattern(dash) lwidth(. medthick) mlcolor(gs7) msize(medlarge)

graph twoway connect var_effect age, sort ///
	m(none none) c(. l) clpattern(solid dash) lwidth(. medthick) mlcolor(gs7) msize(medlarge) ///
	subtitle("varying effect of females", size(medium)) ///
	xtitle("age", size(medium))   xsize(4.5) legend(off) ///
	ytitle("varying effect of females", size(medium)) xlab(25(1)34, labsize(medium)) ylab(, labsize(medium))

*Effect of age on earning by education
gen edu_age = bachelor*age
reg log_ahe age age2 female bachelor edu_age
matrix beta = e(b)	
matrix list beta
//Calculate the estimated effects f1(age)
gen f_age_HS = beta[1,1]*age + beta[1,2]*age2
gen f_age_BS = beta[1,1]*age +beta[1,2]*age2 + beta[1,4] + beta[1,5]*age
gen var_effect_edu = beta[1,4] + beta[1,5]*age

graph twoway connect f_age_HS age, sort ///
	m(none none) c(. l) clpattern(solid dash) lwidth(. medthick) lcolor(blue) msize(medlarge) ///
	subtitle("effect of age", size(medium)) ///
	xtitle("age", size(medium))   xsize(4.5) legend(off) ///
	ytitle("age effects", size(medium)) xlab(25(1)34, labsize(medium)) ylab(, labsize(medium))|| connect f_age_BS age, sort ///
    m(none none) c(. l) clpattern(dash) lwidth(. medthick) lcolor(green) msize(medlarge)

graph twoway connect var_effect_edu age, sort ///
	m(none none) c(. l) clpattern(solid dash) lwidth(. medthick) lcolor(green) msize(medlarge) ///
	subtitle("varying effect of bachelor's degree", size(medium)) ///
	xtitle("age", size(medium))   xsize(4.5) legend(off) ///
	ytitle("varying effect of bachelor's degree", size(medium)) xlab(25(1)34, labsize(medium)) ylab(, labsize(medium))


