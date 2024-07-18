cd "C:\Users\edoar\OneDrive\Desktop\Teoria_e_pratica_dei_modelli_statistici\Homeworks\Homework_8"

use wvs_us3, clear

tab gapov

*Exercise 1.a

ologit gapov age male college religious
estimate store m1
scalar LL = e(ll)
display LL

//the model has 6 parameters (4 + 2 cutpoints) with a log-likelihood of -1470.893   


*Exercise 1.b
//odds ratio model
ologit gapov age male college religious, or

//the coefficient for religious people is exp(beta)=1.712539, indicating that a religious individual compared to a non-religious one (ceteris paribus) has 1.7 times the odds of exceding a certain category. 

test religious  //the coefficient is stronlgy significant

*Exercise 1.c
margins, predict(outcome(3)) dydx(religious) cformat(%5.3f)

*Exercise 1.d
ologit, coeflegend
matrix list e(b)

* compute P(Y=3|age = 50, male = 1, college = 1, religious = 1)
	
scalar cum2=1/(1+exp(-( _b[/cut2]-50*_b[age]-_b[male]-_b[college]-_b[religious])))	// P(Y<=2) 

	display %6.2f cum2 

scalar p3 = 1 - cum2	// P(Y=3) 

	display %6.2f p3

margins, predict(outcome(3)) at(age = 50 male = 1 college = 1 religious = 1) cformat(%5.3f)  //check

*Exercise 1.e

brant, detail

//the assumption of proportional odds is severely violated especially due to the covariates of male (0.563) and religious (0.970). As we should expect since:

tabulate gapov male, row
tabulate gapov religious, row

*Exercise 2.a
ologit gapov age male college religious 
estimates store cumlogit
matrix logit_betas = e(b)
matrix list logit_betas

oprobit gapov age male college religious 
estimates store cumprobit
matrix probit_betas = e(b)  

//they have approximately the same log-likelihood of -1470

estimates table cumlogit cumprobit, b(%9.4f) //not very similar as expected

scalar adj_factor = sqrt(3)/_pi //variance of the logistic distribution
matrix adj_logit = adj_factor*logit_betas 
matrix list adj_logit
estimates store adj_logit

matrix comparison_table = (adj_logit \ probit_betas)

matrix rownames comparison_table = "Adj Logit" "Probit"
matrix colnames comparison_table = beta1 beta2 beta3 beta4 cut1 cut2

matrix list comparison_table //way more similar after adjusting the logit coefficients by scaling the distribution at sigma = 1

*Exercise 2.b

//compute P(Y=i|age = 50, male = 0, college = 1, religious = 1) - logit
estimates restore cumlogit
margins, predict(outcome(1)) at(age = 50 male = 0 college = 1 religious = 1) cformat(%5.3f) 
margins, predict(outcome(2)) at(age = 50 male = 0 college = 1 religious = 1) cformat(%5.3f) 
margins, predict(outcome(3)) at(age = 50 male = 0 college = 1 religious = 1) cformat(%5.3f) 

//compute P(Y=i|age = 50, male = 0, college = 1, religious = 1) - probit
estimates restore cumprobit
margins, predict(outcome(1)) at(age = 50 male = 0 college = 1 religious = 1) cformat(%5.3f) 
margins, predict(outcome(2)) at(age = 50 male = 0 college = 1 religious = 1) cformat(%5.3f) 
margins, predict(outcome(3)) at(age = 50 male = 0 college = 1 religious = 1) cformat(%5.3f) 
//the estimated probabilities are almost identical as expected

//marginals effect of age - logit
estimates restore cumlogit
margins, dydx(age) at(age = 50 college = 1 religious = 1) cformat(%5.3f) 

//marginals effect of age - probit
estimates restore cumprobit
margins, dydx(age) at(age = 50 college = 1 religious = 1) cformat(%5.3f) 

//While the logit regression coefficients are larger since the variance of the logistic distribution is larger than 1, the probabilities(and consequently the marginal effects) are in practice very similar since the larger coefficients are counterbalanced by the smaller pdf of the logistic distribution. As we can see, this applies also to our case were both probabilities and marginal effects are almost identical. 

*Exercise 3.a
estimates restore cumlogit
predict logit_prob, pr

estimates restore cumprobit
predict probit_prob, pr

graph twoway (scatter probit_prob logit_prob), ///
    xtitle("Predicted Probabilities (Logit)") ytitle("Predicted Probabilities (Probit)") ///
    title("Comparison of Logit and Probit Predicted Probabilities")

//as we can see the line is a bisect indicating equal predicted probabilities between the two models

*Exercise 3.a
gencrm gapov age male college religious, link(logit)
estimates store cumseq

*Exercise 3.b

//Compute P(Y=y|age = 50, male = 1, college = 1, religious = 1, Y >= y)
//since P(Y=y) = F(tau - xTb) prod(1 - F(tau - xTb))
scalar condpr_seq1 = exp(_b[/tau1] - 50*_b[age] - _b[male] - _b[college] - _b[religious])/(1 + exp(_b[/tau1] - 50*_b[age] - _b[male] - _b[college] - _b[religious]))
scalar condpr_seq2 = exp(_b[/tau2] - 50*_b[age] - _b[male] - _b[college] - _b[religious])/(1 + exp(_b[/tau1] - 50*_b[age] - _b[male] - _b[college] - _b[religious]))
scalar condpr_seq3 = 1

//P(Y=1|age = 50, male = 1, college = 1, religious = 1)
display condpr_seq1  //since P(Y = s|Y >= s) with s < 1 is zero 

//P(Y=2|age = 50, male = 1, college = 1, religious = 1)
display condpr_seq2 * (1 - condpr_seq1)

//P(Y=3|age = 50, male = 1, college = 1, religious = 1)
display 1 - condpr_seq1 - (condpr_seq2 * (1 - condpr_seq1))
display condpr_seq3 * (1 - condpr_seq1) * (1 - condpr_seq2) //alternatively

display condpr_seq1 + condpr_seq2 * (1 - condpr_seq1) + 1 - condpr_seq1 - (condpr_seq2 * (1 - condpr_seq1)) //check

*Exercise 3.c

gencrm gapov age male college religious, link(logit) free(religious)
estimates store cumseq_rfree
scalar LL_cumseq_rfree = e(ll)

estimates restore cumseq
scalar LL_cumseq = e(ll)
scalar LR_stat = -2 * (LL_cumseq - LL_cumseq_rfree)

scalar alpha = 0.05
scalar chisq_95 = invchi2(2,1 - alpha)
display LR_stat " " chisq_95 //no need for category specific effect
display LR_stat < chisq_95 //check, 1 as true

scalar p_value = 1 - chi2(2,LR_stat)
display p_value
