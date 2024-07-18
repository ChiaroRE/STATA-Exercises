
*generate a dataset with 1000 obs
set obs 1000

*generate x equally spaced between -5 and +5
gen X = -5 + 10*(_n/1000)

*generate Z as binary from a Bernoulli (0.7)
gen U = runiform()
gen Z = U <= 0.7

*generate errors and Y
gen err = rnormal(0,20)
histogram err, bin(25)

gen Y = 100 + 2*X + 10*Z + err

*plot a
twoway (scatter Y X if Z == 0, msymbol(circle) mcolor(blue)) || ///
       (scatter Y X if Z == 1, msymbol(oh) mcolor(red)), ///
	   legend(label(1 "Z = 0" )label(2 "Z = 1"))
	   
*plot b
twoway (scatter Y X if Z == 0, msymbol(circle) mcolor(blue)) (lfit Y X if Z == 0, lcolor(blue))|| ///
       (scatter Y X if Z == 1, msymbol(oh) mcolor(red))(lfit Y X if Z == 1, lcolor(red)), ///
	   legend(label(1 "Z = 0" )label(2 "Fitted for Z = 0")label(3 "Z = 1")label(4 "Fitted for Z = 1"))
	   
*Wald-test for betaX=0
reg Y X Z
matrix beta = e(b)
test X

*test for betaX = 1.5

test X = 1.5

*test for betaX = 1.9
test X = 1.9

*test for betaZ = 0
test Z 

*test for betaX = betaZ = 0
test X Z

*test for betaZ=4*betaX. 
test Z = 4*X

*point d 
regress Y X Z
matrix beta = e(b)
predict residuals, residuals
summarize residuals, detail
scalar std_dev = r(sd)


predict yhat if X == 3 & Z == 0 //no existing points

gen yhat = beta[1,3] + beta[1,1]*3

matrix x_0 = (1 , 3 , 0) 

matrix x_0beta = x_0 * beta' //prediction

*design matrix
matrix x = J(1000, 3, 1)
forval i = 1/1000 {
    matrix x[`i', 2] = X[`i']
}
forval i = 1/1000 {
    matrix x[`i', 3] = Z[`i']
}

matrix list x

matrix x_x = x'*x

matrix list x_x

matrix V = inv(x_x)

matrix A = x_0 * V * x_0'

gen B = sqrt(.0046329) //value from A

*quantile 

scalar qt = 1.962346

gen up_ci_mu = yhat + qt*std_dev *B
gen lo_ci_mu = yhat - qt*std_dev *B

*prediction interval 
gen C = sqrt(1 +.0046329)
gen up_pi = yhat + qt*std_dev *C
gen lo_pi = yhat - qt*std_dev *C


