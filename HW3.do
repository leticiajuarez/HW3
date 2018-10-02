clear all
cd "/Users/leticiajuarez/Desktop/621 - Field Class Labor/Homeworks/HW3"
set more off

*Get Data
use MROZ.dta

label variable kidslt6 "number kids < 6 years"
label variable kidsge6 "number kids 6-18"
g all=1 
g nl= (faminc - wage*hours - huswage*hushrs)/1000
g age2=age^2
g age3=age^3
g ed2=edu^2
g ed3=educ^3
g inter1=age*educ
g inter2=age2*educ
g inter3=age*ed2


global horder age2 age3 ed2 ed3 inter1 inter2 inter3 unem city motheduc fatheduc
global back nwifeinc kidslt6 kidsge6 age educ


****************
*   Question 1 *
****************


*--------
*   A   *
*--------
g w = (hours > 0)
probit w $back $horder

*--------
*   B   *
*--------

program myolsnorm
version 14
args lnf theta1 lnsigma
tempvar sigma
gen double `sigma' = exp(`lnsigma')
quietly replace `lnf' = ln(normalden($ML_y1, `theta1', `sigma'))
end

*OLS
ml model lf myolsnorm (beta: hours = $back $horder)(lnsigma:)
ml search
ml maximize

*Check if it matches OLS REG
reg hours $back $horder


*--------
*   C   *
*--------

program drop _all
capture program drop mytruncated

program mytruncated
version 14
args lnf theta1 lnsigma
tempvar sigma
g double `sigma' = exp(`lnsigma') 
qui replace `lnf' = ln((1/`sigma')*normalden(($ML_y1-`theta1')/`sigma'))-ln(normal(`theta1'/`sigma'))
qui replace $ML_samp=0 if $ML_y1==0
end
end

*multiply by 1/sigma because its not normal (0,1)

*Truncated Version
ml model lf mytruncated (beta: hours = $back $horder)(lnsigma:)
ml search
ml maximize

*Check if it matches OLS REG
truncreg hours $back $horder, ll(0)


*--------
*   D   *
*--------
program drop _all
capture program drop mytobit

program mytobit
version 14
args lnf beta sigma
    tempvar lnlj
    quietly {
        gen double `lnlj' = log(1-normal(`beta'/`sigma')) if $ML_y1==0 
        replace `lnlj' = log((1/`sigma')*normalden(($ML_y1-`beta')/`sigma')) if $ML_y1>0 
        replace `lnf' = `lnlj' 
    }
end
end

*Tobit
ml model lf mytobit (beta: hours = $back $horder)(lnsigma:)
ml search
ml maximize

*Check if it matches OLS REG
tobit hours $back $horder, ll(0)


*--------
*   E   *
*--------
program drop _all
capture program drop myheckman

program myheckman
	version 14
	args lnf theta1 theta2 lnsigma atanhp
	tempvar sigma
	tempvar rho
	gen double `sigma' = exp(`lnsigma')
	gen double `rho'= tanh(`atanhp')
	qui replace `lnf' = ln(normalden($ML_y1, `theta1',`sigma')) + ln(normal((`theta2'+ (`rho'/`sigma')*($ML_y1 -`theta1'))/sqrt(1-`rho'^2))) if $ML_y2==1
	qui replace `lnf' = ln(1-normal(`theta2')) if $ML_y2==0
end
end

*g s=(lwage>0)
replace lwage = 999 if lwage == .
ml model lf myheckman (beta: lwage = $horder) (s: w = $back $horder) (lnsigma:) (atanhp:)
ml search
ml maximize

*See if it coincides:
heckman lwage $horder, select(w = $back $horder)


****************
*   Question 2 *
****************

clear all
use friedberg_cps.dta
g menos65=(age<65)


g threshold_hours = threshold/hrlywage 
g budget_segment = .
replace budget_segment = 1 if belowkink == 1
replace budget_segment = 2 if atkink==1
replace budget_segment = 3 if middlesegment==1
replace budget_segment = 4 if uppersegment==1


**************TAU(from paper(2))

*Create Tau
g tau = 0
replace tau = 0.5 
*if age >= 62 & age <= 64
*replace tau = 0.33 if age >= 65 & age <= 69

* Create applicable Tau
g etau = 0 if belowkink==1
replace etau=tau if middleseg==1 |atkink==1
replace etau=0 if upperseg==1


*************Virtual Income(from paper(2))

*Non virtual income
g virtual= non_labor_inc if belowkink==1
replace virtual= non_labor_inc + threshold*tau if middleseg==1 |atkink==1
replace virtual= non_labor_inc - incss if upperseg==1
replace virtual=non_labor_inc if age>=70

************Net Wage (w*(1-etau))
g netwage = hrlywage  if belowkink==1
replace netwage= hrlywage*(1-etau) if middleseg==1 |atkink==1
replace netwage=hrlywage if upperseg==1




*Create program, hope it works this time


gen netwage1 = hrlywage 
gen netwage2 = hrlywage*(1-tau)
gen netwage3 = hrlywage

gen virtual1 = non_labor_inc
gen virtual2 = non_labor_inc + tau*threshold
gen virtual3 = non_labor_inc - incss

*Try reg in stata first
*truncreg annhours netwage virtual menos65 hsgrad nonwhite married, ll(0)
truncreg annhours netwage1 netwage2 netwage3 virtual1 virtual2 virtual3 threshold_hours budget_segment hsgrad nonwhite married menos65,ll(0)
*matrix beta = e(b)

program drop _all
capture program drop kinked1 

program kinked1
	version 14
	args lnf thetax thetaw thetay lnsigma
	tempvar theta1 theta2 theta3 
	tempvar sigma 
	gen double `sigma' = exp(`lnsigma')
	quietly gen double `theta1' = `thetax' + `thetaw'*$ML_y2 + `thetay'*$ML_y5 
	quietly gen double `theta2' = `thetax' + `thetaw'*$ML_y3 + `thetay'*$ML_y6 
	quietly gen double `theta3' = `thetax' + `thetaw'*$ML_y4 + `thetay'*$ML_y7
	qui replace `lnf' = ln(normalden(($ML_y1 - `theta1')/`sigma')/`sigma') - ln(normal(`theta1'/`sigma')) if $ML_y9==1
	qui replace `lnf' = ln((normal(($ML_y8 - `theta2')/`sigma') - normal(($ML_y8 - `theta1')/`sigma'))/`sigma') - ln(normal(`theta1'/`sigma')) if $ML_y9==2
	qui replace `lnf' = ln(normalden(($ML_y1 - `theta2')/`sigma') * normal(`theta1'/`sigma')/`sigma') - ln(normal(`theta1'/`sigma')) if $ML_y9==3
	qui replace `lnf' = ln(normalden(($ML_y1 - `theta3')/`sigma') * (1-normal(`theta3'/`sigma'))/`sigma') - ln(normal(`theta1'/`sigma')) if $ML_y9==4
end

*quietly replace `lnf' = -999999999999999999999999999 if budget_segment==2 & normal(($ML_y8 - `theta2')/`sigma')-normal(($ML_y8 - `theta1')/`sigma') <0

mat beta = (319, 44, 288, -119, 1004, 35, -0.02, 8.07)

ml model lf kinked1 (betax: annhours netwage1 netwage2 netwage3 virtual1 virtual2 virtual3 threshold_hours budget_segment = hsgrad nonwhite married menos65) (betaw:  ) (betay:  ) (lnsigma:  )
ml init beta, copy			
ml search
ml maximize



************Trying to figure out which is which - HERE THERE IS A PROBLEM ---> THINGS SHOULD COINCIDE
g probar = belowkink + middlesegment + uppersegment + atkink
tab probar,m

g MS=(threshold<earnings<threshold+incss/tau)
g BK=(earnings<threshold)
g atBK=belowkink+ atkink
tab BK belowkink 
tab BK atBK
tab MS middlesegment

test





*****************
* Question 3
*****************

clear all
use 







