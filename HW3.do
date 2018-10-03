clear all
cd "/Users/leticiajuarez/Desktop/621 - Field Class Labor/Homeworks/HW3"
set more off

log using HW3.log, replace


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
****** 2.b
clear all
use friedberg_cps.dta

gen agemin65=age-65
gen budget_segment=0
replace budget_segment = 1 if belowkink==1
replace budget_segment = 2 if middlesegment==1
replace budget_segment = 3 if uppersegment==1

gen threshold_hours=threshold/hrlywage

* Get Tau
gen     etau = 0.5 if inrange(age, 62, 64)
replace etau = 0.33 if inrange(age, 65, 69)
replace etau = 0 if age>69


*Get net wage and virtual wage
foreach x of numlist 1/3 {
	gen netwage`x'=hrlywage
	gen virtual_inc`x'=non_labor_inc 
}

replace netwage2=etau*netwage2 

replace virtual_inc2=non_labor_inc+etau*threshold
replace virtual_inc3=non_labor_inc-incss


*Program for kinked1

program nonlinbudget
	version 14
	args lnf thetax thetaw thetay lnsigma
	tempvar theta1 theta2 theta3 sigma
	quietly gen double `sigma' = exp(`lnsigma')
	quietly gen double `theta1' = `thetax' + `thetaw'*$ML_y2 + `thetay'*$ML_y5 
	quietly gen double `theta2' = `thetax' + `thetaw'*$ML_y3 + `thetay'*$ML_y6 
	quietly gen double `theta3' = `thetax' + `thetaw'*$ML_y4 + `thetay'*$ML_y7

	quietly replace `lnf'= log((1/`sigma')* ///
			normalden(($ML_y1-`theta1')/`sigma')) if $ML_y9==1 //bottom
			
	quietly replace `lnf'= log((1/`sigma')* ///
			normalden(($ML_y1-`theta2')/`sigma')) if $ML_y9==2 // mid
			
	quietly replace `lnf'= log((1/`sigma')* ///
			normalden(($ML_y1-`theta3')/`sigma')) if $ML_y9==3 // top
			
	quietly replace `lnf'= ///
	 log(normal(($ML_y8-`theta2')/`sigma')-normal(($ML_y8-`theta1')/`sigma')) ///
	 if (normal(($ML_y8-`theta2')/`sigma')-normal(($ML_y8-`theta1')/`sigma'))>0 ///
	 & $ML_y9==0 // kink, non negative value
	 
	// How is this making MLE avoid this? OHHHHHH are you saying if you make this term be negative it is v bad?
	 quietly replace `lnf'= -99999 ///
	 if normal(($ML_y8-`theta2')/`sigma')-normal(($ML_y8-`theta1')/`sigma')<=0 ///
	 & $ML_y9==0 // kink negative value, make ml avoid this case

	// why do we need this?
	quietly replace `lnf' = `lnf' - log(normal(`theta1'/`sigma')) //truncation 
end


*Just created some initial values

mat all= [321.3,59.3,263.62,-131.96,937.33,38.8,-.03814,7.099]

ml model lf nonlinbudget (betax: annhours netwage1 netwage2 netwage3 ///
		virtual_inc1 virtual_inc2 virtual_inc3 threshold_hours budget_segment = ///
		hsgrad nonwhite married agemin65) (betaw:) (betay:) (lnsigma:)
ml init all, copy
ml maximize


mat b =e(b)
scalar wagecoef= b[1,6]
gen wage=hrlywage
replace wage=hrlywage*.5 if budget_segment==2 |budget_segment==0
sum wage
scalar wagemean = r(mean)
sum annhours
scalar hoursmean = r(mean)

/*
Uncompensated wage elasticity is the derivative of hours w.r.t. to net wage
times the average net wage over the average hours
*/
di "Uncompensated wage elasticity:" wagecoef*wagemean/hoursmean

****** 2.c 
foreach x of numlist 1/3 {
	gen lnetwage`x'=log(netwage`x') 
}
mat all= [321.3,59.3,263.62,-131.96,937.33,log(38.8),-.03814,7.099]

ml model lf nonlinbudget (betax: annhours lnetwage1 lnetwage2 lnetwage3 ///
		virtual_inc1 virtual_inc2 virtual_inc3 threshold_hours budget_segment = ///
		hsgrad nonwhite married agemin65) (betaw:) (betay:) (lnsigma:)
ml init all, copy
ml maximize
mat b =e(b)
scalar lwagecoef= b[1,6]


/*
************Trying to figure out which is which - HERE THERE IS A PROBLEM ---> THINGS SHOULD COINCIDE
g probar = belowkink + middlesegment + uppersegment + atkink
tab probar,m

g MS=(threshold<earnings<threshold+incss/tau)
g BK=(earnings<threshold)
g atBK=belowkink+ atkink
tab BK belowkink 
tab BK atBK
tab MS middlesegment

*/
*****************
* Question 3
*****************
*The dofile contains the solution for both cases delta=1000 and delta=1500

clear all
use cps_ssa_1978.dta
g earn_minus_threshold=earn-threshold

*Histogram
histogram earn_minus_threshold if inrange(earn_minus_threshold, -2000, 2000), width(50) xline(0)


*Create Groups
*H*
local delta = 1500
*local delta= 1000
g Hstar= (earn_minus_threshold-`delta'<earn<=earn_minus_threshold+`delta')

*Hstarminus
g Hstarminus=(earn_minus_threshold -2*`delta'<earn<earn_minus_threshold -`delta')

*Hstarplus
g Hstarplus=(earn_minus_threshold +`delta'<earn<earn_minus_threshold +2*`delta')

*Create means
foreach var of varlist Hstar Hstarminus Hstarplus{
	qui sum `var'
	local `var'=r(mean)*100		
}

local B = Hstar-(Hstarminus+Hstarplus)
di `B'

local Hstarminus = Hstarminus/`delta'
local Hstarplus=Hstarplus/`delta'

di `Hstarminus'
di `Hstarplus'

*Calculate e
*Create H 
local ache=(Hstarminus + Hstarplus)/2
local e=((`B'/`ache')/(earn_minus_threshold)*(ln((1-0)/(1-0.5))))
di `e'

log close
