*Ado myheckman
program drop _all
capture program drop myheckman

program myheckman
	version 14
	args lnf theta1 theta2 lnsigma atanhp
	tempvar sigma
	tempvar rho
	gen double `sigma' = exp(`lnsigma')
	gen double `rho'= tanh(`atanhp')
    qui replace `lnf' = ln(1-normal(`theta2')) if $ML_y2==0
	qui replace `lnf' = normalden($ML_y1, `theta1',`sigma') + ln(normal((`theta2'+ (`rho'/`sigma')*($ML_y1 -`theta1'))/sqrt(1-`rho'^2))) if $ML_y2==1
end


