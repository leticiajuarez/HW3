
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
