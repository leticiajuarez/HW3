*Ado myolsnorm
program drop _all
capture program drop myolsnorm

program myolsnorm
version 14
args lnf theta1 lnsigma
tempvar sigma
gen double `sigma' = exp(`lnsigma')
quietly replace `lnf' = ln(normalden($ML_y1, `theta1', `sigma'))
end
