
program drop _all
capture program drop myprobit

program myprobit
version 14
args lnf theta1
quietly replace ‘lnf’ = ln(normal(‘theta1’)) if $MLy1==1
quietly replace ‘lnf’ = ln(normal(-‘theta1’)) if $MLy1==0
end
