*Ado mytobit
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
