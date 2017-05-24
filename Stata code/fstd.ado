
********************** Standardization estimator ********************************
* [save from here to next 'end' to file fstd.ado]


* Stata program to obtain probability in Region 1 with distribution    
* of a set of factors equalized to that of Region 0 via standardization  	 
* using logistic outcome model

** ARGUMENTS **

** outcome: name of binary (0/1) outcome indicator variable in data
** region:  name of binary region (0/1) indicator variable in data
**          (Region 0 is assumed to be the reference)
** factors: a string containing a varlist with names of 
**          the measured risk factor variables X1,...,XK;
**	    these may include continuous variables and
**	    categorical variables, the latter coded directly 
**	    as 0/1 binary or to be generated using i.varname


program fstd, rclass
  version 12.0
  args outcome region factors
  
*Fit logistic outcome model
  quietly logit `outcome' i.`region' `factors'
  
*Obtain dataset with factors having distribution in Region 0 
*and predict at Region 1
  preserve
  keep if `region'==0
  quietly margins, noesample  at(`region'=(1)) 
  *Obtain estimator
  mat mat1=r(b)
  restore
  return scalar phat=mat1[1,1] 
end 
**************************** end of fstd.ado **************************************
