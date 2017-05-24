*************************** Doubly robust IPW estimator **************************
* [save from here to next 'end' to file fdr.ado]


* Stata program to obtain probability in Region 1 with distribution    
* of a set of factors equalized to that of Region 0 via doubly robust IPW  	 
* using logistic outcome and propensity score models

** ARGUMENTS **

** outcome: name of binary (0/1) outcome indicator variable in data
** region:  name of binary region (0/1) indicator variable in data
**          (Region 0 is assumed to be the reference)
** factors: a string containing a varlist with names of 
**          the measured risk factor variables X1,...,XK;
**	    these may include continuous variables and
**	    categorical variables, the latter coded directly 
**	    as 0/1 binary or to be generated using i.varname



program fdr, rclass
  version 12.0
  args outcome region factors
 
*Fit propensity score model
  quietly logit `region' `factors'
  
*Obtain predicted propensity scores  
  predict pis
  
*Fit logistic outcome model
  quietly logit `outcome' i.`region' `factors'
  
*Obtain predicted outcomes for augmented part of IPW estimator
  preserve
  gen Rtrue=`region'
  replace `region'=1
  predict yis
  replace `region'=Rtrue
  
*Obtain estimator
  gen numer=((`outcome'*`region')*(1-pis)+(pis-`region')*yis)/pis
  gen denom=(1-`region')
  quietly sum numer
  scalar numerator=r(mean)
  quietly sum denom
  scalar denominator=r(mean)
  restore
  drop pis
  return scalar phat=numerator/denominator
end
******************************** end of fdr.ado***********************************
