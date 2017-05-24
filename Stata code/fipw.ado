
********************************** IPW estimator *********************************
* [save from here to next 'end' to file fipw.ado]

* Stata program to obtain prevalence in Region 1 with distribution    
* of a set of factors equalized to that of Region 0 via IPW  	 
* using logistic propensity score model

** ARGUMENTS **

** outcome: name of binary (0/1) outcome indicator variable in data
** region:  name of binary region (0/1) indicator variable in data
**          (Region 0 is assumed to be the reference)
** factors: a string containing a varlist with names of 
**          the measured risk factor variables X1,...,XK;
**	    these may include continuous variables and
**	    categorical variables, the latter coded directly 
**	    as 0/1 binary or to be generated using i.varname


program fipw, rclass
  version 12.0
  args outcome region factors

*Fit logistic propensity model
  quietly logit `region' `factors'
  
*Obtain predicted propensity scores  
  predict pis
  
*Obtain estimator
  gen numer=(`outcome'*`region')*(1-pis)/pis
  gen denom=`region'*(1-pis)/pis
  quietly sum numer
  scalar numerator=r(mean)
  quietly sum denom
  scalar denominator=r(mean)
  drop pis numer denom
  return scalar phat=numerator/denominator
end

*********************************** end of fipw.ado *******************************
