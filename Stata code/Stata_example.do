**************************************** EXAMPLE WITH SIMULATED DATA ********************************************

* N.B. The ado files fstd.ado, fipw.ado, fdr.ado and compprob.ado need to be placed in 
* current working directory or in the ado-file path (adopath)

* 1. Simulate data with two measured risk factors X1 and X2 and one unmeasured factor U
drop _all
set obs 10000
gen X1 = rbinomial(1,0.5)
gen X2 = rbinomial(1,0.5)
gen U = rbinomial(1,0.5)
gen R = rbinomial(1,invlogit(-0.02-2*X1-0.2*X2-3*U))
gen Y = rbinomial(1,invlogit(-3.5+1*X1+1*X2+2*U))

* 2. Assess distribution of factors in each region
tab R X1, row
tab R X2, row
    *Note: With above parameters, differences in the composition are large for X1, but not for X2.
    *      And both factors have the same effect on the outcome.

* 3. Example use of the three estimation commands: fstd, fipw and fdr
*    Obtain fully-adjusted probability p_U with each of the three estimators
fstd Y R "X1 X2"
di r(phat)

fipw Y R "X1 X2"
di r(phat)

fdr Y R "X1 X2"
di r(phat)

	
* 4. Example use of the compprob command: 
*   Obtain all proposed measures, standard errors and both normal-based and percentile confidence intervals with bootstrap
*   (example for standardization - change method argument to "ipw" or "dr" for other methods)

bootstrap _b, reps(100): compprob "std" Y R "X1 X2"
  * preferably do more, say 1000 replications  i.e. change code to reps(1000)
estat boot, percentile
