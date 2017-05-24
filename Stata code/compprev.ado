
******************************* compprev command: Compare prevalence in two populations ****************

* [save from here to next 'end' to file compprev.ado]

**** Stata program to derive all the proposed measures via one of     ****
**** three estimation methods. 	                                      ****

** ARGUMENTS **

** method:  string indicating estimation method. Possible values are "std" (standardization), 
**         "ipw" (inverse propbability weighting - IPW) and "dr" (doubly rosbust IPW)
** outcome: name of binary (0/1) outcome indicator variable in data
** region:  name of binary region (0/1) indicator variable in data
**          (Region 0 is assumed to be the reference)
** factors: a string containing a varlist with names of 
**          the measured risk factor variables X1,...,XK;
**	    these may include continuous variables and
**	    categorical variables, the latter coded directly 
**	    as 0/1 binary or to be generated using i.varname


program compprev, eclass
  version 12.0
  args method outcome region factors
 
* Recover estimation function corresponding to the selected method

  local fEstim = "f"+"`method'"

* Recover total number of factors

  local nv : word count `factors'
  
* Obtain estimates of q, pUX1...XK, pU, pUXk for k=1,...,K where (K=number of measured risk factors):

  quietly prtest Y, by(R)
  scalar q=r(P_1)
  scalar pUX1toXK=r(P_2)
  `fEstim' `outcome' `region' "`factors'"
  scalar pU=r(phat) 

  foreach X of local factors{
  local Xk `X'
  local Ck: list factors - Xk
 `fEstim' `outcome' `region' "`Ck'"
  mat pUX = (nullmat(pUX),r(phat))
  }
  
* Obtain proposed measures:
  
** Measures of overall impact (as in Table 1 of main text) **
  
* Crude prevalence difference
  scalar bUX1toXK=pUX1toXK-q

* Absolute and relative changes in Region 1 prevalence due to all factors
  scalar Delta=pUX1toXK-pU
  scalar Delta_perc=100*(Delta/pU)

* Remaining prevalence difference
  scalar bU=pU-q 

* % difference unexplained,
  scalar Omega_perc=100*bU/bUX1toXK

* % Net impact on difference
  scalar Gamma_perc=100*(Delta/bU)
  
** Measures of impact by factor (as in Table 2 of main text) **
    
  forvalues k=1/`nv'{
    scalar pUXk=pUX[1,`k']
*Absolute and relative changes in Region 1 prevalence due to risk factor Xk
    scalar DeltaX=pUXk-pU
    scalar DeltaX_perc=100*(DeltaX/pU)
* Prevalence difference resulting from changes in composition in risk factor Xk
    scalar bUX=pUXk-q
* % impact of Xk on difference 
    scalar GammaX_perc=100*(DeltaX/bU)
    mat MeasuresByFactor=(nullmat(MeasuresByFactor), pUXk,DeltaX,DeltaX_perc,bUX,GammaX_perc)
  }
   
* Return estimated quantities

 mat results=(q,pUX1toXK,bUX1toXK,pU,Delta,Delta_perc,bU,Omega_perc,Gamma_perc, MeasuresByFactor)
 local rnames q pUX1toXK bUX1toXK pU Delta Delta_perc bU Omega_perc Gamma_perc
  
 forvalues k=1/`nv'{
 local n1="pUX"+"`k'"
 local n2="DeltaX"+"`k'"
 local n3="DeltaX_perc"+"`k'"
 local n4="bUX"+"`k'"
 local n5="GammaX_perc"+"`k'"
 local rnames `rnames' `n1' `n2' `n3' `n4' `n5'
 }             
 matrix colnames results = `rnames'
 matrix drop MeasuresByFactor pUX
 ereturn post results
end
  
*********************************************** end of compprev.ado****************************************


