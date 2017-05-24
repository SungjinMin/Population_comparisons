####################################### FUNCTIONS ############################################

#### Functions to obtain probability in Region 1 with distribution  ####    
#### of a set of factors equalized to that of Region 0 via three   #### 
#### estimators using logistic outcome and propensity models       #### 

### ARGUMENTS ##
## data: dataframe containing the outcome, region and risk factor variables
## outcome: character vector with name of binary (0/1) outcome indicator variable in data
## region: character vector with name of binary region (0/1) indicator variable in data
##         (Region 0 is assumed to be the reference)
## fact: character vector with names of variables with the maeasured risk factors X1,...,XK
##       (with factor variables formatted as such)

# 1. Standardization ("std")
fstd<-function(data,outcome,region,fact)
{
  #Fit logistic outcome model
  fitOutcome<-glm(as.formula(paste(paste(outcome,"~",region,"+",paste(fact,collapse="+")))),
                  family=binomial(link="logit"),data=data)
  #Obtain dataset with factors having distribution in Region 0
  dat01<-data[data[,region]==0,]
  #Return estimator
  dat01[,region]<-1  
  return(mean(predict(fitOutcome,newdata=dat01,type = "response")))
}

# 2. Inverse probability weighting ("ipw")
fipw<-function(data,outcome,region,fact)
{
  #Fit logistic propensity model
  fitProp<-glm(as.formula(paste(region,"~",paste(fact,collapse="+"))),
               data=data,family=binomial(link="logit"))
  #Obtain weights
  pis<-predict(fitProp,type="response")
  #Return estimator
  return(sum(data[,outcome]*data[,region]*(1-pis)/pis)/sum(data[,region]*(1-pis)/pis))
}

# 3. Doubly robust ("dr")
fdr<-function(data,outcome,region,fact)
{
  #Fit logistic outcome model
  fitOutcome<-glm(as.formula(paste(paste(outcome,"~",region,"+",paste(fact,collapse="+")))),
                  family=binomial(link="logit"),data=data)
  #Obtain predicted outcomes for augmented part of IPW estimator
  dat1<-data
  dat1[,region]<-1
  yis<-predict(fitOutcome,newdata=dat1,type = "response")
  #Fit logistic propensity model
  fitProp<-glm(as.formula(paste(region,"~",paste(fact,collapse="+"))),
               data=data,family=binomial(link="logit"))
  #Obtain weights
  pis<-predict(fitProp,type="response")
  #Return estimator
  return(sum(((data[,outcome]*data[,region]*(1-pis))+(pis-data[,region])*yis)/pis)/sum(1-data[,region]))
}


#### Function to derive all the proposed measures via one of the    ####
#### three estimation methods above. This function is subsequently  ####
#### fed to the "boot" function to obtain standard errors and       ####
#### confidence intervals via non-parametric boostrap (see example) ####

### ARGUMENTS ##
## ind: indices of rows to consider for calculation. Defaults to all
##      (this seemingly useless argument is required by the bootsrap function for resampling)
## method: character vector indicating estimation method. Possible values are "std", "ipw" and "dr"
## All other arguments are as described for the functions above

compprob<-function(data,ind=1:nrow(data),method,outcome,region,fact)
{ 
  dat<-data[ind,]
  
  ## Recover estimation function corresponding to the selected method
  fEstim<-get(paste("f",method,sep=""))
  
  ## Obtain estimates of q, pUX1toXK, pU, pUXk for k=1,to,K where (K=number of measured risk factors)
  q<-(sum(dat[,outcome]==1&dat[,region]==0)/sum(dat[,region]==0))
  pUX1toXK<-(sum(dat[,outcome]==1&dat[,region]==1)/sum(dat[,region]==1))
  pU<-fEstim(dat,outcome,region,fact)
  pUX<-vector()
  for(k in 1:length(fact))
    pUX<-c(pUX,fEstim(dat,outcome,region,fact=setdiff(fact,fact[k])))
  
  ## Obtain proposed measures
  
  # Measures of overall impact (as in Table 1 of main text) #
  
  # Crude probability difference
  bUX1toXK<-pUX1toXK-q
  # Absolute and relative changes in Region 1 probability due to all factors
  Delta<-pUX1toXK-pU
  Delta_perc<-100*(Delta/pU)
  # Remaining probability difference
  bU<-pU-q 
  # % difference unexplained,
  Omega_perc<-100*bU/bUX1toXK
  # % Net impact on difference
  Gamma_perc<-100*(Delta/bU)
  
  # Measures of impact by factor (as in Table 2 of main text) #
  
  MeasuresByFactor<-vector()
  for(k in 1:length(fact))
  {
    # Absolute and relative changes in Region 1 probability due to risk factor Xk
    DeltaX<-pUX[k]-pU
    DeltaX_perc<-100*(DeltaX/pU)
    # probability difference resulting from changes in composition in risk factor Xk
    bUX<-pUX[k]-q
    # % impact of Xk on difference 
    GammaX_perc<-100*(DeltaX/bU)
    
    MeasuresByFactor<-c(MeasuresByFactor,pUX[k],DeltaX,DeltaX_perc,bUX,GammaX_perc)
  }
  
  results<-c(q,pUX1toXK,bUX1toXK,pU,Delta,Delta_perc,bU,Omega_perc,Gamma_perc, MeasuresByFactor)
  names(results)<-c("q","pUX1toXK","bUX1toXK","pU","Delta","Delta_perc","bU","Omega_perc","Gamma_perc",
              paste(c("pUX","DeltaX","DeltaX_perc","bUX","GammaX_perc"),rep(1:length(fact),each=5),sep=""))
  ## Return estimated quantities
  return(results)
  
}
