############################### EXAMPLE WITH SIMULATED DATA ########################################

source("R_functions.R")       # need to place provided script in current working directory
require(boot)                 # need to install this package from CRAN

# 1. Simulate data with two measured risk factors X1 and X2 and one unmeasured factor U
#set.seed(6)
n<-10000
X1<-rbinom(n,1,0.5)
X2<-rbinom(n,1,0.5)
U<-rbinom(n,1,0.5)
R<-rbinom(n,1,inv.logit(-0.02-2*X1-0.2*X2-3*U))
Y<-rbinom(n,1,inv.logit(-3.5+1*X1+1*X2+2*U))
dat<-data.frame(Y,R,X1,X2,U)

# 2. Assess distribution of factors in each region
prop.table(table(R,X1),1)
prop.table(table(R,X2),1)
#Note: With above parameters, differences in the composition are large for X1, but not for X2.
#      And both factors have the same effect on the outcome.


# 3. Example use of individual the three estimation functions: fstd(), fipw() and fdr()
#    Obtain fully-adjusted prevalence p_U with each of the three estimators
fstd(dat, "Y","R",c("X1","X2"))

fipw(dat, "Y","R",c("X1","X2"))

fdr(dat, "Y","R",c("X1","X2"))


# 4. Example use of compprev() function: 
#    Obtain all proposed measures, standard errors and confidence intervals with "boot" 
#    function from "boot" package 

# a) Apply non-parametric bootstrap by feeding "compprev" function and its arguments to "boot" function
#    (example for standardization - change "method" argument for other methods)
#    Try ?boot for explanation of other arguments
bstrap<-boot(data=dat, statistic=compprev, method="std", outcome="Y",region="R",fact=c("X1","X2"), stype="i", R=100)
# preferably do more, say R=1000 replications

# b) Recover estimates and standard errors from bstrap object
RES<-data.frame(EST=bstrap$t0,SE=apply(bstrap$t,2,sd,na.rm=T))

# c) Recover confidence intervals through percentile method from bstrap object
RES$CIlow<-NULL
RES$CIupp<-NULL
for(i in 1:nrow(RES))
{ 
  bt<-boot.ci(bstrap,index=i,type=c("perc"))  
  RES$CIlow[i]<-bt$percent[4]
  RES$CIupp[i]<-bt$percent[5] }

# 4. Print results

fact<-c("X1","X2")

Measure<-c("Region 0 prevalence",
           "Region 1 prevalence",
           "Crude difference",
           "Fully-adjusted Region 1 prevalence",
           "Overall change in Region 1 prevalence",
           "Overall % Change in Region 1 prevalence", 
           "Remaining difference",
           "% Difference unexplained",
           "% Net impact on difference",         
           rep(c("Xk-Unadjusted Region 1 prevalence",
                 "Change in Region 1 prevalence due to Xk",
                 "% Change in Region 1 prevalence due to Xk",
                 "Resulting difference due to Xk",
                 "% Impact of Xk on difference"),length(fact)))

Xk<-c(rep("",9),rep(fact,each=5))

RES<-round(RES,2)

cbind(Xk,Measure,RES)




