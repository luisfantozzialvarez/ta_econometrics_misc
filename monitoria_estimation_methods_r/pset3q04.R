#sets working directory
setwd("~/Dropbox/EESP/2° Tri/Econometria/pset")

#installs and loads package to read stata13 dta file
#install.packages("readstata13")
library(readstata13)

#reads file into data.frame
dados = read.dta13("smk.dta")

#PROBLEM 4 
#QUESTION A
#log-likelihood function for observation
li <-function(beta,y,x)
{
  return(y*as.vector(log(pnorm(x%*%beta))) +  (1-y)*as.vector(log((1-pnorm(x%*%beta)))))
}

#sample log-likelihood
lkl <-function(beta,y,X)
{
  return(sum(li(beta,y,X)))
}

#observation score
si <-function(beta,y,x)
{
  return(y*as.vector((dnorm(x%*%beta))/(pnorm(x%*%beta)))*x -(1-y)*as.vector(dnorm(x%*%beta)/(1-pnorm(x%*%beta)))*x)
}

#sample score
sn <-function(beta,y,X)
{
  return(colSums(si(beta,y,X)))
}


#function that produces table computing t-Stat and p-values from given parameters, vcov Matrix and distribution (defaults to Normal)
testes <- function(teta, var, f = pnorm )
{
  output = cbind("Parameter"=teta,"S.E." = sqrt(diag(var)), "t-Stat" = teta/sqrt(diag(var)), "p-value" = round(2*(1 - f(abs(teta/sqrt(diag(var))))), digits = 5))
  rownames(output) =  rownames(teta) 
  colnames(output) = c("Parameter","S.E.","t-Stat", "p-value")
  return(output)
}

#creates dep var vector and covariate matrix
y = as.vector(dados$smokes)
X = as.matrix(cbind("cons"=rep(1, nrow(dados)), dados[c("white","motheduc", "lfaminc")]))

#logical vector with non-missing observations
non_missing = !is.na(y)&!(as.logical(rowSums(is.na(X))))

#only selects non-missing observations
y=y[non_missing]
X=X[non_missing,]

#runs OLS to use coef estimate as starting vector in optim
beta_ols = solve(t(X)%*%X)%*%t(X)%*%y

#runs optim to find beta_probit. convergence in objective function is set to 10^(-10)
#which is a bit more precise than default
prob_optim = optim(par=beta_ols, fn=lkl,gr =sn, method = "L-BFGS-B", control = list("fnscale"=-1, "factr"=10^(-10)),hessian = TRUE, y=y, X=X)
beta_probit = prob_optim$par

#probit variance-covariance matrix 
vcov_matrix = solve(-prob_optim$hessian)

#summary of results
testes(beta_probit,vcov_matrix)
#We may analyse the direction of effects.
#Comments: being white entails a positive, though not significant at the 5% level, effect on the probability of smoking.
#Family income has a negative and significant effect on the probability of smoking.
#Moher education entails a negative and significant effect.


#uses R glm function to run probit (just to check if results are the same, which they are)
modelo = glm(smokes~white+motheduc+lfaminc,data = dados, family = binomial("probit"))
summary(modelo)

#QUESTION B

#creates dep var vector, covariate and instrument matrix
y = as.vector(dados$smokes)
X = as.matrix(cbind("cons"=rep(1, nrow(dados)), dados[c("white","motheduc", "lfaminc")]))
Z = as.matrix(cbind("cons"=rep(1, nrow(dados)), dados[c("white","motheduc", "fatheduc")]))

#logical vector with non-missing observations
non_missing = !is.na(y)&!(as.logical(rowSums(is.na(X))))&!(as.logical(rowSums(is.na(Z))))

#only selects non-missing observations
y=y[non_missing]
X=X[non_missing,]
Z=Z[non_missing,]

#computes first-stage estimates and residuals
beta_fs = solve(t(Z)%*%Z)%*%t(Z)%*%X[,"lfaminc"]
resid = X[,"lfaminc"] - Z%*%beta_fs

#error variance 
v2 = sum(resid^2)/(nrow(Z)-ncol(Z))
#first stage covariance matrix (homoskedastic, as assumed in the model)
fs_vcov = v2*solve((t(Z)%*%Z))



#adds residual to regressors
X = cbind(X, resid)
colnames(X)[5]="resid"

#runs linear model to use estimate as starting value in optim
beta_linear = solve(t(X)%*%X)%*%t(X)%*%y

#runs optim to find beta_probit_ctrl. convergence in objective function is set to 10^(-10)
#which is a bit more precise than default
ctrl_function_optim = optim(par=beta_linear, fn=lkl,gr =sn, method = "L-BFGS-B", control = list("fnscale"=-1, "factr"=10^(-10)),hessian = TRUE, y=y, X=X)
#we need to re-escale parameters
beta_probit_ctrl_unscaled = ctrl_function_optim$par 
beta_probit_ctrl = beta_probit_ctrl_unscaled/sqrt(1+beta_probit_ctrl_unscaled[5]^2*((v2)*(nrow(Z)-ncol(Z))/nrow(Z)))

#first stage estimates
testes(beta_fs,fs_vcov)
#the instrument satisfies the relevance condition, as it is indeed correlated with 
#lfaminc

#prints second stage
print(beta_probit_ctrl)
#Signs do not change.


#QUESTION C
#observation likelihood
#we denote theta as a vector: theta = (rho_1, sigma_2, delta_1, alpha_1, delta_2).
#see Wooldridge for notation
li_joint <- function(theta,y, endog, exog, instrument)
{
  rho_1=theta[1]
  sigma_2 = theta[2]
  delta_1 = theta[3:5]
  alpha_1 = theta[6]
  delta_2 = theta[7:10]
  
  #creates vector of instruments
  Z = cbind(instrument, exog)
  
  #creates w vector in wooldridge
  w=(exog%*%delta_1 + endog*alpha_1 + (rho_1/sqrt(sigma_2))*(endog - Z%*%delta_2))/sqrt(1-(rho_1)^2)
  
  #creates likelihood
  #lkl_i= (as.vector((pnorm(w)))^y)*((as.vector((1 - pnorm(w)))))^(1-y)*as.vector(((1/sqrt(sigma_2))*dnorm((endog-Z%*%delta_2)/sqrt(sigma_2))))
  
  li= as.vector(log(pnorm(w)))*y + as.vector(log(1 - pnorm(w)))*(1-y) + as.vector(log((1/sqrt(sigma_2))*dnorm((endog-Z%*%delta_2)/sqrt(sigma_2))))
  return(li)
}

#sample log_likelihood.
lkl_joint <-function(theta,y, endog, exog, instrument)
{
  return(sum(li_joint(theta,y, endog, exog, instrument)))
}

#creates dep var vector, covariate and instrument matrix
y = as.vector(dados$smokes)
exog = as.matrix(cbind("cons"=rep(1, nrow(dados)), dados[c("white","motheduc")]))
endog = as.vector(dados$lfaminc)
instrument = as.vector(dados$fatheduc)
#logical vector with non-missing observations
non_missing = !as.logical(rowSums(is.na(as.data.frame(dados))))
#only selects non-missing observations
y=y[non_missing]
exog=exog[non_missing,]
endog = endog[non_missing]
instrument =instrument[non_missing]

#runs optim. starting parameter is vector of 0.5. 
#sets relative tolerance to 10^-10. higher than default
modelo = optim(par = rep(0.5,10), fn = lkl_joint, method = "BFGS", control = list("fnscale"=-1,"reltol"=10^(-10)), hessian=T, y = y, endog=endog, exog=exog, instrument=instrument )

#grabs_coefficients of interest
beta_joint_mle = matrix(modelo$par[3:6],ncol=1)

#covariance matrix, where we grab the block related to the parameters of interest
joint_vcov = solve(-modelo$hessian)[3:6,3:6]

#adds labels to coefs
rownames(beta_joint_mle)=rownames(beta_probit)

#returns results
testes(beta_joint_mle,joint_vcov)

#distance between ctrl_function and joint mle. it is quite close to zero (5.10^(-4)),
#which is just a consequence of Rivers and Vuong's result: when the model
#is just identified, both estimators are numerically equal (difference between estimates
#is due to different optimization methods in each case)
print(sqrt(sum(beta_joint_mle- beta_probit_ctrl[1:4])^2))