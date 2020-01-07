#Remove previously loaded objects
rm(list=ls())

#Parallelization library
library(parallel)

#Sets working directory
setwd("~/Dropbox/EESP/Monitorias/Pos/Bayesiana")

#Loads employment data
empl = read.csv("EmplUK.csv")

#Suppose we are interested in estimating E[y_{it}|K_{it}, L_{it}]
#And we want to model it using polynomials.
#We will select the order of the polynomial by cross-validation
#This can be quite slow, and parallelization may give us a hand
y = empl$output
X = as.matrix(empl[,c("capital","emp")])


#First, we construct a function that, given an index, and a polynomial order,
#estimates a regression by removing the line corresponding to that index, and then computes
#the prediction error
pred_error_cv <- function(index, poly_order, y, X_cov)
{
  #Polynomial matrix
  X_mat = poly(X_cov, degree = poly_order, raw = T, simple = T)
  #Adds intercept
  X_mat = cbind(1, X_mat)
  #Computes OLS fit excluding index 
  y_fit = y[-index]
  X_mat_fit = X_mat[-index, ]
  beta = solve(t(X_mat_fit)%*%X_mat_fit)%*%t(X_mat_fit)%*%y_fit
  #Prediction error for i'th entry
  pred_error = y[index] - t(X_mat[index,])%*%beta
  return(pred_error)
}

#The next function computes, for a given polynomial order, the CV criterion using 
#parallelization
cv_parallel <- function(poly_order, y, X_cov)
{
  #Vector with observation indices
  vec_indices = 1:length(y)
  
  #Creates clusters
  outercluster = makeCluster(detectCores())
  
  #We will apply pred_error_cv to each entry of vec_indices
  error = parSapply(cl = outercluster, X = vec_indices, FUN = pred_error_cv, poly_order = poly_order, y = y, X_cov = X_cov)
  
  #Stops cluster
  stopCluster(outercluster)
  
  #CV - criterion
  cv_crit = sum(error^2)

  return(cv_crit)
}

#We will compute the CV criterion for poly_order ranging from 1 to 4
poly_list = c(1:4)

#Applies cv_parallel to each entry of poly_list: WITHOUT using parallelization in this step!
system.time(crit_list <- sapply(poly_list, cv_parallel, y = y, X_cov = X))

#Order chosen by CV
poly_order_cv = which.min(crit_list)

#When simulating in each cluster, one has to be careful with RNG. Each cluster is a new
#R session. There are ways to control for the seed in each cluster (or, alternatively,
#you can simulate everything in your base R session and then send it to the cluster.
#See https://stat.ethz.ch/R-manual/R-devel/library/parallel/doc/parallel.pdf for references.
#Python reference: https://www.machinelearningplus.com/python/parallel-processing-python/ 
