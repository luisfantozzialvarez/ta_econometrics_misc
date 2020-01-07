#Clears previously loaded object
rm(list=ls())

#Sets working directory
setwd("~/Dropbox/EESP/Monitorias/Pos/Bayesiana")

#Sets seed to allow for replication
set.seed(911)

#Loads data
base = read.csv("jiadata2R.csv")

#Matrix X with covariates
X = cbind("intercept" = 1, base[,c("population","urban","MidWest", "southern")])

#Entry outcomes
y = base[,c("Kmart","WalMart")]

#Function that, given values for covariate index X_1'beta_1 and 
#X_2'beta_3, sensitivity to other players actions Delta_1 and Delta_2 (both nonpositive), and
#idiosyncratic shocks epsilon_1 and epsilon_2 (stacked in vector epsilon), computes predicted Nash equilibrium.
#If in the indeterminacy region, draws equilibrium (1,0) with probability p
compute_nash <- function(index1,index2, Delta1, Delta2, epsilon, p)
{
  epsilon_1 = epsilon[1]
  epsilon_2 = epsilon[2]
  
  if(epsilon_1>= -index1 - Delta1)
  {
    if(epsilon_2 >= - index2 - Delta2)
      nash = c(1,1) else
        nash = c(1,0) 
   } else if(epsilon_2 >= - index2 - Delta2)
            nash = c(0,1) else {
              if(epsilon_1>= -index1){
                if(epsilon_2 >= -index2)
                {
                  draw = rbinom(1,1,p)
                  nash = c(draw, 1-draw)
                } else nash = c(1,0)
              } else if(epsilon_2 >= -index2)
                nash = c(0,1) else
                  nash = c(0,0)
 }
return(nash)
}

#Number of draws
Nreps = 1000

par_seq = c()
for(jj in 1:Nreps)
{
  #Draws parameters from prior
  beta1 = rnorm(ncol(X))
  beta2 = rnorm(ncol(X))
  Delta1 = -rchisq(1,1)
  Delta2 = -rchisq(1,1)
  p = runif(1)

  #Creates artifficial dataset from parameters
  artifficial = apply(X, 1, function(x){
    index1 = sum(beta1*x)
    index2 = sum(beta2*x)
    
    #Draws shocks
    epsilon = rlogis(2)
  
    return(compute_nash(index1,index2, Delta1, Delta2, epsilon, p))
  })
  artifficial = t(artifficial)
  
  discrepancy = mean(rowSums(abs(artifficial - y))>0)
  
  par_seq = rbind(par_seq,cbind(t(beta1),t(beta2),"Delta1"=Delta1,"Delta2"=Delta2,"p"=p,"discrepancy"=discrepancy))
}

#Criterion
#Acceptance rate
acc = 0.2

#Uniform vector
unif_vec = runif(Nreps)

#Choose tolerance parameter so as to ensure acceptance
tol_grid = seq(0,10, by = 0.001)
#Implied acceptance rates (smooth rejection rule)
implied_acc = sapply(tol_grid, function(h){mean(unif_vec<=dnorm(par_seq[,"discrepancy"]/h)/dnorm(0))})

#Tolerance
h = tol_grid[which(implied_acc == acc)]

#Accepted draws
accepted = unif_vec<=dnorm(par_seq[,"discrepancy"]/h)/dnorm(0)

par_accepted = par_seq[accepted,]

colnames(par_accepted)[1:(2*ncol(X))] = c(paste(colnames(X),"_1",sep=""),paste(colnames(X),"_2",sep=""))

#Now we can plot estimates! 
plot(seq(0,5,by=0.01), dchisq(seq(0,5,by=0.01),1),type = "l", col = "red", main = "- Delta - Kmart")
lines(density(-par_accepted[,"Delta1"]),col = "blue")
legend("topright", c("Prior","Posterior"), lty=c(1,1), col=c("red","blue"),bty="n")

#Now we can plot estimates! 
plot(seq(0,5,by=0.01), dchisq(seq(0,5,by=0.01),1),type = "l", col = "red", main = "- Delta - Walmart")
lines(density(-par_accepted[,"Delta2"]),col = "blue")
legend("topright", c("Prior","Posterior"), lty=c(1,1), col=c("red","blue"),bty="n")
