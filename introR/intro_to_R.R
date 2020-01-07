#Introduction to R

#0. Assigning values to variables: using = or -> is a matter of taste
a = 10
b <- 10
10 -> c
d = "Hello World"

filepath = "~/Dropbox/EESP/Monitorias/introR"

print(a)
print(b)
print(c)
print(d)

#1. Vectors, matrices, lists and dataframes
#1.1 Vectors
#Vectors store a sequence of elements of the same type (boolean, double, character, Date)

#Some examples:
#Declaring an empty vector
vec1 = c()

#Let's generate a vector with the numbers from 1 to 10 by using a loop
i=1 
while (i <= 10){
  #Updating a vector by adding an entry to it
  vec1 = c(vec1, i)
  #updates index
  i = i + 1
}

print(vec1)

#Or, more compactly:
vec2 = 1:10
print(vec2)

#Let's create a vector with the even numbers from 1 to 10
vec3 = c()

for(i in 1:10)
  if(i %% 2 == 0)
    vec3 = c(vec3, i)

print(vec3)

#Or, more simply
vec4 = seq(2, 10, by = 2)

#Logical operators also work with vectors
i = 15
print(i > 10)

print(vec4 > 6)

#One can select a subset of a vector by selecting a subset of its indices
print(vec4[c(1,3,5)])

#Or by using a logical vector of same dimension
print(vec4[vec4>6])

#Vectors work with other types too
#Vector with all days in 2016
days2016 = seq(from = as.Date("2016-01-01"), to = as.Date("2016-12-31"), by = 1)
print(days2016)

#1.2 Matrices
#Matrices extend vectors to the multidimensional case. As in the case of vectors, 
#all elements are of (and forced/cast to be) the same type. When working with numeric elements,
#most tools of matrix algebra are available in R.

#Declaring a matrix. There are many ways, e.g. by "reshaping" a vector
mat1 = matrix(1:9, nrow = 3, ncol = 3)
print(mat1)

#By concatenating vectors
vec1 = 1:10
vec2 = seq(100, 120, length = 10 )
mat2 = cbind(vec1, vec2)
mat3 = rbind(vec1, vec2)

print(mat2)
print(mat3)

#Or by other commands
id4 = diag(rep(1,4))
print(id4)

#Vectors and matrices work with the usual matrix algebra operations, e.g.
#Matrix multiplication (vectors are assumed to be column vectors)
v =  1:4
print(id4%*%v)
print(t(v)%*%id4%*%v)

#Not to be confused with! (equally useful)
print(v*id4)

#Regular sums
mat4 = matrix(1:16, nrow = 4)
print(mat4+id4)

#Inversion
print(mat2)
print(solve(t(mat2)%*%mat2))

#Kronecker products
print(mat4%x%id4)

#etc.

#As in vectors, we may select a subset of a matrix by either passing 
#a vector of indices or logical vectors, but now there are two dimensions
print(mat1)
print(mat1[3,])
print(mat1[,2:3])
print(mat1[mat1[,1] >= 2,])

#1.3 Lists
#We may see a list as an extension of a vector, but allowing entries to have different values.

#Example
person = list("name" = "Joao", "is.female" = F, "date.birth" = as.Date("1994-12-01") )
print(person)
print(person$name)
print(person[[1]])

matrices = list()
#Adding a same matrix 10 times to a list
for(i in 1:10)
  matrices[[i]] = mat1

print(matrices)

#1.4 Dataframe
#Just as one may see a list as a multi-type extension of a vector, so may one see a dataframe
#as a natural extension of a matrix
dados = data.frame("nome" = c("Joao", "Maria", "Jose"), "birth" = rep("2010-10-01",3), "weight" = c(50,55,60))

print(dados)
print(dados$nome)
print(dados[,c("nome", "weight")])
print(dados[dados$weight<57,])

#A dataframe is a database-like object. It is also the natural environment for many of R's statistical
#commands

#Sets a working directory (default filepath for accessing files)
#You declared your filepath at the beginning
setwd(filepath)

#Loads a CSV file as a dataframe
base = read.csv("fehr2007.csv")
View(base)

#Some summary statistics
summary(base)

#Let's run a regression of totrev on high, block2 and block3
ols1 = lm(totrev~high+block2+block3, data = base)
summary(ols1)

#We could do it by hand too, but just have to take care to remove missing (and convert things
#to vectors and matrices)
y = base$totrev
X = as.matrix(base[,c("high", "block2", "block3")])
X = cbind(1, X)
complete = complete.cases(cbind(y,X))

y = y[complete]
X = X[complete,]

bols = solve(t(X)%*%X)%*%t(X)%*%y
print(bols)

#2. Functions

#Functions allow for reutilising the code; and make things a lot clearer.

#Example1: Function that generates Tt draws from an AR1: yt = alpha + rho y_{t-1} + epsilon_t, 
#where epsilon is normally distributed with std sigma (defaults to 1) and starting value y0.

ar1 <- function(Tt, alpha, rho, y0, sigma = 1)
{
  path = c(y0)
  noise = rnorm(Tt, mean = 0, sd = sigma)
  for(t in 1:Tt)
    path = c(path, alpha + rho*path[t] + noise[t])
  return(path)
}

#Generating some draws
series1 = ar1(1000, 0, 0.7, 0, 2)
#Plotting results
plot(0:1000, series1, type = "l")

#Now an integrated process
series2 = ar1(1000, 0, 1, 0)
plot(as.Date("2010-01-01")+0:1000, series2, type = "l")

#Numerical functions may also be used in optimization, integration; all of which are available
#in R's base command

#Example: function that returns a square of a number
f1 <- function(x)
{
  return(x^2)
}

print(f1(4))

#Let's integrate f1 from 0 to 1
print(integrate(f1, 0, 1))

#Let's minimise f1 using the BFGS algorithm, starting from x = 2 and without passing a gradient
#(R computes it numerically)
print(optim(fn = f1, par = c(2), method = "BFGS"))

#3. Packages and beyond

#Despite the breadth of R's base packages, sometimes we may want to go beyond; or simply not write our
#commands from scratch. R has a large repository of user-made packages, with methods written in R or
#even in low-abstraction languages. We may download packages from CRAN (R's default repository)
#or from other places (Github etc.). For most things, CRAN suffices.

#Suppose we did not know (or did not bother to do it by hand) how to compute HC-standard errors 
#in OLS. Package sandwich allows us to do this. Package lmtest allows us to report nice
#test statistics using arbitrary vcov-matrices. Let's download both from CRAN

#Uncomment the line below to download packages
#install.packages(c("sandwich","lmtest"), dependencies = T)

#Loading packages
library(sandwich)
library(lmtest)

#Running a model
ols2 <- lm(totrev~high*block3+block2, data= base)

#Homoskedastic SEs
summary(ols2)

#Now using HC0 SEs
coeftest(ols2, vcov. = vcovHC(ols2, type = "HC0"))

#Let's unload the packages (they overwrote some base functions)
detach(package:sandwich, unload = T)
detach(package:lmtest, unload = T)




