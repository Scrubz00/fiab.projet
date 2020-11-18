# TP 1 

## Question 1

phi <- function(x){
  rep <- ((x[3]*(1-(1-x[1])*(1-x[2]))*(1-(1-x[4])*(1-x[5])))+(1-x[3])*(1-(1-x[1]*x[4])*(1-x[2]*x[5])))*(1-(1-x[6]*x[7])*(1-x[8])*(1-x[9]))
  return(rep)
}

# question 2

phi_function <- function(lambda, beta, t){
  R1 <- 1-pweibull(t, shape = beta, scale = lambda)
  R2 <- 1-pweibull(t, shape = beta, scale = lambda)
  R3 <- 1-pweibull(t, shape = beta, scale = lambda)
  R4 <- 1-pweibull(t, shape = beta, scale = lambda)
  R5 <- 1-pweibull(t, shape = beta, scale = lambda)
  R6 <- 1-pweibull(t, shape = beta, scale = lambda)
  R7 <- 1-pweibull(t, shape = beta, scale = lambda)
  R8 <- 1-pweibull(t, shape = beta, scale = lambda)
  R9 <- 1-pweibull(t, shape = beta, scale = lambda)
  rep <- ((R3[t]*(1-(1-R1[t])*(1-R2[t]))*(1-(1-R4[t])*(1-R5[t])))+(1-R3[t])*(1-(1-R1[t]*R4[t])*(1-R2[t]*R5[t])))*(1-(1-R6[t]*R7[t])*(1-R8[t])*(1-R9[t]))
  return(rep)
}




## Question 3

plot(phi_function(20,2, 0:35), type = 'l')

## Question 4

survie <- function(lambda, beta, t){
  R1 <- 1-pweibull(t, shape = beta, scale = lambda)
  R2 <- 1-pweibull(t, shape = beta, scale = lambda)
  R3 <- 1-pweibull(t, shape = beta, scale = lambda)
  R4 <- 1-pweibull(t, shape = beta, scale = lambda)
  R5 <- 1-pweibull(t, shape = beta, scale = lambda)
  R6 <- 1-pweibull(t, shape = beta, scale = lambda)
  R7 <- 1-pweibull(t, shape = beta, scale = lambda)
  R8 <- 1-pweibull(t, shape = beta, scale = lambda)
  R9 <- 1-pweibull(t, shape = beta, scale = lambda)
  rep <- -log(((R3[t]*(1-(1-R1[t])*(1-R2[t]))*(1-(1-R4[t])*(1-R5[t])))+(1-R3[t])*(1-(1-R1[t]*R4[t])*(1-R2[t]*R5[t])))*(1-(1-R6[t]*R7[t])*(1-R8[t])*(1-R9[t]))/t)
  # rep <- -log(rep)/t
  return(rep)
  # rep <- -log(1 - phi_function(lambda, beta, t))/t
}

survie <- function(lambda, beta, t){
  rep=NULL
  for(i in 1:length(t)){
    R1 <- 1-pweibull(t[i], shape = beta, scale = lambda)
    R2 <- 1-pweibull(t[i], shape = beta, scale = lambda)
    R3 <- 1-pweibull(t[i], shape = beta, scale = lambda)
    R4 <- 1-pweibull(t[i], shape = beta, scale = lambda)
    R5 <- 1-pweibull(t[i], shape = beta, scale = lambda)
    R6 <- 1-pweibull(t[i], shape = beta, scale = lambda)
    R7 <- 1-pweibull(t[i], shape = beta, scale = lambda)
    R8 <- 1-pweibull(t[i], shape = beta, scale = lambda)
    R9 <- 1-pweibull(t[i], shape = beta, scale = lambda)
    rep[i] <- -log(((R3*(1-(1-R1)*(1-R2))*(1-(1-R4)*(1-R5)))+(1-R3)*(1-(1-R1*R4)*(1-R2*R5)))*(1-(1-R6*R7)*(1-R8)*(1-R9)))/t[i]
  }
  return(rep)
}


survie(100, 2, seq(0.1, 20, 0.1))
plot(survie(1000, 2, seq(0.1, 20, 0.1)), type = 'l')

# Question 5 
# simulation d'une loi de weibull avec la méthode des quantile. 


# Question 5 

inverse_pweibull <- function(lambda, beta, u){
  rep <- lambda*(-(log(1-u)^(1/beta)))
  return(rep)
}

phi2 <- function(a, n, lambda, beta){
  t <- seq(0, a,length.out =n)
  y <- rep(0,n)
  u <- runif(9,0,1)
  x <- matrix(0, nrow = n, ncol = 9)
  
  for(i in 1:n){
    for(j in 1:9){
      if(t[i] < (lambda*(-log(u[j]))^(1/beta))){
        x[i,j] <- 1
      }
    }
    y[i] <- phi(x[i,])
  }
  return(y)
}
phi2(4,20,2,1)
plot(phi2(4,20,2,1),type="l")

# question 6 

phi23 <- function(a, n, lambda, beta){
  t <- seq(0, a,length.out =n)
  y <- rep(0,n)
  u <- runif(9,0,1)
  x <- matrix(0, nrow = n, ncol = 9)
  for(i in 1:n){
    for(j in 1:9){
      if(t[i] < (lambda*(-log(u[j]))^(1/beta))){
        x[i,j] <- 1
      }
      
    }
    if (x[i,j]==0) { break }
    
  }
  return(i-1)
}
phi23(4,36,2,1)

# question 7

n_realisation_T<-function(a, n, lambda, beta,ntot){
  vect=NULL
  for(i in 1:ntot){
    vect[i]=phi23(a, n, lambda, beta)
  }
  return(vect)
}
n_realisation_T(4,36,2,1,40)
sum(n_realisation_T(4,36,2,1,40))

mu=function(a, n, lambda, beta,ntot){
  v=n_realisation_T(a, n, lambda, beta,ntot)
  1/ntot*sum(v)
}
mu(4,36,2,1,40)

######### 
# Exercice 1

phi <- function(x){
  rep <- x[5]*x[6]*((1-(1-x[1]*x[3])*(1-x[2]*x[4]))*(1-(1-x[7]*x[8])*(1-x[9]*x[10])))+(1-x[5]*x[6])*((1-(1-x[1]*x[3]*x[7]*x[8])*(1-x[2]*x[4]*x[9]*x[10])))
  return(rep)
}
phi(c(1,1,0,1,1,1,1,1,0,1))
## Exercice 2

lambda = 5
beta = 5
t <- seq(0, 30, 0.1)
X <- matrix(data = rep(0, 10*length(t)), ncol = 10, nrow = length(t))
S <- rep(0,length(t))
u <- runif(10, 0, 1)

for(i in 1:length(t)){
  for(j in 1:10){
    if(t[i] < (lambda*(-log(u[j]))^(1/beta))){
      X[i,j] <- 1
    }
  }
  S[i] <- phi(X[i,])
}

# Exercice 3
phi2 <- function(a, n, lambda, beta){
  t <- seq(0, a,length.out =n)
  y <- rep(0,n)
  u <- runif(10,0,1)
  x <- matrix(0, nrow = n, ncol = 10)
  
  for(i in 1:n){
    for(j in 1:9){
      if(t[i] < (lambda*(-log(u[j]))^(1/beta))){
        x[i,j] <- 1
      }
    }
    y[i] <- phi(x[i,])
  }
  return(y)
}
phi2(4,20,10,1)
plot(phi2(4,20,2,1),type="l")