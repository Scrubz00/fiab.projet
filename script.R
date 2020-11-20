library(magrittr)
library(dplyr)
library(ggplot2)

# TP 1 

## Question 1

phi <- function(x){
  rep <- ((x[3]*(1-(1-x[1])*(1-x[2]))*(1-(1-x[4])*(1-x[5])))+(1-x[3])*(1-(1-x[1]*x[4])*(1-x[2]*x[5])))*(1-(1-x[6]*x[7])*(1-x[8])*(1-x[9]))
  return(rep)
}

# question 2

phi_function <- function(loi, lambda, beta, t){
  R1 <- 1-loi(t, shape = beta, scale = lambda)
  R2 <- 1-loi(t, shape = beta, scale = lambda)
  R3 <- 1-loi(t, shape = beta, scale = lambda)
  R4 <- 1-loi(t, shape = beta, scale = lambda)
  R5 <- 1-loi(t, shape = beta, scale = lambda)
  R6 <- 1-loi(t, shape = beta, scale = lambda)
  R7 <- 1-loi(t, shape = beta, scale = lambda)
  R8 <- 1-loi(t, shape = beta, scale = lambda)
  R9 <- 1-loi(t, shape = beta, scale = lambda)
  rep <- ((R3[t]*(1-(1-R1[t])*(1-R2[t]))*(1-(1-R4[t])*(1-R5[t])))+(1-R3[t])*(1-(1-R1[t]*R4[t])*(1-R2[t]*R5[t])))*(1-(1-R6[t]*R7[t])*(1-R8[t])*(1-R9[t]))
  return(rep)
}




## Question 3

plot(phi_function(pweibull, 20, 2, 0:35), type = 'l')

g <- ggplot() +
  geom_line(aes(x = 1:35, y = phi_function(pweibull, 20, 2, 0:35))) +
  labs(title = "survie du système",
       x = "temps",
       y = "proba de survie")

g

## Question 4

# survie <- function(lambda, beta, t){
#   R1 <- 1-pweibull(t, shape = beta, scale = lambda)
#   R2 <- 1-pweibull(t, shape = beta, scale = lambda)
#   R3 <- 1-pweibull(t, shape = beta, scale = lambda)
#   R4 <- 1-pweibull(t, shape = beta, scale = lambda)
#   R5 <- 1-pweibull(t, shape = beta, scale = lambda)
#   R6 <- 1-pweibull(t, shape = beta, scale = lambda)
#   R7 <- 1-pweibull(t, shape = beta, scale = lambda)
#   R8 <- 1-pweibull(t, shape = beta, scale = lambda)
#   R9 <- 1-pweibull(t, shape = beta, scale = lambda)
#   rep <- -log(((R3[t]*(1-(1-R1[t])*(1-R2[t]))*(1-(1-R4[t])*(1-R5[t])))+(1-R3[t])*(1-(1-R1[t]*R4[t])*(1-R2[t]*R5[t])))*(1-(1-R6[t]*R7[t])*(1-R8[t])*(1-R9[t]))/t)
#   # rep <- -log(rep)/t
#   return(rep)
#   # rep <- -log(1 - phi_function(lambda, beta, t))/t
# }

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


survie(20, 2, seq(0.1, 20, 0.1))

g <- ggplot() +
  geom_line(aes(x = seq(0.1, 20, 0.1), y = survie(20, 2, seq(0.1, 20, 0.1)))) +
  labs(x = "t",
       y = "-ln(F(t))/t")

g


# Question 5 
# simulation d'une loi de weibull avec la méthode des quantile. 

f_repar_weibull <- function(lambda, beta, x){
  rep <- 1-exp(-(x/lambda)^beta)
  return(rep)
}

f_repar_weibull(1,1,0:5)

realisation_weibull<- function(lambda,beta,a){
  
  u=runif(1, min=0, max=1)
  g=f_repar_weibull(lambda,beta,a) 
  v=g>u
  
  for(i in 1:length(a)){
    nb=i
    if (v[i]==TRUE) { break }
  }
  
  return(nb)
}

realisation_weibull(1,1,1:10)

n_realisation_weibull<-function(n,lambda,beta,a){
  vect=NULL
  for(i in 1:n){
    vect[i]=realisation_weibull(lambda,beta,a)
  }
  return(vect)
}
n_realisation_weibull(10,1,1,1:5)

rweibull(10,1,1)

x<- 0:10
plot(f_repar_weibull(1,1,x),type = "l")
plot(pweibull(x,1,1),type = "l")


# Question 5 

inverse_pweibull <- function(lambda, beta, u){
  rep <- lambda*(-(log(1-u)^(1/beta)))
  return(rep)
}

phi2 <- function(lambda, beta, a, n){
  u <- runif(9)
  t <- seq(0, a, n)
  x <- matrix(0, nrow = n, ncol = 9)
  
  for(i in 1:n){
    for(j in 1:9){
      if(t[i] < inverse_pweibull(lambda, beta, u)){
        x[i,j] <- 1
      }
    }
    rep[i] <- phi(x[i,])
  }
}

# Question 6

lambda <- 5
beta <- 5

t <- seq(0, 10, 0.1)

R <- matrix(rep(0, length(t)*9), nrow = length(t), ncol = 9)
S <- rep(0, length(t))

for(i in 1:length(t)){
  y <- 1 - pweibull(t[i], lambda, beta)
  R[i,] <- rep(y, 9)
  S[i] <- survie(lambda, beta, R[i,])
}


# Projet 2

## Exercice 1

phi <- function(x){
  rep <- (x[5]*((1-(1-x[1])*(1-x[2]))*(1-(1-x[3])*(1-x[4])))+(1-x[5])*(1-(1-x[1]*x[3])*(1-x[2]*x[4])))*x[6]*(1-(1-x[7]*x[10])*(1-x[8]*x[9]))
  return(rep)
}



## Exercice 2

lambda = 5
beta = 5
t <- seq(0, 30, 0.1)

simu_etat_systeme <- function(lambda, beta, t){
  X <- matrix(data = rep(0, 10*length(t)), ncol = 10, nrow = length(t))
  S <- rep(0,length(t))
  u <- runif(10, 0, 1)
  w <- rweibull(10, lambda, beta)
  max <- max(t)
  
  for(i in 1:length(t)){
    for(j in 1:10){
      if(t[i] < w[j]){
        X[i,j] <- 1
      }
    }
    S[i] <- phi(X[i,])
    if((S[i] == 0) && (S[i-1] == 1)){
      max <- t[i]
    }
  }
  return(list(etat = S, last = max))
}

test <- simu_etat_systeme(lambda, beta, t)

g <- ggplot() +
  geom_line(aes(x = t, y = test[[1]])) +
  labs(x = "t",
       y = "etat")

g

# Esperance

esperance_T <- function(n){
  n <- 100
  simu <- rep(-1, n)
  for(i in 1:n){
    simu[i] <- simu_etat_systeme(lambda, beta, t)[[2]]
  }
  return(mean(simu))
}
esperance_T(100)

# Exercice 3

a <- 10
n <- 100
lambda <- 5
beta <- 5
inter <- 5
t <- seq(0, a, length.out = n + 1)

phi2 <- function(a, n, lambda, beta, t, inter){
  y <- rep(0,n)
  u <- runif(10,0,1)
  x <- matrix(0, nrow = n, ncol = 10)
  p <- a/n*(1:n)*inter # p : vecteur qui contient les différents moments ou le méchanicien remet en marche la composante x[6]
  # p ne donne pas ce que je veux
  w <- rweibull(10, lambda, beta)
  
  for(i in 1:length(t)){
    for(j in 1:10){
      if(j == 6){
        if(t[i] %in% p){
          w[j] <- w[j] + t[i]
        }
      }
      if(t[i] < w[j]){
        x[i,j] <- 1
      }
    }
    y[i] <- phi(x[i,])
  }
  return(y)
}
# la fonction ne compile pas
