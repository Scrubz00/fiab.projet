library(magrittr)
library(dplyr)
library(ggplot2)

# TP 1 

## Question 1

phi <- function(x){
  rep <- ((x[3]*(1-(1-x[1])*(1-x[2]))*(1-(1-x[4])*(1-x[5])))+(1-x[3])*(1-(1-x[1]*x[4])*(1-x[2]*x[5])))*(1-(1-x[6]*x[7])*(1-x[8])*(1-x[9]))
  return(rep)
}

## Question 2

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

# plot(phi_function(pweibull, 20, 2, 0:35), type = 'l')

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


# survie(20, 2, seq(0.1, 20, 0.1))

g <- ggplot() +
  geom_line(aes(x = seq(0.1, 20, 0.1), y = survie(20, 2, seq(0.1, 20, 0.1)))) +
  labs(x = "t",
       y = "-ln(F(t))/t")

g


# Question 5 
# simulation d'une loi de weibull avec la méthode des quantile. 

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
# phi2(4,20,2,1)
# plot(phi2(4,20,2,1),type="l")

g <- ggplot() +
  geom_line(aes(x = 1:20, y = phi2(4,20,2,1))) +
  labs(y = "y")

g

# Question 5 

# inverse_pweibull <- function(lambda, beta, u){
#   rep <- lambda*(-(log(1-u)^(1/beta)))
#   return(rep)
# }
# 
# phi2 <- function(lambda, beta, a, n){
#   u <- runif(9)
#   t <- seq(0, a, n)
#   x <- matrix(0, nrow = n, ncol = 9)
#   
#   for(i in 1:n){
#     for(j in 1:9){
#       if(t[i] < inverse_pweibull(lambda, beta, u)){
#         x[i,j] <- 1
#       }
#     }
#     rep[i] <- phi(x[i,])
#   }
# }

# Question 6

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

# Question 7

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

# Projet 2

## Question 1

phi <- function(x){
  rep <- (x[5]*((1-(1-x[1])*(1-x[2]))*(1-(1-x[3])*(1-x[4])))+(1-x[5])*(1-(1-x[1]*x[3])*(1-x[2]*x[4])))*x[6]*(1-(1-x[7]*x[10])*(1-x[8]*x[9]))
  return(rep)
}

# Question 2

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
      max <- t[i - 1]
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

# Simulation

simu_T <- function(n){
  n <- 100
  simu <- rep(-1, n)
  for(i in 1:n){
    simu[i] <- simu_etat_systeme(lambda, beta, t)[[2]]
  }
  return(simu)
}

n <- 100
simu <- simu_T(n)

esperance <- mean(simu)

confint <- c(mean(simu) - 1.96 * sd(simu)/sqrt(n), mean(simu) + 1.96 * sd(simu)/sqrt(n))

# code younes



# Exercice 3

# a <- 10
# n <- 100
# lambda <- 5
# beta <- 5
# inter <- 5
# t <- seq(0, a, length.out = n + 1)
# 
# phi2 <- function(a, n, lambda, beta, t, inter){
#   y <- rep(0,n)
#   u <- runif(10,0,1)
#   x <- matrix(0, nrow = n, ncol = 10)
#   p <- a/n*(1:n)*inter # p : vecteur qui contient les différents moments ou le méchanicien remet en marche la composante x[6]
#   # p ne donne pas ce que je veux
#   w <- rweibull(10, lambda, beta)
#   
#   for(i in 1:length(t)){
#     for(j in 1:10){
#       if(j == 6){
#         if(t[i] %in% p){
#           w[j] <- w[j] + t[i]
#         }
#       }
#       if(t[i] < w[j]){
#         x[i,j] <- 1
#       }
#     }
#     y[i] <- phi(x[i,])
#   }
#   return(y)
# }
# la fonction ne compile pas


# code camille

# Exercice 3
#  a

u=seq(0, 4,length.out =40)
# ----------------------------------------------------------------------------#
fonction_interval=function(n,inter,i){
  t=NULL
  d=NULL
  for (v in seq(1,n,inter)) {
    if(v==i){
      t[v]=1
    }
    else{
      t[v]=0
    }
  }
  t=t[!is.na(t)]
  t
  w=sum(t)
  if(w==0){
    d=0
  }
  else{
    d=1 
  }
  return(d)
}

fonction_interval(20,2,7)
seq(1,20,2)

#---------------------------------------------------------------------------#

phi2 <- function(a, n, lambda, beta,inter){
  t <- seq(0, a,length.out =n)
  y <- rep(0,n)
  u <- runif(10,0,1)
  x <- matrix(0, nrow = n, ncol = 10)
  
  for(i in 1:n){
    for(j in c(1,2,3,4,5,7,8,9,10)){
      if(t[i] < (lambda*(-log(u[j]))^(1/beta))){
        x[i,j] <- 1
      }
    }
  }
  d=0
  for(i in 1:n){
    if(t[i-d] < (lambda*(-log(u[6]))^(1/beta))){
      x[i,6] <- 1
    }
    if(fonction_interval(n,inter,i)==1 & x[i,6]==0){ 
      d <- i
      u[6] <- runif(1,0,1)
    }
  }
  # y[i] <- phi(x[i,])
  return(x)
}
(b=phi2(20,20,10,15,4))

phi3 <- function(a, n, lambda, beta,inter){
  y=NULL
  matrice=phi2(a, n, lambda, beta,inter)
  for (i in 1:n) {
    y[i]=phi(matrice[i,]) 
  }
  return(y)
}

a <- 20
n <- 20
lambda <- 15
beta <- 3
inter <- 7

(s=phi3(a,n,lambda,beta,inter))

g <- ggplot() +
  geom_line(aes(x = seq(0, a, length.out = n), y = phi3(a,n,lambda,beta,inter))) +
  labs(title = "état du système avec réparation du composant 6",
    x = "temps",
    y = "état du système")

g
#  b

#---   E(T)  ------------------------------------------------------

tp_i_de_panne <- function(a, n, lambda, beta,inter){
  y=NULL
  d=0
  matrice=phi2(a, n, lambda, beta,inter)
  for (i in 1:n) {
    y[i]=phi(matrice[i,]) 
  }
  for (i in n:1){
    if(y[i]==1 & i==n ){
      d=n
      break
    }
    if(y[i]==1){
      d=i+1
      break
    }
  }
  #  if(d==n){
  #    return("Pas de panne sur le temps T")
  #  }
  #  if(d<n & d>0){
  #    return(d)
  #  }
  return(d)
}
tp_i_de_panne(20,20,15,3,7)

nbtot_realisation_t=function(a, n, lambda, beta,inter,nbtot){
  vector=NULL
  for(i in 1:nbtot){
    vector[i]=tp_i_de_panne(a, n, lambda, beta,inter)
  }
  return(vector)
}

nbtot_realisation_t(20,20,15,3,7,8)

E_T=function(a, n, lambda, beta,inter,nbtot){
  t=nbtot_realisation_t(a, n, lambda, beta,inter,nbtot)
  moyenne=(1/nbtot)*sum(t)
  return(moyenne)
}
E_T(20,20,15,3,7,8)

#---   E(N)  ------------------------------------------------------

nb_intervention <- function(a, n, lambda, beta,inter){
  t <- seq(0, a,length.out =n)
  y <- rep(0,n)
  u <- runif(10,0,1)
  x <- matrix(0, nrow = n, ncol = 10)
  
  for(i in 1:n){
    for(j in c(1,2,3,4,5,7,8,9,10)){
      if(t[i] < (lambda*(-log(u[j]))^(1/beta))){
        x[i,j] <- 1
      }
    }
  }
  d=0
  intervention=0
  for(i in 1:n){
    if(t[i-d] < (lambda*(-log(u[6]))^(1/beta))){
      x[i,6] <- 1
    }
    if(fonction_interval(n,inter,i)==1){
      intervention <- intervention + 1
      if(x[i,6] == 0){
        d <- i
        u[6] <- runif(1,0,1)
      }
    }
  }
  return(intervention)
}
nb_intervention(20,20,15,3,7)


nb_realisation_intervention=function(a, n, lambda, beta,inter,nbtot){
  vector=NULL
  for(i in 1:nbtot){
    vector[i]=nb_intervention(a, n, lambda, beta,inter)
  }
  return(vector)
}

nb_realisation_intervention(20,20,15,3,7,7)

moyenne_intervention=function(a, n, lambda, beta,inter,nbtot){
  moyenne=(1/nbtot)*sum(nb_realisation_intervention(a, n, lambda, beta,inter,nbtot))
  return(moyenne)
}

moyenne_intervention(20,20,15,3,7,7)

recompense<-function(a, n, lambda, beta,inter,nbtot,cout,gain){
  r=gain*E_T(a, n, lambda, beta,inter,nbtot)-cout*moyenne_intervention(a, n, lambda, beta,inter,nbtot)
  return(r)
}
recompense(20,20,15,3,3,7,2,3.5)

a_val <- 20
n_val <- 20
lambda_val <- 15
beta_val <- 3
nbtot_val <- 7
cout_val <- 100
gain_val <- 250

recompense<-function(a=a_val, n=n_val, lambda=lambda_val, beta=beta_val,inter,nbtot=nbtot_val,cout=cout_val,gain=gain_val){
  r=gain*E_T(a, n, lambda, beta,inter,nbtot)-cout*moyenne_intervention(a, n, lambda, beta,inter,nbtot)
  return(r)
}
recompense(2)
solution=optimize(recompense,c(0, 20),maximum = TRUE)
solution

for(i in 1:n){
  y[i] <- recompense(i)
}
plot(y)
