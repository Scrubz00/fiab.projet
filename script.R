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
# Question 6 
exp(1)
