
if(!require("fields")){install.packages("fields")}
if(!require("mnormt")){install.packages("mnormt")}
if(!require("MCMCpack")){install.packages("MCMCpack")}
if(!require("actuar")){install.packages("actuar")}
if(!require("ggplot2")){install.packages("ggplot2")}
if(!require("kernlab")){install.packages("kernlab")}

if(!require("tidyverse")){install.packages("tidyverse")}
if(!require("readr")){install.packages("readr")}
if(!require("psych")){install.packages("psych")}


library("readr") 
library("psych") 

library("fields")
library("mnormt")
library("MCMCpack")
library("actuar")
library("ggplot2")
library("kernlab")
library(readxl)


datos <- read_xls('est46114_s06_data.xls')
datos2 <- datos[,2:length(datos)]
datos.pca <- prcomp(datos2, scale. = TRUE)
summary(datos.pca)
ls(datos.pca)


biplot(datos.pca, cex = 0.4)



#Definiendo la distribución posterior

data <- datos2

a0 <- 1
s0 <- 1
m0 <- matrix(0,ncol=1,nrow=80)
B0 <- diag(1,ncol=80,nrow=80) 

gaussian.posterior <- function(data,m0,s0,a0,B0){
  xbar <- as.matrix(colMeans(data))
  S <- cov(data)
  n <- nrow(data)
  p <- ncol(data)
  # Posterior hiperparameters
  sn <- s0 + n
  an <- a0 + n/2
  mn <- (s0*m0 + n*xbar)/sn
  # mn <- (n*xbar)/sn
  Bn <- B0 + (n/2)*(S + (s0/sn)*(xbar-m0)%*%t(xbar-m0))
  #Bn <- B0 + (n/2)*(S + (s0/sn)*(xbar)%*%t(xbar))
  # Output
  output <- list(mn,sn,an,Bn)
  return(output)
 }

output <- gaussian.posterior(data,m0,s0,a0,B0)



#Algoritmo de Simulación

M <- 10000
mu.sim <- matrix(NA,nrow=M, ncol=ncol(data))
Lambda.sim <- array(NA,dim=c(M,ncol(data),ncol(data)))
e.sim <- matrix(NA,nrow=M, ncol=ncol(data))
V.sim <- array(NA,dim=c(M,ncol(data),ncol(data)))
C.sim <- array(NA,dim=c(M,nrow(data),ncol(data)))
m <- 1; X <- as.matrix(data)
for(m in 1:M){
  # Simulacion (mu,Lambda)
  Lambda.sim[m,,] <- rWishart(1, output[[3]], output[[4]])
  mu.sim[m,] <- mvrnorm(1, mu=output[[1]], Sigma=solve(output[[2]]*Lambda.sim[m,,]), tol = 1e-6)
  # Simulacion (e,V) + C
  eigen_aux <- eigen(solve(Lambda.sim[m,,]))
  e.sim[m,] <- eigen_aux$values
  V.sim[m,,] <- eigen_aux$vectors
  C.sim[m,,] <- X %*% V.sim[m,,]
}


hist(e.sim[,1])
summary(C.sim[,1,1])
