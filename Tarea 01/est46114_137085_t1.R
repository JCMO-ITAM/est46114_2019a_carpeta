library(tidyverse)
library(ggcorrplot)
library(ggthemes)
library("fields")
library("mvtnorm")
library("MCMCpack")
library("actuar")
library("ggplot2")
library("kernlab")

tipo = readxl::read_xls('d:/Users/rene.rosado/Desktop/est46114_s06_data.xls')

names(tipo) = str_replace_all(names(tipo), ' ', '_')
names(tipo) = str_replace_all(names(tipo), '[\\./]', '_')

colMeans(tipo[,-1])

# Original
tipo.r  = tipo[,-1] 

tipo.r %>% scale() %>%
  var() %>% data.frame() %>% 
  mutate(v = row.names(.)) %>% 
  gather('var',Canada:Zambia, -v) %>%
  ggplot(aes(x=v, y = var, fill = `Canada:Zambia`)) + 
  geom_tile() + 
  labs(x= '', y = '', fill = 'Covarianza') +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

tipo.r %>%
  cor() %>%
  ggcorrplot()

tipo.pca <- prcomp(tipo.r, scale. = TRUE, center = TRUE)
summary(tipo.pca)

round(tipo.pca$rotation[, 1:3], 2)
biplot(tipo.pca, cex = c(0.4,1))

data = tipo.r

a0 <- 1
s0 <- 1
m0 <- matrix(0,ncol=1,nrow=ncol(data))
B0 <- diag(1,ncol=ncol(data),nrow=ncol(data)) 

gaussian.posterior <- function(data,m0,s0,a0,B0){
  xbar <- as.matrix(colMeans(data))
  S <- cov(data)
  n <- nrow(data)
  p <- ncol(data)
  # Posterior hiperparameters
  sn <- s0 + n
  an <- a0 + n/2
  mn <- (s0*m0 + n*xbar)/sn
  Bn <- B0 + (n/2)*(S + (s0/sn)*(xbar-m0)%*%t(xbar-m0))
  # Output
  output <- list(mn,sn,an,Bn)
  return(output)
}

output <- gaussian.posterior(data,m0,s0,a0,B0)

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
  mu.sim[m,] <- rmvnorm(1, mean=output[[1]], sigma=solve(output[[2]]*Lambda.sim[m,,]))
  # Simulacion (e,V) + C
  eigen_aux <- eigen(solve(Lambda.sim[m,,]))
  e.sim[m,] <- eigen_aux$values
  V.sim[m,,] <- eigen_aux$vectors
  C.sim[m,,] <- X %*% V.sim[m,,]
}


hist(e.sim[,1])
summary(e.sim[,2])

hist(C.sim[,1,9])

K <- (tcrossprod(as.matrix(tipo.r)) + 1)^2

sK <- svd(K)
F <- sK$u%*%diag(sK$d)

layout(matrix(c(1,1,2,3),2,2))
plot(F[,1:2])

hist(F[,1],main="",col="lightblue")
hist(F[,2],main="",col="lightblue")

tipo %>% gather(Country, Currency,Canada:Zambia, -Date) %>%
  ggplot(aes(x = Date, y = log(Currency+1), col = Country)) + geom_line()

PC1 = tipo.pca$x %>%data.frame() %>%
  ggplot(aes(x = seq(1:492), y = PC1)) + 
  geom_line() + labs(x = '')
PC2 = tipo.pca$x %>%data.frame() %>%
  ggplot(aes(x = seq(1:492), y = PC2)) + 
  geom_line() + labs(x = '')

PC3 =tipo.pca$x %>%data.frame() %>%
  ggplot(aes(x = seq(1:492), y = PC3)) + 
  geom_line() + labs(x = '')

gridExtra::grid.arrange(PC1,PC2,PC3, ncol = 1)


# EN USD
tipo.r  = 1/tipo[,-1] 

tipo.r$Nicaragua = NULL

tipo.r %>% scale() %>%
  var() %>% data.frame() %>% 
  mutate(v = row.names(.)) %>% 
  gather('var',Canada:Zambia, -v) %>%
  ggplot(aes(x=v, y = var, fill = `Canada:Zambia`)) + 
  geom_tile() + 
  labs(x= '', y = '', fill = 'Covarianza') +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

tipo.r %>%
  cor() %>%
  ggcorrplot()

tipo.pca <- prcomp(tipo.r, scale. = TRUE, center = TRUE)
summary(tipo.pca)

round(tipo.pca$rotation[, 1:3], 2)
biplot(tipo.pca, cex = c(0.4,1))

data = tipo.r

a0 <- 1
s0 <- 1
m0 <- matrix(0,ncol=1,nrow=ncol(data))
B0 <- diag(1,ncol=ncol(data),nrow=ncol(data)) 

gaussian.posterior <- function(data,m0,s0,a0,B0){
  xbar <- as.matrix(colMeans(data))
  S <- cov(data)
  n <- nrow(data)
  p <- ncol(data)
  # Posterior hiperparameters
  sn <- s0 + n
  an <- a0 + n/2
  mn <- (s0*m0 + n*xbar)/sn
  Bn <- B0 + (n/2)*(S + (s0/sn)*(xbar-m0)%*%t(xbar-m0))
  # Output
  output <- list(mn,sn,an,Bn)
  return(output)
}

output <- gaussian.posterior(data,m0,s0,a0,B0)

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
  mu.sim[m,] <- rmvnorm(1, mean=output[[1]], sigma=solve(output[[2]]*Lambda.sim[m,,]))
  # Simulacion (e,V) + C
  eigen_aux <- eigen(solve(Lambda.sim[m,,]))
  e.sim[m,] <- eigen_aux$values
  V.sim[m,,] <- eigen_aux$vectors
  C.sim[m,,] <- X %*% V.sim[m,,]
}


hist(e.sim[,1])
summary(e.sim[,2])

hist(C.sim[,1,9])

K <- (tcrossprod(as.matrix(tipo.r)) + 1)^2

sK <- svd(K)
F <- sK$u%*%diag(sK$d)

layout(matrix(c(1,1,2,3),2,2))
plot(F[,1:2])

hist(F[,1],main="",col="lightblue")
hist(F[,2],main="",col="lightblue")

tipo %>% gather(Country, Currency,Canada:Zambia, -Date) %>%
  ggplot(aes(x = Date, y = log(Currency+1), col = Country)) + geom_line()

PC1 = tipo.pca$x %>%data.frame() %>%
  ggplot(aes(x = seq(1:492), y = PC1)) + 
  geom_line() + labs(x = '')
PC2 = tipo.pca$x %>%data.frame() %>%
  ggplot(aes(x = seq(1:492), y = PC2)) + 
  geom_line() + labs(x = '')

PC3 =tipo.pca$x %>%data.frame() %>%
  ggplot(aes(x = seq(1:492), y = PC3)) + 
  geom_line() + labs(x = '')

gridExtra::grid.arrange(PC1,PC2,PC3, ncol = 1)


#ORIGINALES
# Rendimiento Mensual
tipo.r  = tipo[,-1] %>%
  mutate_all(function(x) as.numeric(log(x/lag(x,1)))) 

nans = which(lapply(tipo.r,function(x) sum(is.nan(x)))!=0)

tipo.r = tipo.r[,-c(nans)]
tipo.r = tipo.r[-c(1),]

tipo.r %>% scale() %>%
  var() %>% data.frame() %>% 
  mutate(v = row.names(.)) %>% 
  gather('var',Canada:Zambia, -v) %>%
  ggplot(aes(x=v, y = var, fill = `Canada:Zambia`)) + 
  geom_tile() + 
  labs(x= '', y = '', fill = 'Covarianza') +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

tipo.r %>%
  cor() %>%
  ggcorrplot()

tipo.pca <- prcomp(tipo.r, scale. = TRUE, center = TRUE)
summary(tipo.pca)

round(tipo.pca$rotation[, 1:3], 2)
biplot(tipo.pca, cex = c(0.4,1))

data = tipo.r

a0 <- 1
s0 <- 1
m0 <- matrix(0,ncol=1,nrow=ncol(data))
B0 <- diag(1,ncol=ncol(data),nrow=ncol(data)) 

gaussian.posterior <- function(data,m0,s0,a0,B0){
  xbar <- as.matrix(colMeans(data))
  S <- cov(data)
  n <- nrow(data)
  p <- ncol(data)
  # Posterior hiperparameters
  sn <- s0 + n
  an <- a0 + n/2
  mn <- (s0*m0 + n*xbar)/sn
  Bn <- B0 + (n/2)*(S + (s0/sn)*(xbar-m0)%*%t(xbar-m0))
  # Output
  output <- list(mn,sn,an,Bn)
  return(output)
}

output <- gaussian.posterior(data,m0,s0,a0,B0)

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
  mu.sim[m,] <- rmvnorm(1, mean=output[[1]], sigma=solve(output[[2]]*Lambda.sim[m,,]))
  # Simulacion (e,V) + C
  eigen_aux <- eigen(solve(Lambda.sim[m,,]))
  e.sim[m,] <- eigen_aux$values
  V.sim[m,,] <- eigen_aux$vectors
  C.sim[m,,] <- X %*% V.sim[m,,]
}


hist(e.sim[,1])
summary(e.sim[,2])

hist(C.sim[,1,9])

K <- (tcrossprod(as.matrix(tipo.r)) + 1)^2

sK <- svd(K)
F <- sK$u%*%diag(sK$d)

layout(matrix(c(1,1,2,3),2,2))
plot(F[,1:2])

hist(F[,1],main="",col="lightblue")
hist(F[,2],main="",col="lightblue")

tipo %>% gather(Country, Currency,Canada:Zambia, -Date) %>%
ggplot(aes(x = Date, y = log(Currency+1), col = Country)) + geom_line()

PC1 = tipo.pca$x %>%data.frame() %>%
  ggplot(aes(x = seq(1:491), y = PC1)) + 
  geom_line() + labs(x = '')
PC2 = tipo.pca$x %>%data.frame() %>%
  ggplot(aes(x = seq(1:491), y = PC2)) + 
  geom_line() + labs(x = '')

PC3 =tipo.pca$x %>%data.frame() %>%
  ggplot(aes(x = seq(1:491), y = PC3)) + 
  geom_line() + labs(x = '')

gridExtra::grid.arrange(PC1,PC2,PC3, ncol = 1)


#ORIGINALES
# Rendimiento YTD
tipo.r  = tipo[,-1] %>%
  mutate_all(function(x) as.numeric(log(x/lag(x,12)))) 

nans = which(lapply(tipo.r,function(x) sum(is.nan(x)))!=0)

tipo.r = tipo.r[,-c(nans)]
tipo.r = tipo.r[-c(1:12),]


tipo.r %>% scale() %>%
  var() %>% data.frame() %>% 
  mutate(v = row.names(.)) %>% 
  gather('var',Canada:Zambia, -v) %>%
  ggplot(aes(x=v, y = var, fill = `Canada:Zambia`)) + 
  geom_tile() + 
  labs(x= '', y = '', fill = 'Covarianza') +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

tipo.r %>%
  cor() %>%
  ggcorrplot()

tipo.pca <- prcomp(tipo.r, scale. = TRUE, center = TRUE)
summary(tipo.pca)

round(tipo.pca$rotation[, 1:3], 2)
biplot(tipo.pca, cex = c(0.4,1))

data = tipo.r

a0 <- 1
s0 <- 1
m0 <- matrix(0,ncol=1,nrow=ncol(data))
B0 <- diag(1,ncol=ncol(data),nrow=ncol(data)) 

gaussian.posterior <- function(data,m0,s0,a0,B0){
  xbar <- as.matrix(colMeans(data))
  S <- cov(data)
  n <- nrow(data)
  p <- ncol(data)
  # Posterior hiperparameters
  sn <- s0 + n
  an <- a0 + n/2
  mn <- (s0*m0 + n*xbar)/sn
  Bn <- B0 + (n/2)*(S + (s0/sn)*(xbar-m0)%*%t(xbar-m0))
  # Output
  output <- list(mn,sn,an,Bn)
  return(output)
}

output <- gaussian.posterior(data,m0,s0,a0,B0)

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
  mu.sim[m,] <- rmvnorm(1, mean=output[[1]], sigma=solve(output[[2]]*Lambda.sim[m,,]))
  # Simulacion (e,V) + C
  eigen_aux <- eigen(solve(Lambda.sim[m,,]))
  e.sim[m,] <- eigen_aux$values
  V.sim[m,,] <- eigen_aux$vectors
  C.sim[m,,] <- X %*% V.sim[m,,]
}


hist(e.sim[,1])
summary(e.sim[,2])

hist(C.sim[,1,9])

K <- (tcrossprod(as.matrix(tipo.r)) + 1)^2

sK <- svd(K)
F <- sK$u%*%diag(sK$d)

layout(matrix(c(1,1,2,3),2,2))
plot(F[,1:2])

hist(F[,1],main="",col="lightblue")
hist(F[,2],main="",col="lightblue")

tipo %>% gather(Country, Currency,Canada:Zambia, -Date) %>%
  ggplot(aes(x = Date, y = log(Currency+1), col = Country)) + geom_line()

PC1 = tipo.pca$x %>%data.frame() %>%
  ggplot(aes(x = seq(1:480), y = PC1)) + 
  geom_line() + labs(x = '')
PC2 = tipo.pca$x %>%data.frame() %>%
  ggplot(aes(x = seq(1:480), y = PC2)) + 
  geom_line() + labs(x = '')

PC3 =tipo.pca$x %>%data.frame() %>%
  ggplot(aes(x = seq(1:480), y = PC3)) + 
  geom_line() + labs(x = '')

gridExtra::grid.arrange(PC1,PC2,PC3, ncol = 1)
