data <- read.csv("est46114_s06_data.csv")
data <- as.data.frame(data)
data <- as.matrix(data)
T <- nrow(data)
p <- ncol(data)
Ty <- T/12
datats <- ts(data[,3:p],start=c(1970, 1), end=c(2010, 12), frequency=12)
years <- seq(1970,2010,1)
meansts <- NA * datats[c(1:41),]
t <- 1
for(t in 1:Ty){
  meansts[t, ] <- colMeans(data[which(data[,"Year"]==years[t]),3:p])
}
# Auxiliares
rep.row<-function(x,n){
  matrix(rep(x,each=n),nrow=n)
}
rep.col<-function(x,n){
  matrix(rep(x,each=n), ncol=n, byrow=TRUE)
}
# ---
datatsc <- datats
t <- 1
for(t in 1:Ty){
  datatsc[which(data[,"Year"]==years[t]),] <- datats[which(data[,"Year"]==years[t]),] - rep.row(meansts[t, ],12)
}
# Data 4 class
datatsc.class <- datatsc[,c("Canada","Mexico")]
datats.class <- datats[,c("Canada","Mexico")]

plot(datats.class)

plot(datatsc.class)

matplot(datatsc.class,
        type = "l",
        ylab = "Response", xlab = "Time")

#--------------

rm(list=ls())

if(!require("rstan")){install.packages("rstan")}
suppressMessages(library("rstan"))

if(!require("bayesdfa")){install.packages("bayesdfa")}
suppressMessages(library("bayesdfa"))

if(!require("factorstochvol")){install.packages("factorstochvol")}
suppressMessages(library("factorstochvol"))

set.seed(33)

data.sim <- sim_dfa(num_trends = 2, 
                    num_years = 500, 
                    num_ts = 3)
ls(data.sim)

data.sim$sigma
data.sim$Z
data.sim$x

data.sim.ts <- ts(t(data.sim$y_sim), start = c(1970,1),frequency = 12)
plot(data.sim.ts)

data.pred.ts <- ts(t(data.sim$pred), start = c(1970,1),frequency = 12)
plot(data.pred.ts)

data.x.ts <- ts(t(data.sim$x), start = c(1970,1),frequency = 12)
plot(data.x.ts)

# Modelos

data.sim.ts.models <- find_dfa_trends(
  y = t(data.sim.ts), iter = 300,
  kmin = 1, kmax = 2, 
  chains = 1, compare_normal = FALSE,
  variance = "equal", convergence_threshold = 1.1,
  control = list(adapt_delta = 0.95, max_treedepth = 20))

data.sim.ts.models$best_model

data.sim.ts.models$summary

# Modelo 1

data.sim.ts.model <- fit_dfa(y = t(data.sim.ts), 
                             iter = 300, chains = 1)

data.sim.ts.model.plot <- plot_fitted(data.sim.ts.model)
print(data.sim.ts.model.plot)

## Modelo 2
#
#data.sim.ts.model <- fit_dfa(y = t(data.sim.ts),
#                             num_trends = 2,
#                             iter = 300, chains = 1)
#
#data.sim.ts.model.plot <- plot_fitted(data.sim.ts.model)
#print(data.sim.ts.model.plot)


# Fitted

y <- sim_dfa(num_trends = 2, num_years = 20, num_ts = 4)
m <- fit_dfa(y = y, num_trends = 2, iter = 200, chains = 1)
p <- plot_fitted(m)
print(p)

