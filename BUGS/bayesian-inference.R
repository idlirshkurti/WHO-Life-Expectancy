# ------------- Model Fitting Using Bayesian Inference ------------------- #

n<-length(full2$Country)
J<-length(unique(full$Country))

# We use the saved JAGS model J1.jags

full2$Country<-as.factor(full2$Country)
full2$Year<-as.factor(full2$Year)

r1.inits <- list(beta = rnorm(J), mu = rnorm(1),
sigma = runif(1), sigma.b = runif(1))
r1.jags <- jags.model("J1.bug",data=full,inits = r1.inits)
r1.vars <- c("beta", "mu", "sigma", "sigma.b")
r1.sim <- coda.samples(r1.jags, r1.vars, n.iter = 10000)
summary(r1.sim)

# Trace-plots and density-plots for parameters
xyplot(r1.sim[, c(195:197)])
densityplot(r1.sim[, c(195:197)])

# HPD intervals
HPDinterval(r1.sim)

###### model34 from above #######

C <- 5 # Number of chains
N <- 1000000 # Number of iterations
B = 500000 # Burn-ins
I = 1 # Thinning parameter

r3.inits <- list(beta = rnorm(194), alpha1 = rnorm(1),
alpha2 = rnorm(1), alpha3 = rnorm(1),
alpha4 = rnorm(1), alpha5 = rnorm(1), mu1 = rnorm(1),
mu2 = rnorm(1), mu3 = rnorm(1), mu = rnorm(1),
sigma = runif(1), sigma.b = runif(1),sigma.b1 = runif(1),
sigma.b2 = runif(1), sigma.b3 = runif(1))

# We use the saved JAGS model J1.jags
r3.jags <- jags.model("J3.bug",data=na.omit(full2),inits = r3.inits)
r3.vars <- c("beta","alpha1","alpha2","alpha3","alpha4","alpha5","mu1","mu2","mu3","mu",
"sigma", "sigma.b", "sigma.b1", "sigma.b2", "sigma.b3")

r3.sim <- coda.samples(r3.jags, r3.vars, n.iter = N,thin=I, n.burnin=B)
summary(r3.sim)

# Trace-plots and density-plots for parameters
xyplot(r3.sim[,195],
xlim=c(700000,750000))
densityplot(r2.sim[, c(195:197)])

# HPD intervals
HPDinterval(r3.sim)
