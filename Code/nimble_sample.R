require(nimble)
## par: beta1, beta2, alpha0, alpha1, alpha2, lambda0
code <- nimbleCode({
  beta1 ~ dnorm(0, sd = 10)
  beta2 ~ dnorm(0, sd = 10)
  xi ~ dnorm(0, sd = 10)
  alpha0 ~ dnorm(0, sd = 10)
  alpha1 ~ dnorm(0, sd = 10)
  alpha2 ~ dnorm(0, sd = 10)
  lambda0 ~ dgamma(0.01, 0.01)
  
  for (i in 1:100) {
    for (j in 1:100) {
      ## 0.02: since true data window is [-1, 1]
      lambda_D[i, j] <- lambda0 * exp(beta1 * 0.02 * (i - 50) + 
                                        beta2 * 0.02 * (j - 50))
    }
  }
  s_ll <- 4 * mean(lambda_D[1:100, 1:100])
  for (i in 1:N) {
    lambda[i] <- lambda0 * exp(beta1 * x[i] + beta2 * y[i])
    mark_logit[i] <- alpha0 + xi * lambda[i] / 1000 + alpha1 * z1[i] + alpha2 * z2[i]
    logmark[i] <- -mark[i] * log(1+exp(-mark_logit[i])) + 
      (1 - mark[i]) * (-mark_logit[i] - log(1+exp(-mark_logit[i])))
  }
  ll_m <- sum(logmark[1:N])
  log_ll <- sum(log(lambda[1:N]))
  
})

##' define log-likelihood
llFun <- nimbleFunction(
  setup <- function(model) {},
  
  run <- function() {
    ll_m <- model$ll_m
    s_ll <- model$s_ll
    log_ll <- model$log_ll
    ll <- ll_m - s_ll + log_ll
    returnType(double())
    return(ll[1])
  }
)


constants <- list(N = length(pp_iten))

data <- list(
  x = simudata$x,
  y = simudata$y,
  mark = simudata$mark,
  z1 = simudata$z1,
  z2 = simudata$z2
)

## initial value of MCMC sampling
inits <- list(beta1 = 0, beta2 = 0, lambda0 = 1, xi = 0, alpha0 = 0, 
              alpha1 = 0, alpha2 = 0)
Rmodel <- nimbleModel(code = code, constants = constants, data = data, 
                      inits = inits, check = FALSE)
RllFun<-llFun(Rmodel)
mcmcConf <- configureMCMC(Rmodel,nodes=NULL)
## loglikelihood control MH algorithm
mcmcConf$addSampler(target = 'beta1', type = 'RW_llFunction', 
                    control = list(llFunction = RllFun, includesTarget = FALSE))
mcmcConf$addSampler(target = 'beta2', type = 'RW_llFunction', 
                    control = list(llFunction = RllFun, includesTarget = FALSE))
mcmcConf$addSampler(target = 'lambda0', type = 'RW_llFunction', 
                    control = list(llFunction = RllFun, includesTarget = FALSE))
mcmcConf$addSampler(target = 'xi', type = 'RW_llFunction', 
                    control = list(llFunction = RllFun, includesTarget = FALSE))
mcmcConf$addSampler(target = 'alpha0', type = 'RW_llFunction', 
                    control = list(llFunction = RllFun, includesTarget = FALSE))
mcmcConf$addSampler(target = 'alpha1', type = 'RW_llFunction', 
                    control = list(llFunction = RllFun, includesTarget = FALSE))
mcmcConf$addSampler(target = 'alpha2', type = 'RW_llFunction', 
                    control = list(llFunction = RllFun, includesTarget = FALSE))
## Complie Command
Rmcmc <- buildMCMC(mcmcConf)
Cmodel<-compileNimble(Rmodel)
Cmcmc <- compileNimble(Rmcmc, project = Rmodel)
## MCMC run
Cmcmc$run(20000)
## get MCMC sample
samples <- as.matrix(Cmcmc$mvSamples)[10001:20000, ]
