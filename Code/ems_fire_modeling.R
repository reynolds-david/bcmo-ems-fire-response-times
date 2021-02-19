# EMS and fire response times modeling
# David Reynolds

# Read in the data
library(readr)
ems_fire <- read_csv("ems_fire.csv", col_types = "cfTTTTTTffccnnnnncnnnn")

# Filter for 2016/2017/2018 and EMS calls only
library(dplyr)
ems <- ems_fire %>% 
  filter(service == "EMS", year != 2019)

## Create 15-minute time windows

# Create month_year
library(stringr)
ems$month_year <- as.factor(paste(str_sub(ems$date, 1, 2), str_sub(ems$date, 7, 10), sep = "/"))

# Convert call to POSIXct
library(lubridate)
ems$call <- ymd_hms(ems$call)

# Cut call by 15-minute intervals
ems$window <- cut(ems$call, breaks = "15 min")

# Count the number of calls in each window
ems_windows <- count(ems, month_year, window)
ems_windows$window <- as.character(ems_windows$window)
ems_windows$window <- str_sub(ems_windows$window, 12, 19)
ems_windows <- count(ems_windows, month_year, window)

# Specify the time intervals as the columns
library(tidyr)
ems_windows <- spread(ems_windows, window, n)
ems_windows <- as.data.frame(ems_windows)
rownames(ems_windows) <- ems_windows$month_year
ems_windows <- ems_windows[, -1]
ems_windows[is.na(ems_windows)] <- 0

## Create a grid for Boone County

# Create Boone County polygon
library(maps)
library(splancs)
boone <- map("county", "missouri,boone", fill = TRUE, col = "transparent", plot = FALSE)
boone <- as.points(boone$x, boone$y)

# Generate centroids inside Boone County
ems_grid <- gridpts(boone, npts = 1000)

# Plot the Boone County polygon and the centroids
pointmap(ems_grid)
polymap(boone, add = TRUE)

# Convert lat and lon to points
ems_points <- as.points(ems$lon, ems$lat)

# Plot the Boone County polygon and the location of calls
pointmap(ems_points)
polymap(boone, add = TRUE)

# Assign each call to its nearest centroid
library(spatstat)
ems_ppp_window1 <- owin(xrange = c(min(ems$lon), max(ems$lon)), yrange = c(min(ems$lat), max(ems$lat)))
ems_points_ppp <- ppp(ems$lon, ems$lat, ems_ppp_window1)
ems_grid_ppp <- as.matrix(ems_grid)
ems_ppp_window2 <- owin(xrange = c(min(ems_grid_ppp[,1]), max(ems_grid_ppp[,1])), yrange = c(min(ems_grid_ppp[,2]), max(ems_grid_ppp[,2])))
ems_grid_ppp <- ppp(ems_grid_ppp[,1], ems_grid_ppp[,2], ems_ppp_window2)
nn <- nncross(ems_points_ppp, ems_grid_ppp)
ems$cell <- nn$which

# Count the number of calls in each cell
ems_cells <- count(ems, month_year, cell)
ems_cells <- spread(ems_cells, cell, n)
ems_cells <- as.data.frame(ems_cells)
ems_cells_rownames <- ems_cells$month_year
ems_cells <- ems_cells[, -1]
ems_cells[is.na(ems_cells)] <- 0

# Make sure cells with no calls are included
cols <- as.numeric(colnames(ems_cells))
ems_cells2 <- matrix(NA, nrow = nrow(ems_cells), ncol = 1000)
j <- 1
for (i in 1:1000) {
  if (i %in% cols) {
    ems_cells2[,i] <- ems_cells[,j]
    j <- j + 1
  } else {
    ems_cells2[,i] <- rep(0, nrow(ems_cells))
  }
}

ems_cells <- ems_cells2
rownames(ems_cells) <- ems_cells_rownames

## Perform PCA

# On the time windows
time_pca <- prcomp(ems_windows, scale = TRUE)
time_pca_loadings <- time_pca$rotation

# On the cells
grid_pca <- prcomp(ems_cells, scale = TRUE)
grid_pca_loadings <- grid_pca$rotation

## Perform NMF
library(NMF)

# On the time windows
time_nmf <- nmf(t(ems_windows), 5, method = "lee")
time_nmf_w <- basis(time_nmf)

# On the cells
grid_nmf <- nmf(t(ems_cells), 5, method = "lee")
grid_nmf_w <- basis(grid_nmf)

## Fit the model

# Par: beta1, beta2, alpha0, alpha1, alpha2, lambda0
library(nimble)
code <- nimbleCode({
  beta1 ~ dnorm(0, sd = 10)
  beta2 ~ dnorm(0, sd = 10)
  xi ~ dnorm(0, sd = 10)
  alpha0 ~ dnorm(0, sd = 10)
  alpha1 ~ dnorm(0, sd = 10)
  alpha2 ~ dnorm(0, sd = 10)
  lambda0 ~ dgamma(0.01, 0.01)
  
  for (i in 1:ncol(ems_cells)) {
      # 0.02: Integration
      lambda_D[i] <- lambda0 * exp(beta1 * x1[i] + 
                                        beta2 * x2[i] + beta3 * x3[i] +...)
  }
  s_ll <- mean(lambda_D[1:100, 1:100])
  for (i in 1:N) {
    lambda[i] <- lambda0 * exp(beta1 * x1_star[i] + beta2 * x2_star[i] + ...)
    mark_mean[i] <- alpha0 + xi * lambda[i] / 1000 + alpha1 * z1_star[i] + alpha2 * z2_star[i] + ...
    logmark[i] <- -mark[i] * log(1+exp(-mark_logit[i])) + 
      (1 - mark[i]) * (-mark_logit[i] - log(1+exp(-mark_logit[i])))
  }
  ll_m <- sum(logmark[1:N])
  log_ll <- sum(log(lambda[1:N]))
})

# Define log-likelihood
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

constants <- list(N = nrow(ems))

data <- list(
  mark = ems$call_arrive,
  z1 = grid_pca_loadings[,1],
  z2 = time_pca_loadings[,2],
  z3 = 
  z4 =
  z5 = 
)

# Initial values for MCMC sampling
inits <- list(beta1 = 0, beta2 = 0, lambda0 = 1, xi = 0, alpha0 = 0, 
              alpha1 = 0, alpha2 = 0)
Rmodel <- nimbleModel(code = code, constants = constants, data = data, 
                      inits = inits, check = FALSE)
RllFun<-llFun(Rmodel)
mcmcConf <- configureMCMC(Rmodel,nodes=NULL)

# Loglikelihood controls MH algorithm
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

# Compile command
Rmcmc <- buildMCMC(mcmcConf)
Cmodel<-compileNimble(Rmodel)
Cmcmc <- compileNimble(Rmcmc, project = Rmodel)

# MCMC run
Cmcmc$run(20000)

# Get MCMC samples
samples <- as.matrix(Cmcmc$mvSamples)[10001:20000, ]
