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
ems_grid <- gridpts(boone, npts = 250)

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
ems_cells2 <- matrix(NA, nrow = nrow(ems_cells), ncol = nrow(ems_grid))
j <- 1
for (i in 1:nrow(ems_grid)) {
  if (i %in% cols) {
    ems_cells2[,i] <- ems_cells[,j]
    j <- j + 1
  } else {
    ems_cells2[,i] <- rep(0, nrow(ems_cells))
  }
}

ems_cells <- ems_cells2
rownames(ems_cells) <- ems_cells_rownames
ems_cells <- replace(ems_cells, ems_cells == 0, 0.001)

## Perform PCA

# On the time windows
time_pca <- prcomp(ems_windows)
time_pca_loadings <- time_pca$rotation

# On the cells
grid_pca <- prcomp(ems_cells)
grid_pca_loadings <- grid_pca$rotation

## Perform NMF
library(NMF)

# On the time windows
time_nmf <- nmf(t(ems_windows), 5, method = "lee")
time_nmf_w <- basis(time_nmf)

# On the cells
grid_nmf <- nmf(t(ems_cells), 5, method = "lee")
grid_nmf_w <- basis(grid_nmf)

# Plot first NMF basis over time
library(ggplot2)
library(scales)
nmf_basis_time <- as.data.frame(cbind(rownames(time_nmf_w), time_nmf_w[,1]))
nmf_basis_time$value <- as.numeric(nmf_basis_time$V2)
nmf_basis_time$time <- as.POSIXct(nmf_basis_time$V1, format = "%H:%M:%S")

ggplot(data = nmf_basis_time, aes(x = time, y = value, group = 1)) + geom_line() + 
  scale_x_datetime(breaks = date_breaks("5 hour"), labels = date_format("%H:%M:%S"))

# Plot the NMF bases over space
library(akima)
library(ggplot2)
library(reshape2)
plot_basis <- function(x) {
  nmf_basis_space <- as.data.frame(cbind(ems_grid, x))
  nmf_basis_space <- nmf_basis_space %>% 
    select("lon" = V1, "lat" = V2, "value" = x)
  nmf_basis_space <- with(nmf_basis_space, interp(x = lon, y = lat, z = value, duplicate = "median"))
  filled.contour(x = nmf_basis_space$x, y = nmf_basis_space$y, z = nmf_basis_space$z, 
                 color.palette = colorRampPalette(c("white", "blue")))
}

plot_basis(grid_nmf_w[,1])
plot_basis(grid_nmf_w[,2])
plot_basis(grid_nmf_w[,3])
plot_basis(grid_nmf_w[,4])
plot_basis(grid_nmf_w[,5])

## Fit the model

# Obtain x and z
z <- str_sub(as.character(ems$window), 12, 19)
z <- as.data.frame(z)
z <- z %>% 
  select("time" = z)
time_nmf <- as.data.frame(time_nmf_w)
time_nmf$time <- rownames(time_nmf)
z <- left_join(x = z, y = time_nmf, by = "time")
z$time <- NULL

x <- ems$cell
x <- as.data.frame(x)
x <- x %>% 
  select("cell" = x)
x$cell <- as.character(x$cell)
grid_nmf <- as.data.frame(grid_nmf_w)
grid_nmf$cell <- rownames(grid_nmf)
x <- left_join(x = x, y = grid_nmf, by = "cell")
x$cell <- NULL

# Initialize the parameters
library(nimble)
code <- nimbleCode({
  alpha0 ~ dnorm(0, sd = 10)
  alpha1 ~ dnorm(0, sd = 10)
  alpha2 ~ dnorm(0, sd = 10)
  alpha3 ~ dnorm(0, sd = 10)
  alpha4 ~ dnorm(0, sd = 10)
  alpha5 ~ dnorm(0, sd = 10)
  beta1 ~ dnorm(0, sd = 10)
  beta2 ~ dnorm(0, sd = 10)
  beta3 ~ dnorm(0, sd = 10)
  beta4 ~ dnorm(0, sd = 10)
  beta5 ~ dnorm(0, sd = 10)
  xi ~ dnorm(0, sd = 10)
  lambda0 ~ dgamma(0.01, 0.01)
  
# Integration 
  for (i in 1:ncol(ems_cells)) {
      lambda_D[i] <- lambda0 * exp(beta1 * x1[i] + beta2 * x2[i] + beta3 * x3[i] + 
                                     beta4 * x4[i] + beta5 * x5[i])
  }
  s_ll <- mean(lambda_D[1:100, 1:100])
  for (i in 1:N) {
    lambda[i] <- lambda0 * exp(beta1 * x1[i] + beta2 * x2[i] + beta3 * x3[i] + 
                                 beta4 * x4[i] + beta5 * x5[i])
    mark_mean[i] <- alpha0 + xi * lambda[i] / 1000 + alpha1 * z1[i] + alpha2 * z2[i] + 
      alpha3 * z3[i] + alpha4 * z4[i] + alpha5 * z5[i]
    logmark[i] <- -mark[i] * log(1+exp(-mark_logit[i])) + 
      (1 - mark[i]) * (-mark_logit[i] - log(1+exp(-mark_logit[i])))
  }
  ll_m <- sum(logmark[1:N])
  log_ll <- sum(log(lambda[1:N]))
})

# Define the log-likelihood
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
  x1 = grid_nmf_w[,1],
  x2 = grid_nmf_w[,2],
  x3 = grid_nmf_w[,3],
  x4 = grid_nmf_w[,4],
  x5 = grid_nmf_w[,5],
  z1 = time_nmf_w[,1],
  z2 = time_nmf_w[,2],
  z3 = time_nmf_w[,3],
  z4 = time_nmf_w[,4],
  z5 = time_nmf_w[,5]
)

# Initial values for MCMC sampling
inits <- list(alpha0 = 0, alpha1 = 0, alpha2 = 0, alpha3 = 0, alpha4 = 0, alpha5 = 0, 
              beta1 = 0, beta2 = 0, beta3 = 0, beta4 = 0, beta5 = 0, xi = 0, lambda0 = 1)
Rmodel <- nimbleModel(code = code, constants = constants, data = data, inits = inits, 
                      check = FALSE)
RllFun <- llFun(Rmodel)
mcmcConf <- configureMCMC(Rmodel, nodes = NULL)

# The log-likelihood controls the MH algorithm
mcmcConf$addSampler(target = 'alpha0', type = 'RW_llFunction', 
                    control = list(llFunction = RllFun, includesTarget = FALSE))
mcmcConf$addSampler(target = 'alpha1', type = 'RW_llFunction', 
                    control = list(llFunction = RllFun, includesTarget = FALSE))
mcmcConf$addSampler(target = 'alpha2', type = 'RW_llFunction', 
                    control = list(llFunction = RllFun, includesTarget = FALSE))
mcmcConf$addSampler(target = 'alpha3', type = 'RW_llFunction', 
                    control = list(llFunction = RllFun, includesTarget = FALSE))
mcmcConf$addSampler(target = 'alpha4', type = 'RW_llFunction', 
                    control = list(llFunction = RllFun, includesTarget = FALSE))
mcmcConf$addSampler(target = 'alpha5', type = 'RW_llFunction', 
                    control = list(llFunction = RllFun, includesTarget = FALSE))
mcmcConf$addSampler(target = 'beta1', type = 'RW_llFunction', 
                    control = list(llFunction = RllFun, includesTarget = FALSE))
mcmcConf$addSampler(target = 'beta2', type = 'RW_llFunction', 
                    control = list(llFunction = RllFun, includesTarget = FALSE))
mcmcConf$addSampler(target = 'beta3', type = 'RW_llFunction', 
                    control = list(llFunction = RllFun, includesTarget = FALSE))
mcmcConf$addSampler(target = 'beta4', type = 'RW_llFunction', 
                    control = list(llFunction = RllFun, includesTarget = FALSE))
mcmcConf$addSampler(target = 'beta5', type = 'RW_llFunction', 
                    control = list(llFunction = RllFun, includesTarget = FALSE))
mcmcConf$addSampler(target = 'xi', type = 'RW_llFunction', 
                    control = list(llFunction = RllFun, includesTarget = FALSE))
mcmcConf$addSampler(target = 'lambda0', type = 'RW_llFunction', 
                    control = list(llFunction = RllFun, includesTarget = FALSE))

# Compile command
Rmcmc <- buildMCMC(mcmcConf)
Cmodel<-compileNimble(Rmodel)
Cmcmc <- compileNimble(Rmcmc, project = Rmodel)

# MCMC run
Cmcmc$run(20000)

# Get MCMC samples
samples <- as.matrix(Cmcmc$mvSamples)[10001:20000, ]
