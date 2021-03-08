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

## Generate B-spline basis matrix
library(splines)
time_basis <- bs(1:96, 5)

## Perform NMF
library(NMF)

# On the time windows
time_nmf <- nmf(t(ems_windows), 5, method = "lee")
time_nmf_w <- basis(time_nmf)

# On the cells
grid_nmf <- nmf(t(ems_cells), 5, method = "lee")
grid_nmf_w <- basis(grid_nmf)

## Plot the NMF bases over time
library(dplyr)
library(ggplot2)
library(reshape2)

# Melt time_nmf_w by time
time_nmf_w_melt <- as.data.frame(time_nmf_w)
time_nmf_w_melt$time <- rownames(time_nmf_w_melt)
time_nmf_w_melt <- melt(time_nmf_w_melt, "time")

# Recode variable
time_nmf_w_melt$variable <- as.character(time_nmf_w_melt$variable)
time_nmf_w_melt$variable <- recode(time_nmf_w_melt$variable, V1 = "1", V2 = "2", V3 = "3", V4 = "4", V5 = "5")
time_nmf_w_melt$variable <- as.numeric(time_nmf_w_melt$variable)

# Plot the bases
ggplot(data = time_nmf_w_melt, aes(x = time, y = variable, fill = value)) + geom_tile() + 
  labs(title = "NMF bases over time", x = "Time", y = "Basis", fill = "Value") + theme_minimal() + 
  theme(axis.text.x = element_blank(), axis.title.x = element_text(face = "bold", size = 15), 
        axis.title.y = element_text(face = "bold", size = 15), plot.title = element_text(face = "bold", size = 25),
        legend.title = element_text(face = "bold", size = 15), panel.grid.major = element_blank()) +
  scale_fill_gradient(low = "white", high = "red")

## Plot the NMF bases over space

## Define plot_basis
grid_points <- as.points(ems_grid)
plot_basis <- function(x) {
  nmf_basis_space <- as.data.frame(cbind(ems_grid, x))
  nmf_basis_space <- nmf_basis_space %>% 
    select("lon" = V1, "lat" = V2, "value" = x)
  ggplot() + geom_polygon(data = as.data.frame(boone), aes(x = V1, y = V2), color = "deepskyblue3", fill = "white") +
    geom_tile(data = nmf_basis_space, aes(x = lon, y = lat, fill = value))
}

ggplot(data = nmf_basis_space, aes(x = lon, y = lat, z = value)) + stat_contour(geom = "polygon", aes(fill = ..level..))

plot_basis(grid_nmf_w[,1])
plot_basis(grid_nmf_w[,2])
plot_basis(grid_nmf_w[,3])
plot_basis(grid_nmf_w[,4])
plot_basis(grid_nmf_w[,5])

## Obtain x and z for 2019

# Read in 2019 data
ems_2019 <- read_csv("ems_fire.csv", col_types = "cfTTTTTTffccnnnnncnnnn")
ems_2019 <- ems_2019 %>% 
  filter(service == "EMS", year == 2019)

# Create month_year
ems_2019$month_year <- as.factor(paste(str_sub(ems_2019$date, 1, 2), str_sub(ems_2019$date, 7, 10), sep = "/"))

# Convert call to POSIXct
ems_2019$call <- ymd_hms(ems_2019$call)

# Cut call by 15-minute intervals
ems_2019$window <- cut(ems_2019$call, breaks = "15 min")

# Assign each call to its nearest centroid
ems_ppp_window1 <- owin(xrange = c(min(ems_2019$lon), max(ems_2019$lon)), yrange = c(min(ems_2019$lat), max(ems_2019$lat)))
ems_points_ppp <- ppp(ems_2019$lon, ems_2019$lat, ems_ppp_window1)
nn <- nncross(ems_points_ppp, ems_grid_ppp)
ems_2019$cell <- nn$which

# Obtain x and z
z <- str_sub(as.character(ems_2019$window), 12, 19)
z <- as.data.frame(z)
z <- z %>% 
  select("time" = z)
time_nmf <- as.data.frame(time_nmf_w)
time_nmf$time <- rownames(time_nmf)
z <- left_join(x = z, y = time_nmf, by = "time")
z$time <- NULL

x <- ems_2019$cell
x <- as.data.frame(x)
x <- x %>% 
  select("cell" = x)
x$cell <- as.character(x$cell)
grid_nmf <- as.data.frame(grid_nmf_w)
grid_nmf$cell <- rownames(grid_nmf)
x <- left_join(x = x, y = grid_nmf, by = "cell")
x$cell <- NULL

## Obtain z_basis
z_basis <- str_sub(as.character(ems_2019$window), 12, 19)
z_basis <- as.data.frame(z_basis)
z_basis <- z_basis %>% 
  select("time" = z_basis)
time_basis <- as.data.frame(time_basis)
time_basis$time <- rownames(time_nmf)
z_basis <- left_join(x = z_basis, y = time_basis, by = "time")
z_basis$time <- NULL

## Fit the model

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
  sigma2 ~ dinvgamma(0.1, 0.1)
  
# Perform integration 
  for (i in 1:nrow(grid_nmf)) {
      lambda_D[i] <- lambda0 * exp(beta1 * grid_nmf_w[i,1] + beta2 * grid_nmf_w[i,2] + beta3 * grid_nmf_w[i,3] + 
                                     beta4 * grid_nmf_w[i,4] + beta5 * grid_nmf_w[i,5])
  }
  s_ll <- mean(lambda_D[1:247])
  for (i in 1:N) {
    lambda[i] <- lambda0 * exp(beta1 * x1[i] + beta2 * x2[i] + beta3 * x3[i] + 
                                 beta4 * x4[i] + beta5 * x5[i])
    mark_mean[i] <- alpha0 + xi * lambda[i]/10000 + alpha1 * z1[i] + alpha2 * z2[i] + 
      alpha3 * z3[i] + alpha4 * z4[i] + alpha5 * z5[i]
    logmark[i] <- -0.5 * log(2 * 3.14) - 0.5 * log(sigma2) - (mark[i] - mark_mean[i])^2 / (2 * sigma2)
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

constants <- list(N = nrow(x))

data <- list(
  grid_nmf_w = grid_nmf_w,
  mark = ems_2019$call_arrive,
  x1 = x[,1],
  x2 = x[,2],
  x3 = x[,3],
  x4 = x[,4],
  x5 = x[,5],
  z1 = z[,1],
  z2 = z[,2],
  z3 = z[,3],
  z4 = z[,4],
  z5 = z[,5]
)

# Specify initial values
inits <- list(alpha0 = 0, alpha1 = 0, alpha2 = 0, alpha3 = 0, alpha4 = 0, alpha5 = 0, 
              beta1 = 0, beta2 = 0, beta3 = 0, beta4 = 0, beta5 = 0, xi = 0, lambda0 = 1, sigma2 = 1)
Rmodel <- nimbleModel(code = code, constants = constants, data = data, inits = inits, check = FALSE)
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
mcmcConf$addSampler(target = 'sigma2', type = 'RW_llFunction', 
                    control = list(llFunction = RllFun, includesTarget = FALSE))

# Compile
Rmcmc <- buildMCMC(mcmcConf)
Cmodel<-compileNimble(Rmodel)
Cmcmc <- compileNimble(Rmcmc, project = Rmodel)

# Run the sampler
Cmcmc$run(10000)

# Get MCMC samples
samples <- as.matrix(Cmcmc$mvSamples)[5001:10000,]
samples <- samples[seq(1, nrow(samples), by = 10),]

### Perform MCMC diagnostics
library(coda)

## Convert all the columns of samples into an MCMC object
alpha0_mcmc <- as.mcmc(samples[,1])
alpha1_mcmc <- as.mcmc(samples[,2])
alpha2_mcmc <- as.mcmc(samples[,3])
alpha3_mcmc <- as.mcmc(samples[,4])
alpha4_mcmc <- as.mcmc(samples[,5])
alpha5_mcmc <- as.mcmc(samples[,6])
beta1_mcmc <- as.mcmc(samples[,7])
beta2_mcmc <- as.mcmc(samples[,8])
beta3_mcmc <- as.mcmc(samples[,9])
beta4_mcmc <- as.mcmc(samples[,10])
beta5_mcmc <- as.mcmc(samples[,11])
lambda0_mcmc <- as.mcmc(samples[,12])
sigma2_mcmc <- as.mcmc(samples[,13])
xi_mcmc <- as.mcmc(samples[,14])

## Perform diagnostics

# Trace plots
traceplot(alpha0_mcmc)
traceplot(alpha1_mcmc)
traceplot(alpha2_mcmc)
traceplot(alpha3_mcmc)
traceplot(alpha4_mcmc)
traceplot(alpha5_mcmc)
traceplot(beta1_mcmc)
traceplot(beta2_mcmc)
traceplot(beta3_mcmc)
traceplot(beta4_mcmc)
traceplot(beta5_mcmc)
traceplot(lambda0_mcmc)
traceplot(sigma2_mcmc)
traceplot(xi_mcmc)

# Autocorrelation plots
autocorr.plot(alpha0_mcmc)
autocorr.plot(alpha1_mcmc)
autocorr.plot(alpha2_mcmc)
autocorr.plot(alpha3_mcmc)
autocorr.plot(alpha4_mcmc)
autocorr.plot(alpha5_mcmc)
autocorr.plot(beta1_mcmc)
autocorr.plot(beta2_mcmc)
autocorr.plot(beta3_mcmc)
autocorr.plot(beta4_mcmc)
autocorr.plot(beta5_mcmc)
autocorr.plot(lambda0_mcmc)
autocorr.plot(sigma2_mcmc)
autocorr.plot(xi_mcmc)

# Effective sample size
effectiveSize(alpha0_mcmc)
effectiveSize(alpha1_mcmc)
effectiveSize(alpha2_mcmc)
effectiveSize(alpha3_mcmc)
effectiveSize(alpha4_mcmc)
effectiveSize(alpha5_mcmc)
effectiveSize(beta1_mcmc)
effectiveSize(beta2_mcmc)
effectiveSize(beta3_mcmc)
effectiveSize(beta4_mcmc)
effectiveSize(beta5_mcmc)
effectiveSize(lambda0_mcmc)
effectiveSize(sigma2_mcmc)
effectiveSize(xi_mcmc)

# Geweke diagnostic
geweke.diag(alpha0_mcmc)
geweke.diag(alpha1_mcmc)
geweke.diag(alpha2_mcmc)
geweke.diag(alpha3_mcmc)
geweke.diag(alpha4_mcmc)
geweke.diag(alpha5_mcmc)
geweke.diag(beta1_mcmc)
geweke.diag(beta2_mcmc)
geweke.diag(beta3_mcmc)
geweke.diag(beta4_mcmc)
geweke.diag(beta5_mcmc)
geweke.diag(lambda0_mcmc)
geweke.diag(sigma2_mcmc)
geweke.diag(xi_mcmc)

# Plot histograms for the parameters
ggplot(data = as.data.frame(alpha0_mcmc), aes(x = alpha0_mcmc)) + geom_histogram()
ggplot(data = as.data.frame(alpha1_mcmc), aes(x = alpha1_mcmc)) + geom_histogram()
ggplot(data = as.data.frame(alpha2_mcmc), aes(x = alpha2_mcmc)) + geom_histogram()
ggplot(data = as.data.frame(alpha3_mcmc), aes(x = alpha3_mcmc)) + geom_histogram()
ggplot(data = as.data.frame(alpha4_mcmc), aes(x = alpha4_mcmc)) + geom_histogram()
ggplot(data = as.data.frame(alpha5_mcmc), aes(x = alpha5_mcmc)) + geom_histogram()
ggplot(data = as.data.frame(beta1_mcmc), aes(x = beta1_mcmc)) + geom_histogram()
ggplot(data = as.data.frame(beta2_mcmc), aes(x = beta2_mcmc)) + geom_histogram()
ggplot(data = as.data.frame(beta3_mcmc), aes(x = beta3_mcmc)) + geom_histogram()
ggplot(data = as.data.frame(beta4_mcmc), aes(x = beta4_mcmc)) + geom_histogram()
ggplot(data = as.data.frame(beta5_mcmc), aes(x = beta5_mcmc)) + geom_histogram()
ggplot(data = as.data.frame(lambda0_mcmc), aes(x = lambda0_mcmc)) + geom_histogram()
ggplot(data = as.data.frame(sigma2_mcmc), aes(x = sigma2_mcmc)) + geom_histogram()
ggplot(data = as.data.frame(xi_mcmc), aes(x = xi_mcmc)) + geom_histogram()
