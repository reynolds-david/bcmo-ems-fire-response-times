# EMS and fire response times exploratory analysis
# David Reynolds

# Read in the data
library(readr)
ems_fire <- read_csv("ems_fire.csv", col_types = "cfTTTTTTffccnnnncnnnn")

# Convert the responses to numeric
time_differences <- c("call_dispatch", "dispatch_enroute", "enroute_arrive", "call_arrive")
ems_fire[, time_differences] <- lapply(ems_fire[, time_differences], as.numeric)

# Take the log of the responses
ems_fire[, time_differences] <- lapply(ems_fire[, time_differences], log)

# Plot all points
library(ggplot2)
ggplot(data = ems_fire, aes(x = lon, y = lat)) + geom_point()

# Take a random sample of calls
set.seed(1)
ems_fire_sample <- ems_fire[sample(nrow(ems_fire), 500), ]

# Plot points for call_dispatch
ggplot(data = ems_fire_sample, aes(x = lon, y = lat, color = call_dispatch)) + geom_point()
  
# Plot points for dispatch_enroute
ggplot(data = ems_fire_sample, aes(x = lon, y = lat, color = dispatch_enroute)) + geom_point()

# Plot points for enroute_arrive
ggplot(data = ems_fire_sample, aes(x = lon, y = lat, color = enroute_arrive)) + geom_point()

# Plot points for call_arrive
ggplot(data = ems_fire_sample, aes(x = lon, y = lat, color = call_arrive)) + geom_point()

### Contour plots

## call_arrive

# Interpolation
library(akima)
call_arrive <- with(ems_fire, interp(x = lon, y = lat, z = call_arrive, duplicate = "median"))

# Change data to long format
library(reshape2)
call_arrive_melt <- melt(call_arrive$z, na.rm = TRUE)
names(call_arrive_melt) <- c("x", "y", call_arrive)

# Continue
