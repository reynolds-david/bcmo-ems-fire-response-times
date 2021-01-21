# EMS and fire response times exploratory analysis
# David Reynolds

# Read in the data
library(readr)
ems_fire <- read_csv("ems_fire.csv", col_types = "cfTTTTTTffccnnnncnnnn")

# Plot all points
library(ggplot2)
ggplot(data = ems_fire, aes(x = lon, y = lat)) + geom_point()

# Take a random sample of calls
set.seed(1)
ems_fire_sample <- ems_fire[sample(nrow(ems_fire), 500), ]

# Convert time difference variables to numeric
time_differences <- c("call_dispatch", "dispatch_enroute", "enroute_arrive", "call_arrive")
ems_fire_sample[, time_differences] <- lapply(ems_fire_sample[, time_differences], as.numeric)

# Plot points for call_dispatch
ggplot(data = ems_fire_sample, aes(x = lat, y = lon, color = call_dispatch)) + geom_point()

# Plot points for dispatch_enroute
ggplot(data = ems_fire_sample, aes(x = lat, y = lon, color = dispatch_enroute)) + geom_point()

# Plot points for enroute_arrive
ggplot(data = ems_fire_sample, aes(x = lat, y = lon, color = enroute_arrive)) + geom_point()

# Plot points for call_arrive
ggplot(data = ems_fire_sample, aes(x = lat, y = lon, color = call_arrive)) + geom_point()
