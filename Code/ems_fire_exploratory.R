# EMS and fire response times exploratory analysis
# David Reynolds

# Read in the data
library(readr)
ems_fire <- read_csv("ems_fire.csv", col_types = "cfTTTTTTffccnnnnncnnnn")

# Filter for after 1/31/2018 and EMS calls only
library(dplyr)
library(lubridate)
ems_fire <- ems_fire %>% 
  filter(service == "EMS", call >= ymd_hms("2018-01-31 00:00:00"))

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

## call_dispatch

# Interpolation
library(akima)
call_dispatch <- with(ems_fire, interp(x = lon, y = lat, z = call_dispatch, duplicate = "median"))

# Change data to long format
library(reshape2)
call_dispatch_melt <- melt(call_dispatch$z, na.rm = TRUE)
names(call_dispatch_melt) <- c("x", "y", "call_dispatch")
call_dispatch_melt$lon <- call_dispatch$x[call_dispatch_melt$x]
call_dispatch_melt$lat <- call_dispatch$y[call_dispatch_melt$y]

# Generate plots using plot and ggplot
filled.contour(x = call_dispatch$x, y = call_dispatch$y, z = call_dispatch$z, 
               color.palette = colorRampPalette(c("white", "blue")), xlab = "Longitude", ylab = "Latitude", 
               main = "Time from call to dispatch", key.title = title(main = "Time (m)", 
                                                                     cex.main = 1))

ggplot(data = call_dispatch_melt, aes(x = lon, y = lat, z = call_dispatch)) +
  geom_tile(aes(fill = call_dispatch)) +
  stat_contour() +
  ggtitle("Time from call to dispatch") +
  xlab("Longitude") +
  ylab("Latitude") +
  scale_fill_continuous(name = "Log of time (minutes)", low = "white", high = "blue") +
  theme(plot.title = element_text(size = 25, face = "bold"),
        legend.title = element_text(size = 15),
        axis.text = element_text(size = 15),
        axis.title.x = element_text(size = 20, vjust = -0.5),
        axis.title.y = element_text(size = 20, vjust = 0.2),
        legend.text = element_text(size = 10))

## dispatch_enroute

# Interpolation
dispatch_enroute <- with(ems_fire, interp(x = lon, y = lat, z = dispatch_enroute, duplicate = "median"))

# Change data to long format
dispatch_enroute_melt <- melt(dispatch_enroute$z, na.rm = TRUE)
names(dispatch_enroute_melt) <- c("x", "y", "dispatch_enroute")
dispatch_enroute_melt$lon <- dispatch_enroute$x[dispatch_enroute_melt$x]
dispatch_enroute_melt$lat <- dispatch_enroute$y[dispatch_enroute_melt$y]

# Generate plots using plot and ggplot
filled.contour(x = dispatch_enroute$x, y = dispatch_enroute$y, z = dispatch_enroute$z, 
               color.palette = colorRampPalette(c("white", "blue")), xlab = "Longitude", ylab = "Latitude", 
               main = "Time from dispatch to enroute", cex.main = 0.75, key.title = title(main = "Time (m)", 
                                                                     cex.main = 1))

ggplot(data = dispatch_enroute_melt, aes(x = lon, y = lat, z = dispatch_enroute)) +
  geom_tile(aes(fill = dispatch_enroute)) +
  stat_contour() +
  ggtitle("Time from dispatch to enroute") +
  xlab("Longitude") +
  ylab("Latitude") +
  scale_fill_continuous(name = "Log of time (minutes)", low = "white", high = "blue") +
  theme(plot.title = element_text(size = 25, face = "bold"),
        legend.title = element_text(size = 15),
        axis.text = element_text(size = 15),
        axis.title.x = element_text(size = 20, vjust = -0.5),
        axis.title.y = element_text(size = 20, vjust = 0.2),
        legend.text = element_text(size = 10))

## enroute_arrive

# Interpolation
enroute_arrive <- with(ems_fire, interp(x = lon, y = lat, z = enroute_arrive, duplicate = "median"))

# Change data to long format
enroute_arrive_melt <- melt(enroute_arrive$z, na.rm = TRUE)
names(enroute_arrive_melt) <- c("x", "y", "enroute_arrive")
enroute_arrive_melt$lon <- enroute_arrive$x[enroute_arrive_melt$x]
enroute_arrive_melt$lat <- enroute_arrive$y[enroute_arrive_melt$y]

# Generate plots using plot and ggplot
filled.contour(x = enroute_arrive$x, y = enroute_arrive$y, z = enroute_arrive$z, 
               color.palette = colorRampPalette(c("white", "blue")), xlab = "Longitude", ylab = "Latitude", 
               main = "Time from enroute to arrival", cex.main = 0.75, key.title = title(main = "Time (m)", 
                                                                                          cex.main = 1))

ggplot(data = enroute_arrive_melt, aes(x = lon, y = lat, z = enroute_arrive)) +
  geom_tile(aes(fill = enroute_arrive)) +
  stat_contour() +
  ggtitle("Time from enroute to arrival") +
  xlab("Longitude") +
  ylab("Latitude") +
  scale_fill_continuous(name = "Log of time (minutes)", low = "white", high = "blue") +
  theme(plot.title = element_text(size = 25, face = "bold"),
        legend.title = element_text(size = 15),
        axis.text = element_text(size = 15),
        axis.title.x = element_text(size = 20, vjust = -0.5),
        axis.title.y = element_text(size = 20, vjust = 0.2),
        legend.text = element_text(size = 10))

## dispatch_arrive

# Interpolation
dispatch_arrive <- with(ems_fire, interp(x = lon, y = lat, z = dispatch_arrive, duplicate = "median"))

# Change data to long format
dispatch_arrive_melt <- melt(dispatch_arrive$z, na.rm = TRUE)
names(dispatch_arrive_melt) <- c("x", "y", "dispatch_arrive")
dispatch_arrive_melt$lon <- dispatch_arrive$x[dispatch_arrive_melt$x]
dispatch_arrive_melt$lat <- dispatch_arrive$y[dispatch_arrive_melt$y]

# Generate plots using plot and ggplot
filled.contour(x = dispatch_arrive$x, y = dispatch_arrive$y, z = dispatch_arrive$z, 
               color.palette = colorRampPalette(c("white", "blue")), xlab = "Longitude", ylab = "Latitude", 
               main = "Time from dispatch to arrival", key.title = title(main = "Time (m)", 
                                                                     cex.main = 1))

ggplot(data = dispatch_arrive_melt, aes(x = lon, y = lat, z = dispatch_arrive)) +
  geom_tile(aes(fill = dispatch_arrive)) +
  stat_contour() +
  ggtitle("Time from dispatch to arrival") +
  xlab("Longitude") +
  ylab("Latitude") +
  scale_fill_continuous(name = "Log of time (minutes)", low = "white", high = "blue") +
  theme(plot.title = element_text(size = 25, face = "bold"),
        legend.title = element_text(size = 15),
        axis.text = element_text(size = 15),
        axis.title.x = element_text(size = 20, vjust = -0.5),
        axis.title.y = element_text(size = 20, vjust = 0.2),
        legend.text = element_text(size = 10))

## call_arrive

# Interpolation
call_arrive <- with(ems_fire, interp(x = lon, y = lat, z = call_arrive, duplicate = "median"))

# Change data to long format
call_arrive_melt <- melt(call_arrive$z, na.rm = TRUE)
names(call_arrive_melt) <- c("x", "y", "call_arrive")
call_arrive_melt$lon <- call_arrive$x[call_arrive_melt$x]
call_arrive_melt$lat <- call_arrive$y[call_arrive_melt$y]

# Generate plots using plot and ggplot
filled.contour(x = call_arrive$x, y = call_arrive$y, z = call_arrive$z, 
               color.palette = colorRampPalette(c("white", "blue")), xlab = "Longitude", ylab = "Latitude", 
               main = "Time from call to arrival", key.title = title(main = "Time (m)", 
                                                                    cex.main = 1))

ggplot(data = call_arrive_melt, aes(x = lon, y = lat, z = call_arrive)) +
  geom_tile(aes(fill = call_arrive)) +
  stat_contour() +
  ggtitle("Time from call to arrival") +
  xlab("Longitude") +
  ylab("Latitude") +
  scale_fill_continuous(name = "Log of time (minutes)", low = "white", high = "blue") +
  theme(plot.title = element_text(size = 25, face = "bold"),
        legend.title = element_text(size = 15),
        axis.text = element_text(size = 15),
        axis.title.x = element_text(size = 20, vjust = -0.5),
        axis.title.y = element_text(size = 20, vjust = 0.2),
        legend.text = element_text(size = 10))
