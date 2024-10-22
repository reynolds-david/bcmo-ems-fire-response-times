# EMS and fire response times data processing
# David Reynolds

# Import the original data
library(readxl)
ems_16 <- read_excel("2016EMS.xls")
fire_16 <- read_excel("2016Fire.xls")
ems_17 <- read_excel("2017EMS.xls")
fire_17 <- read_excel("2017Fire.xls")
ems_18 <- read_excel("2018EMS.xls")
fire_18 <- read_excel("2018Fire.xls")
ems_19 <- read_excel("2019EMS.xls")
fire_19 <- read_excel("2019Fire.xls")

# Combine the data
ems_fire <- rbind(ems_16, fire_16, ems_17, fire_17, ems_18, fire_18, ems_19, fire_19)

# Save the original data as a CSV
library(readr)
write_csv(ems_fire, "/Users/davidreynolds/Downloads/Documents/Mizzou/2020-21/Classes/Spring semester/STAT 8090/bcmo-ems-fire-response-times/Data/Original data/ems_fire_original.csv")

# Filter for calls that were in Boone County
library(dplyr)
ems_fire <- ems_fire %>% 
  filter(Nature != "MUTUAL AID (OUT OF COUNTY)")

# Convert to time data type
library(stringr)
ems_fire$date <- str_sub(as.character(ems_fire$CallTime), 1, 10)
ems_fire$year <- as.factor(str_sub(ems_fire$date, 7, 10))
ems_fire$CallTime <- as.POSIXct(ems_fire$CallTime, format = "%m/%d/%Y %H:%M:%S")
ems_fire$FirstDispatchTime <- as.POSIXct(ems_fire$FirstDispatchTime, format = "%m/%d/%Y %H:%M:%S")
ems_fire$FirstEnroute <- as.POSIXct(ems_fire$FirstEnroute, format = "%m/%d/%Y %H:%M:%S")
ems_fire$FirstArrive <- as.POSIXct(ems_fire$FirstArrive, format = "%m/%d/%Y %H:%M:%S")
ems_fire$FirstTransport <- as.POSIXct(ems_fire$FirstTransport, format = "%m/%d/%Y %H:%M:%S")
ems_fire$LastClear <- as.POSIXct(ems_fire$LastClear, format = "%m/%d/%Y %H:%M:%S")

# Create time difference columns
library(tidyverse)
ems_fire$call_dispatch <- difftime(ems_fire$FirstDispatchTime, ems_fire$CallTime, units = "min")
ems_fire$dispatch_enroute <- difftime(ems_fire$FirstEnroute, ems_fire$FirstDispatchTime, units = "min")
ems_fire$enroute_arrive <- difftime(ems_fire$FirstArrive, ems_fire$FirstEnroute, units = "min")
ems_fire$dispatch_arrive <- ems_fire$dispatch_enroute + ems_fire$enroute_arrive
ems_fire$call_arrive <- difftime(ems_fire$FirstArrive, ems_fire$CallTime, units = "min")

# Filter for calls with time differences greater than zero
ems_fire <- ems_fire %>% 
  filter(call_dispatch > 0, dispatch_enroute > 0, enroute_arrive > 0, call_arrive > 0)

# Isolate x and y coordinates
xy <- ems_fire %>% select(GeoX, GeoY)
library(proj4)
proj4string <- "+proj=tmerc +lat_0=35.83333333333334 +lon_0=-92.5 +k=0.9999333333333333 +x_0=500000.0000000002 +y_0=0 +ellps=GRS80 +datum=NAD83 +to_meter=0.3048006096012192 +no_defs"

# Project data
pj <- project(xy, proj4string, inverse = TRUE)
latlon <- data.frame(lon = pj$x, lat = pj$y)

# Combine data
ems_fire <- cbind(ems_fire, latlon)

# Rename columns
ems_fire <- ems_fire %>% 
  select(date, year, "call" = CallTime, "dispatch" = FirstDispatchTime, "enroute" = FirstEnroute,
         "arrive" = FirstArrive, "transport" = FirstTransport, "clear" = LastClear, "service" = Service, 
         "agency" = Agency, "nature" = Nature, "unit" = PrimaryUnit, call_dispatch, 
         dispatch_enroute, enroute_arrive, dispatch_arrive, call_arrive, "street" = Street, "x" = GeoX, "y" = GeoY, lon, lat)

# Remove observations with (0, 0)
ems_fire <- subset(ems_fire, x != 0 & y != 0)

# Remove duplicated rows
library(data.table)
ems_fire$call_factor <- as.factor(ems_fire$call)
ems_fire <- data.table(ems_fire)
ems_fire <- ems_fire[, .SD[which.min(dispatch)], by = list(call_factor, street)]
ems_fire <- as.data.frame(ems_fire)[, -1]

# Convert time columns to characters
time <- c("call", "dispatch", "enroute", "arrive", "transport", "clear")
ems_fire[, time] <- lapply(ems_fire[, time], as.character)

# Convert the responses to numeric
response <- c("call_dispatch", "dispatch_enroute", "enroute_arrive", "dispatch_arrive", "call_arrive")
ems_fire[, response] <- lapply(ems_fire[, response], as.numeric)

# Take the log of the responses
ems_fire[, response] <- lapply(ems_fire[, response], log)

# Reorder columns
ems_fire <- ems_fire %>% 
  select(date, year, call, dispatch, enroute, arrive, transport, clear, service, agency, nature, 
         unit, call_dispatch, dispatch_enroute, enroute_arrive, dispatch_arrive, call_arrive, street, 
         x, y, lon, lat)

# Remove more observations that are outside of Boone County
library(maps)
library(splancs)
ems_fire_points <- as.points(ems_fire$lon, ems_fire$lat)
boone <- map("county", "missouri,boone", fill = TRUE, col = "transparent", plot = FALSE)
boone <- as.points(boone$x, boone$y)
pointmap(ems_fire_points)
polymap(boone, add = TRUE)
in_out <- inout(ems_fire_points, boone)
ems_fire$in_out <- in_out
ems_fire <- ems_fire %>% 
  filter(in_out == 1)
ems_fire$in_out <- NULL

# Save the data as a CSV
write_csv(ems_fire, "/Users/davidreynolds/Downloads/Documents/Mizzou/2020-21/Classes/Spring semester/STAT 8090/bcmo-ems-fire-response-times/Data/ems_fire.csv")
