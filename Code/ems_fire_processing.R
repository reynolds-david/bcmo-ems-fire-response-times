# Import the original data
setwd("~/Downloads/Documents/Mizzou/2020-21/EMS response times/Original data")
library(readxl)
ems_16 <- read_excel("2016EMS.xls")
fire_16 <- read_excel("2016Fire.xls")
ems_17 <- read_excel("2017EMS.xls")
fire_17 <- read_excel("2017Fire.xls")
ems_18 <- read_excel("2018EMS.xls")
fire_18 <- read_excel("2018Fire.xls")
ems_19 <- read_excel("2019EMS.xls")
fire_19 <- read_excel("2019Fire.xls")

# Join the data
ems_fire <- rbind(ems_16, fire_16, ems_17, fire_17, ems_18, fire_18, ems_19, fire_19)

# Save original data as CSV
library(readr)
write_csv(ems_fire, "/Users/davidreynolds/Downloads/Documents/Mizzou/2020-21/EMS response times/Original data/ems_fire_original.csv")

# Filter for calls that were in Boone County
library(dplyr)
ems_fire <- ems_fire %>% 
  filter(Nature != "MUTUAL AID (OUT OF COUNTY)")

# Convert to time data type
library(stringr)
ems_fire$CallDate <- str_sub(as.character(ems_fire$CallTime), 1, 10)
ems_fire$year <- as.factor(str_sub(ems_fire$CallDate, 7, 10))
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
ems_fire$call_arrive <- difftime(ems_fire$FirstArrive, ems_fire$CallTime, units = "min")

# Create alpha character column (FIX THIS)
ems_fire$alpha <- str_sub(ems_fire$Nature, 3, 3)

# Filter for calls with time differences greater than zero
ems_fire <- ems_fire %>% 
  filter(call_dispatch > 0, dispatch_enroute > 0, enroute_arrive > 0, call_arrive > 0)

# Isolate x and y coordinates
xy <- ems_fire %>% select(GeoX, GeoY)
library(proj4)
proj4string <- "+proj=tmerc +lat_0=35.83333333333334 +lon_0=-92.5 +k=0.9999333333333333 +x_0=500000.0000000002 +y_0=0 +ellps=GRS80 +datum=NAD83 +to_meter=0.3048006096012192 +no_defs"

# Project data
pj <- project(xy, proj4string, inverse = TRUE)
latlon <- data.frame(lat = pj$x, lon = pj$y)

# Join data
ems_fire <- cbind(ems_fire, latlon)

#Rename columns (ADD ALPHA)
ems_fire <- ems_fire %>% 
  select("date" = CallDate, year, "call" = CallTime, "dispatch" = FirstDispatchTime, "enroute" = FirstEnroute,
         "arrive" = FirstArrive, "transport" = FirstTransport, "clear" = LastClear, "service" = Service, 
         "agency" = Agency, "nature" = Nature, "unit" = PrimaryUnit, call_dispatch, 
         dispatch_enroute, enroute_arrive, call_arrive, "street" = Street, "x" = GeoX, "y" = GeoY, lat, lon)

ems_fire <- subset(ems_fire, x != 0 & y != 0)

# Save data as CSV file
write_csv(ems_fire, "/Users/davidreynolds/Downloads/Documents/Mizzou/2020-21/Classes/Spring semester/STAT 8090/bcmo-ems-fire-response-times/Data/ems_fire.csv")

# Group by time interval and address
ems_fire$Street <- as.factor(ems_fire$Street)
ems_fire_split <- split(ems_fire, paste0(ems_fire$Street))
result <- unique(ems_fire[, c("Street")])
result <- result[order(result$Street), ]
result$rows <- lapply(ems_fire_split, FUN = cumsum(c(1, diff.Date(df$Date)) >= 1))

#1. Group by street, use cumsum to get the call for each address
#2. Group by street and call, assign as new call variable
#3. Filter for the first vehicle within each unique call

ems_fire_19 <- ems_fire %>% 
  filter(year == 2019)
ems_fire_19 <- head(ems_fire_19, 100)
