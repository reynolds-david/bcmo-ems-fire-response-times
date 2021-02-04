# EMS and fire response times modeling
# David Reynolds

# Read in the data
library(readr)
ems_fire <- read_csv("ems_fire.csv", col_types = "cfTTTTTTffccnnnnncnnnn")

# Filter for 2019 and EMS calls only
library(dplyr)
ems <- ems_fire %>% 
  filter(service == "EMS", year == 2019)

## Create 15-minute time windows

# Create month
library(stringr)
ems$month <- as.factor(str_sub(ems$date, 1, 2))

# Convert call to POSIXct
library(lubridate)
ems$call <- ymd_hms(ems$call)

# Cut call by 15-minute intervals
ems$window <- cut(ems$call, breaks = "15 min")

# Get the number of calls in each window
ems_windows <- count(ems, month, window)
ems_windows$window <- as.character(ems_windows$window)
ems_windows$window <- str_sub(ems_windows$window, 12, 19)
ems_windows <- ems_windows %>% 
  group_by(month, window) %>% 
  summarize(n = sum(n))

## Create a grid for Boone County

# Create the grid
xgrid <- seq(-92.6, -92, length.out = 32)
ygrid <- seq(38.6, 39.3, length.out = 32)

# Assign each call to a cell
xcell <- unlist(lapply(ems$lon, function(x) min(which(xgrid > x))))
ycell <- unlist(lapply(ems$lat, function(y) min(which(ygrid > y))))
ems$cell <- (length(xgrid) - 1) * ycell + xcell

# Count the number of calls in each cell
ems_cells <- ems %>% 
  count(cell)

## Perform PCA

# On the time windows
time_pca <- prcomp(X, scale = TRUE)
time_pca_loadings <- time_pca$rotation

# On the cells
grid_pca <- prcomp(X, scale = TRUE)
grid_pca_loadings <- grid_pca$rotation

## Perform NMF
library(NMF)

# On the time windows
time_nmf <- nmf(X, 3, method = "lee")
time_nmf_w <- basis(time_nmf)

# On the cells
grid_nmf <- nmf(X, 3, method = "lee")
grid_nmf_w <- basis(grid_nmf)
