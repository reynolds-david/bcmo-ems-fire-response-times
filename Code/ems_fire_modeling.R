# EMS and fire response times modeling
# David Reynolds

# Read in the data
library(readr)
ems_fire <- read_csv("ems_fire.csv", col_types = "cfTTTTTTffccnnnncnnnn")

# Filter for after 1/31/2018 and EMS calls only
library(dplyr)
library(lubridate)
ems_fire <- ems_fire %>% 
  filter(service == "EMS", call >= ymd_hms("2018-01-31 00:00:00"))

# 