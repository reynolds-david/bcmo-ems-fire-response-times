# EMS and fire response times modeling
# David Reynolds

# Read in the data
library(readr)
ems_fire <- read_csv("ems_fire.csv", col_types = "cfTTTTTTffccnnnncnnnn")

# Take the log of the responses
response <- c("call_dispatch", "dispatch_enroute", "enroute_arrive", "call_arrive")
ems_fire[, response] <- lapply(ems_fire[, response], log)

# Blah