#EMS and fire response times in Boone County
#David Reynolds

#Import data
library(tidyverse)
setwd("~/Downloads/Documents/Mizzou/2020-21/EMS response times")
ems_fire <- read_csv("ems_fire.csv")

#Rank nature of calls by count
nature_rank <- ems_fire %>%
  count(Nature, sort = T) %>%
  group_by(Nature)
nrow(nature_rank)

write_csv(nature_rank, "/Users/davidreynolds/Downloads/Documents/Mizzou/2020-21/EMS Response Times/nature_rank.csv")

#Calculate proportion of EMS calls dispatched in less than two minutes
ems_under_two <- ems_fire %>% 
  filter(Service == "EMS", Call_Dispatch < 2)
nrow(ems_under_two) / nrow(ems) * 100

#Calculate means for time difference columns
mean(ems_fire$Call_Dispatch, na.rm = TRUE)
mean(ems_fire$Dispatch_Enroute, na.rm = TRUE)
mean(ems_fire$Enroute_Arrive, na.rm = TRUE)
mean(ems_fire$Call_Arrive, na.rm = TRUE)

#Calculate mean time differences by Service
ems <- ems_fire %>% 
  filter(Service == "EMS")
mean(ems$Call_Dispatch, na.rm = TRUE)
mean(ems$Dispatch_Enroute, na.rm = TRUE)
mean(ems$Enroute_Arrive, na.rm = TRUE)
mean(ems$Call_Arrive, na.rm = TRUE)

fire <- ems_fire %>% 
  filter(Service == "FIRE")
mean(fire$Call_Dispatch, na.rm = TRUE)
mean(fire$Dispatch_Enroute, na.rm = TRUE)
mean(fire$Enroute_Arrive, na.rm = TRUE)
mean(fire$Call_Arrive, na.rm = TRUE)

ems_fire$Service <- as.factor(ems_fire$Service)
t.test(Call_Dispatch ~ Service, data = ems_fire)
t.test(Dispatch_Enroute ~ Service, data = ems_fire)
t.test(Enroute_Arrive ~ Service, data = ems_fire)
t.test(Call_Arrive ~ Service, data = ems_fire)

#Calculate mean time differences by Alpha
ems_fire_Alpha <- ems_fire %>% 
  filter(Alpha == "O" | Alpha == "A" | Alpha == "B" | Alpha == "C" | Alpha == "D" | Alpha == "E")

Call_Dispatch_Alpha <- ems_fire_Alpha %>% 
  select(Call_Dispatch, Alpha) %>% 
  group_by(Alpha) %>% 
  summarize(mean_cd = mean(Call_Dispatch)) %>% 
  arrange(desc(mean_cd))

Dispatch_Enroute_Alpha <- ems_fire_Alpha %>% 
  select(Dispatch_Enroute, Alpha) %>% 
  group_by(Alpha) %>% 
  summarize(mean_de = mean(Dispatch_Enroute)) %>% 
  arrange(desc(mean_de))

Enroute_Arrive_Alpha <- ems_fire_Alpha %>% 
  select(Enroute_Arrive, Alpha) %>% 
  group_by(Alpha) %>% 
  summarize(mean_ea = mean(Enroute_Arrive)) %>% 
  arrange(desc(mean_ea))

Call_Arrive_Alpha <- ems_fire_Alpha %>% 
  select(Call_Arrive, Alpha) %>% 
  group_by(Alpha) %>% 
  summarize(mean_ca = mean(Call_Arrive)) %>% 
  arrange(desc(mean_ca))

ems_fire_Alpha$Alpha <- as.factor(ems_fire_Alpha$Alpha)
Call_Dispatch_Alpha_aov <- aov(Call_Dispatch ~ Alpha, data = ems_fire_Alpha)
summary(Call_Dispatch_Alpha_aov)

Dispatch_Enroute_Alpha_aov <- aov(Dispatch_Enroute ~ Alpha, data = ems_fire_Alpha)
summary(Dispatch_Enroute_Alpha_aov)

Enroute_Arrive_Alpha_aov <- aov(Enroute_Arrive ~ Alpha, data = ems_fire_Alpha)
summary(Enroute_Arrive_Alpha_aov)

Call_Arrive_Alpha_aov <- aov(Call_Arrive ~ Alpha, data = ems_fire_Alpha)
summary(Call_Arrive_Alpha_aov)

#Calculate mean time differences by Nature
ems_fire$Nature <- as.factor(ems_fire$Nature)
ems_fire$Call_Dispatch <- as.numeric(ems_fire$Call_Dispatch)
Call_Dispatch_Nature <- ems_fire %>% 
  select(Nature, Call_Dispatch) %>%
  group_by(Nature) %>%
  summarize(count_nature = n(), mean_cd = mean(Call_Dispatch)) %>% 
  arrange(desc(mean_cd)) %>% 
  filter(count_nature > 10)

ems_fire$Dispatch_Enroute <- as.numeric(ems_fire$Dispatch_Enroute)
Dispatch_Enroute_Nature <- ems_fire %>% 
  select(Nature, Dispatch_Enroute) %>% 
  group_by(Nature) %>%
  summarize(count_nature = n(), mean_de = mean(Dispatch_Enroute)) %>% 
  arrange(desc(mean_de)) %>% 
  filter(count_nature > 10)

ems_fire$Enroute_Arrive <- as.numeric(ems_fire$Enroute_Arrive)
Enroute_Arrive_Nature <- ems_fire %>% 
  select(Nature, Enroute_Arrive) %>% 
  group_by(Nature) %>%
  summarize(count_nature = n(), mean_ea = mean(Enroute_Arrive)) %>% 
  arrange(desc(mean_ea)) %>% 
  filter(count_nature > 10)

ems_fire$Call_Arrive <- as.numeric(ems_fire$Call_Arrive)
Call_Arrive_Nature <- ems_fire %>% 
  select(Nature, Call_Arrive) %>% 
  group_by(Nature) %>%
  summarize(count_nature = n(), mean_ca = mean(Call_Arrive)) %>% 
  arrange(desc(mean_ca)) %>% 
  filter(count_nature > 10)

#Calculate mean time differences by agency
Call_Dispatch_Agency <- ems_fire %>% 
  select(Agency, Call_Dispatch) %>% 
  group_by(Agency) %>% 
  summarize(mean_cd = mean(Call_Dispatch)) %>% 
  arrange(desc(mean_cd))

Dispatch_Enroute_Agency <- ems_fire %>% 
  select(Agency, Dispatch_Enroute) %>% 
  group_by(Agency) %>% 
  summarize(mean_de = mean(Dispatch_Enroute)) %>% 
  arrange(desc(mean_de))

Enroute_Arrive_Agency <- ems_fire %>% 
  select(Agency, Enroute_Arrive) %>% 
  group_by(Agency) %>% 
  summarize(mean_ea = mean(Enroute_Arrive)) %>% 
  arrange(desc(mean_ea))

Call_Arrive_Agency <- ems_fire %>% 
  select(Agency, Call_Arrive) %>% 
  group_by(Agency) %>% 
  summarize(mean_ca = mean(Call_Arrive)) %>% 
  arrange(desc(mean_ca))

ems_fire$Agency <- as.factor(ems_fire_alpha$Agency)
Call_Dispatch_Agency_aov <- aov(Call_Dispatch ~ Agency, data = ems_fire)
summary(Call_Dispatch_Agency_aov)

Dispatch_Enroute_Agency_aov <- aov(Dispatch_Enroute ~ Agency, data = ems_fire)
summary(Dispatch_Enroute_Agency_aov)

Enroute_Arrive_Agency_aov <- aov(Enroute_Arrive ~ Agency, data = ems_fire)
summary(Enroute_Arrive_Agency_aov)

Call_Arrive_Agency_aov <- aov(Call_Arrive ~ Agency, data = ems_fire)
summary(Call_Arrive_Agency_aov)

#Remove outliers
ems_fire_hist <- ems_fire
ems_fire_hist$Call_Dispatch <- as.numeric(ems_fire_hist$Call_Dispatch)
ems_fire_hist$Dispatch_Enroute <- as.numeric(ems_fire_hist$Dispatch_Enroute)
ems_fire_hist$Enroute_Arrive <- as.numeric(ems_fire_hist$Enroute_Arrive)
ems_fire_hist$Call_Arrive <- as.numeric(ems_fire_hist$Call_Arrive)

lh_cd <- quantile(ems_fire_hist$Call_Dispatch, probs = 0.25)
uh_cd <- quantile(ems_fire_hist$Call_Dispatch, probs = 0.75)
step_cd <- 1.5 * (uh_cd - lh_cd)
ems_fire_hist_cd <- ems_fire_hist %>% 
  filter(Call_Dispatch > lh_cd - step_cd & Call_Dispatch < uh_cd + step_cd)
nrow(ems_fire) - nrow(ems_fire_hist_cd)
(nrow(ems_fire_hist_cd) - nrow(ems_fire)) / nrow(ems_fire)

lh_de <- quantile(ems_fire_hist$Dispatch_Enroute, probs = 0.25)
uh_de <- quantile(ems_fire_hist$Dispatch_Enroute, probs = 0.75)
step_de <- 1.5 * (uh_de - lh_de)
ems_fire_hist_de <- ems_fire_hist %>% 
  filter(Dispatch_Enroute > lh_de - step_de & Dispatch_Enroute < uh_de + step_de)
nrow(ems_fire) - nrow(ems_fire_hist_de)
(nrow(ems_fire_hist_de) - nrow(ems_fire)) / nrow(ems_fire)

lh_ea <- quantile(ems_fire_hist$Enroute_Arrive, probs = 0.25)
uh_ea <- quantile(ems_fire_hist$Enroute_Arrive, probs = 0.75)
step_ea <- 1.5 * (uh_ea - lh_ea)
ems_fire_hist_ea <- ems_fire_hist %>% 
  filter(Enroute_Arrive > lh_ea - step_ea & Enroute_Arrive < uh_ea + step_ea)
nrow(ems_fire) - nrow(ems_fire_hist_ea)
(nrow(ems_fire_hist_ea) - nrow(ems_fire)) / nrow(ems_fire)

lh_ca <- quantile(ems_fire_hist$Call_Arrive, probs = 0.25)
uh_ca <- quantile(ems_fire_hist$Call_Arrive, probs = 0.75)
step_ca <- 1.5 * (uh_ca - lh_ca)
ems_fire_hist_ca <- ems_fire_hist %>% 
  filter(Call_Arrive > lh_ca - step_ca & Call_Arrive < uh_ca + step_ca)
nrow(ems_fire) - nrow(ems_fire_hist_ca)
(nrow(ems_fire_hist_ca) - nrow(ems_fire)) / nrow(ems_fire)

#Plot histograms for time differences
call_dispatch_hist <- ggplot(ems_fire_hist, aes(x = Call_Dispatch)) + 
  geom_histogram(aes(y = ..density..), color = "black", fill = "white") + 
  geom_density(alpha = 0.2, fill = "#66B2FF")
call_dispatch_hist
cd_hist <- ggplot(ems_fire_hist_cd, aes(x = Call_Dispatch)) + 
  geom_histogram(aes(y = ..density..), color = "black", fill = "white") + 
  geom_density(alpha = 0.2, fill = "#66B2FF")
cd_hist

dispatch_enroute_hist <- ggplot(ems_fire_hist, aes(x = Dispatch_Enroute)) +
  geom_histogram(aes(y = ..density..), color = "black", fill = "white") + 
  geom_density(alpha = 0.2, fill = "#66B2FF")
dispatch_enroute_hist
de_hist <- ggplot(ems_fire_hist_de, aes(x = Dispatch_Enroute)) +
  geom_histogram(aes(y = ..density..), color = "black", fill = "white") + 
  geom_density(alpha = 0.2, fill = "#66B2FF")
de_hist

enroute_arrive_hist <- ggplot(ems_fire_hist, aes(x = Enroute_Arrive)) +
  geom_histogram(aes(y = ..density..), color = "black", fill = "white") + 
  geom_density(alpha = 0.2, fill = "#66B2FF")
enroute_arrive_hist
ea_hist <- ggplot(ems_fire_hist_ea, aes(x = Enroute_Arrive)) +
  geom_histogram(aes(y = ..density..), color = "black", fill = "white") + 
  geom_density(alpha = 0.2, fill = "#66B2FF")
ea_hist

call_arrive_hist <- ggplot(ems_fire_hist, aes(x = Call_Arrive)) +
  geom_histogram(aes(y = ..density..), color = "black", fill = "white") + 
  geom_density(alpha = 0.2, fill = "#66B2FF")
call_arrive_hist
ca_hist <- ggplot(ems_fire_hist_ca, aes(x = Call_Arrive)) +
  geom_histogram(aes(y = ..density..), color = "black", fill = "white") + 
  geom_density(alpha = 0.2, fill = "#66B2FF")
ca_hist

#Extract year
library(stringr)
ems_fire$Year <- str_sub(ems_fire$CallDate, 1, 4)

#Line plot for Call_Dispatch
call_dispatch_year <- ems_fire %>% 
  select(Year, Call_Dispatch) %>% 
  group_by(Year) %>% 
  summarize(Call_Dispatch_Avg = mean(Call_Dispatch))

call_dispatch_line <- ggplot(data = call_dispatch_year, aes(x = Year, y = Call_Dispatch_Avg, group = 1)) + 
  geom_line(color = "#66B2FF") + geom_point()
call_dispatch_line

call_dispatch_aov <- aov(Call_Dispatch ~ Year, data = ems_fire)
summary(call_dispatch_aov)

#Line plot for Dispatch_Enroute
dispatch_enroute_year <- ems_fire %>% 
  select(Year, Dispatch_Enroute) %>% 
  group_by(Year) %>% 
  summarize(Dispatch_Enroute_Avg = mean(Dispatch_Enroute))

dispatch_enroute_line <- ggplot(data = dispatch_enroute_year, aes(x = Year, y = Dispatch_Enroute_Avg, group = 1)) + 
  geom_line(color = "#66B2FF") + geom_point()
dispatch_enroute_line

dispatch_enroute_aov <- aov(Dispatch_Enroute ~ Year, data = ems_fire)
summary(dispatch_enroute_aov)

#Line plot for Enroute_Arrive
enroute_arrive_year <- ems_fire %>% 
  select(Year, Enroute_Arrive) %>% 
  group_by(Year) %>% 
  summarize(Enroute_Arrive_Avg = mean(Enroute_Arrive))

enroute_arrive_line <- ggplot(data = enroute_arrive_year, aes(x = Year, y = Enroute_Arrive_Avg, group = 1)) + 
  geom_line(color = "#66B2FF") + geom_point()
enroute_arrive_line

enroute_arrive_aov <- aov(Enroute_Arrive ~ Year, data = ems_fire)
summary(enroute_arrive_aov)

#Line plot for Call_Arrive
call_arrive_year <- ems_fire %>% 
  select(Year, Call_Arrive) %>% 
  group_by(Year) %>% 
  summarize(Call_Arrive_Avg = mean(Call_Arrive))

call_arrive_line <- ggplot(data = call_arrive_year, aes(x = Year, y = Call_Arrive_Avg, group = 1)) + 
  geom_line(color = "#66B2FF") + geom_point()
call_arrive_line

call_arrive_aov <- aov(Call_Arrive ~ Year, data = ems_fire)
summary(call_arrive_aov)

##BHC
bhc <- ems_fire %>% 
  filter(Agency == "BHC")

#Rank nature of calls by count
bhc_nature_rank <- bhc %>%
  count(Nature, sort = T) %>%
  group_by(Nature)

#Calculate means for time difference columns
mean(bhc$Call_Dispatch)
mean(bhc$Dispatch_Enroute)
mean(bhc$Enroute_Arrive)
mean(bhc$Call_Arrive)

#Calculate time differences by nature
bhc$Call_Dispatch <- as.numeric(bhc$Call_Dispatch)
bhc_cd_Nature <- bhc %>% 
  select(Nature, Call_Dispatch) %>%
  group_by(Nature) %>%
  summarize(count_nature = n(), mean_cd = mean(Call_Dispatch)) %>% 
  arrange(desc(mean_cd)) %>% 
  filter(count_nature > 10)

bhc$Dispatch_Enroute <- as.numeric(bhc$Dispatch_Enroute)
bhc_de_Nature <- bhc %>% 
  select(Nature, Dispatch_Enroute) %>% 
  group_by(Nature) %>%
  summarize(count_nature = n(), mean_de = mean(Dispatch_Enroute)) %>% 
  arrange(desc(mean_de)) %>% 
  filter(count_nature > 10)

bhc$Enroute_Arrive <- as.numeric(bhc$Enroute_Arrive)
bhc_ea_Nature <- bhc %>% 
  select(Nature, Enroute_Arrive) %>% 
  group_by(Nature) %>%
  summarize(count_nature = n(), mean_ea = mean(Enroute_Arrive)) %>% 
  arrange(desc(mean_ea)) %>% 
  filter(count_nature > 10)

bhc$Call_Arrive <- as.numeric(bhc$Call_Arrive)
bhc_ca_Nature <- bhc %>% 
  select(Nature, Call_Arrive) %>% 
  group_by(Nature) %>%
  summarize(count_nature = n(), mean_ca = mean(Call_Arrive)) %>% 
  arrange(desc(mean_ca)) %>% 
  filter(count_nature > 10)

#Remove outliers
bhc_hist <- bhc
bhc_hist$Call_Dispatch <- as.numeric(bhc_hist$Call_Dispatch)
bhc_hist$Dispatch_Enroute <- as.numeric(bhc_hist$Dispatch_Enroute)
bhc_hist$Enroute_Arrive <- as.numeric(bhc_hist$Enroute_Arrive)
bhc_hist$Call_Arrive <- as.numeric(bhc_hist$Call_Arrive)

lh_cd <- quantile(bhc_hist$Call_Dispatch, probs = 0.25)
uh_cd <- quantile(bhc_hist$Call_Dispatch, probs = 0.75)
step_cd <- 1.5 * (uh_cd - lh_cd)
bhc_hist_cd <- bhc_hist %>% 
  filter(Call_Dispatch > lh_cd - step_cd & Call_Dispatch < uh_cd + step_cd)

lh_de <- quantile(bhc_hist$Dispatch_Enroute, probs = 0.25)
uh_de <- quantile(bhc_hist$Dispatch_Enroute, probs = 0.75)
step_de <- 1.5 * (uh_de - lh_de)
bhc_hist_de <- bhc_hist %>% 
  filter(Dispatch_Enroute > lh_de - step_de & Dispatch_Enroute < uh_de + step_de)

lh_ea <- quantile(bhc_hist$Enroute_Arrive, probs = 0.25)
uh_ea <- quantile(bhc_hist$Enroute_Arrive, probs = 0.75)
step_ea <- 1.5 * (uh_ea - lh_ea)
bhc_hist_ea <- bhc_hist %>% 
  filter(Enroute_Arrive > lh_ea - step_ea & Enroute_Arrive < uh_ea + step_ea)

lh_ca <- quantile(bhc_hist$Call_Arrive, probs = 0.25)
uh_ca <- quantile(bhc_hist$Call_Arrive, probs = 0.75)
step_ca <- 1.5 * (uh_ca - lh_ca)
bhc_hist_ca <- bhc_hist %>% 
  filter(Call_Arrive > lh_ca - step_ca & Call_Arrive < uh_ca + step_ca)

#Plot histograms for time differences
cd_hist <- ggplot(bhc_hist_cd, aes(x = Call_Dispatch)) + 
  geom_histogram(aes(y = ..density..), color = "black", fill = "white") + 
  geom_density(alpha = 0.2, fill = "#66B2FF")
cd_hist

de_hist <- ggplot(bhc_hist_de, aes(x = Dispatch_Enroute)) +
  geom_histogram(aes(y = ..density..), color = "black", fill = "white") + 
  geom_density(alpha = 0.2, fill = "#66B2FF")
de_hist

ea_hist <- ggplot(bhc_hist_ea, aes(x = Enroute_Arrive)) +
  geom_histogram(aes(y = ..density..), color = "black", fill = "white") + 
  geom_density(alpha = 0.2, fill = "#66B2FF")
ea_hist

ca_hist <- ggplot(bhc_hist_ca, aes(x = Call_Arrive)) +
  geom_histogram(aes(y = ..density..), color = "black", fill = "white") + 
  geom_density(alpha = 0.2, fill = "#66B2FF")
ca_hist

#Extract year
library(stringr)
bhc$Year <- str_sub(bhc$CallDate, 1, 4)

#Line plot for Call_Dispatch
call_dispatch_year <- bhc %>% 
  select(Year, Call_Dispatch) %>% 
  group_by(Year) %>% 
  summarize(Call_Dispatch_Avg = mean(Call_Dispatch))

call_dispatch_line <- ggplot(data = call_dispatch_year, aes(x = Year, y = Call_Dispatch_Avg, group = 1)) + 
  geom_line(color = "#66B2FF") + geom_point()
call_dispatch_line

call_dispatch_aov <- aov(Call_Dispatch ~ Year, data = bhc)
summary(call_dispatch_aov)

#Line plot for Dispatch_Enroute
dispatch_enroute_year <- bhc %>% 
  select(Year, Dispatch_Enroute) %>% 
  group_by(Year) %>% 
  summarize(Dispatch_Enroute_Avg = mean(Dispatch_Enroute))

dispatch_enroute_line <- ggplot(data = dispatch_enroute_year, aes(x = Year, y = Dispatch_Enroute_Avg, group = 1)) + 
  geom_line(color = "#66B2FF") + geom_point()
dispatch_enroute_line

dispatch_enroute_aov <- aov(Dispatch_Enroute ~ Year, data = bhc)
summary(dispatch_enroute_aov)

#Line plot for Enroute_Arrive
enroute_arrive_year <- bhc %>% 
  select(Year, Enroute_Arrive) %>% 
  group_by(Year) %>% 
  summarize(Enroute_Arrive_Avg = mean(Enroute_Arrive))

enroute_arrive_line <- ggplot(data = enroute_arrive_year, aes(x = Year, y = Enroute_Arrive_Avg, group = 1)) + 
  geom_line(color = "#66B2FF") + geom_point()
enroute_arrive_line

enroute_arrive_aov <- aov(Enroute_Arrive ~ Year, data = bhc)
summary(enroute_arrive_aov)

#Line plot for Call_Arrive
call_arrive_year <- bhc %>% 
  select(Year, Call_Arrive) %>% 
  group_by(Year) %>% 
  summarize(Call_Arrive_Avg = mean(Call_Arrive))

call_arrive_line <- ggplot(data = call_arrive_year, aes(x = Year, y = Call_Arrive_Avg, group = 1)) + 
  geom_line(color = "#66B2FF") + geom_point()
call_arrive_line

call_arrive_aov <- aov(Call_Arrive ~ Year, data = bhc)
summary(call_arrive_aov)

##UHC
uhc <- ems_fire %>% 
  filter(Agency == "UHC")

#Rank nature of calls by count
uhc_nature_rank <- uhc %>%
  count(Nature, sort = T) %>%
  group_by(Nature)

#Calculate means for time difference columns
mean(uhc$Call_Dispatch)
mean(uhc$Dispatch_Enroute)
mean(uhc$Enroute_Arrive)
mean(uhc$Call_Arrive)

#Calculate time differences by nature
uhc$Call_Dispatch <- as.numeric(uhc$Call_Dispatch)
uhc_cd_Nature <- uhc %>% 
  select(Nature, Call_Dispatch) %>%
  group_by(Nature) %>%
  summarize(count_nature = n(), mean_cd = mean(Call_Dispatch)) %>% 
  arrange(desc(mean_cd)) %>% 
  filter(count_nature > 10)

uhc$Dispatch_Enroute <- as.numeric(uhc$Dispatch_Enroute)
uhc_de_Nature <- uhc %>% 
  select(Nature, Dispatch_Enroute) %>% 
  group_by(Nature) %>%
  summarize(count_nature = n(), mean_de = mean(Dispatch_Enroute)) %>% 
  arrange(desc(mean_de)) %>% 
  filter(count_nature > 10)

uhc$Enroute_Arrive <- as.numeric(uhc$Enroute_Arrive)
uhc_ea_Nature <- uhc %>% 
  select(Nature, Enroute_Arrive) %>% 
  group_by(Nature) %>%
  summarize(count_nature = n(), mean_ea = mean(Enroute_Arrive)) %>% 
  arrange(desc(mean_ea)) %>% 
  filter(count_nature > 10)

uhc$Call_Arrive <- as.numeric(uhc$Call_Arrive)
uhc_ca_Nature <- uhc %>% 
  select(Nature, Call_Arrive) %>% 
  group_by(Nature) %>%
  summarize(count_nature = n(), mean_ca = mean(Call_Arrive)) %>% 
  arrange(desc(mean_ca)) %>% 
  filter(count_nature > 10)

#Remove outliers
uhc_hist <- uhc
uhc_hist$Call_Dispatch <- as.numeric(uhc_hist$Call_Dispatch)
uhc_hist$Dispatch_Enroute <- as.numeric(uhc_hist$Dispatch_Enroute)
uhc_hist$Enroute_Arrive <- as.numeric(uhc_hist$Enroute_Arrive)
uhc_hist$Call_Arrive <- as.numeric(uhc_hist$Call_Arrive)

lh_cd <- quantile(uhc_hist$Call_Dispatch, probs = 0.25)
uh_cd <- quantile(uhc_hist$Call_Dispatch, probs = 0.75)
step_cd <- 1.5 * (uh_cd - lh_cd)
uhc_hist_cd <- uhc_hist %>% 
  filter(Call_Dispatch > lh_cd - step_cd & Call_Dispatch < uh_cd + step_cd)

lh_de <- quantile(uhc_hist$Dispatch_Enroute, probs = 0.25)
uh_de <- quantile(uhc_hist$Dispatch_Enroute, probs = 0.75)
step_de <- 1.5 * (uh_de - lh_de)
uhc_hist_de <- uhc_hist %>% 
  filter(Dispatch_Enroute > lh_de - step_de & Dispatch_Enroute < uh_de + step_de)

lh_ea <- quantile(uhc_hist$Enroute_Arrive, probs = 0.25)
uh_ea <- quantile(uhc_hist$Enroute_Arrive, probs = 0.75)
step_ea <- 1.5 * (uh_ea - lh_ea)
uhc_hist_ea <- uhc_hist %>% 
  filter(Enroute_Arrive > lh_ea - step_ea & Enroute_Arrive < uh_ea + step_ea)

lh_ca <- quantile(uhc_hist$Call_Arrive, probs = 0.25)
uh_ca <- quantile(uhc_hist$Call_Arrive, probs = 0.75)
step_ca <- 1.5 * (uh_ca - lh_ca)
uhc_hist_ca <- uhc_hist %>% 
  filter(Call_Arrive > lh_ca - step_ca & Call_Arrive < uh_ca + step_ca)

#Plot histograms for time differences
cd_hist <- ggplot(uhc_hist_cd, aes(x = Call_Dispatch)) + 
  geom_histogram(aes(y = ..density..), color = "black", fill = "white") + 
  geom_density(alpha = 0.2, fill = "#66B2FF")
cd_hist

de_hist <- ggplot(uhc_hist_de, aes(x = Dispatch_Enroute)) +
  geom_histogram(aes(y = ..density..), color = "black", fill = "white") + 
  geom_density(alpha = 0.2, fill = "#66B2FF")
de_hist

ea_hist <- ggplot(uhc_hist_ea, aes(x = Enroute_Arrive)) +
  geom_histogram(aes(y = ..density..), color = "black", fill = "white") + 
  geom_density(alpha = 0.2, fill = "#66B2FF")
ea_hist

ca_hist <- ggplot(uhc_hist_ca, aes(x = Call_Arrive)) +
  geom_histogram(aes(y = ..density..), color = "black", fill = "white") + 
  geom_density(alpha = 0.2, fill = "#66B2FF")
ca_hist

#Extract year
library(stringr)
uhc$Year <- str_sub(uhc$CallDate, 1, 4)

#Line plot for Call_Dispatch
call_dispatch_year <- uhc %>% 
  select(Year, Call_Dispatch) %>% 
  group_by(Year) %>% 
  summarize(Call_Dispatch_Avg = mean(Call_Dispatch))

call_dispatch_line <- ggplot(data = call_dispatch_year, aes(x = Year, y = Call_Dispatch_Avg, group = 1)) + 
  geom_line(color = "#66B2FF") + geom_point()
call_dispatch_line

call_dispatch_aov <- aov(Call_Dispatch ~ Year, data = uhc)
summary(call_dispatch_aov)

#Line plot for Dispatch_Enroute
dispatch_enroute_year <- uhc %>% 
  select(Year, Dispatch_Enroute) %>% 
  group_by(Year) %>% 
  summarize(Dispatch_Enroute_Avg = mean(Dispatch_Enroute))

dispatch_enroute_line <- ggplot(data = dispatch_enroute_year, aes(x = Year, y = Dispatch_Enroute_Avg, group = 1)) + 
  geom_line(color = "#66B2FF") + geom_point()
dispatch_enroute_line

dispatch_enroute_aov <- aov(Dispatch_Enroute ~ Year, data = uhc)
summary(dispatch_enroute_aov)

#Line plot for Enroute_Arrive
enroute_arrive_year <- uhc %>% 
  select(Year, Enroute_Arrive) %>% 
  group_by(Year) %>% 
  summarize(Enroute_Arrive_Avg = mean(Enroute_Arrive))

enroute_arrive_line <- ggplot(data = enroute_arrive_year, aes(x = Year, y = Enroute_Arrive_Avg, group = 1)) + 
  geom_line(color = "#66B2FF") + geom_point()
enroute_arrive_line

enroute_arrive_aov <- aov(Enroute_Arrive ~ Year, data = uhc)
summary(enroute_arrive_aov)

#Line plot for Call_Arrive
call_arrive_year <- uhc %>% 
  select(Year, Call_Arrive) %>% 
  group_by(Year) %>% 
  summarize(Call_Arrive_Avg = mean(Call_Arrive))

call_arrive_line <- ggplot(data = call_arrive_year, aes(x = Year, y = Call_Arrive_Avg, group = 1)) + 
  geom_line(color = "#66B2FF") + geom_point()
call_arrive_line

call_arrive_aov <- aov(Call_Arrive ~ Year, data = uhc)
summary(call_arrive_aov)

##BCFD
bcfd <- ems_fire %>% 
  filter(Agency == "BCFD")

#Rank nature of calls by count
bcfd_nature_rank <- bcfd %>%
  count(Nature, sort = T) %>%
  group_by(Nature)

#Calculate means for time difference columns
mean(bcfd$Call_Dispatch)
mean(bcfd$Dispatch_Enroute)
mean(bcfd$Enroute_Arrive)
mean(bcfd$Call_Arrive)

#Calculate time differences by nature
bcfd$Call_Dispatch <- as.numeric(bcfd$Call_Dispatch)
bcfd_cd_Nature <- bcfd %>% 
  select(Nature, Call_Dispatch) %>%
  group_by(Nature) %>%
  summarize(count_nature = n(), mean_cd = mean(Call_Dispatch)) %>% 
  arrange(desc(mean_cd)) %>% 
  filter(count_nature > 10)

bcfd$Dispatch_Enroute <- as.numeric(bcfd$Dispatch_Enroute)
bcfd_de_Nature <- bcfd %>% 
  select(Nature, Dispatch_Enroute) %>% 
  group_by(Nature) %>%
  summarize(count_nature = n(), mean_de = mean(Dispatch_Enroute)) %>% 
  arrange(desc(mean_de)) %>% 
  filter(count_nature > 10)

bcfd$Enroute_Arrive <- as.numeric(bcfd$Enroute_Arrive)
bcfd_ea_Nature <- bcfd %>% 
  select(Nature, Enroute_Arrive) %>% 
  group_by(Nature) %>%
  summarize(count_nature = n(), mean_ea = mean(Enroute_Arrive)) %>% 
  arrange(desc(mean_ea)) %>% 
  filter(count_nature > 10)

bcfd$Call_Arrive <- as.numeric(bcfd$Call_Arrive)
bcfd_ca_Nature <- bcfd %>% 
  select(Nature, Call_Arrive) %>% 
  group_by(Nature) %>%
  summarize(count_nature = n(), mean_ca = mean(Call_Arrive)) %>% 
  arrange(desc(mean_ca)) %>% 
  filter(count_nature > 10)

#Remove outliers
bcfd_hist <- bcfd
bcfd_hist$Call_Dispatch <- as.numeric(bcfd_hist$Call_Dispatch)
bcfd_hist$Dispatch_Enroute <- as.numeric(bcfd_hist$Dispatch_Enroute)
bcfd_hist$Enroute_Arrive <- as.numeric(bcfd_hist$Enroute_Arrive)
bcfd_hist$Call_Arrive <- as.numeric(bcfd_hist$Call_Arrive)

lh_cd <- quantile(bcfd_hist$Call_Dispatch, probs = 0.25)
uh_cd <- quantile(bcfd_hist$Call_Dispatch, probs = 0.75)
step_cd <- 1.5 * (uh_cd - lh_cd)
bcfd_hist_cd <- bcfd_hist %>% 
  filter(Call_Dispatch > lh_cd - step_cd & Call_Dispatch < uh_cd + step_cd)

lh_de <- quantile(bcfd_hist$Dispatch_Enroute, probs = 0.25)
uh_de <- quantile(bcfd_hist$Dispatch_Enroute, probs = 0.75)
step_de <- 1.5 * (uh_de - lh_de)
bcfd_hist_de <- bcfd_hist %>% 
  filter(Dispatch_Enroute > lh_de - step_de & Dispatch_Enroute < uh_de + step_de)

lh_ea <- quantile(bcfd_hist$Enroute_Arrive, probs = 0.25)
uh_ea <- quantile(bcfd_hist$Enroute_Arrive, probs = 0.75)
step_ea <- 1.5 * (uh_ea - lh_ea)
bcfd_hist_ea <- bcfd_hist %>% 
  filter(Enroute_Arrive > lh_ea - step_ea & Enroute_Arrive < uh_ea + step_ea)

lh_ca <- quantile(bcfd_hist$Call_Arrive, probs = 0.25)
uh_ca <- quantile(bcfd_hist$Call_Arrive, probs = 0.75)
step_ca <- 1.5 * (uh_ca - lh_ca)
bcfd_hist_ca <- bcfd_hist %>% 
  filter(Call_Arrive > lh_ca - step_ca & Call_Arrive < uh_ca + step_ca)

#Plot histograms for time differences
cd_hist <- ggplot(bcfd_hist_cd, aes(x = Call_Dispatch)) + 
  geom_histogram(aes(y = ..density..), color = "black", fill = "white") + 
  geom_density(alpha = 0.2, fill = "#66B2FF")
cd_hist

de_hist <- ggplot(bcfd_hist_de, aes(x = Dispatch_Enroute)) +
  geom_histogram(aes(y = ..density..), color = "black", fill = "white") + 
  geom_density(alpha = 0.2, fill = "#66B2FF")
de_hist

ea_hist <- ggplot(bcfd_hist_ea, aes(x = Enroute_Arrive)) +
  geom_histogram(aes(y = ..density..), color = "black", fill = "white") + 
  geom_density(alpha = 0.2, fill = "#66B2FF")
ea_hist

ca_hist <- ggplot(bcfd_hist_ca, aes(x = Call_Arrive)) +
  geom_histogram(aes(y = ..density..), color = "black", fill = "white") + 
  geom_density(alpha = 0.2, fill = "#66B2FF")
ca_hist

#Extract year
library(stringr)
bcfd$Year <- str_sub(bcfd$CallDate, 1, 4)

#Line plot for Call_Dispatch
call_dispatch_year <- bcfd %>% 
  select(Year, Call_Dispatch) %>% 
  group_by(Year) %>% 
  summarize(Call_Dispatch_Avg = mean(Call_Dispatch))

call_dispatch_line <- ggplot(data = call_dispatch_year, aes(x = Year, y = Call_Dispatch_Avg, group = 1)) + 
  geom_line(color = "#66B2FF") + geom_point()
call_dispatch_line

call_dispatch_aov <- aov(Call_Dispatch ~ Year, data = bcfd)
summary(call_dispatch_aov)

#Line plot for Dispatch_Enroute
dispatch_enroute_year <- bcfd %>% 
  select(Year, Dispatch_Enroute) %>% 
  group_by(Year) %>% 
  summarize(Dispatch_Enroute_Avg = mean(Dispatch_Enroute))

dispatch_enroute_line <- ggplot(data = dispatch_enroute_year, aes(x = Year, y = Dispatch_Enroute_Avg, group = 1)) + 
  geom_line(color = "#66B2FF") + geom_point()
dispatch_enroute_line

dispatch_enroute_aov <- aov(Dispatch_Enroute ~ Year, data = bcfd)
summary(dispatch_enroute_aov)

#Line plot for Enroute_Arrive
enroute_arrive_year <- bcfd %>% 
  select(Year, Enroute_Arrive) %>% 
  group_by(Year) %>% 
  summarize(Enroute_Arrive_Avg = mean(Enroute_Arrive))

enroute_arrive_line <- ggplot(data = enroute_arrive_year, aes(x = Year, y = Enroute_Arrive_Avg, group = 1)) + 
  geom_line(color = "#66B2FF") + geom_point()
enroute_arrive_line

enroute_arrive_aov <- aov(Enroute_Arrive ~ Year, data = bcfd)
summary(enroute_arrive_aov)

#Line plot for Call_Arrive
call_arrive_year <- bcfd %>% 
  select(Year, Call_Arrive) %>% 
  group_by(Year) %>% 
  summarize(Call_Arrive_Avg = mean(Call_Arrive))

call_arrive_line <- ggplot(data = call_arrive_year, aes(x = Year, y = Call_Arrive_Avg, group = 1)) + 
  geom_line(color = "#66B2FF") + geom_point()
call_arrive_line

call_arrive_aov <- aov(Call_Arrive ~ Year, data = bcfd)
summary(call_arrive_aov)

#Create separate dataframes for before and after new CAD system was put into place
library(stringr)
ems_old <- ems_fire %>% 
  filter(CallDate < "2018-01-30", Service == "EMS")
ems_new <- ems_fire %>% 
  filter(CallDate > "2018-01-29", Service == "EMS")

#Remove old dataframe outliers 
ems_old$Call_Dispatch <- as.numeric(ems_old$Call_Dispatch)
ems_old$Dispatch_Enroute <- as.numeric(ems_old$Dispatch_Enroute)
ems_old$Enroute_Arrive <- as.numeric(ems_old$Enroute_Arrive)
ems_old$Call_Arrive <- as.numeric(ems_old$Call_Arrive)

lh_cd <- quantile(ems_old$Call_Dispatch, probs = 0.25)
uh_cd <- quantile(ems_old$Call_Dispatch, probs = 0.75)
step_cd <- 1.5 * (uh_cd - lh_cd)
ems_old_cd <- ems_old %>% 
  filter(Call_Dispatch > lh_cd - step_cd & Call_Dispatch < uh_cd + step_cd)

lh_de <- quantile(ems_old$Dispatch_Enroute, probs = 0.25)
uh_de <- quantile(ems_old$Dispatch_Enroute, probs = 0.75)
step_de <- 1.5 * (uh_de - lh_de)
ems_old_de <- ems_old %>% 
  filter(Dispatch_Enroute > lh_de - step_de & Dispatch_Enroute < uh_de + step_de)

lh_ea <- quantile(ems_old$Enroute_Arrive, probs = 0.25)
uh_ea <- quantile(ems_old$Enroute_Arrive, probs = 0.75)
step_ea <- 1.5 * (uh_ea - lh_ea)
ems_old_ea <- ems_old %>% 
  filter(Enroute_Arrive > lh_ea - step_ea & Enroute_Arrive < uh_ea + step_ea)

lh_ca <- quantile(ems_old$Call_Arrive, probs = 0.25)
uh_ca <- quantile(ems_old$Call_Arrive, probs = 0.75)
step_ca <- 1.5 * (uh_ca - lh_ca)
ems_old_ca <- ems_old %>% 
  filter(Call_Arrive > lh_ca - step_ca & Call_Arrive < uh_ca + step_ca)

#Plot histograms for old dataframe
cd_hist_old <- ggplot(ems_old_cd, aes(x = Call_Dispatch)) + 
  geom_histogram(aes(y = ..density..), color = "black", fill = "white") + 
  geom_density(alpha = 0.2, fill = "#66B2FF")
cd_hist_old

de_hist_old <- ggplot(ems_old_de, aes(x = Dispatch_Enroute)) +
  geom_histogram(aes(y = ..density..), color = "black", fill = "white") + 
  geom_density(alpha = 0.2, fill = "#66B2FF")
de_hist_old

ea_hist_old <- ggplot(ems_old_ea, aes(x = Enroute_Arrive)) +
  geom_histogram(aes(y = ..density..), color = "black", fill = "white") + 
  geom_density(alpha = 0.2, fill = "#66B2FF")
ea_hist_old

ca_hist_old <- ggplot(ems_old_ca, aes(x = Call_Arrive)) +
  geom_histogram(aes(y = ..density..), color = "black", fill = "white") + 
  geom_density(alpha = 0.2, fill = "#66B2FF")
ca_hist_old

#Remove new dataframe outliers 
ems_new$Call_Dispatch <- as.numeric(ems_new$Call_Dispatch)
ems_new$Dispatch_Enroute <- as.numeric(ems_new$Dispatch_Enroute)
ems_new$Enroute_Arrive <- as.numeric(ems_new$Enroute_Arrive)
ems_new$Call_Arrive <- as.numeric(ems_new$Call_Arrive)

lh_cd <- quantile(ems_new$Call_Dispatch, probs = 0.25)
uh_cd <- quantile(ems_new$Call_Dispatch, probs = 0.75)
step_cd <- 1.5 * (uh_cd - lh_cd)
ems_new_cd <- ems_new %>% 
  filter(Call_Dispatch > lh_cd - step_cd & Call_Dispatch < uh_cd + step_cd)

lh_de <- quantile(ems_new$Dispatch_Enroute, probs = 0.25)
uh_de <- quantile(ems_new$Dispatch_Enroute, probs = 0.75)
step_de <- 1.5 * (uh_de - lh_de)
ems_new_de <- ems_new %>% 
  filter(Dispatch_Enroute > lh_de - step_de & Dispatch_Enroute < uh_de + step_de)

lh_ea <- quantile(ems_new$Enroute_Arrive, probs = 0.25)
uh_ea <- quantile(ems_new$Enroute_Arrive, probs = 0.75)
step_ea <- 1.5 * (uh_ea - lh_ea)
ems_new_ea <- ems_new %>% 
  filter(Enroute_Arrive > lh_ea - step_ea & Enroute_Arrive < uh_ea + step_ea)

lh_ca <- quantile(ems_new$Call_Arrive, probs = 0.25)
uh_ca <- quantile(ems_new$Call_Arrive, probs = 0.75)
step_ca <- 1.5 * (uh_ca - lh_ca)
ems_new_ca <- ems_new %>% 
  filter(Call_Arrive > lh_ca - step_ca & Call_Arrive < uh_ca + step_ca)

#Plot histograms for old dataframe
cd_hist_new <- ggplot(ems_new_cd, aes(x = Call_Dispatch)) + 
  geom_histogram(aes(y = ..density..), color = "black", fill = "white") + 
  geom_density(alpha = 0.2, fill = "#66B2FF")
cd_hist_new

de_hist_new <- ggplot(ems_new_de, aes(x = Dispatch_Enroute)) +
  geom_histogram(aes(y = ..density..), color = "black", fill = "white") + 
  geom_density(alpha = 0.2, fill = "#66B2FF")
de_hist_new

ea_hist_new <- ggplot(ems_new_ea, aes(x = Enroute_Arrive)) +
  geom_histogram(aes(y = ..density..), color = "black", fill = "white") + 
  geom_density(alpha = 0.2, fill = "#66B2FF")
ea_hist_new

ca_hist_new <- ggplot(ems_new_ca, aes(x = Call_Arrive)) +
  geom_histogram(aes(y = ..density..), color = "black", fill = "white") + 
  geom_density(alpha = 0.2, fill = "#66B2FF")
ca_hist_new

#Compare means for time differences before and after the new CAD system was put into place
mean(ems_old$Call_Dispatch, na.rm = TRUE)
mean(ems_new$Call_Dispatch, na.rm = TRUE)
t.test(ems_old$Call_Dispatch, ems_new$Call_Dispatch)

mean(ems_old$Dispatch_Enroute, na.rm = TRUE)
mean(ems_new$Dispatch_Enroute, na.rm = TRUE)
t.test(ems_old$Dispatch_Enroute, ems_new$Dispatch_Enroute)

mean(ems_old$Enroute_Arrive, na.rm = TRUE)
mean(ems_new$Enroute_Arrive, na.rm = TRUE)
t.test(ems_old$Enroute_Arrive, ems_new$Enroute_Arrive)

mean(ems_old$Call_Arrive, na.rm = TRUE)
mean(ems_new$Call_Arrive, na.rm = TRUE)
t.test(ems_old$Call_Arrive, ems_new$Call_Arrive)

#Create dataframe with just EMS calls
ems <- ems_fire %>% 
  filter(Service == "EMS")
library(stringr)
ems$Year <- str_sub(ems$CallDate, 1, 4)

#Line plot for Call_Dispatch
call_dispatch_year <- ems %>% 
  select(Year, Call_Dispatch) %>% 
  group_by(Year) %>% 
  summarize(Call_Dispatch_Avg = mean(Call_Dispatch))

call_dispatch_line <- ggplot(data = call_dispatch_year, aes(x = Year, y = Call_Dispatch_Avg, group = 1)) + 
  geom_line(color = "#66B2FF") + geom_point()
call_dispatch_line

call_dispatch_aov <- aov(Call_Dispatch ~ Year, data = ems)
summary(call_dispatch_aov)

#Line plot for Dispatch_Enroute
dispatch_enroute_year <- ems %>% 
  select(Year, Dispatch_Enroute) %>% 
  group_by(Year) %>% 
  summarize(Dispatch_Enroute_Avg = mean(Dispatch_Enroute))

dispatch_enroute_line <- ggplot(data = dispatch_enroute_year, aes(x = Year, y = Dispatch_Enroute_Avg, group = 1)) + 
  geom_line(color = "#66B2FF") + geom_point()
dispatch_enroute_line

dispatch_enroute_aov <- aov(Dispatch_Enroute ~ Year, data = ems)
summary(dispatch_enroute_aov)

#Line plot for Enroute_Arrive
enroute_arrive_year <- ems %>% 
  select(Year, Enroute_Arrive) %>% 
  group_by(Year) %>% 
  summarize(Enroute_Arrive_Avg = mean(Enroute_Arrive))

enroute_arrive_line <- ggplot(data = enroute_arrive_year, aes(x = Year, y = Enroute_Arrive_Avg, group = 1)) + 
  geom_line(color = "#66B2FF") + geom_point()
enroute_arrive_line

enroute_arrive_aov <- aov(Enroute_Arrive ~ Year, data = ems)
summary(enroute_arrive_aov)

#Line plot for Call_Arrive
call_arrive_year <- ems %>% 
  select(Year, Call_Arrive) %>% 
  group_by(Year) %>% 
  summarize(Call_Arrive_Avg = mean(Call_Arrive))

call_arrive_line <- ggplot(data = call_arrive_year, aes(x = Year, y = Call_Arrive_Avg, group = 1)) + 
  geom_line(color = "#66B2FF") + geom_point()
call_arrive_line

call_arrive_aov <- aov(Call_Arrive ~ Year, data = ems)
summary(call_arrive_aov)
