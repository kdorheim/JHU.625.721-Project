# Analysis and visualization script for the 625.721 semester project.  

# 0. Set Up --------------------------------------------------------------------------------------------------
# Load the libraries 
library(locfit)
library(logspline)
library(extRemes)
library(ggplot2)
library(dplyr)

# Set the plotting theme 
theme_set(theme_bw())

# Read in data and format 
tasmax_df <- read.csv(here::here("tasmax.csv"))  %>% 
  rename(value = TXx) %>%  
  mutate(experiment = ifelse(experiment == "ssp119", "low", experiment)) %>% 
  mutate(experiment = ifelse(experiment == "ssp245", "medium", experiment)) %>% 
  mutate(experiment = ifelse(experiment == "ssp585", "high", experiment))

# Figure out which ensemble members to use, since the low scenario has the 
# smallest ensemble size limit to the ensemble members that are included there. 
tasmax_df %>% 
  filter(experiment == "low") %>% 
  pull(ensemble) %>% 
  unique() -> 
  ens

tasmax_df <- filter(tasmax_df, ensemble %in% ens)

# Historical 
tasmax_df %>% 
  filter(experiment == "historical") %>% 
  filter(year %in% 1850:1860) %>% 
  mutate(period = NA) -> 
  hist_data

# Near Term Period 
# SSP119
tasmax_df %>% 
  filter(experiment == "low") %>% 
  filter(year %in% 2020:2030) %>% 
  mutate(period = "1") -> 
  ssp119_data1

# SSP245 
tasmax_df %>% 
  filter(experiment == "medium") %>% 
  filter(year %in% 2020:2030) %>% 
  mutate(period = "1") -> 
  ssp245_data1

# SSP585 
tasmax_df %>% 
  filter(experiment == "high") %>% 
  filter(year %in% 2020:2030) %>% 
  mutate(period = "1") -> 
  ssp585_data1


# Long Term 
# SSP119 
tasmax_df %>% 
  filter(experiment == "low") %>% 
  filter(year %in% 2090:2100) %>% 
  mutate(period = "2")  -> 
  ssp119_data2


# SSP245 
tasmax_df %>% 
  filter(experiment == "medium") %>% 
  filter(year %in% 2090:2100) %>% 
  mutate(period = "2") -> 
  ssp245_data2

# SSP585 
tasmax_df %>% 
  filter(experiment == "high") %>% 
  filter(year %in% 2090:2100) %>% 
  mutate(period = "2") -> 
  ssp585_data2

# Visualize the time series  ------------------------------------------------------------------------------------
rbind(hist_data) %>% 
  mutate(year = as.integer(year)) %>% 
  ggplot(aes(year, value)) + 
  geom_point() + 
  labs(title = "Historical (1850-1860) Annual Hottest Day", 
       y = "Temperature (F)", 
       x = "Year")

rbind(ssp119_data1, ssp245_data1, ssp585_data1) %>% 
  mutate(year = as.integer(year)) %>% 
  ggplot(aes(year, value)) + 
  geom_point() + 
  facet_grid(factor(experiment, levels=c("low", "medium", "high"))~., switch="both") + 
  labs(title = "Near Term Future (2020-2030) Annual Hottest Day", 
       y = "Temperature (F)", 
       x = "Year")


rbind(ssp119_data2, ssp245_data2, ssp585_data2) %>% 
  mutate(year = as.integer(year)) %>% 
  ggplot(aes(year, value)) + 
  geom_point() + 
  facet_grid(factor(experiment, levels=c("low", "medium", "high"))~., switch="both") + 
  labs(title = "Long Term Future (2090-2100) Annual Hottest Day", 
       y = "Temperature (F)", 
       x = "Year")


# Summary Stats  --------------------------------------------------------------------------------------------------
# Take a look at the distributions of the hottest day of the year.
# We will not be fitting GEV models to data here. 

hist_data$value %>% summary()

rbind(hist_data) %>% 
  ggplot(aes(value)) + 
  geom_histogram(position="identity", alpha = .5) + 
  facet_wrap("experiment", ncol = 1) + 
  labs(title = "Historical (1850-1860) Annual Hottest Day", 
       y = "Count", 
       x = "Temperature (F)")

rbind(ssp119_data1, ssp245_data1, ssp585_data1)  %>% 
  group_by(experiment) %>% 
  summarise(min = min(value), 
            max = max(value))

rbind(ssp119_data1, ssp245_data1, ssp585_data1) %>% 
  ggplot(aes(value, fill = experiment)) + 
  geom_histogram(position="identity", alpha = .5) + 
  facet_grid(factor(experiment, levels=c("low", "medium", "high"))~., switch="both") + 
  labs(title = "Near Term Future (2020-2030) Annual Hottest Day", 
       y = "Count", 
       x = "Temperature (F)") + 
  theme(legend.title = element_blank())

rbind(ssp119_data1, ssp245_data1, ssp585_data1) %>% 
  group_by(experiment) %>% 
  summarise(min = min(value), max = max(value))

rbind(ssp119_data2, ssp245_data2, ssp585_data2)  %>% 
  group_by(experiment) %>% 
  summarise(min = min(value), 
            max = max(value))

rbind(ssp119_data1, ssp245_data2, ssp585_data2) %>% 
  ggplot(aes(value, fill = experiment)) + 
  geom_histogram(position="identity", alpha = .5) + 
  facet_grid(factor(experiment, levels=c("low", "medium", "high"))~., switch="both") + 
  labs(title = "Long Term Future (2090-2100) Annual Hottest Day", 
       y = "Count", 
       x = "Temperature (F)") + 
  theme(legend.title = element_blank())


# Compare all the near term distributions with one another - according 
# to these tests this these distributions are not different. 
t.test(ssp245_data1$value, ssp119_data1$value)
t.test(ssp245_data1$value, ssp585_data1$value)
t.test(ssp119_data1$value, ssp585_data1$value)

# near vs long term 
t.test(ssp245_data1$value, ssp119_data2$value) # not different 
t.test(ssp245_data1$value, ssp245_data2$value) # different
t.test(ssp245_data1$value, ssp585_data2$value) # different 

# compare across long-term scenarios 
t.test(ssp119_data2$value, ssp245_data2$value) # different 
t.test(ssp245_data2$value, ssp585_data2$value) # different 


# Model Fits --------------------------------------------------------------------------------------------------
fit_hist <- fevd(x = hist_data$value, type = "GEV", units = "deg F" )
plot(fit_hist)

fit_ssp2451 <-fevd(x = ssp245_data1$value, type = "GEV", units = "deg F" )
fit_ssp2451$results$par
plot(fit_ssp2451)


# Long Term 
fit_ssp1192 <- fevd(x = ssp119_data2$value, type = "GEV", units = "deg F")
fit_ssp1192$results$par
plot(fit_ssp1192)

fit_ssp2452 <-fevd(x = ssp245_data2$value, type = "GEV", units = "deg F" )
fit_ssp2452$results$par
plot(fit_ssp2452)

fit_ssp5852 <- fevd(x = ssp585_data2$value, type = "GEV", units = "deg F" )
fit_ssp5852$results$par
plot(fit_ssp5852)


hist(ssp119_data2$value, prob = T, col=rgb(0, 0, 255, max = 255, alpha = 75), 
     xlim = c(72, 115), ylim = c(0, 0.20), border=F)
hist(ssp245_data2$value, prob = T, add = T, col=rgb(0, 255, 0, max = 255, alpha = 75),  border=F)
hist(ssp585_data2$value, prob = T, add = T, col=rgb(255, 0, 0, max = 255, alpha = 75),  border=F)
fit <- distill(fit_ssp1192)
y <-revd(10000,loc=fit[["location"]],scale=fit[["scale"]],shape=fit[["shape"]])
plot(logspline(y),add=T,col="blue", lw = 3)
fit <- distill(fit_ssp2452)
y <-revd(10000,loc=fit[["location"]],scale=fit[["scale"]],shape=fit[["shape"]])
plot(logspline(y),add=T,col="green", lw = 3)
fit <- distill(fit_ssp5852)
y <-revd(10000,loc=fit[["location"]],scale=fit[["scale"]],shape=fit[["shape"]])
plot(logspline(y),add=T,col="red", lw = 3)


# Find the expected value ----------------------------------------------------------------------


find_expected_value <- function(fit){
  out <- fit[["location"]] + (gamma(1-fit[["shape"]]) - 1) * fit[["scale"]]/ fit[["shape"]]
  return(out)
}

find_expected_value(distill(fit_hist))
find_expected_value(distill(fit_ssp2451))

find_expected_value(distill(fit_ssp1192))
find_expected_value(distill(fit_ssp2452))
find_expected_value(distill(fit_ssp5852))


# Estimating the probability exceeding 95% ----------------------------------------------------------------------

pextRemes(fit_hist, c(95, 100, 105), lower.tail = FALSE)
pextRemes(fit_ssp2451, c(95, 100, 105), lower.tail = FALSE)
pextRemes(fit_ssp1192, c(95, 100, 105), lower.tail = FALSE)
pextRemes(fit_ssp2452, c(95, 100, 105), lower.tail = FALSE)
pextRemes(fit_ssp5852, c(95, 100, 105), lower.tail = FALSE)

