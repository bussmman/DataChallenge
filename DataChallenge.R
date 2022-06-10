################################################################################
## Data Challenge ##############################################################
################################################################################

###############################
## Part 1: Data and Packages ##
###############################

## Load the necessary packages
library(tidyverse)
library(dplyr)
library(lubridate)
library(sf)

## Read the required data
roedeer_raw = read_csv("data/all_deer_cleaned.csv")

## Check the data
head(roedeer_raw)           # First few rows
str(roedeer_raw)            # Data types of the variables 
table(roedeer_raw$reh)      # Number of observations per deer

## Calculate mean location of deer, incl. a circular buffer around the deer
## with the radius of the standard deviation 
loc_mean = summarize(group_by(roedeer_raw, reh), X = mean(x), Y = mean(y))
loc_var = summarize(group_by(roedeer_raw, reh), X = sd(x), Y = sd(y))
loc_mean$radius = sqrt(loc_var$X^2 + loc_var$Y^2)

## Plot the mean location of the deer
ggplot(loc_mean, aes(x = X, y = Y, col = reh)) +
  geom_point() +
  theme_classic()

## Transform the roedeer dataframe into an sf-object
loc_mean_sf = st_as_sf(loc_mean, coords = c("X","Y"))

## Again plot the mean location of the deer
ggplot(loc_mean_sf) +
  geom_sf() +
  theme_classic()

## enrich the sf object with the buffer
loc_mean_buff = st_buffer(loc_mean_sf, loc_mean_sf$radius)

## Plot the mean location incl. buffer of the deer
ggplot(loc_mean_buff, aes(fill = reh)) +
  geom_sf() +
  theme_classic()

## Extract the deers, whose buffers overlap
l = length(loc_mean_buff$reh)
roedeer_overlap = vector("list", l)

for (i in 1:l) {
  b = st_intersection(loc_mean_buff$geometry[i],loc_mean_buff$geometry[-i])
  
  if (length(b) > 0) {
    roedeer_overlap[i] = b
  }
}

loc_mean_buff$overlap = roedeer_overlap
temp = sapply(loc_mean_buff$overlap, is.null)
loc_mean_overlap = loc_mean_buff[!temp,]
loc_mean_overlap = loc_mean_overlap[,c(1:3)]

## Again plot the buffers for the deers that overlap
ggplot(loc_mean_overlap, aes(fill = reh)) +
  geom_sf() +
  theme_classic()

# --> There are three couples that are overlapping! Do they belong together?
# --> 2 & 7     5 & 6     12 & 13

## Round the timestamps to 5 minutes
roedeer_raw$datetime_utc = round_date(
  roedeer_raw$datetime_utc, unit = "5 minutes")

## Extract the movement data for the three couples
roedeer_couple = roedeer_raw[roedeer_raw$reh == loc_mean_overlap$reh,]
table(roedeer_couple$reh)
# For some reason not correct --> observations are missing!!!!!!!!!!!!!
# Guess I have to do it the following way:
roedeer_02 = roedeer_raw[roedeer_raw$reh == "RE02",]
roedeer_05 = roedeer_raw[roedeer_raw$reh == "RE05",]
roedeer_06 = roedeer_raw[roedeer_raw$reh == "RE06",]
roedeer_07 = roedeer_raw[roedeer_raw$reh == "RE07",]
roedeer_12 = roedeer_raw[roedeer_raw$reh == "RE12",]
roedeer_13 = roedeer_raw[roedeer_raw$reh == "RE13",]
roedeer_couple = rbind(roedeer_02,roedeer_05,roedeer_06,
                       roedeer_07,roedeer_12,roedeer_13)
table(roedeer_couple$reh)
# That's more like it!

## Plot the movement data of the deer couples
ggplot(roedeer_couple, aes(x = x, y = y, col = reh)) +
  geom_point() +
  theme_classic()

# ##
# ggplot(roedeer_couple, aes(x = datetime_utc, y = y, col = reh)) +
#   geom_point() +
#   theme_classic()

## Calculate the euclidian distance between the couples at all times
# Couple 2 & 7
roedeer_2_7 = left_join(roedeer_02, roedeer_07, by="datetime_utc")
roedeer_2_7$dist = sqrt((roedeer_2_7$x.x - roedeer_2_7$x.y)^2 +
                          (roedeer_2_7$y.x - roedeer_2_7$y.y)^2)

ggplot(roedeer_2_7, aes(x = datetime_utc, y = dist)) +
  geom_point() +
  theme_classic()

# Couple 5 & 6
roedeer_5_6 = left_join(roedeer_05, roedeer_06, by="datetime_utc")
roedeer_5_6$dist = sqrt((roedeer_5_6$x.x - roedeer_5_6$x.y)^2 +
                          (roedeer_5_6$y.x - roedeer_5_6$y.y)^2)

ggplot(roedeer_5_6, aes(x = datetime_utc, y = dist)) +
  geom_point() +
  theme_classic()

# Couple 12 & 13
roedeer_12_13 = left_join(roedeer_12, roedeer_13, by="datetime_utc")
roedeer_12_13$dist = sqrt((roedeer_12_13$x.x - roedeer_12_13$x.y)^2 +
                          (roedeer_12_13$y.x - roedeer_12_13$y.y)^2)

ggplot(roedeer_12_13, aes(x = datetime_utc, y = dist)) +
  geom_point() +
  theme_classic()
