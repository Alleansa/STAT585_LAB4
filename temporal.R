# Below are my thoughts for temporal analysis:
# We could do a plot of volume sold over time
# possible options are: for particular city, particular year, gallons or liters

# still need to figure out all of aggregated data and save off file to data file in project
library(tidyverse)

load(file = 'data/story.RData')

story_data %>% mutate(Date = mdy(Date),
                      City = tolower(City)) %>% 
  filter(Lon !='NaN')-> story_data

# adding date variables
story_clean <- story_data %>% mutate(Month = lubridate::month(Date, label = TRUE),
                                      Year = lubridate::year(Date),
                                      Mon_Yr = lubridate::floor_date(Date, unit = "month"))

vol_agg <- story_clean %>% group_by(Year, Month, City) %>% 
  summarize_at(vars('Volume.Sold..Liters.', 'Volume.Sold..Gallons.'),
               .funs = sum)
vol_agg$Year <- factor(vol_agg$Year, order = TRUE)

vol2 <- story_clean %>% group_by(Mon_Yr, City) %>% 
  summarize_at(vars('Volume.Sold..Liters.', 'Volume.Sold..Gallons.'),
               .funs = sum)

ggplot(vol2, aes(x = Mon_Yr, y = Volume.Sold..Liters., group = City, color = City)) +
  geom_line()

ggplot(filter(vol_agg, City == 'ames')) + 
  geom_line(aes(x = Month, y = Volume.Sold..Liters., 
                group = Year, color = Year)) + ylab("Volume Sold (Liters)")
# ugh, color palette is so ugly...

ggplot(vol_agg) + 
  geom_point(aes(x = Month, y =  Volume.Sold..Liters., 
                 group = City, color = City)) + ylab("Volume Sold (Liters)")

ggplot(filter(vol_agg, Year == 2017 & City != 'Ames')) + 
  geom_line(aes(x = Month, y =  Volume.Sold..Liters., 
                group = City, color = City)) + ylab("Volume Sold (Liters)")
