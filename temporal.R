# Below are my thoughts for temporal analysis:
# We could do a plot of volume sold over time
# possible options are: for particular city, particular year, gallons or liters

# still need to figure out all of aggregated data and save off file to data file in project
story <- readr::read_csv("/home/kmc/Desktop/Iowa_Liquor_Sales-Story.csv")

library(tidyverse)
story_clean <- story %>% mutate(new_date = lubridate::mdy(Date))

# format city variable since some names are all capitals
story_clean$City <- stringr::str_to_title(story_clean$City)
# format of store


# adding date variables
story_clean <- story_clean %>% mutate(Month = lubridate::month(new_date, label = TRUE),
                                Year = lubridate::year(new_date),
                                Mon_Yr = lubridate::floor_date(new_date, unit = "month"))

vol_agg <- story_clean %>% group_by(Year, Month, City) %>% 
                  summarize_at(vars('Volume Sold (Liters)', 'Volume Sold (Gallons)'),
                               .funs = sum)
vol_agg$Year <- factor(vol_agg$Year, order = TRUE)

vol2 <- story_clean %>% group_by(Mon_Yr, City) %>% 
  summarize_at(vars('Volume Sold (Liters)', 'Volume Sold (Gallons)'),
               .funs = sum)
                               
ggplot(vol2, aes(x = Mon_Yr, y = `Volume Sold (Liters)`, group = City, color = City)) +
         geom_point()

ggplot(filter(vol_agg, City == 'Ames')) + 
         geom_line(aes(x = Month, y = `Volume Sold (Liters)`, 
                       group = Year, color = Year)) + ylab("Volume Sold (Liters)")
# ugh, color palette is so ugly...

ggplot(vol_agg) + 
  geom_point(aes(x = Month, y = `Volume Sold (Liters)`, 
                group = City, color = City)) + ylab("Volume Sold (Liters)")

ggplot(filter(vol_agg, Year == 2017 & City != 'Ames')) + 
  geom_line(aes(x = Month, y = `Volume Sold (Liters)`, 
                 group = City, color = City)) + ylab("Volume Sold (Liters)")
