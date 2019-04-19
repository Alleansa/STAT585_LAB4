library(tidyverse)
url <- "https://data.iowa.gov/resource/m3tr-qhgy.json?county=Story"
story_list <- jsonlite::read_json(url)
new.distinct.names <- story_list %>% 
  purrr::map(.x, 
             .f=~names(rbind.data.frame(rlist::list.flatten(.x),0)))
unlisted <- story_list %>% 
  purrr::map(.f = ~rbind.data.frame(unlist(.x, recursive=T, use.names=T)))
unlisted.info <- purrr::map2(unlisted,
                             new.distinct.names,
                             .f= ~purrr::set_names(.x, .y))
story_dat <- do.call(plyr::rbind.fill, unlisted.info)
rm(url, story_list, new.distinct.names, unlisted, unlisted.info)
story_dat$date <- as.character(story_dat$date)
story_dat$date <- substr(story_dat$date, 1, nchar(story_dat$date)-13)
story_dat$date <- strptime(story_dat$date, "%Y-%m-%d")
story_dat$date <- format(story_dat$date, "%Y/%m/%d")

story_clean <- story_dat %>% mutate(new_date = lubridate::ymd(date))

# format city variable since some names are all capitals
story_clean$City <- stringr::str_to_title(story_clean$city)
# format of store


# adding date variables
story_clean <- story_clean %>% mutate(Month = lubridate::month(new_date, label = TRUE),
                                Year = lubridate::year(new_date),
                                Mon_Yr = lubridate::floor_date(new_date, unit = "month"))

story_clean$sale_liters <- as.numeric(story_clean$sale_liters)
story_clean$sale_gallons <- as.numeric(story_clean$sale_gallons)

vol_agg <- story_clean %>% group_by(Year, Month, city) %>% 
                  summarize_at(vars(sale_liters, sale_gallons),
                               .funs = sum)
vol_agg$Year <- factor(vol_agg$Year, order = TRUE)

vol2 <- story_clean %>% group_by(Mon_Yr, city) %>% 
  summarize_at(vars(sale_liters, sale_gallons),
               .funs = sum)
                               
ggplot(vol2, aes(x = Mon_Yr, y = sale_liters, group = city, color = city)) +
         geom_point() + geom_line() + theme_bw()

vol_agg$city <- as.character(vol_agg$city)

ggplot(filter(vol_agg, city == "AMES")) + 
         geom_line(aes(x = Month, y = sale_liters, 
                       group = Year, color = Year)) + ylab("Volume Sold (Liters)") +
  theme_bw()

vol_agg$interact <- interaction(vol_agg$city, vol_agg$Year)

ggplot(filter(vol_agg, city != "AMES"), aes(x = Month, y = sale_liters, group = interact, color = city)) + 
  geom_point() + geom_line() + 
  ylab("Volume Sold (Liters)") + theme_bw()


ggplot(filter(vol_agg, Year == 2015 & city == "AMES")) + 
  geom_line(aes(x = Month, y = sale_liters, 
                 group = city, color = city)) + ylab("Volume Sold (Liters)")
