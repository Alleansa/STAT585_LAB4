library(tidyverse)
# Get the api address
url <- "https://data.iowa.gov/resource/m3tr-qhgy.json?county=Story"

# Make 1000 list of stores in Story county
story_list <- jsonlite::read_json(url)

# Get the variable names using list.flatten
new.distinct.names <- story_list %>% 
  purrr::map(.x, 
             .f=~names(rbind.data.frame(rlist::list.flatten(.x),0)))

#  Get the observations from variables
unlisted <- story_list %>% 
  purrr::map(.f = ~rbind.data.frame(unlist(.x, recursive=T, use.names=T)))

# Make a list of data frames
unlisted.info <- purrr::map2(unlisted,
                             new.distinct.names,
                             .f= ~purrr::set_names(.x, .y))

# Combine data frames in the list
story_dat <- do.call(plyr::rbind.fill, unlisted.info)

# We do not need these any more.
rm(url, story_list, new.distinct.names, unlisted, unlisted.info)
