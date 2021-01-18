


# Load packages -----------------------------------------------------------

library(readr)
library(dplyr)
library(lubridate)
library(purrr)
library(tidyr)
library(tibble)
library(ggplot2)
library(stringr)
library(unpivotr) # for dealing with the SWW spot data

# Paths to data -----------------------------------------------------------

SWW_spot_folder <- "Data_Input/SWW_Spot"
SWW_spot_files <- list.files(SWW_spot_folder)
SWW_spot_path <- paste(SWW_spot_folder, SWW_spot_files, sep = "/")


# Bring SWW Spot WQ data into list and tidy -------------------------------

# Read in SWW spot sample data
SWW_spot <- read_csv(SWW_spot_path, col_names = FALSE) # we'll name the cols later
#SWW_spot <- map(SWW_spot_path, read_csv) # for when/if there are more... and naming conventions are better


# Separate into list of dfs based on blank row 
SWW_spot <- SWW_spot %>% 
  group_split(grp = cumsum(rowSums(is.na(.)) == ncol(.)), .keep = FALSE) %>%
  map_at(.at = -1, tail, -1)

# tidy into something useful
SWW_spot <- SWW_spot %>% 
  map(.,~ unpivotr::as_cells(.x) %>% # make the data long format   #rectify(SWW_spot[[1]]) had a look at the data structure before stripping out the headings
        unpivotr::behead("up", "determinand") %>% 
        behead("up", "units") %>% 
        behead("left", "location") %>% 
        behead("left", "datetime") %>% 
        behead("left", "sww_id") %>% 
        behead("left", "sww_location_code") %>% 
        behead("left", "sww_comments") %>% 
        behead("left", "sww_sample_type") %>% 
        filter(!is.na(chr)) %>% # remove all the NA results
        mutate(result = as.numeric(chr),
               datetime = dmy_hm(datetime),
               WTW = word(location), #first word in location
               location = str_squish(location), # remove random spaces
               location = str_replace_all(location, " ", "_"),
               location = str_replace_all(location, "_-_", "-")) %>% 
        select(location, datetime, determinand, units, result, WTW, sww_location_code, sww_sample_type)) # select out the cols of interest

# quick check: SWW_spot[[2]]

# name list elements using location name
names(SWW_spot) <- SWW_spot %>% 
  map(., ~ pull(distinct(., location)))

# bind into single list element per location
SWW_spot <- split(SWW_spot, names(SWW_spot)) %>% 
  map(bind_rows)

# output as csv files for 'extracted data' folder use (not run)
# for (i in seq_along(SWW_spot)){
#   filename = paste0(names(SWW_spot[i]), "2010-2020_09.csv")
#   write_csv(SWW_spot[[i]], filename)
# }
