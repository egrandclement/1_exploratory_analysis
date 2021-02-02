# Aim of the script: This script opens the data for Wistlandpound (contained 
# amongst other sites in one sheet) and turns it into long form
# Basic plot are then made





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

# Paths to data ----------------------------------------------------------------

# NOTE THIS CURRENTLY ONLY WORKS WHEN THERE IS ONLY ONE FILE IN THE FOLDER - NEEDS AMENDING TO IDENTIFY THE ONE FILE ONLY
SWW_spot_folder <- "Data_Input/SWW_Spot"
#SWW_spot_files <- list.files(SWW_spot_folder)
SWW_spot_files <- paste("upstream thinking Exe Uni_BFlemming_Horedown_Northcombe_Prewly_2010_2020_09.csv")
SWW_spot_path <- paste(SWW_spot_folder, SWW_spot_files, sep = "/")


# Bring SWW Spot WQ data into list and tidy ------------------------------------

# Read in SWW spot sample data
SWW_spot <- read_csv(SWW_spot_path, col_names = FALSE) # we'll name the cols later

SWW_spot <- SWW_spot[-c(1),]  # removes the first row that only contains the words "report"

#SWW_spot  <- read_csv("upstream thinking Exe Uni_BFlemming_Horedown_Northcombe_Prewly_2010_2020_09.csv", col_names = FALSE)
SWW_spot <- SWW_spot %>%
  group_split(grp = cumsum(rowSums(is.na(.[,-1])) == ncol(.[,-1])), .keep = FALSE)

SWW_spot <- SWW_spot %>%
  map(.,~ unpivotr::as_cells(.x) %>% # make the data long format   #rectify(SWW_spot[[1]]) had a look at the data structure before stripping out the headings
        unpivotr::behead("up", "location") %>%
        fill(location) %>% # fill location down col
        unpivotr::behead("up", "determinand") %>%
        behead("up", "units") %>%
        behead("left", "datetime") %>%
        behead("left", "sww_x") %>%
        behead("left", "sww_y") %>%
        behead("left", "sww_z") %>%
        filter(!is.na(chr)) %>% # remove all the NA results
        mutate(result = as.numeric(chr),
               datetime = dmy_hm(datetime),
               WTW = word(location), #first word in location
               location = str_squish(location), # remove random spaces
               location = str_replace_all(location, " ", "_"),
               location = str_replace_all(location, "_-_", "-")) %>%
        select(location, datetime, determinand, units, result, WTW, sww_x, sww_y, sww_z))
names(SWW_spot) <- SWW_spot %>%
  map(., ~ pull(distinct(., location))) # gives each tibble the name of the location





# add week and water year

SWW_spot <- SWW_spot %>%
  map(.,mutate(., week = strftime(datetime, format = "%V"),
                  water_year(datetime, origin = "usgs"))  %>%
        select(location, datetime, determinand, units, result, WTW, week, water_year,sww_x, sww_y, sww_z))
names(SWW_spot) <- SWW_spot %>%
  map(., ~ pull(distinct(., location)))

dat$water_year <- water_year(dat$datetime, origin = "usgs")   # reference to water year starting on the 1/10
dat$week <- as.factor(strftime(dat$datetime, format = "%V")) # reference to number of week of the year

# bind into single list element per location
SWW_spot <- split(SWW_spot, names(SWW_spot)) %>%
  map(bind_rows)


# get summary statistics per hydrological year and parameter for all sites
summary <- dat %>%
  group_by(water_year, determinand) %>%
  summarise(mean = mean(result, .drop = TRUE),
            n = sum(!is.na(result)),
            min = min(result),
            max = max(result)) %>%
  group_split() # makes a list for each





# Select sites of interest -----------------------------------------------------

# get a list of all the sites
list_sites <- names(SWW_spot) # identifies the names of each tibble within the large list

# remove the sites not to be used
sites_keep <- grep("HOREDOWN", names(SWW_spot), value=TRUE) # makes a list of the names of tibble that contain "Bratton"

# bind all the tibbles and keep only what I want
dat <- bind_rows(SWW_spot) %>%
  filter(location %in% sites_keep)                # this makes a whole table with all the data I want to keep!


# Add references to water years etc --------------------------------------------


if (!require('lfstat')) install.packages('lfstat'); library('lfstat')
dat$water_year <- water_year(dat$datetime, origin = "usgs")   # reference to water year starting on the 1/10
dat$week <- as.factor(strftime(dat$datetime, format = "%V")) # reference to number of week of the year


# group and make a table of summary statistics ---------------------------------

summary_HOR <- dat %>%
  group_by(water_year, determinand) %>%
  summarise(mean = mean(result, .drop = TRUE),
            n = sum(!is.na(result)),
            min = min(result),
            max = max(result)) %>%
  group_split() # makes a list for each




# summarize data per week and plot

test <- dat %>%
  group_by(location, water_year, week, determinand) %>%
  summarise(mean = mean(result, .drop = TRUE),
            n = sum(!is.na(result)))

list_sites


# Plot -------------------------------------------------------------------------

ggplot

test_2 <- dat %>%
  filter(location=="BRATTON_FLEMING_WTW-RAW_WATER-RESERVOIR_UNSPECIFIED")

ggplot(test_2, aes (x=datetime, y=result))+
  geom_point()


sut476 <- counts_norm %>% filter(gene==) 

sut476_wt <- sut476 %>% filter(strain=="WT")

ggplot(sut476_wt,aes(x=time,y=log_norm_count)) +
  geom_point() +
  geom_smooth(method="lm")



################################################################################
# this selects locations and then makes a list of tibbles
test_2 <- bind_rows(SWW_spot) %>%
  filter(location %in% sites_keep) %>%
  group_by(location)%>%   # split  
  group_split()           # group as a list of items.

# This is to group into list of tibbles
test_2 <- dat %>%
  group_by(location)%>%   # Identify the grouping variable
  group_split()           # group as a list of items.

# This is to group into list of tibbles
test_2 <- bind_rows(dat) %>%
  group_by(location)%>%   # split  
  group_split()           # group as a list of items.


# check if the data needs to be converted to GMT?
#dat$datetime  <- as.POSIXct(strptime(as.character(dat$datetime),                # N.B. strptime() converts from character to POSIXct time
#"%d/%m/%Y %H:%M",                          # Format of origional text (toggle on/off as required)
"%Y-%m-%d %H:%M:%S",                      # Format of origional text (toggle on/off as required)
tz = "UTC"))  

# make a similar table just for the raw water
# remove the sites not to be used
list_raw <- sites_keep[grepl("RAW_WATER", sites_keep)] # 
