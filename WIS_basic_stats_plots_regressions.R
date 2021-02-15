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
if (!require('lfstat')) install.packages('lfstat'); library('lfstat')

# Set up the plot theme and colour palette -------------------------------------

R_grad_UsT <- colorRampPalette(c(
  rgb(229, 109, 11, max=255),   
  rgb(248, 171, 16, max=255),   
  rgb(253, 231, 37, max=255), 
  rgb(180, 222, 80, max=255), 
  rgb(108, 179, 63, max=255),  
  rgb(102, 158, 144, max=255),  
  rgb(0, 132, 145, max=255),  
  rgb(0, 104, 139, max=255)))

theme_set(theme_bw() + theme(panel.grid.major = element_blank(), 
                             panel.grid.minor = element_blank(),
                             axis.text = element_text(size = 8),
                             axis.text.x = element_text(angle = 0, vjust = 1, hjust = 0.5, colour = "gray28", margin = margin(t=5)),
                             axis.text.y = element_text(angle = 0, vjust = 0.5, colour = "gray28", margin = margin(r=5)),
                             axis.title.x = element_text(size = 8, margin = margin(t=5)),
                             axis.title.y = element_text(size = 8, margin = margin(r=5)),
                             panel.grid = element_blank(),
                             plot.margin = unit(c(0.25, 0.25, 0.25, 0.25), units = , "cm"),
                             plot.title = element_text(size = 8, vjust = 1, hjust = 0.5),
                             legend.title = element_text(size = 8),
                             legend.text = element_text(size = 8),
                             legend.key.size = unit(1,"line"),
                             legend.position = c(0.85, 0.9)))


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


# get a list of all the sites - this needs to be done at this stage!
list_sites <- names(SWW_spot) # identifies the names of each tibble within the large list


# NEEDS TO ACCOUNT FOR THE CHANGE IN BST / GMT


# add week and water year in a long table --------------------------------------

SWW_spot <- bind_rows(SWW_spot) %>%
  mutate(water_year =  water_year(datetime, origin = "usgs"),  # reference to water year starting on the 1/10)
         week = as.factor(strftime(datetime, format = "%V"))) 

# returns it into a list of tibbles
#SWW_spot <- SWW_spot %>% 
  #group_by(location) %>%
  #group_split()   # splits it
#names(SWW_spot) <- SWW_spot %>%
  #map(., ~ pull(distinct(., location))) # renames it with the name of the location - do not use otherwise it breaks the code later



# Calculate summary statistics per hydrological year and parameter for all sites ------

summary <- SWW_spot %>%
  group_by(location, determinand, units, water_year) %>%
  summarise(mean = mean(result, .drop = TRUE),
            n = sum(!is.na(result)),
            min = min(result),
            max = max(result)) 

# group per location to save one table for each

summary <- summary %>%
  group_by(location) %>%
  group_split()   # makes into a list per location

names(summary) <- summary %>%
  map(., ~ pull(distinct(., location))) # renames it with the name of the location


# output as csv files for 'extracted data' folder use (not run) - CANNOT FIND A WAY TO GET TO SAVE IT IN THE APPROPRIATE FOLDER
for (i in seq_along(summary)){
  filename = paste0(names(summary[i]), "_2010-20_stats.csv")
  write_csv(summary[[i]], filename)
}




# Plots for HOREDOWN -----------------------------------------------------------

# Select sites of interest -----------------------------------------------------

# remove the sites not to be used

# identify the site to keep in the list
print(list_sites)
sites_keep <- list_sites[grepl("HOREDOWN",list_sites)] # makes a list of the names of tibble that contain "Bratton"

# select and bind all the tibbles and keep only what I want
dat <- bind_rows(SWW_spot) %>%
  filter(location %in% sites_keep)                # this makes a whole table with all the data I want to keep!

dat$month <- format(dat$datetime, format="%m")


# group and make a table of summary statistics ---------------------------------

summary_HOR <- dat %>%
  group_by(determinand, units, water_year) %>%
  summarise(mean = mean(result, .drop = TRUE),
            n = sum(!is.na(result)),
            min = min(result),
            max = max(result)) 
#%>%
  #group_split() # makes a list for each




# summarize data per week and plot

test <- dat %>%
  group_by(water_year, month, determinand, units) %>%  # location needs adding as a parameter if this is to be applied to several locations
  summarise(mean = mean(result, .drop = TRUE),
            n = sum(!is.na(result)))

list_sites


# Plot the proportion of blue green per week as a TS ---------------------------

# select the determinands to be plotted (together) and select the matching data to be plotted
det_list <- unique(test$determinand)
algae_list <- det_list[grepl("Algae",det_list)] # makes a list of the names of tibble that contain "Bratton"
algae_list <- algae_list[-grep("Comments", algae_list)]

# Filter the appropriate date and make into a wide format
dat_alg <- dat %>%
  filter(determinand %in% algae_list) 
dat_alg <- spread(dat_alg,determinand, result)
dat_alg <- dat_alg [dat_alg$units == "cells/ml",]      # removes data that are not in cells/ml


# aggregate the data per week / water_year

# plot
ggplot(dat_alg, aes (x=datetime, y=result))+
  geom_point() +
  facet_wrap(vars(determinand))

aggregate( Total_Blue_Green ~ Month + Year , Algae , mean )




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
