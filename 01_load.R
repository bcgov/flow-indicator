# Copyright 2025 Province of British Columbia
#
# Licensed under the Apache License, Version 2.0 (the "License");
# you may not use this file except in compliance with the License.
# You may obtain a copy of the License at
#
# http://www.apache.org/licenses/LICENSE-2.0
#
# Unless required by applicable law or agreed to in writing, software distributed under the License is distributed on an "AS IS" BASIS,
# WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
# See the License for the specific language governing permissions and limitations under the License.

# Purpose:  Script loads data and uses various criteria to filter out flow recording stations in BC.

library(tidyverse)
library(lubridate)
library(tidyhydat)
library(stringr)

# Set up folder structure, for intermediate data
if(!dir.exists('data')) dir.create('data')

# Set up /www folder - used for shiny app
if(!dir.exists('app/www')) dir.create('app/www')

##### First pass to filter for stations with complete data

## Get all BC stations with "flow"
# STATIONS TO REMOVE BASED ON JON RECOMMENDATION (Trending Cluster Recommendations.xlsx)
stations_all_bc_list <- unique(hy_annual_stats(prov_terr_state_loc = "BC") %>%
                                 filter(Parameter == "Flow") %>%
                                 filter(! (STATION_NUMBER %in% c("07FD002",
                                                                 "08ND025",
                                                                 "08NH126",
                                                                 "08NM050",
                                                                 "08NM002",
                                                                 "08GC006",
                                                                 "08GC007",
                                                                 "07FA004"
                                 ))) %>%
                                 pull(STATION_NUMBER))

## Pulls HYDAT daily flow data for all BC stations that measure flow
#  Step takes a few minutes to complete

hydat_daily_all <- hy_daily_flows(station_number = stations_all_bc_list)

# Setting up water year (wYear) and low flow year (lfYear)
# wYear is defined as the period between October 1st of one year and September 30th of the next.
# See https://water.usgs.gov/nwc/explain_data.html
# and https://www.epa.gov/waterdata/definition-and-characteristics-low-flows-dflow#year for details

# lfYear is defined as the period starting from April 1st to March 31st.
# See https://www.epa.gov/waterdata/definition-and-characteristics-low-flows-dflow#year for details

hydat_daily_all <- hydat_daily_all %>%
  mutate(Year = year(Date),
         wYear = case_when(month(Date) >= 10 ~ year(Date),
                           month(Date) < 10 ~ year(Date) - 1),
         lfYear = case_when(month(Date) >= 4 ~ year(Date),
                            month(Date) < 4 ~ year(Date) - 1))

# Create daily station data for calendar year
daily_station_data_Year <- hydat_daily_all %>%
  filter(!is.na(Value)) %>%
  group_by(STATION_NUMBER, Year) %>%
  summarise(n = n(),
            ndays = max(yday(as.Date(paste0("31-12-", year(Date)), format = "%d-%m-%Y"))), #yday() has accounted for leap years when counting days per calender year.
            perc_daily_missing = ((ndays - n) / ndays) * 100) %>%
  select(STATION_NUMBER,
         Year,
         perc_daily_missing_Year = perc_daily_missing)

# Create daily station data for water year
daily_station_data_wYear <- hydat_daily_all %>%
  filter(!is.na(Value)) %>%
  group_by(STATION_NUMBER, wYear) %>%
  summarise(n = n(),
            #ndays = max(yday(as.Date(paste0("31-12-", year(Date)), format = "%d-%m-%Y"))), #yday() does not account for leap years when counting days per water year. This may inflate the percentage of missing values.
            ndays = as.numeric(as.Date(paste0("01-10-", max(wYear)+1), format = "%d-%m-%Y")-as.Date(paste0("01-10-", max(wYear)), format = "%d-%m-%Y")), # Zhuoyan thinks this is accurate way to counting days per water year.
            perc_daily_missing = ((ndays - n) / ndays) * 100) %>%
  select(STATION_NUMBER,
         Year = wYear,
         perc_daily_missing_wYear = perc_daily_missing)

# Do the same for low flow year
daily_station_data_lfYear <- hydat_daily_all %>%
  filter(!is.na(Value)) %>%
  group_by(STATION_NUMBER, lfYear) %>%
  summarise(n = n(),
            #ndays = max(yday(as.Date(paste0("31-12-", year(Date)), format = "%d-%m-%Y"))), #yday() does not account for leap years when counting days per low flow year. This may inflate the percentage of missing values.
            ndays = as.numeric(as.Date(paste0("01-10-", max(lfYear)+1), format = "%d-%m-%Y")-as.Date(paste0("01-10-", max(lfYear)), format = "%d-%m-%Y")), # Zhuoyan thinks this is accurate way to counting days per low flow year.
            perc_daily_missing = ((ndays - n) / ndays) * 100) %>%
  select(STATION_NUMBER,
         Year = lfYear,
         perc_daily_missing_lfYear = perc_daily_missing)

daily_station_data <- daily_station_data_wYear %>%
  left_join(daily_station_data_lfYear) %>%
  left_join(daily_station_data_Year) %>%
  left_join(hy_stations(), by = "STATION_NUMBER") %>%
  left_join(hy_stn_regulation(), by = "STATION_NUMBER") %>%
  filter(Year >= 1915)

#Create complete station-year df
minYear <- min(daily_station_data$Year)
maxYear <- max(daily_station_data$Year)
years <- seq(minYear, maxYear)
stations <- unique(daily_station_data$STATION_NUMBER)

station_year <- expand.grid(stations, years) %>%
  select("STATION_NUMBER" = "Var1", "Year" = "Var2")# this will then be joined with each step of filtering (new column with keep/discard based on filter)

station_summary <- daily_station_data %>%
  group_by(STATION_NUMBER) %>%
  summarise(n_years = n(),
            incomplete_wYears = sum(perc_daily_missing_wYear>0),
            incomplete_lfYears = sum(perc_daily_missing_lfYear>0, na.rm=TRUE), # remove NA before summing
            year_min = min(Year),
            year_max = max(Year)) %>%
  left_join(hy_stations(), by = "STATION_NUMBER") %>%
  left_join(hy_stn_regulation(), by = "STATION_NUMBER")

# stations_filt_list <- unique(stations_filt$STATION_NUMBER)

saveRDS(hydat_daily_all, file = 'data/hydat_daily_all.rds')
saveRDS(daily_station_data, file = 'data/daily_station_data.rds')
# saveRDS(stations_filt_list, file = 'data/stations_filt_list.rds')
# saveRDS(stations_filt, file = 'data/stations_filt_no_missing.rds')
saveRDS(station_year, file = 'data/station_year.rds')
saveRDS(station_summary, file = 'data/station_summary.rds')
