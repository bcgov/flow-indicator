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

### Load in data that was produced in the '02_clean.R' script.

library(EnvStats)
library(tidyverse)
library(data.table)
library(tidyhydat)
library(sf)
library(fasstr)
library(rmapshaper)
library(mice)

if(!exists("final_station_summary_wYear")){final_station_summary_wYear = read_csv('data/finalstns_wYear.csv')}
if(!exists("final_station_summary_lfYear")){final_station_summary_lfYear = read_csv('data/finalstns_lfYear.csv')}
if(!exists("final_station_summary_cYear")){final_station_summary_cYear = read_csv('data/finalstns_cYear.csv')}
if(!exists("filtered_station_year_wYear")){filtered_station_year_wYear = read_csv('data/finalstnyr_wYear.csv')}
if(!exists("filtered_station_year_lfYear")){filtered_station_year_lfYear = read_csv('data/finalstnyr_lfYear.csv')}
if(!exists("filtered_station_year_cYear")){filtered_station_year_cYear = read_csv('data/finalstnyr_cYear.csv')}

# Load regime group table
regime_groups <- read.csv("data/river_groups.csv")

# If no /www folder (used for the shiny app, and also for static results PDF)
if(!dir.exists('app/www')) dir.create('app/www')

# Pull out the stations to keep from the loading script
stations_to_keep <- final_station_summary_wYear %>%
  pull(STATION_NUMBER)

# The below code calculates the following flow variables:
# 1. Average (mean) flow per year,
# 2. Timing of freshet (i.e. day of year by which 50% of flow has passed),
# 3. Summer (May to late October) 7-day flow minimum / day of year of 7-day flow minimum
# 4. Peak annual flow (7-day flow maximum)

# Read in ALL daily flow info from the database for our stations to keep.
# Note: this includes 'gappy' data we identified in script 1.
# ~5 million rows, 7 columns.
# Add year, water year and low flow year
flow_dat <- tidyhydat::hy_daily_flows(stations_to_keep) %>%
  mutate(Year = year(Date),
         wYear = case_when(month(Date) >= 10 ~ year(Date),
                           month(Date) < 10 ~ year(Date) - 1),
         lfYear = case_when(month(Date) >= 4 ~ year(Date),
                            month(Date) < 4 ~ year(Date) - 1)) %>%
  filter(Parameter == 'Flow') %>%
  filter(!is.na(Value)) %>%
  mutate(Month = lubridate::month(Date)) %>%
  left_join(regime_groups)

#Restrict by station_year list (this will remove years with missing data)
# Water year
flow_dat_filtered_wYear <- flow_dat %>%
  filter(paste0(STATION_NUMBER, wYear) %in% paste0(filtered_station_year_wYear$STATION_NUMBER,filtered_station_year_wYear$Year)) %>%
  group_by(STATION_NUMBER) %>%
  summarize(min_Date = as.Date(min(Date)), max_Date = as.Date(max(Date))) %>%
  group_by(STATION_NUMBER) %>%
  mutate(Date = list(seq(min_Date, max_Date, by='1 day'))) %>%
  unnest(cols = c(Date)) %>% # Lines 65 to 69 add all dates across time periods including those with NAs. This is for subsequent imputation analysis.
  select(-min_Date, -max_Date) %>%
  left_join(flow_dat %>%
              filter(paste0(STATION_NUMBER, wYear) %in% paste0(filtered_station_year_wYear$STATION_NUMBER,filtered_station_year_wYear$Year))) %>%
  mutate(wYear = case_when(month(Date) >= 10 ~ year(Date),
                           month(Date) < 10 ~ year(Date) - 1),
         lfYear = case_when(month(Date) >= 4 ~ year(Date),
                            month(Date) < 4 ~ year(Date) - 1),
         Month = month(Date),
         DoY = case_when(month(Date) >= 10 ~ yday(Date) - yday(paste0(year(Date),"-09-30")),
                          #month(Date) < 10 ~ yday(Date) + (365 - yday(paste0(year(Date),"-09-30"))))) %>%
                         month(Date) < 10 ~ as.numeric(as.Date(Date)-as.Date(paste0(wYear,"-09-30")))),
         Missing = case_when(is.na(Value) ~ "Y",
                             !is.na(Value) ~ "N")) %>%
  dplyr::select(-lfYear, -Year)

# Low flow year
flow_dat_filtered_lfYear <- flow_dat %>%
  filter(paste0(STATION_NUMBER, lfYear) %in% paste0(filtered_station_year_lfYear$STATION_NUMBER,filtered_station_year_lfYear$Year)) %>%
  group_by(STATION_NUMBER) %>%
  summarize(min_Date = as.Date(min(Date)), max_Date = as.Date(max(Date))) %>%
  group_by(STATION_NUMBER) %>%
  mutate(Date = list(seq(min_Date, max_Date, by='1 day'))) %>%
  unnest(cols = c(Date)) %>% # Lines 86 to 90 add all dates across time periods including those with NAs. This is for subsequent imputation analysis.
  select(-min_Date, -max_Date) %>%
  left_join(flow_dat %>%
              filter(paste0(STATION_NUMBER, lfYear) %in% paste0(filtered_station_year_lfYear$STATION_NUMBER,filtered_station_year_lfYear$Year))) %>%
  mutate(wYear = case_when(month(Date) >= 10 ~ year(Date),
                           month(Date) < 10 ~ year(Date) - 1),
         lfYear = case_when(month(Date) >= 4 ~ year(Date),
                            month(Date) < 4 ~ year(Date) - 1),
         Month = month(Date),
         DoY = case_when(month(Date) >= 4 ~ yday(Date) - yday(paste0(year(Date),"-03-31")),
                         #month(Date) < 4 ~ yday(Date) + (365 - yday(paste0(year(Date),"-03-31"))))) %>%
                         month(Date) < 4 ~ as.numeric(as.Date(Date)-as.Date(paste0(lfYear,"-03-31")))),
         Missing = case_when(is.na(Value) ~ "Y",
                             !is.na(Value) ~ "N")) %>%
  dplyr::select(-wYear, -Year)

#Calendar year
flow_dat_filtered_cYear <- flow_dat %>%
  mutate(Year = year(Date)) %>%
  filter(paste0(STATION_NUMBER, Year) %in% paste0(filtered_station_year_cYear$STATION_NUMBER,filtered_station_year_cYear$Year)) %>%
  group_by(STATION_NUMBER) %>%
  summarize(min_Date = as.Date(min(Date)), max_Date = as.Date(max(Date))) %>%
  group_by(STATION_NUMBER) %>%
  mutate(Date = list(seq(min_Date, max_Date, by='1 day'))) %>%
  unnest(cols = c(Date)) %>% # Lines 108 to 112 add all dates across time periods including those with NAs. This is for subsequent imputation analysis.
  select(-min_Date, -max_Date) %>%
  left_join(flow_dat %>%
              filter(paste0(STATION_NUMBER, Year) %in% paste0(filtered_station_year_cYear$STATION_NUMBER,filtered_station_year_cYear$Year))) %>%
  mutate(wYear = case_when(month(Date) >= 10 ~ year(Date),
                           month(Date) < 10 ~ year(Date) - 1),
         lfYear = case_when(month(Date) >= 4 ~ year(Date),
                            month(Date) < 4 ~ year(Date) - 1),
         DoY = yday(Date),
         Month = month(Date),
         Year = year(Date),
         Missing = case_when(is.na(Value) ~ "Y",
                             !is.na(Value) ~ "N")) %>%
  dplyr::select(-wYear, -lfYear)

# Imputation using mice package==========================================================================
# Water year
wYear_m <- max(5, round(sum(is.na(flow_dat_filtered_wYear$Value))/length(flow_dat_filtered_wYear$Value) *100)) # Determine how many imputed datasets should be generated.
flow_dat_filtered_wYear_imp <- mice(flow_dat_filtered_wYear %>%
              select(-Symbol, -Regime, -clustGroup, -Missing), m = wYear_m, , seed=50000)

flow_dat_filtered_wYear_imp_long <- mice::complete(flow_dat_filtered_wYear_imp, action='long') %>%
  left_join(flow_dat_filtered_wYear %>%
              select(-Value),
            by = join_by(STATION_NUMBER, Parameter, Date, wYear, Month, DoY))

# Low flow year
lfYear_m <- max(5, round(sum(is.na(flow_dat_filtered_lfYear$Value))/length(flow_dat_filtered_lfYear$Value) *100)) # Determine how many imputed datasets should be generated.
flow_dat_filtered_lfYear_imp <- mice(flow_dat_filtered_lfYear %>%
                                      select(-Symbol, -Regime, -clustGroup, -Missing), m = lfYear_m, , seed=50000)

flow_dat_filtered_lfYear_imp_long <- mice::complete(flow_dat_filtered_lfYear_imp, action='long') %>%
  left_join(flow_dat_filtered_lfYear %>%
              select(-Value),
            by = join_by(STATION_NUMBER, Parameter, Date, lfYear, Month, DoY))

#Calendar year
cYear_m <- max(5, round(sum(is.na(flow_dat_filtered_cYear$Value))/length(flow_dat_filtered_cYear$Value) *100)) # Determine how many imputed datasets should be generated.
flow_dat_filtered_cYear_imp <- mice(flow_dat_filtered_cYear %>%
                                       select(-Symbol, -Regime, -clustGroup, -Missing), m = cYear_m, , seed=50000)

flow_dat_filtered_cYear_imp_long <- mice::complete(flow_dat_filtered_cYear_imp, action='long') %>%
  left_join(flow_dat_filtered_cYear %>%
              select(-Value),
            by = join_by(STATION_NUMBER, Parameter, Date, Year, Month, DoY))

#Find the years with all values being NAs
wYear_na <- flow_dat_filtered_wYear %>%
  group_by(STATION_NUMBER, wYear) %>%
  summarize(mean=mean(Value,na.rm = TRUE)) %>%
  filter(is.na(mean)) %>%
  select(STATION_NUMBER, wYear)

lfYear_na <- flow_dat_filtered_lfYear %>%
  group_by(STATION_NUMBER, lfYear) %>%
  summarize(mean=mean(Value,na.rm = TRUE)) %>%
  filter(is.na(mean)) %>%
  select(lfYear)

cYear_na <- flow_dat_filtered_cYear %>%
  group_by(STATION_NUMBER, Year) %>%
  summarize(mean=mean(Value,na.rm = TRUE)) %>%
  filter(is.na(mean)) %>%
  select(Year)

# METRICS ==========================================================================

# Annual Values =========================================================
# Use water year (October - October)
annual_mean_dat <- flow_dat_filtered_wYear_imp_long %>% # use the imputation datasets to calculate the average
  filter(!(paste0(STATION_NUMBER, wYear) %in% paste0(wYear_na$STATION_NUMBER,wYear_na$wYear))) %>% # remove the years with all values being NAs
  group_by(wYear,STATION_NUMBER, .imp) %>%
  summarize(Average = mean(Value),
            Missing = case_when(any(Missing =="Y") ~ "Y",
                                .default = "N")) %>%
  group_by(wYear,STATION_NUMBER) %>%
  summarize(Average = mean(Average), # follow Rubin's Rules to pool the averages from imputed datasets.
            Missing = case_when(any(Missing =="Y") ~ "Y",
                                .default = "N")) %>%
  #filter(wYear >= 1915) %>%# remove some random early year that looks to not have been removed by gap code (no idea why!). Zhuoyan's comment: I did not see any gaps in early years.
  rename(Year = wYear) %>%
  ungroup()

# Check if data gaps are gone
ggplot(annual_mean_dat, aes(Year,STATION_NUMBER, colour = Average)) +
  geom_point()

# Timing of Freshet =========================================================
# Snow-Dominated - early peak - Use water year (October - October)
flow_timing_earlypeak <- flow_dat_filtered_wYear_imp_long %>%
  filter(Regime == "Snow-Dominated - Early Peak") %>%
  group_by(STATION_NUMBER, wYear, .imp) %>%
  mutate(RowNumber = row_number(),
         TotalFlow = sum(Value),
         FlowToDate = cumsum(Value)) %>%
  group_by(wYear, STATION_NUMBER, .id, Date, Month, DoY, clustGroup, Regime, RowNumber) %>%
  summarize(TotalFlow = mean(TotalFlow),
            FlowToDate = mean(FlowToDate),
            Missing = case_when(any(Missing =="Y") ~ "Y",
                                .default = "N")) %>% # follow Rubin's Rules to pool the averages from imputed datasets.
  group_by(STATION_NUMBER, wYear) %>%
  filter(FlowToDate > TotalFlow/2) %>% # We define the timing of Freshet as the point at which cumulative flow exceeds half of the annual flow.
  slice(1) %>%
  mutate(DoY_50pct_TotalQ = DoY,
         Date = as.Date(DoY, origin = "2000-10-01")) %>%
  ungroup() %>%
  dplyr::select(STATION_NUMBER,
                Year = wYear,
                DoY_50pct_TotalQ,
                Date,
                Missing)

# Snow-Dominated- late peak - Use Calendar Year
flow_timing_latepeak <- flow_dat_filtered_cYear_imp_long %>%
  filter(Regime == "Snow-Dominated - Late Peak") %>%
  group_by(STATION_NUMBER, Year, .imp) %>%
  mutate(RowNumber = row_number(),
         TotalFlow = sum(Value),
         FlowToDate = cumsum(Value)) %>%
  group_by(Year, STATION_NUMBER, .id, Date, Month, DoY, clustGroup, Regime, RowNumber) %>%
  summarize(TotalFlow = mean(TotalFlow),
            FlowToDate = mean(FlowToDate),
            Missing = case_when(any(Missing =="Y") ~ "Y",
                                .default = "N")) %>% # follow Rubin's Rules to pool the averages from imputed datasets.
  group_by(STATION_NUMBER, Year) %>%
  filter(FlowToDate > TotalFlow/2) %>% # We define the criterion (cumulative flow > 1/2 annual flow).
  slice(1) %>%
  mutate(DoY_50pct_TotalQ = DoY,
         Date = as.Date(DoY, origin = "2000-01-01")) %>%
  ungroup() %>%
  dplyr::select(STATION_NUMBER,
                Year,
                DoY_50pct_TotalQ,
                Date,
                Missing)


# Mixed Regime - Use summer only (March to September)
flow_timing_mixed <- flow_dat_filtered_cYear_imp_long %>%
  filter(Regime == "Mixed Regimes" & between(Month, 3, 9)) %>%
  group_by(STATION_NUMBER,Year, .imp)%>%
  mutate(RowNumber = row_number(),
         TotalFlow = sum(Value),
         FlowToDate = cumsum(Value)) %>%
  group_by(Year, STATION_NUMBER, .id, Date, Month, DoY, clustGroup, Regime, RowNumber) %>%
  summarize(TotalFlow = mean(TotalFlow),
            FlowToDate = mean(FlowToDate),
            Missing = case_when(any(Missing =="Y") ~ "Y",
                                .default = "N")) %>% # follow Rubin's Rules to pool the averages from imputed datasets.
  group_by(STATION_NUMBER, Year) %>%
  filter(FlowToDate > TotalFlow/2) %>%
  slice(1) %>%
  mutate(DoY_50pct_TotalQ = DoY,
         Date = as.Date(DoY, origin = "2000-01-01")) %>%
  ungroup() %>%
  dplyr::select(STATION_NUMBER,
                Year,
                DoY_50pct_TotalQ,
                Date,
                Missing)

# Bind together
flow_timing_dat <- bind_rows(flow_timing_earlypeak,
                            flow_timing_latepeak,
                            flow_timing_mixed)

# Return to 50% Mean Annual Discharge (Start of Low Summer Flow) =========================================================

# use fasstr function to calculate % Mean Annual Discharge -50%
MAD_station <- flow_dat_filtered_lfYear_imp_long %>% #There is no NA in Value so ignore the warning messages about missing data.
               group_by(.imp) %>%
               group_modify(~ calc_longterm_mean(., values = Value,
                                 groups = STATION_NUMBER,
                                 percent_MAD = 50)) %>%
               group_by(STATION_NUMBER) %>%
               summarise(LTMAD = mean(LTMAD), # follow Rubin's Rules to pool the averages from imputed datasets.
                        `50%MAD` = mean(`50%MAD`))

# Use 20% for Rain-dominated stations
MAD_station_rain <- flow_dat_filtered_lfYear_imp_long %>% #There is no NA in Value so ignore the warning messages about missing data.
                    group_by(.imp) %>%
                    group_modify(~ calc_longterm_mean(.,
                                 values = Value,
                                 groups = STATION_NUMBER,
                                 percent_MAD = 20)) %>%
                    group_by(STATION_NUMBER) %>%
                    summarise(LTMAD = mean(LTMAD), # follow Rubin's Rules to pool the averages from imputed datasets.
                    `20%MAD` = mean(`20%MAD`))

rtn_2_mad_perc_rain <- flow_dat_filtered_lfYear_imp_long %>%
  group_by(STATION_NUMBER, Date, Parameter, Symbol, lfYear, Month, clustGroup, Regime, DoY, .imp) %>%
  summarize(Value = mean(Value)) %>%
  group_by(STATION_NUMBER, Date, Parameter, Symbol, lfYear, Month, clustGroup, Regime, DoY) %>%
  summarize(Value = mean(Value)) %>% # follow Rubin's Rules to pool the averages from imputed datasets.
  filter(Regime == "Rain-Dominated") %>%
  left_join(flow_dat_filtered_lfYear %>%
              select(-Symbol)) %>%
  left_join(MAD_station) %>%
  group_by(STATION_NUMBER) %>%
  mutate(below_mad_perc = case_when (Value <= '20%MAD' ~ 1,
                                     .default = 0)) %>%
  group_by(STATION_NUMBER, lfYear) %>%
  mutate(peak_date = DoY[which.max(Value)]) %>%
  filter(below_mad_perc == 1 & DoY>peak_date) %>%
  arrange(Date) %>%
  slice(1) %>%
  dplyr::select(STATION_NUMBER,
                Year = lfYear,
                R2MAD_DoY = DoY,
                Missing)

rtn_2_mad_perc_rest <- flow_dat_filtered_lfYear_imp_long %>%
  group_by(STATION_NUMBER, Date, Parameter, Symbol, lfYear, Month, clustGroup, Regime, DoY, .imp) %>%
  summarize(Value = mean(Value)) %>%
  group_by(STATION_NUMBER, Date, Parameter, Symbol, lfYear, Month, clustGroup, Regime, DoY) %>%
  summarize(Value = mean(Value)) %>% # follow Rubin's Rules to pool the averages from imputed datasets.
  filter(Regime != "Rain-Dominated") %>%
  left_join(flow_dat_filtered_lfYear %>%
             select(-Symbol)) %>%
  left_join(MAD_station) %>%
  group_by(STATION_NUMBER) %>%
  mutate(below_mad_perc = case_when (Value <= '50%MAD' ~ 1,
                                     .default = 0)) %>%
  group_by(STATION_NUMBER, lfYear) %>%
  mutate(peak_date = DoY[which.max(Value)]) %>%
  filter(below_mad_perc == 1 & DoY>peak_date) %>%
  arrange(Date) %>%
  slice(1) %>%
  dplyr::select(STATION_NUMBER,
                Year = lfYear,
                R2MAD_DoY = DoY,
                Missing)

#Bind together
rtn_2_mad_perc = bind_rows(rtn_2_mad_perc_rain,
                           rtn_2_mad_perc_rest)

# Low flows (7 day window) ========================================================================

low_flow_dat_filtered_7day <- stations_to_keep %>% map( ~ {

  # Tell us which station the map function is on...
  print(paste0('Working on station ',.x))

  # Grab daily flows for the station of interest in this iteration...
  daily_flows <- flow_dat_filtered_lfYear_imp_long %>% # use the imputation datasets to calculate the average
    filter(STATION_NUMBER ==.x)

  # Use {data.table} package to convert our dataframe into a data.table object
  daily_flows_dt <- daily_flows %>%
    group_by(.imp) %>%
    data.table::data.table(key = c('STATION_NUMBER','lfYear'))

  # Calculate the rolling average with a 'window' of 7 days, such that a given day's
  # mean flow is the average of that day plus six days LATER in the year ("align = 'right'").
  for (i in 1:lfYear_m) {
    daily_flows_dt$flow_7_Day <- as.numeric(rep(NA, nrow(daily_flows_dt)))
    daily_flows_dt[daily_flows_dt$.imp == i, "flow_7_Day"] <- frollmean(daily_flows_dt[daily_flows_dt$.imp == i, Value], 7, align = 'right', na.rm=T)
  }

  # Convert back from data.table object to a dataframe, and clean it up.

  # Filter for just summer months. (July to August)

  min_7_day_dat <- daily_flows_dt %>%
    as_tibble() %>%
    group_by(across(c(-.imp, -Value, -flow_7_Day))) %>%
    summarize(Value = mean(Value),
              flow_7_Day = mean(flow_7_Day, na.rm = TRUE)) %>% # follow Rubin's Rules to pool the averages from imputed datasets.
    ungroup() %>%
    # Missing data can produce identical minimum flow values.
    # Keep only the latest record for each such duplication.
    filter(flow_7_Day != lead(flow_7_Day) & between(Month, 6, 10)) %>%
    group_by(lfYear) %>%
    slice_min(flow_7_Day) %>%
    group_by(lfYear,flow_7_Day) %>%
    slice(1) %>%
    ungroup() %>%
    dplyr::select(-Parameter,-Value, -Symbol, -Month, Min_7_Day_summer_DoY = DoY,
                  Min_7_Day_summer_Date = Date, Min_7_Day_summer = flow_7_Day)

  summer_low_flows <- min_7_day_dat %>%
    dplyr::select(STATION_NUMBER, Year = lfYear, everything())

  min_7_day_dat <- daily_flows_dt %>%
    as_tibble() %>%
    group_by(across(c(-.imp, -Value, -flow_7_Day))) %>%
    summarize(Value = mean(Value),
              flow_7_Day = mean(flow_7_Day, na.rm = TRUE)) %>% # follow Rubin's Rules to pool the averages from imputed datasets.
    ungroup() %>%
    # Missing data can produce identical minimum flow values.
    # Keep only the latest record for each such duplication.
    filter(flow_7_Day != lead(flow_7_Day)) %>%
    group_by(lfYear) %>%
    slice_min(flow_7_Day) %>%
    group_by(lfYear,flow_7_Day) %>%
    slice(1) %>%
    ungroup() %>%
    dplyr::select(-Parameter,-Value, -Symbol, -Month, Min_7_Day_DoY = DoY,
                  Min_7_Day_Date = Date, Min_7_Day = flow_7_Day)

  low_flows <- min_7_day_dat %>%
    dplyr::select(STATION_NUMBER, Year = lfYear, everything()) %>%
    left_join(summer_low_flows, by = join_by(STATION_NUMBER, Year, clustGroup, Regime, Missing))
}) %>%
  bind_rows()

# Peak Flow (3 day window) =================================================================

high_flow_dat_filtered_3day <- stations_to_keep %>% map( ~ {

  # Tell us which station the map function is on...
  print(paste0('Working on station ',.x))

  # Grab daily flows for the station of interest in this iteration...
  daily_flows <- flow_dat_filtered_wYear_imp_long |>
    filter(STATION_NUMBER ==.x)

  # Use {data.table} package to convert our dataframe into a data.table object
  daily_flows_dt <- daily_flows %>%
    group_by(.imp) %>%
    data.table::data.table(key = c('STATION_NUMBER','wYear'))

  # Calculate the rolling average with a 'window' of 3 days, such that a given day's
  # mean flow is the average of that day plus two days LATER in the year ("align = 'right'").
  for (i in 1:wYear_m) {
    daily_flows_dt$flow_3_Day <- rep(0, nrow(daily_flows_dt))
    daily_flows_dt[daily_flows_dt$.imp == i, "flow_3_Day"] <- frollmean(daily_flows_dt[daily_flows_dt$.imp == i, Value], 3, align = 'right', na.rm=T)
  }

  # Find Peak flows (3-day average flows, could be any time of year)
  max_3_day_dat <- daily_flows_dt %>%
    as_tibble() %>%
    group_by(across(c(-.imp, -Value, -flow_3_Day))) %>%
    summarize(Value = mean(Value),
              flow_3_Day = mean(flow_3_Day, na.rm = TRUE)) %>% # follow Rubin's Rules to pool the averages from imputed datasets.
    ungroup() %>%
    # Missing data can produce identical minimum flow values.
    # Keep only the latest record for each such duplication.
    group_by(wYear) %>%
    slice_max(flow_3_Day) %>%
    group_by(wYear,flow_3_Day) %>%
    slice(1) %>%
    ungroup() %>%
    dplyr::select(-Parameter,-Value,-Symbol, -Month,
                  Max_3_Day_DoY = DoY, Max_3_Day_Date = Date,
                  Max_3_Day = flow_3_Day, Year = wYear)
}) %>%
  bind_rows()

# Bind all the metrics together in single dataframe
annual_flow_dat_filtered <- annual_mean_dat  %>%
  left_join(flow_timing_dat) %>%
  left_join(rtn_2_mad_perc) %>%
  left_join(low_flow_dat_filtered_7day, by = c("STATION_NUMBER"="STATION_NUMBER", "Year"= "Year", "Missing")) %>%
  left_join(high_flow_dat_filtered_3day, by = c("STATION_NUMBER"="STATION_NUMBER", "Year"= "Year", "clustGroup", "Regime", "Missing")) %>%
  dplyr::select(-ends_with("_Date"))

# Monthly Values =========================================================

## Here we are only calculating three variables by month.
## i. Average flow
## ii. 7-day Average Low Flow
## iii. 3-day Average Peak flow
## iii. values needed for hydrograph:
###     a. 50% quantiles (percentiles?) of normal flow
###     b. 90% quantiles (percentiles?) of normal flow


# Check for missing data within each year and removes months with more than 10% missing
# Calculate the mean flow for each month
monthly_mean_dat_filtered <- flow_dat_filtered_wYear_imp_long %>%
  filter(!(paste0(STATION_NUMBER, wYear) %in% paste0(wYear_na$STATION_NUMBER,wYear_na$wYear))) %>% # Remove years with all values having NAs
  group_by(wYear,Month,STATION_NUMBER) %>%
  #mutate(n = n(),
  #  perc_na = ((days_in_month(Month)-n())/days_in_month(Month))*100) %>%
  #filter(perc_na<10) %>%
  reframe(median_flow = median(Value,na.rm=T),
          mean_flow = mean(Value, na.rm = T),
          Missing = case_when(any(Missing =="Y") ~ "Y",
                              .default = "N")) %>%
  rename(Year = wYear)


# Quantiles (for hydrograph)
monthly_quantiles_dat <- flow_dat_filtered_wYear_imp_long %>%
  filter(!(paste0(STATION_NUMBER, wYear) %in% paste0(wYear_na$STATION_NUMBER,wYear_na$wYear))) %>% # Remove years with all values having NAs
  group_by(Month,STATION_NUMBER) %>%
  reframe(percentiles = list(quantile(Value, probs = c(0.05,0.25,0.50,0.75,0.95)))) %>%
  unnest_wider(percentiles) %>%
  dplyr::rename(five_perc = `5%`, twentyfive_perc = `25%`,
                median_flow = `50%`,
                seventyfive_perc = `75%`, ninetyfive_perc = `95%`)

# Find the 7-day low flows per month.
monthly_7day_lowflow_dat_filtered <- stations_to_keep %>% map( ~ {

  # Tell us which station the map function is on...
  print(paste0('Working on station ',.x))

  # Grab daily flows for the station of interest in this iteration...
  daily_flows <- flow_dat_filtered_lfYear_imp_long %>%
    filter(STATION_NUMBER == .x)

  # Use {data.table} package to convert our dataframe into a data.table object
  daily_flows_dt <- daily_flows %>%
    group_by(.imp) %>%
    data.table::data.table(key = c('STATION_NUMBER','lfYear','Month'))

  # Calculate the rolling average with a 'window' of 7 days, such that a given day's
  # mean flow is the average of that day plus six days LATER in the year ("align = 'right'").
  for (i in 1:lfYear_m) {
    daily_flows_dt$flow_7_Day <- rep(0, nrow(daily_flows_dt))
    daily_flows_dt[daily_flows_dt$.imp == i, "flow_7_Day"] <- frollmean(daily_flows_dt[daily_flows_dt$.imp == i, Value], 7, align = 'right', na.rm=T)
  }

  # Convert back from data.table object to a dataframe, and clean it up.

  min_7_day_dat = daily_flows_dt %>%
    as_tibble() %>%
    group_by(across(c(-.imp, -Value, -flow_7_Day))) %>%
    summarize(Value = mean(Value),
              flow_7_Day = mean(flow_7_Day, na.rm = TRUE)) %>% # follow Rubin's Rules to pool the averages from imputed datasets.
    ungroup() %>%
    # Missing data can produce identical minimum flow values.
    # Keep only the latest record for each such duplication.
    filter(flow_7_Day != lead(flow_7_Day)) %>%
    group_by(lfYear,Month) %>%
    slice_min(flow_7_Day) %>%
    group_by(lfYear,Month,flow_7_Day) %>%
    slice(1) %>%
    ungroup() %>%
    dplyr::select(-Parameter,-Value,-Symbol,
                  Min_7_Day_DoY = DoY, Min_7_Day_Date = Date,
                  Min_7_Day = flow_7_Day)

  min_7_day_dat %>%
    dplyr::select(STATION_NUMBER, Month, Year = lfYear, everything())
}) %>%
  bind_rows()

# 3-day peak flows per month
monthly_3day_highflow_dat_filtered <- stations_to_keep %>% map( ~ {

  # Tell us which station the map function is on...
  print(paste0('Working on station ',.x))

  # Grab daily flows for the station of interest in this iteration...
  daily_flows <- flow_dat_filtered_wYear_imp_long %>%
    filter(STATION_NUMBER == .x) %>%
    group_by(STATION_NUMBER, wYear, Month) %>%
    ungroup()

  # Use {data.table} package to convert our dataframe into a data.table object
  daily_flows_dt <- daily_flows %>%
    group_by(.imp) %>%
    data.table::data.table(key = c('STATION_NUMBER','wYear','Month'))

  # Calculate the rolling average with a 'window' of 3 days, such that a given day's
  # mean flow is the average of that day plus two days LATER in the year ("align = 'right'").
  for (i in 1:wYear_m) {
    daily_flows_dt$flow_3_Day <- rep(0, nrow(daily_flows_dt))
    daily_flows_dt[daily_flows_dt$.imp == i, "flow_3_Day"] <- frollmean(daily_flows_dt[daily_flows_dt$.imp == i, Value], 3, align = 'right', na.rm=T)
  }

  # Convert back from data.table object to a dataframe, and clean it up.
  max_3_day_dat <- daily_flows_dt %>%
    as_tibble() %>%
    group_by(across(c(-.imp, -Value, -flow_3_Day))) %>%
    summarize(Value = mean(Value),
              flow_3_Day = mean(flow_3_Day, na.rm = TRUE)) %>% # follow Rubin's Rules to pool the averages from imputed datasets.
    ungroup() %>%
    # Missing data can produce identical minimum flow values.
    # Keep only the latest record for each such duplication.
    filter(flow_3_Day != lead(flow_3_Day)) %>%
    group_by(wYear,Month) %>%
    slice_max(flow_3_Day) %>%
    group_by(wYear,Month,flow_3_Day) %>%
    slice(1) %>%
    ungroup() %>%
    dplyr::select(-Parameter,-Value,-Symbol,
                  Max_3_Day_DoY = DoY, Max_3_Day_Date = Date,
                  Max_3_Day = flow_3_Day)

  max_3_day_dat %>%
    dplyr::select(STATION_NUMBER, Month, Year = wYear, everything())
}) %>%
  bind_rows()

# Combine these monthly datasets into 2 files:
## 1. Average flow + low flow + peak flow (metrics for the app).
## 2. Average flow + quantiles (needed for hydrograph)

monthly_flow_dat_filtered <- monthly_mean_dat_filtered %>%
  left_join(monthly_7day_lowflow_dat_filtered,
            by = join_by(STATION_NUMBER, Year, Month, Missing)) %>%
              dplyr::select(-Min_7_Day_DoY) %>%
  left_join(monthly_3day_highflow_dat_filtered,
            by = join_by(STATION_NUMBER, Year, Month, Regime, clustGroup, Missing)) %>%
              dplyr::select(-Max_3_Day_DoY) %>%
  mutate(Month = month.abb[Month]) %>%
  dplyr::rename('Average' = mean_flow) %>%
  dplyr::select(-ends_with("_Date"))

hydrograph_dat <- monthly_quantiles_dat %>%
  mutate(Month = month.abb[Month])

#calculate Mean Annual Discharge and add to hydrograph data
hydrograph_dat <- flow_dat_filtered_wYear_imp_long %>%
  filter(!(paste0(STATION_NUMBER, wYear) %in% paste0(wYear_na$STATION_NUMBER,wYear_na$wYear))) %>% # Remove years with all values having NAs
  group_by(STATION_NUMBER, .imp) %>%
  mutate(MAD = mean(Value)) %>%
  group_by(STATION_NUMBER) %>%
  mutate(MAD = mean(MAD)) %>% # follow Rubin's Rules to pool the averages from imputed datasets.
  dplyr::select(STATION_NUMBER, MAD) %>%
  distinct() %>%
  left_join(hydrograph_dat)

saveRDS(annual_flow_dat_filtered, 'app/www/annual_flow_dat.rds')
saveRDS(hydrograph_dat, 'app/www/hydrograph_dat.rds')
saveRDS(monthly_flow_dat_filtered,'app/www/monthly_flow_dat.rds')

# Get station locations
stations_sf <- tidyhydat::hy_stations(station_number = unique(annual_flow_dat_filtered$STATION_NUMBER)) %>%
  mutate(STATION_NAME = stringr::str_to_title(STATION_NAME),
         HYD_STATUS = stringr::str_to_title(HYD_STATUS)) %>%
  st_as_sf(coords = c("LONGITUDE","LATITUDE"), crs = 4326) %>%
  dplyr::select(STATION_NUMBER,STATION_NAME,HYD_STATUS) %>%
  left_join(final_station_summary_wYear)

## Jons script for watersheds and major basins ============================================

# BC Boundaries
bound <- bcmaps::bc_bound_hres()%>%
  st_as_sf(crs = 4326)

# WSC Basins and clip to BC (remove Alberta and territory basins)
wsc_basins <- bcmaps::wsc_drainages()%>%
  filter(MAJOR_DRAINAGE_AREA_CD != "05",
         !SUB_SUB_DRAINAGE_AREA_CD %in% c("07AA","07AB",
                                          "10ED","10FA")) %>%
  st_as_sf(crs = 4326) %>%
  st_intersection(bound) %>%
  st_transform(crs = 4326)

# View the map
mapview::mapview(wsc_basins, zcol = "SUB_SUB_DRAINAGE_AREA_CD")
mapview::mapview(wsc_basins, zcol = "SUB_DRAINAGE_AREA_CD")
mapview::mapview(wsc_basins, zcol = "MAJOR_DRAINAGE_AREA_CD")


# Group subbasins to create major and sub-basin groups
trending_basins <- wsc_basins  %>%
  mutate(Major_Basin = case_when(SUB_DRAINAGE_AREA_CD == "08N" ~ "Columbia",
                                 SUB_SUB_DRAINAGE_AREA_CD %in% c("08MH") ~ "Coastal",
                                 SUB_DRAINAGE_AREA_CD %in% c("08M","08L","08K","08J") ~ "Fraser",
                                 SUB_DRAINAGE_AREA_CD %in% c("08H","08G","08F","08O") ~ "Coastal",
                                 SUB_DRAINAGE_AREA_CD %in% c("07F","07E") ~ "Peace",
                                 SUB_DRAINAGE_AREA_CD %in% c("10A","10B","10C","10D","10E","10F") ~ "Liard",
                                 SUB_DRAINAGE_AREA_CD %in% c("08A","08B","08C","08D","08E","09A") ~ "Northwest"),
         Sub_Basin = case_when(SUB_SUB_DRAINAGE_AREA_CD == "08NL" ~ "Similkameen",
                               SUB_SUB_DRAINAGE_AREA_CD == "08NM" ~ "Okanagan",
                               SUB_SUB_DRAINAGE_AREA_CD == "08NN" ~ "Kettle",
                               #SUB_SUB_DRAINAGE_AREA_CD %in% c("08NL","08NM","08NN") ~ "Similkameen-Okanagan-Kettle",
                               SUB_DRAINAGE_AREA_CD == "08H" ~ "Vancouver Island",
                               SUB_DRAINAGE_AREA_CD == "08O" ~ "Haida Gwaii",
                               SUB_SUB_DRAINAGE_AREA_CD %in% c("08GD","08GC","08GB","08GA","08MH") ~ "South Coast",
                               SUB_SUB_DRAINAGE_AREA_CD %in% c("08GF","08GE") ~ "Central and North Coast",
                               SUB_DRAINAGE_AREA_CD == "08F" ~ "Central and North Coast",
                               SUB_DRAINAGE_AREA_CD == "08D" ~ "Nass",
                               SUB_DRAINAGE_AREA_CD == "08J" ~ "Nechako",
                               SUB_DRAINAGE_AREA_CD == "08E" ~ "Skeena",
                               SUB_DRAINAGE_AREA_CD == "08C" ~ "Stikine",
                               SUB_DRAINAGE_AREA_CD == "09A" ~ "Yukon",
                               SUB_DRAINAGE_AREA_CD == "08L" ~ "Thompson",
                               SUB_DRAINAGE_AREA_CD == "07F" ~ "Peace", # edited from Upper Peace
                               SUB_DRAINAGE_AREA_CD == "07E" ~ "Williston Lake",
                               SUB_DRAINAGE_AREA_CD == "10C" ~ "Fort Nelson",
                               SUB_DRAINAGE_AREA_CD %in% c("10A","10B") ~ "Upper and Central Liard",
                               SUB_SUB_DRAINAGE_AREA_CD %in% c("08KA","08KB","08KD","08KC","08KE","08KG","08KF") ~ "Upper Fraser",
                               SUB_SUB_DRAINAGE_AREA_CD %in% c("08MD","08ME","08MG","08MF","08MC") ~ "Lower Fraser",
                               SUB_SUB_DRAINAGE_AREA_CD %in% c("08NC","08NB","08NA","08ND","08NE") ~ "Columbia",
                               SUB_SUB_DRAINAGE_AREA_CD %in% c("08MB","08MA") ~ "Chilcotin",
                               SUB_SUB_DRAINAGE_AREA_CD == "08KH" ~ "Quesnel",
                               SUB_SUB_DRAINAGE_AREA_CD %in% c("08AB","08AC","08BF","08BE") ~ "Yukon", #Karly edits
                               SUB_SUB_DRAINAGE_AREA_CD %in% c("07GC","07GD","07GB") ~ "Peace", # Karly edits
                               SUB_SUB_DRAINAGE_AREA_CD %in% c("08BB","08BA","08BC") ~ "Stikine",# This is wrong maybe, this area doesn't fit mapping
                               SUB_SUB_DRAINAGE_AREA_CD %in% c("10DA","10DB","07OA") ~ "Upper and Central Liard",# This is wrong maybe, this area doesn't fit mapping
                               SUB_SUB_DRAINAGE_AREA_CD == "07OC" ~ "Fort Nelson", # Karly added
                               SUB_SUB_DRAINAGE_AREA_CD %in% c("08NG","08NF","08NK","08NP",
                                                               "08NH", "08NJ") ~ "Kootenay"))
# make simple table for viewing data
trending_basins_table <- trending_basins %>%
  st_drop_geometry()

# view the basin groupings by all basins
mapview::mapview(trending_basins, zcol = "Major_Basin")
mapview::mapview(trending_basins, zcol = "Sub_Basin")

# Merge major basins and view features
major_basins <- trending_basins %>%
  group_by(Major_Basin) %>%
  summarize(geometry = sf::st_union(geometry)) %>%
  ungroup()%>%
  ms_simplify()
mapview::mapview(list(major_basins), zcol = c("Major_Basin"))

# Merge sub basins and view features
sub_basins <- trending_basins %>%
  group_by(Sub_Basin) %>%
  summarize(geometry = sf::st_union(geometry)) %>%
  filter(!is.na(Sub_Basin)) %>%
  ungroup() %>%
  ms_simplify()
mapview::mapview(list(sub_basins), zcol = c("Sub_Basin"))

#Merge major basins and sub basins
basins = sub_basins %>%
  st_intersection(major_basins) %>%
  ms_simplify()
mapview::mapview(list(basins), zcol = c("Sub_Basin"))

#spatial join with basins
stations_sf <- stations_sf %>%
  st_intersection(basins) %>%
  left_join(regime_groups)

#Create BC boundary
bound <- major_basins %>%
  st_union() %>%
  ms_simplify()

write_sf(bound, 'app/www/bound.gpkg')
write_sf(stations_sf, 'app/www/stations.gpkg')
write_sf(basins, 'app/www/basins.gpkg')
write_sf(sub_basins, 'app/www/sub_basins.gpkg')
write_sf(major_basins, 'app/www/major_basins.gpkg')

