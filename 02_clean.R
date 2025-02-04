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

library(tidyverse)
library(data.table)
library(tidyhydat)
library(sf)

### Load in data that was accessed in the '01_load.R' script.
hydat_daily_all <- read_rds('./data/hydat_daily_all.rds')
#stations_filt_list <- read_rds("./data/stations_filt_list.rds")
#stations_filt <- read_rds("./data/stations_filt_no_missing.rds")
daily_station_data <- read_rds("./data/daily_station_data.rds")
station_year <- read_rds("./data/station_year.rds")
station_summary <- read_rds("./data/station_summary.rds")

# There are five main filtering/cleaning stages =================================================
# 1. Remove station-years with missing data (based on threshold value)
# 2. Identifying (adding filter option) downstream and upstream stations on the same river
# 3. Removing regulated rivers
# 4. Removing stations with large data gaps, as well as data preceding large gaps (after removing station-years with missing data)
# 5. Removing stations with less than 10 years of data

# Add years with data to station_year
sy_df <- daily_station_data %>%
  mutate(any_data = 1) %>%
  select(STATION_NUMBER, Year, any_data)

station_year <- station_year %>%
  left_join(sy_df)

# 1. Remove years with missing data (based on threshold value) ====================================

# Set threshold (%)
threshold = 3

# Identify years that have any missing data and merge with station_year for both water and low flow years
percent_missing_wYear <- daily_station_data %>%
  filter(perc_daily_missing_wYear <= threshold) %>%
  mutate(missing_dat_wYear = 1) %>%
  select(STATION_NUMBER, Year, missing_dat_wYear)

percent_missing_lfYear <- daily_station_data %>%
  filter(perc_daily_missing_lfYear <= threshold) %>%
  mutate(missing_dat_lfYear = 1) %>%
  select(STATION_NUMBER, Year, missing_dat_lfYear)

percent_missing_Year <- daily_station_data %>%
  filter(perc_daily_missing_Year <= threshold) %>%
  mutate(missing_dat_Year = 1) %>%
  select(STATION_NUMBER, Year, missing_dat_Year)

station_year_filters <- station_year %>%
  left_join(percent_missing_wYear) %>%
  left_join(percent_missing_lfYear) %>%
  left_join(percent_missing_Year)

# 2. Identifying downstream and upstream stations on the same river ===============================

## Manually check for multiple stations on same river (choosing most downstream station)
## filter if station names
check_dup_all <- station_summary %>%
  mutate(Name = word(STATION_NAME,1,2))%>%
  select(STATION_NUMBER, Name, STATION_NAME, DRAINAGE_AREA_GROSS, LATITUDE, LONGITUDE)

check_dup_stations <- station_summary %>%
  dplyr::mutate(Name = word(STATION_NAME,1,2)) %>%
  # See how many rows each name has in the dataset.
  add_count(Name) %>%
  # Just keep those that have some repetition in the first two words.
  filter(n > 1)

# currently using a manual table to filter out stations on the same river, keeping downstream station

stns_dup_table <- tibble::tribble(
  ~STATION_NUMBER,               ~Name,                                      ~STATION_NAME, ~STATION_KEEP, ~REP_STATION,
  "08LB069",    "BARRIERE RIVER",               "BARRIERE RIVER BELOW SPRAGUE CREEK",         FALSE, "08LB020",
  "08LB020",    "BARRIERE RIVER",                      "BARRIERE RIVER AT THE MOUTH",          TRUE, "08LB020",
  "08LC042",    "BESSETTE CREEK",        "BESSETTE CREEK ABOVE LUMBY LAGOON OUTFALL",         FALSE, "08LC039",
  "08LC039",    "BESSETTE CREEK",            "BESSETTE CREEK ABOVE BEAVERJACK CREEK",          TRUE, "08LC039",
  "08MB007",         "BIG CREEK",                  "BIG CREEK BELOW GRAVEYARD CREEK",         FALSE, "08MB006",
  "08MB006",         "BIG CREEK",                  "BIG CREEK ABOVE GROUNDHOG CREEK",          TRUE, "08MB006",
  "08ME023",      "BRIDGE RIVER", "BRIDGE RIVER (SOUTH BRANCH) BELOW BRIDGE GLACIER",         FALSE, "08ME028",
  "08ME028",      "BRIDGE RIVER",                  "BRIDGE RIVER ABOVE DOWNTON LAKE",          TRUE, "08ME028",
  "08EE003",     "BULKLEY RIVER",                       "BULKLEY RIVER NEAR HOUSTON",         FALSE, "08EE005",
  "08EE004",     "BULKLEY RIVER",                           "BULKLEY RIVER AT QUICK",         FALSE, "08EE005",
  "08EE005",     "BULKLEY RIVER",                      "BULKLEY RIVER NEAR SMITHERS",          TRUE, "08EE005",
  "08GA072",   "CHEAKAMUS RIVER",               "CHEAKAMUS RIVER ABOVE MILLAR CREEK",         FALSE, "08GA043",
  "08GA043",   "CHEAKAMUS RIVER",                 "CHEAKAMUS RIVER NEAR BRACKENDALE",          TRUE, "08GA043",
  "08MA002",      "CHILKO RIVER",            "CHILKO RIVER AT OUTLET OF CHILKO LAKE",         FALSE, "08MA001",
  "08MA001",      "CHILKO RIVER",                       "CHILKO RIVER NEAR REDSTONE",          TRUE, "08MA001",
  "08MH016",  "CHILLIWACK RIVER",    "CHILLIWACK RIVER AT OUTLET OF CHILLIWACK LAKE",         FALSE, "08MH001",
  "08MH103",  "CHILLIWACK RIVER",              "CHILLIWACK RIVER ABOVE SLESSE CREEK",         FALSE, "08MH001",
  "08MH001",  "CHILLIWACK RIVER",              "CHILLIWACK RIVER AT VEDDER CROSSING",          TRUE, "08MH001",
  "08LG048",   "COLDWATER RIVER",                   "COLDWATER RIVER NEAR BROOKMERE",         FALSE, "08LG010",
  "08LG010",   "COLDWATER RIVER",                       "COLDWATER RIVER AT MERRITT",          TRUE, "08LG010",
  "08NA002",    "COLUMBIA RIVER",                      "COLUMBIA RIVER AT NICHOLSON",         FALSE, "08NE049",
  "08NB005",    "COLUMBIA RIVER",                         "COLUMBIA RIVER AT DONALD",         FALSE, "08NE049",
  "08NE049",    "COLUMBIA RIVER",                      "COLUMBIA RIVER AT BIRCHBANK",          TRUE, "08NE049",
  "08MF062",  "COQUIHALLA RIVER",              "COQUIHALLA RIVER BELOW NEEDLE CREEK",         FALSE, "08MF068",
  "08MF068",  "COQUIHALLA RIVER",           "COQUIHALLA RIVER ABOVE ALEXANDER CREEK",          TRUE, "08MF068",
  "08MH002",   "COQUITLAM RIVER",                "COQUITLAM RIVER AT PORT COQUITLAM",         FALSE, "08MH141",
  "08MH141",   "COQUITLAM RIVER",             "COQUITLAM RIVER ABOVE COQUITLAM LAKE",          TRUE, "08MH141",
  "08HA002",    "COWICHAN RIVER",                  "COWICHAN RIVER AT LAKE COWICHAN",         FALSE, "08HA011",
  "08HA011",    "COWICHAN RIVER",                       "COWICHAN RIVER NEAR DUNCAN",          TRUE, "08HA011",
  "08NH119",      "DUNCAN RIVER",                    "DUNCAN RIVER BELOW B.B. CREEK",         FALSE, "08NH118",
  "08NH126",      "DUNCAN RIVER",                    "DUNCAN RIVER BELOW DUNCAN DAM",         FALSE, "08NH118",
  "08NH118",      "DUNCAN RIVER",                 "DUNCAN RIVER BELOW LARDEAU RIVER",          TRUE, "08NH118",
  "08HD018",         "ELK RIVER",                    "ELK RIVER ABOVE CAMPBELL LAKE",          TRUE, "08HD018",
  "08NK002",         "ELK RIVER",                              "ELK RIVER AT FERNIE",          TRUE, "08NK002",
  "08NK016",         "ELK RIVER",                             "ELK RIVER NEAR NATAL",         FALSE, "08NK002",
  "08LB024",    "FISHTRAP CREEK",                       "FISHTRAP CREEK NEAR MCLURE",          TRUE, "08LB024",
  "08MH153",    "FISHTRAP CREEK",         "FISHTRAP CREEK AT INTERNATIONAL BOUNDARY",          TRUE, "08MH153",
  "08KA004",      "FRASER RIVER",                          "FRASER RIVER AT HANSARD",         FALSE, "08MF005",
  "08KA005",      "FRASER RIVER",                          "FRASER RIVER AT MCBRIDE",         FALSE, "08MF005",
  "08KA007",      "FRASER RIVER",                         "FRASER RIVER AT RED PASS",         FALSE, "08MF005",
  "08KB001",      "FRASER RIVER",                          "FRASER RIVER AT SHELLEY",         FALSE, "08MF005",
  "08MC018",      "FRASER RIVER",                     "FRASER RIVER NEAR MARGUERITE",         FALSE, "08MF005",
  "08MD013",      "FRASER RIVER",                    "FRASER RIVER AT BIG BAR CREEK",         FALSE, "08MF005",
  "08MF040",      "FRASER RIVER",                   "FRASER RIVER ABOVE TEXAS CREEK",         FALSE, "08MF005",
  "08MH024",      "FRASER RIVER",                          "FRASER RIVER AT MISSION",         FALSE, "08MF005",
  "08MF005",      "FRASER RIVER",                             "FRASER RIVER AT HOPE",          TRUE, "08MF005",
  "08HC001",        "GOLD RIVER",                     "GOLD RIVER BELOW UCONA RIVER",          TRUE, "08HC001",
  "08NB014",        "GOLD RIVER",                    "GOLD RIVER ABOVE PALMER CREEK",          TRUE, "08NB014",
  "08LG041",     "GUICHON CREEK",            "GUICHON CREEK AT OUTLET OF MAMIT LAKE",         FALSE, "08LG004",
  "08LG056",     "GUICHON CREEK",        "GUICHON CREEK ABOVE TUNKWA LAKE DIVERSION",         FALSE, "08LG004",
  "08LG067",     "GUICHON CREEK",                       "GUICHON CREEK AT THE MOUTH",         FALSE, "08LG004",
  "08LG004",     "GUICHON CREEK",                  "GUICHON CREEK NEAR LOWER NICOLA",          TRUE, "08LG004",
  "07FA003",     "HALFWAY RIVER",                 "HALFWAY RIVER ABOVE GRAHAM RIVER",         FALSE, "07FA006",
  "07FA006",     "HALFWAY RIVER",                 "HALFWAY RIVER NEAR FARRELL CREEK",          TRUE, "07FA006",
  "08GD008",    "HOMATHKO RIVER",        "HOMATHKO RIVER AT INLET TO TATLAYOKO LAKE",         FALSE, "08GD004",
  "08GD004",    "HOMATHKO RIVER",                      "HOMATHKO RIVER AT THE MOUTH",          TRUE, "08GD004",
  "08KH010",    "HORSEFLY RIVER",              "HORSEFLY RIVER ABOVE MCKINLEY CREEK",         FALSE, "08KH031",
  "08KH031",    "HORSEFLY RIVER",                "HORSEFLY RIVER ABOVE QUESNEL LAKE",          TRUE, "08KH031",
  "08NF001",    "KOOTENAY RIVER",              "KOOTENAY RIVER AT KOOTENAY CROSSING",         FALSE, "08NG065",
  "08NG065",    "KOOTENAY RIVER",                    "KOOTENAY RIVER AT FORT STEELE",          TRUE, "08NG065",
  "07FB006",      "MURRAY RIVER",               "MURRAY RIVER ABOVE WOLVERINE RIVER",         FALSE, "07FB002",
  "07FB002",      "MURRAY RIVER",                      "MURRAY RIVER NEAR THE MOUTH",          TRUE, "07FB002",
  "08JA017",     "NECHAKO RIVER",              "NECHAKO RIVER BELOW CHESLATTA FALLS",         FALSE, "08JC002",
  "08JC001",     "NECHAKO RIVER",                      "NECHAKO RIVER AT VANDERHOOF",         FALSE, "08JC002",
  "08JC002",     "NECHAKO RIVER",                     "NECHAKO RIVER AT ISLE PIERRE",          TRUE, "08JC002",
  "07ED001",      "NATION RIVER",                 "NATION RIVER NEAR FORT ST. JAMES",         FALSE, "07ED003",
  "07ED003",      "NATION RIVER",                      "NATION RIVER NEAR THE MOUTH",         TRUE, "07ED003",
  "08LG006",      "NICOLA RIVER",                 "NICOLA RIVER NEAR SPENCES BRIDGE",         FALSE, "08LG049",
  "08LG065",      "NICOLA RIVER",            "NICOLA RIVER AT OUTLET OF NICOLA LAKE",         FALSE, "08LG049",
  "08LG049",      "NICOLA RIVER",                   "NICOLA RIVER ABOVE NICOLA LAKE",          TRUE, "08LG049",
  "08LB047",    "NORTH THOMPSON",             "NORTH THOMPSON RIVER AT BIRCH ISLAND",         FALSE, "08LB064",
  "08LB064",    "NORTH THOMPSON",                   "NORTH THOMPSON RIVER AT MCLURE",          TRUE, "08LB064",
  "08NM002",    "OKANAGAN RIVER",                 "OKANAGAN RIVER AT OKANAGAN FALLS",         FALSE, "08NM085",
  "08NM050",    "OKANAGAN RIVER",                      "OKANAGAN RIVER AT PENTICTON",         FALSE, "08NM085",
  "08NM085",    "OKANAGAN RIVER",                       "OKANAGAN RIVER NEAR OLIVER",          TRUE, "08NM085",
  "07EF001",       "PEACE RIVER",                       "PEACE RIVER AT HUDSON HOPE",         FALSE, "07FD010",
  "07FA004",       "PEACE RIVER",                     "PEACE RIVER ABOVE PINE RIVER",         FALSE, "07FD010",
  "07FD002",       "PEACE RIVER",                          "PEACE RIVER NEAR TAYLOR",         FALSE, "07FD010",
  "07FD010",       "PEACE RIVER",                    "PEACE RIVER ABOVE ALCES RIVER",          TRUE, "07FD010",
  "08HB084",   "PUNTLEDGE RIVER",                  "PUNTLEDGE RIVER BELOW DIVERSION",         FALSE, "08HB006",
  "08HB006",   "PUNTLEDGE RIVER",                     "PUNTLEDGE RIVER AT COURTENAY",          TRUE, "08HB006",
  "08KH001",     "QUESNEL RIVER",                          "QUESNEL RIVER AT LIKELY",         FALSE, "08KH006",
  "08KH006",     "QUESNEL RIVER",                       "QUESNEL RIVER NEAR QUESNEL",          TRUE, "08KH006",
  "08HD021",     "QUINSAM RIVER",                 "QUINSAM RIVER AT ARGONAUT BRIDGE",         FALSE, "08HD005",
  "08HD027",     "QUINSAM RIVER",           "QUINSAM RIVER BELOW LOWER QUINSAM LAKE",         FALSE, "08HD005",
  "08HD005",     "QUINSAM RIVER",                "QUINSAM RIVER NEAR CAMPBELL RIVER",          TRUE, "08HD005",
  "08HD006",      "SALMON RIVER",                        "SALMON RIVER NEAR SAYWARD",          TRUE, "08HD006",
  "08HD007",      "SALMON RIVER",                 "SALMON RIVER ABOVE MEMEKAY RIVER",         FALSE, "08HD006",
  "08HD015",      "SALMON RIVER",       "SALMON RIVER ABOVE CAMPBELL LAKE DIVERSION",         FALSE, "08HD006",
  "08HD032",      "SALMON RIVER",       "SALMON RIVER BELOW CAMPBELL LAKE DIVERSION",         FALSE, "08HD006",
  "08KC001",      "SALMON RIVER",                  "SALMON RIVER NEAR PRINCE GEORGE",          TRUE, "08KC001",
  "08LE020",      "SALMON RIVER",                         "SALMON RIVER AT FALKLAND",         FALSE, "08LE021",
  "08LE021",      "SALMON RIVER",                     "SALMON RIVER NEAR SALMON ARM",          TRUE, "08LE021",
  "08MH090",      "SALMON RIVER",               "SALMON RIVER AT 72 AVENUE, LANGLEY",          TRUE, "08MH090",
  "08GA030",     "SEYMOUR RIVER",               "SEYMOUR RIVER NEAR NORTH VANCOUVER",          TRUE, "08GA030",
  "08GA077",     "SEYMOUR RIVER",                 "SEYMOUR RIVER BELOW ORCHID CREEK",         FALSE, "08GA030",
  "08GA079",     "SEYMOUR RIVER",                     "SEYMOUR RIVER ABOVE LAKEHEAD",         FALSE, "08GA030",
  "08LE027",     "SEYMOUR RIVER",                   "SEYMOUR RIVER NEAR SEYMOUR ARM",          TRUE, "08LE027",
  "08LC003",     "SHUSWAP RIVER",                         "SHUSWAP RIVER NEAR LUMBY",         FALSE, "08LC002",
  "08LC018",     "SHUSWAP RIVER",  "SHUSWAP RIVER AT OUTLET OF SUGAR LAKE RESERVOIR",         FALSE, "08LC002",
  "08LC002",     "SHUSWAP RIVER",                       "SHUSWAP RIVER NEAR ENDERBY",          TRUE, "08LC002",
  "08NL007", "SIMILKAMEEN RIVER",                   "SIMILKAMEEN RIVER AT PRINCETON",         FALSE, "08NL038",
  "08NL070", "SIMILKAMEEN RIVER",         "SIMILKAMEEN RIVER ABOVE GOODFELLOW CREEK",         FALSE, "08NL038",
  "08NL038", "SIMILKAMEEN RIVER",                    "SIMILKAMEEN RIVER NEAR HEDLEY",          TRUE, "08NL038",
  "08EE012",     "SIMPSON CREEK",                       "SIMPSON CREEK AT THE MOUTH",          TRUE, "08EE012",
  "08HF013",     "SIMPSON CREEK",               "SIMPSON CREEK NEAR KOPRINO HARBOUR",          TRUE, "08HF013",
  "08EB005",      "SKEENA RIVER",                  "SKEENA RIVER ABOVE BABINE RIVER",         FALSE, "08EF001",
  "08EF001",      "SKEENA RIVER",                              "SKEENA RIVER AT USK",          TRUE, "08EF001",
  "08LG068",       "SPIUS CREEK",                   "SPIUS CREEK BELOW SILVER CREEK",         FALSE, "08LG008",
  "08LG008",       "SPIUS CREEK",                         "SPIUS CREEK NEAR CANFORD",          TRUE, "08LG008",
  "08GC005",   "THEODOSIA RIVER",       "THEODOSIA RIVER DIVERSION ABOVE OLSEN LAKE",         FALSE, "08GC008",
  "08GC006",   "THEODOSIA RIVER",                 "THEODOSIA RIVER DIVERSION BYPASS",         FALSE, "08GC008",
  "08GC007",   "THEODOSIA RIVER",       "THEODOSIA RIVER BELOW OLSEN LAKE DIVERSION",         FALSE, "08GC008",
  "08GC008",   "THEODOSIA RIVER",               "THEODOSIA RIVER ABOVE SCOTTY CREEK",          TRUE, "08GC008",
  "08NL071",    "TULAMEEN RIVER",                 "TULAMEEN RIVER BELOW VUICH CREEK",         FALSE, "08NL024",
  "08NL024",    "TULAMEEN RIVER",                      "TULAMEEN RIVER AT PRINCETON",          TRUE, "08NL024",
  "08NN015",       "WEST KETTLE",                 "WEST KETTLE RIVER NEAR MCCULLOCH",         FALSE, "08NN003",
  "08NN003",       "WEST KETTLE",                  "WEST KETTLE RIVER AT WESTBRIDGE",          TRUE, "08NN003",
  "08HE008",    "ZEBALLOS RIVER",                      "ZEBALLOS RIVER AT MOOK PEAK",         FALSE, "08HE006",
  "08HE006",    "ZEBALLOS RIVER",                     "ZEBALLOS RIVER NEAR ZEBALLOS",          TRUE, "08HE006"
)

check_drainage_rule <- check_dup_stations %>%
  arrange(Name, DRAINAGE_AREA_GROSS) %>%
  group_by(Name) %>%
  mutate(Drainage_Order = ifelse(DRAINAGE_AREA_GROSS == max(DRAINAGE_AREA_GROSS), TRUE, FALSE)) %>%
  left_join(stns_dup_table, by=c("STATION_NUMBER", "Name", "STATION_NAME")) %>%
  mutate(Match = case_when(
    Drainage_Order == TRUE & STATION_KEEP == TRUE ~ "Yes",
    Drainage_Order == FALSE & STATION_KEEP == FALSE ~ "Yes",
    .default = "No"))

# Duplicated streams to keep (from manual assessment)
stns_dup_keep <- stns_dup_table %>%
  filter(STATION_KEEP) %>%
  pull(STATION_NUMBER)

# Duplicated streams to remove
stn_dup_remove <- check_dup_stations %>%
  filter(!STATION_NUMBER %in% stns_dup_keep) %>%
  pull(STATION_NUMBER)

# Remove the duplicated streams (keep if keep_dup == 1)
station_summary <- station_summary %>%
  mutate(keep_dup = case_when(STATION_NUMBER %in% stn_dup_remove ~ NA,
                              .default = 1))

# Add to station_year_filter df
station_year_filters <- station_year_filters %>%
  left_join(station_summary %>% select(STATION_NUMBER, keep_dup))

# Filtering out stations that are regulated by dams -----------------------

####### Check for station regulation (plot data and see (first pass))

# Filter for regulated stations
check_reg <- station_summary %>%
  filter(REGULATED != FALSE) %>%
  select(STATION_NUMBER, STATION_NAME, "Year_from", "Year_to")


# Manual checks on regulated stations for degree of regulation (open to interpretation)
# STN <- "08NM232"
# stns_reg_sf <-  stns_reg %>%
#   filter(STATION_NUMBER == STN) %>%
#   mutate(STATION= paste0(STATION_NUMBER, " - ", STATION_NAME)) %>%
#   select(STATION, LONGITUDE, LATITUDE) %>%
#   st_as_sf(coords = c("LONGITUDE", "LATITUDE"),
#            crs = 4326)
# mapview::mapview(stns_reg_sf)
# plot_daily_stats(station_number = STN, complete_years = TRUE, add_year = 2000)
# SCREEN <- screen_flow_data(station_number = STN)
# for (stn in unique(stns_reg$STATION_NUMBER)) {
#   ggsave(filename = paste0("station_selection/regulation_checks/", stn, ".png"), height = 4, width = 10,
#          plot = plot_daily_stats(station_number = stn, complete_years = TRUE, add_year = 2000)[[1]]
#   )
# }
# currently using a manual table to filter out regulated streams, keeping downstream station

check_reg_results <- tibble::tribble(
  ~STATION_NUMBER, ~REG_KEEP, ~Year_from_REG,
  "07FD010", FALSE,          NA,
  "08EC004",  TRUE,          NA,
  "08EG016",  TRUE,          NA,
  "08FE002", FALSE,          NA,
  "08GA010",  TRUE,          NA,
  "08GA022",  TRUE,          NA,
  "08GA030",  TRUE,       1970,
  "08GA043",  TRUE,          NA,
  "08GA047",  TRUE,          NA,
  "08GA076",  TRUE,          NA,
  "08HA011",  TRUE,          NA,
  "08HB002",  TRUE,          NA,
  "08HB006", FALSE,          NA,
  "08HB011", FALSE,          NA,
  "08HB022", FALSE,          NA,
  "08HB023", FALSE,          NA,
  "08HB034",  TRUE,          NA,
  "08HB041", FALSE,          NA,
  "08HB092",  TRUE,          NA,
  "08HC001",  TRUE,          NA,
  "08HC005",  TRUE,          NA,
  "08HD003", FALSE,          NA,
  "08HD005",  TRUE,          NA,
  "08HD006",  TRUE,          NA,
  "08HD018",  TRUE,          NA,
  "08HD026", FALSE,          NA,
  "08HE006",  TRUE,          NA,
  "08JA013", FALSE,          NA,
  "08JB008", FALSE,          NA,
  "08JC002",  TRUE,       1978,
  "08KH020", FALSE,          NA,
  "08LB078",  TRUE,          NA,
  "08LC002",  TRUE,          NA,
  "08LC039",  TRUE,          NA,
  "08LE021",  TRUE,          NA,
  "08LE112",  TRUE,          NA,
  "08LF002",  TRUE,          NA,
  "08LF007",  TRUE,          NA,
  "08LF027",  TRUE,          NA,
  "08LG004", FALSE,          NA,
  "08LG010", FALSE,          NA,
  "08LG049",  TRUE,          NA,
  "08MC040",  TRUE,          NA,
  "08ME002",  TRUE,          NA,
  "08ME003", FALSE,          NA,
  "08MF005",  TRUE,          NA,
  "08MG025",  TRUE,          NA,
  "08MH005",  TRUE,          NA,
  "08MH090",  TRUE,          NA,
  "08MH098",  TRUE,          NA,
  "08MH153",  TRUE,          NA,
  "08NA011",  TRUE,          NA,
  "08ND025",  TRUE,          NA,
  "08NE049",  TRUE,          NA,
  "08NE123",  TRUE,          NA,
  "08NE126", FALSE,          NA,
  "08NG002",  TRUE,          NA,
  "08NH118", FALSE,          NA,
  "08NJ158", FALSE,          NA,
  "08NL039",  TRUE,          NA,
  "08NL045",  TRUE,          NA,
  "08NM037",  TRUE,          NA,
  "08NM065", FALSE,          NA,
  "08NM085", FALSE,          NA,
  "08NM200",  TRUE,          NA,
  "08NN003",  TRUE,          NA,
  "08NN026",  TRUE,          NA,
  "08HB008",  TRUE,          NA,
  "08HB029",  TRUE,          NA,
  "08NM116",  TRUE,          NA,
  "08NM232", FALSE,          NA
) %>%
  left_join(check_reg, by = "STATION_NUMBER") %>%
  mutate(Year_from = ifelse(is.na(Year_from), Year_from_REG, Year_from))


stn_reg_keep <- check_reg_results %>%
  filter(REG_KEEP) %>%
  pull(STATION_NUMBER)

stn_reg_remove <- check_reg_results %>%
  filter(!REG_KEEP) %>%
  pull(STATION_NUMBER)

# Filter and add years of regulated to filter (keep if keep_reg == TRUE)
station_summary <- station_summary %>%
  select(-Year_to, -Year_from) %>%
  mutate(keep_reg = case_when(STATION_NUMBER %in% stn_reg_remove~ FALSE,
                              .default = TRUE)) %>%
  filter(keep_reg) %>%
  left_join(check_reg_results %>%
              select(-Year_from_REG, -STATION_NAME), by = "STATION_NUMBER")

# Add column to station_year (keep if keep_reg == 1)
station_year_filters <- station_year_filters %>%
  mutate(keep_reg = case_when(STATION_NUMBER %in% stn_reg_remove ~ NA,
                              .default = 1))

# Filtering out stations with large data gaps -----------------------------

#First, fill in missing years with NAs using station_year df (use wYear for this)
stns_ann_data <- hydat_daily_all %>%
  filter(STATION_NUMBER %in% unique(station_summary$STATION_NUMBER)) %>%
  group_by(STATION_NUMBER, wYear) %>%
  summarise(Ann_Mean = mean(Value, na.rm = TRUE)) %>%
  ungroup()

stns_ann_data <- station_year_filters %>%
  left_join(stns_ann_data, by = join_by(STATION_NUMBER, Year == wYear))

stns_ann <- unique(stns_ann_data$STATION_NUMBER)

# Including missing data years
ggplot(stns_ann_data %>% filter(!(is.na(Ann_Mean)|is.na(missing_dat_wYear))), aes(Year,STATION_NUMBER, colour = Ann_Mean))+
  geom_point()

## Andrew attempts at pulling out data gaps (decision = if > x year gap, remove all previous data)
# First, invert data and do cumsum based on NAs

threshold_gap = 5

dat <- stns_ann_data %>%
  arrange(STATION_NUMBER, -Year) %>%
  mutate(NAs = case_when(is.na(Ann_Mean)|is.na(missing_dat_wYear) ~ 1, # for cumulative sum
                         .default = 0),
         NAs2 = case_when(is.na(Ann_Mean)|is.na(missing_dat_wYear)  ~ 1, # for grouping (resetting when hits non-NA)
                          .default = NA)) %>%
  group_by(STATION_NUMBER, grp = cumsum(is.na(NAs2))) %>%
  mutate(cum_sum = cumsum(NAs))%>%
  ungroup() %>%
  group_by(STATION_NUMBER) %>%
  mutate(gap = case_when(sum(cum_sum==threshold_gap)>0~ 0,
                            .default = 1)) %>%
  mutate(rn = row_number())

gappy_dat <- dat %>%
  filter(gap == 0)

clean_dat <- dat %>%
  filter(gap == 1)

#remove any data that is beyond the threshold of missing data
rown <- gappy_dat %>%
  group_by(STATION_NUMBER) %>%
  filter(cum_sum == threshold_gap) %>%
  select(STATION_NUMBER, rn2 = rn)

#remove rows between the first missing data and threshold number
gappy_dat_clean = gappy_dat %>%
  left_join(rown) %>%
  group_by(STATION_NUMBER) %>%
  filter(rn %in% seq(1:unique(rn2))) %>%
  filter(!is.na(Ann_Mean))

stns_ann_data_clean <- bind_rows(clean_dat, gappy_dat_clean) %>%
  mutate(year_gaps = 1)

ggplot(stns_ann_data_clean, aes(x = Year, y = STATION_NUMBER, colour = Ann_Mean))+
  geom_point()

station_year_filters <- station_year_filters %>%
  left_join(stns_ann_data_clean %>% select(STATION_NUMBER, Year, year_gaps), by = join_by(STATION_NUMBER, Year)) %>%
  distinct()

# Remove stations with less than 10 years of data for each water and low flow year
small_ss_wYear <- station_year_filters %>%
  filter(!(is.na(missing_dat_wYear) | is.na(year_gaps) | is.na(keep_reg))) %>%
  group_by(STATION_NUMBER) %>%
  mutate(keep_small_wYear = case_when(n() < 10 ~ NA,
                                .default = 1)) %>%
  select(STATION_NUMBER, Year, keep_small_wYear)

small_ss_lfYear <- station_year_filters %>%
  filter(!(is.na(missing_dat_lfYear) | is.na(year_gaps) | is.na(keep_reg))) %>%
  group_by(STATION_NUMBER) %>%
  mutate(keep_small_lfYear = case_when(n() < 10 ~ NA,
                                      .default = 1)) %>%
  select(STATION_NUMBER, Year, keep_small_lfYear)

station_year_filters <- station_year_filters %>%
  left_join(small_ss_wYear) %>%
  left_join(small_ss_lfYear)

# Remove stations that do not have data within last 5 years
recent_years_threshold <- year(Sys.Date()) - 5

recent_years <- station_year_filters %>%
  group_by(STATION_NUMBER) %>%
  mutate(recent_years = case_when(max(Year)>=recent_years_threshold ~ 1,
                                  .default = NA))

station_year_filters <- station_year_filters %>%
  left_join(recent_years)

# Create filtered df (keep upstream stations) - water year
filtered_station_year_wYear <- station_year_filters %>%
  filter(!(is.na(missing_dat_wYear) | is.na(year_gaps) | is.na(keep_reg) | is.na(keep_small_wYear) |is.na(recent_years)))

final_stations_wYear = unique(filtered_station_year_wYear$STATION_NUMBER)

final_station_summary_wYear <- filtered_station_year_wYear %>%
  group_by(STATION_NUMBER) %>%
  summarise(N_years = n(),
            Min_Year = min(Year),
            Max_Year = max(Year),
            Total_Years = Max_Year - Min_Year +1,
            keep = unique(keep_dup))

# Create filtered df (keep upstream stations) - low flow year
filtered_station_year_lfYear <- station_year_filters %>%
  filter(!(is.na(missing_dat_lfYear) | is.na(year_gaps) | is.na(keep_reg) | is.na(keep_small_lfYear|is.na(recent_years))))

final_stations_lfYear = unique(filtered_station_year_lfYear$STATION_NUMBER)

final_station_summary_lfYear <- filtered_station_year_lfYear %>%
  group_by(STATION_NUMBER) %>%
  summarise(N_years = n(),
            Min_Year = min(Year),
            Max_Year = max(Year),
            Total_Years = Max_Year - Min_Year +1,
            keep = unique(keep_dup))

# Create filtered df (keep upstream stations) - calendar year
filtered_station_year_cYear <- station_year_filters %>%
  filter(!(is.na(missing_dat_Year) | is.na(year_gaps) | is.na(keep_reg) | is.na(keep_small_lfYear|is.na(recent_years))))

final_stations_cYear <- unique(filtered_station_year_lfYear$STATION_NUMBER)

final_station_summary_cYear <- filtered_station_year_cYear %>%
  group_by(STATION_NUMBER) %>%
  summarise(N_years = n(),
            Min_Year = min(Year),
            Max_Year = max(Year),
            Total_Years = Max_Year - Min_Year +1,
            keep = unique(keep_dup))


write.csv(final_station_summary_wYear, "data/finalstns_wYear.csv", row.names = F)
write.csv(final_station_summary_lfYear, "data/finalstns_lfYear.csv", row.names = F)
write.csv(final_station_summary_cYear, "data/finalstns_cYear.csv", row.names = F)
write.csv(filtered_station_year_wYear, "data/finalstnyr_wYear.csv", row.names = F)
write.csv(filtered_station_year_lfYear, "data/finalstnyr_lfYear.csv", row.names = F)
write.csv(filtered_station_year_cYear, "data/finalstnyr_cYear.csv", row.names = F)


