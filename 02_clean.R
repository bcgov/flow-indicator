# Copyright 2023 Province of British Columbia
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
hydat_daily_all = read_rds('./data/hydat_daily_all.rds')
stations_filt_list = read_rds("./data/stations_filt_list.rds")
stations_filt = read_rds("./data/stations_filt.rds")
daily_station_data = read_rds("./data/daily_station_data.rds")

# There are three main filtering/cleaning stages --------------------------
# 1. Removing stations on the same river
# 2. Removing regulated rivers
# 3. Removing stations with large data gaps

# 1. Filtering out stations on the same river --------

## manual check for multiple stations on same river (choosing most downstream station)
## filter if station names
check_dup_all <- stations_filt %>%
  mutate(Name = word(STATION_NAME,1,2))%>%
  select(STATION_NUMBER, Name, STATION_NAME, DRAINAGE_AREA_GROSS, LATITUDE, LONGITUDE)

check_dup_stations <- stations_filt |>
  dplyr::mutate(Name = word(STATION_NAME,1,2)) |>
  # See how many rows each name has in the dataset.
  add_count(Name) |>
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

check_drainage_rule <- check_dup_stations |>
  arrange(Name, DRAINAGE_AREA_GROSS) |>
  group_by(Name) |>
  mutate(Drainage_Order = ifelse(DRAINAGE_AREA_GROSS == max(DRAINAGE_AREA_GROSS), TRUE, FALSE)) |>
  left_join(stns_dup_table, by=c("STATION_NUMBER", "Name", "STATION_NAME")) |>
  mutate(Match = case_when(
    Drainage_Order == TRUE & STATION_KEEP == TRUE ~ "Yes",
    Drainage_Order == FALSE & STATION_KEEP == FALSE ~ "Yes",
    .default = "No"))

## Karly to check with Jon on these things


# Duplicated streams to keep (from manual assessment)
stns_dup_keep <- stns_dup_table %>%
  filter(STATION_KEEP) %>%
  pull(STATION_NUMBER)
# Duplicated streams to remove
stn_dup_remove <- check_dup_stations %>%
  filter(!STATION_NUMBER %in% stns_dup_keep) %>%
  pull(STATION_NUMBER)

# Remove the duplicated streams
stations_filt <- stations_filt %>%

 filter(!STATION_NUMBER %in% stn_dup_remove)


# Filtering out stations that are regulated by dams -----------------------

####### Check for station regulation (plot data and see (first pass))

# Filter for regulated stations
check_reg <- stations_filt %>%
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

# Filter and add years of regulated to filter
stations_filt <- stations_filt %>%
  select(-Year_to, -Year_from) %>%
  filter(!STATION_NUMBER %in% stn_reg_remove) %>%
  left_join(check_reg_results %>%
              select(-Year_from_REG, -STATION_NAME), by = "STATION_NUMBER")


# Filtering out stations with large data gaps -----------------------------

###### Year filtering
n_years_filt = 10

stns_ann_data <- hydat_daily_all %>%
  filter(STATION_NUMBER %in% unique(stations_filt$STATION_NUMBER)) %>%
  mutate(Year = year(Date)) %>%
  group_by(STATION_NUMBER, Year) %>%
  summarise(Ann_Mean = mean(Value, na.rm = FALSE)) %>%
  group_by(STATION_NUMBER) %>%
  filter(sum(!is.na(Ann_Mean)) >= n_years_filt) |>
  ungroup()

stns_ann <- unique(stns_ann_data$STATION_NUMBER)

# ggplot(stns_ann_data, aes(Year,STATION_NUMBER, colour = Ann_Mean))+
#   geom_point()
# plotly::ggplotly()


# # Remove stations with large recent gaps
# # add years to filter from old gappy data
# stations_filt4 <- stations_filt %>%
#   filter(!STATION_NUMBER %in% remove_custom$STATION_NUMBER) %>%
#   left_join(dates_custom, by = "STATION_NUMBER") %>%
#   mutate(Year_from = ifelse(is.na(Year_from), Year_from_OLD, Year_from),
#          Year_from = ifelse(is.na(Year_from), year_min, Year_from)) %>%
#   select(-Year_from_OLD)


# For each station, filter out big data gaps. Also,
# filter out NA rows early on in each dataset. This section is coded
# using the {purrr} package's "map" function, which carries out
# a chunk of code for each element in a list (in this case,
# all the unique station numbers). A map function is nice here because
# we are using one table ('stations_filt') to filter a second table
# ('stns_ann_data')

stns_ann_data2 = purrr::map(unique(stations_filt$STATION_NUMBER), ~ {

  stn_yr_from <- stations_filt %>%
    dplyr::filter(STATION_NUMBER == .x) %>%
    dplyr::pull(Year_from)

  # Nothing but NA for 'Year_from'? (could be worth investigating why)
  # Use the first year of data instead.
  if(is.na(stn_yr_from)){
    stn_yr_from = stations_filt |>
      filter(STATION_NUMBER == .x) |>
      dplyr::select(year_min)
  }

  stns_ann_data %>%
    dplyr::filter(STATION_NUMBER == .x,
           Year >= stn_yr_from) |>
    dplyr::mutate(Ann_Mean_na_as_0 = tidyr::replace_na(Ann_Mean, 0)) |>
    dplyr::mutate(sum_up_to_row = cumsum(Ann_Mean_na_as_0)) |>
    dplyr::filter(sum_up_to_row > 0) |>
    dplyr::select(-Ann_Mean_na_as_0, -sum_up_to_row)
}) |>
  dplyr::bind_rows() |>
  # Reapply the n years filter number once we've trimmed out big data gaps!
  dplyr::filter(sum(!is.na(Ann_Mean)) >= n_years_filt)

# # Modify the earliest permissible years for stations in stations_filt, based on
# # the stns_ann_data2 table calculated above.
# stations_filt = stations_filt |>
#   left_join(
#     stns_ann_data2 |>
#       group_by(STATION_NUMBER) |>
#       summarise(min_year = min(Year))
#   ) |>
#   mutate(year_min = min_year) |>
#   mutate(n_years = year_max - year_min) |>
#   dplyr::select(-min_year)

# # Convert to spatial file and check out interim results.
# mapfilt4 <- sf::st_as_sf(stations_filt %>% select(STATION_NUMBER, LONGITUDE, LATITUDE), coords = c("LONGITUDE", "LATITUDE"),crs = 4326)
# mapview::mapview(mapfilt4)

final_stations_summary <- stns_ann_data2 %>%
  filter(!is.na(Ann_Mean)) %>%
  group_by(STATION_NUMBER) %>%
  summarise(N_Years = n(),
            Min_Year = min(Year),
            Max_Year = max(Year),
            Total_Years = Max_Year - Min_Year +1)

write.csv(final_stations_summary, "data/finalstns.csv", row.names = F)

write_rds(stns_ann_data2, 'data/filtered_annual_data.rds')

# # Get station spatial files.
# stations_for_spatial_table = tidyhydat::hy_stations(station_number =  final_stations_summary$STATION_NUMBER) |>
#   tidyr::as_tibble() |>
#   sf::st_as_sf(coords = c("LONGITUDE","LATITUDE"), crs = 4326)
#
# # Write to app's www folder.
# sf::write_sf(stations_for_spatial_table,
#              'app/www/stations.gpkg')
