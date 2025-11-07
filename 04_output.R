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

library(EnvStats)
library(tidyverse)
library(data.table)
library(tidyhydat)
library(sf)
library(cowplot)
library(envreportutils)
library(webshot)
library(htmlwidgets)
library(ggpubr)
library(leaflet)
library(ggtext)
library(scales)

#Load data
if(!exists("basins")){basins = st_read('app/www/basins.gpkg')}
if(!exists("sub_basins")){sub_basins = st_read('app/www/sub_basins.gpkg')}
if(!exists("major_basins")){major_basins = st_read('app/www/major_basins.gpkg')}

if(!exists("stations_sf")){stations_sf = st_read('app/www/stations.gpkg')}

if(!exists("annual_flow_dat")){annual_flow_dat = readRDS('app/www/annual_flow_dat.rds')}
if(!exists("monthly_flow_dat")){monthly_flow_dat = readRDS('app/www/monthly_flow_dat.rds')}

if(!exists("regime_groups")){regime_groups = read.csv('app/www/river_groups.csv')}

if(!exists("hydrograph_dat")){hydrograph_dat = readRDS('app/www/hydrograph_dat.rds')}

# Remove upstream stations (n = 185)
stations_filt <- stations_sf %>%
  filter(keep == 1)

# Merge with regime info
stations_filt <- stations_filt %>%
  left_join(regime_groups) %>%
  filter(!is.na(Regime))

# Subset annual flow data with filtered station list
annual_flow_dat <- annual_flow_dat %>%
  filter(STATION_NUMBER %in% stations_filt$STATION_NUMBER)

## FIGURE 1 - Map of stations and regime type =====================================
#Color scheme - for REGIMES (leflet map)
mypal <- colorFactor(palette = c("#D55E00", "#E69F00", "#F0E442","#009E73"),
                    domain = stations_filt,
                    levels = c("Snow-Dominated - Early Peak",
                               "Snow-Dominated - Late Peak",
                               "Mixed Regimes",
                               "Rain-Dominated"),
                    ordered = T)

leaflet(options =
          leafletOptions(zoomControl = FALSE, maxZoom = 5.68,
                         minZoom = 5.68)) %>%
  #fitBounds(lng1 = -139.229207, lat1 = 48.113982, lng2 = -115, lat2 = 60.145537) %>%
  setView(lat = 54.6, lng = -125.8, zoom = 0) %>%
  addTiles(group = "Streets") %>%
  addPolygons(data = sub_basins,
              color = "black",
              fillColor = "white",
              weight = 1,
              fillOpacity = 0.2,
              label = ~Sub_Basin) %>%
  addPolygons(data = major_basins,
              color = "black",
              fillColor = "white",
              weight = 2,
              fillOpacity = 0.2) %>%
  addCircleMarkers(data = stations_filt,
                   radius = 3,
                   color = "black",
                   weight = 0.5,
                   fillOpacity = 1,
                   fillColor = ~mypal(Regime)) %>%
  addLegend(pal = mypal,
            values = ~Regime,
            title = "River Flow Stations",
            data =stations_filt,
            opacity = 1,
            layerId = 'legend',
            position = 'topright') %>%

  ## save html to png
  saveWidget("temp.html", selfcontained = FALSE)
webshot("temp.html", file = "print_ver/out/figs/static_leaflet.png",
        cliprect = "viewport", zoom = 2)

# Alternative to leaflet
ggplot() +
  geom_sf(data = sub_basins,
          fill = NA,
          color = "gray") +
  geom_sf(data = major_basins,
          fill = NA,
          color = "black",
          linewidth = 0.1) +
  geom_point(data = stations_filt,
          aes(x = st_coordinates(geom)[,1], y = st_coordinates(geom)[,2], color = Regime),
          size = 3) +
  scale_color_manual(values = c("#F0E442","#009E73", "#D55E00", "#E69F00"))

## Function for calculating Mann-Kendall results
# creates magnitude, significance and colour scheme columns
# Magnitude is currently based on more than 2 day per decade change, 1 - 2 day per decade change and less than 1 day change for timing,
# and more than 5% change, 1 - 5% change and less than 1% change for volume.
calculate_MK_results <- function(data,chosen_variable){

  yeardat = data %>%
    group_by(STATION_NUMBER) %>%
    summarise(minYear =min(Year),
              maxYear = max(Year),
              range = max(Year) - min(Year))

 data %>%
     filter(!is.na(chosen_variable)) %>%
      dplyr::select(STATION_NUMBER,
                    Year,
                    values = !!sym(chosen_variable)) %>%
   filter(!is.na(values)) %>%
      add_count(STATION_NUMBER, name = 'number_records') |>
      filter(number_records > 3) |>
      group_by(STATION_NUMBER) %>%
      reframe(MK_results = kendallTrendTest(values ~ Year)[c('statistic','p.value','estimate')]) %>%
      unnest(MK_results) %>%
      unnest_longer(col = MK_results) %>%
      group_by(STATION_NUMBER) %>%
      mutate(MK_results_id = c('Statistic','P_value','Tau','Slope','Intercept')) %>%
      pivot_wider(names_from = MK_results_id, values_from = MK_results) %>%
      mutate(direction = case_when(Slope <0 ~ "negative",
                                   .default = "positive")) %>%
    left_join(yeardat) %>%
    group_by(direction) %>%
    mutate(begin_flow = (Intercept + (Slope * minYear)+0.00000001),
           end_flow = (Intercept + (Slope * maxYear)+0.00000001)) %>%
    mutate(per_change = (((end_flow - begin_flow)/begin_flow))/range*100,
           change_timing =  (end_flow - begin_flow)/range) %>%
    ungroup() %>%
    mutate(
      magnitude_fixed = fcase(
        change_timing < -0.2 & P_value <= 0.05 & chosen_variable %in% c('DoY_50pct_TotalQ', 'DoY_90pct_TotalQ', 'R2MAD_DoY', 'Min_7_Day_summer_DoY','Max_7_Day_DoY','Min_3_Day_DoY','Max_3_Day_DoY'), "> 2 days earlier",
        between(change_timing, -0.2, -0.1) & P_value <= 0.05 & chosen_variable %in% c('DoY_50pct_TotalQ', 'DoY_90pct_TotalQ', 'R2MAD_DoY', 'Min_7_Day_summer_DoY','Max_7_Day_DoY','Min_3_Day_DoY','Max_3_Day_DoY'), "1 - 2 days earlier",
        between(change_timing, -0.1, 0.1) & P_value <= 0.05 & chosen_variable %in% c('DoY_50pct_TotalQ', 'DoY_90pct_TotalQ', 'R2MAD_DoY', 'Min_7_Day_summer_DoY','Max_7_Day_DoY','Min_3_Day_DoY','Max_3_Day_DoY'), "< 1 days change",
        between(change_timing, 0.1, 0.2) & P_value <= 0.05 & chosen_variable %in% c('DoY_50pct_TotalQ', 'DoY_90pct_TotalQ', 'R2MAD_DoY', 'Min_7_Day_summer_DoY','Max_7_Day_DoY','Min_3_Day_DoY','Max_3_Day_DoY'), "1 - 2 days later",
        change_timing > 0.2 & P_value <= 0.05 & chosen_variable %in% c('DoY_50pct_TotalQ', 'DoY_90pct_TotalQ', 'R2MAD_DoY', 'Min_7_Day_summer_DoY','Max_7_Day_DoY','Min_3_Day_DoY','Max_3_Day_DoY'), "> 2 days later",
        P_value > 0.05 & chosen_variable %in% c('DoY_50pct_TotalQ', 'DoY_90pct_TotalQ', 'R2MAD_DoY', 'Min_7_Day_summer_DoY','Max_7_Day_DoY','Min_3_Day_DoY','Max_3_Day_DoY'), "Non-significant trend",
        per_change < -0.5 & P_value <= 0.05 & (!chosen_variable %in% c('DoY_50pct_TotalQ', 'DoY_90pct_TotalQ', 'R2MAD_DoY', 'Min_7_Day_summer_DoY','Max_7_Day_DoY','Min_3_Day_DoY','Max_3_Day_DoY')), "> 5% decrease",
        between(per_change, -0.5, -0.1) & P_value <= 0.05 & (!chosen_variable %in% c('DoY_50pct_TotalQ', 'DoY_90pct_TotalQ', 'R2MAD_DoY', 'Min_7_Day_summer_DoY','Max_7_Day_DoY','Min_3_Day_DoY','Max_3_Day_DoY')), "1 - 5% decrease",
        between(per_change, -0.1, 0.1) & P_value <= 0.05 & (!chosen_variable %in% c('DoY_50pct_TotalQ', 'DoY_90pct_TotalQ', 'R2MAD_DoY', 'Min_7_Day_summer_DoY','Max_7_Day_DoY','Min_3_Day_DoY','Max_3_Day_DoY')), "< 1% change",
        between(per_change, 0.1, 0.5) & P_value <= 0.05 & (!chosen_variable %in% c('DoY_50pct_TotalQ', 'DoY_90pct_TotalQ', 'R2MAD_DoY', 'Min_7_Day_summer_DoY','Max_7_Day_DoY','Min_3_Day_DoY','Max_3_Day_DoY')), "1 - 5% increase",
        per_change > 0.5 & P_value <= 0.05 & (!chosen_variable %in% c('DoY_50pct_TotalQ', 'DoY_90pct_TotalQ', 'R2MAD_DoY', 'Min_7_Day_summer_DoY','Max_7_Day_DoY','Min_3_Day_DoY','Max_3_Day_DoY')), "> 5% increase",
        P_value > 0.05 & (!chosen_variable %in% c('DoY_50pct_TotalQ', 'DoY_90pct_TotalQ', 'R2MAD_DoY', 'Min_7_Day_summer_DoY','Max_7_Day_DoY','Min_3_Day_DoY','Max_3_Day_DoY')), "Non-significant trend"
      ),
      significant = case_when(P_value <=0.05~ 1,
                              .default = 0.1),
      color = fcase(
        change_timing < -0.2 & P_value <= 0.05 & chosen_variable %in% c('DoY_50pct_TotalQ', 'DoY_90pct_TotalQ', 'R2MAD_DoY', 'Min_7_Day_summer_DoY','Max_7_Day_DoY','Min_3_Day_DoY','Max_3_Day_DoY'),"#ff0000",
        between(change_timing, -0.2, -0.1) & P_value <= 0.05 & chosen_variable %in% c('DoY_50pct_TotalQ', 'DoY_90pct_TotalQ', 'R2MAD_DoY', 'Min_7_Day_summer_DoY','Max_7_Day_DoY','Min_3_Day_DoY','Max_3_Day_DoY'), "#009E73",
        between(change_timing, -0.1, 0.1) & P_value <= 0.05 & chosen_variable %in% c('DoY_50pct_TotalQ', 'DoY_90pct_TotalQ', 'R2MAD_DoY', 'Min_7_Day_summer_DoY','Max_7_Day_DoY','Min_3_Day_DoY','Max_3_Day_DoY'), "#0072B2",
        between(change_timing, 0.1, 0.2) & P_value <= 0.05 & chosen_variable %in% c('DoY_50pct_TotalQ', 'DoY_90pct_TotalQ', 'R2MAD_DoY', 'Min_7_Day_summer_DoY','Max_7_Day_DoY','Min_3_Day_DoY','Max_3_Day_DoY'), "#D55E00",
        change_timing > 0.2 & P_value <= 0.05 & chosen_variable %in% c('DoY_50pct_TotalQ', 'DoY_90pct_TotalQ', 'R2MAD_DoY', 'Min_7_Day_summer_DoY','Max_7_Day_DoY','Min_3_Day_DoY','Max_3_Day_DoY'), "#CC79A7",
        P_value > 0.05 & chosen_variable %in% c('DoY_50pct_TotalQ', 'DoY_90pct_TotalQ', 'R2MAD_DoY', 'Min_7_Day_summer_DoY','Max_7_Day_DoY','Min_3_Day_DoY','Max_3_Day_DoY'), "grey",
        per_change < -0.5 & P_value <= 0.05 & (!chosen_variable %in% c('DoY_50pct_TotalQ', 'DoY_90pct_TotalQ', 'R2MAD_DoY', 'Min_7_Day_summer_DoY','Max_7_Day_DoY','Min_3_Day_DoY','Max_3_Day_DoY')), "#ff0000",
        between(per_change, -0.5, -0.1) & P_value <= 0.05 & (!chosen_variable %in% c('DoY_50pct_TotalQ', 'DoY_90pct_TotalQ', 'R2MAD_DoY', 'Min_7_Day_summer_DoY','Max_7_Day_DoY','Min_3_Day_DoY','Max_3_Day_DoY')), "#009E73",
        between(per_change, -0.1, 0.1) & P_value <= 0.05 & (!chosen_variable %in% c('DoY_50pct_TotalQ', 'DoY_90pct_TotalQ', 'R2MAD_DoY', 'Min_7_Day_summer_DoY','Max_7_Day_DoY','Min_3_Day_DoY','Max_3_Day_DoY')), "#0072B2",
        between(per_change, 0.1, 0.5) & P_value <= 0.05 & (!chosen_variable %in% c('DoY_50pct_TotalQ', 'DoY_90pct_TotalQ', 'R2MAD_DoY', 'Min_7_Day_summer_DoY','Max_7_Day_DoY','Min_3_Day_DoY','Max_3_Day_DoY')), "#D55E00",
        per_change > 0.5 & P_value <= 0.05 & (!chosen_variable %in% c('DoY_50pct_TotalQ', 'DoY_90pct_TotalQ', 'R2MAD_DoY', 'Min_7_Day_summer_DoY','Max_7_Day_DoY','Min_3_Day_DoY','Max_3_Day_DoY')), "#CC79A7",
        P_value > 0.05 & (!chosen_variable %in% c('DoY_50pct_TotalQ', 'DoY_90pct_TotalQ', 'R2MAD_DoY', 'Min_7_Day_summer_DoY','Max_7_Day_DoY','Min_3_Day_DoY','Max_3_Day_DoY')), "grey")
    )
}

# Calculate for each metric, join with station info and add new metric name
mk_annual <- calculate_MK_results(annual_flow_dat, chosen_variable = "Average")  %>%
  left_join(stations_sf, by = "STATION_NUMBER") %>%
  mutate(metric = "Average Annual Flow")

mk_peak <- calculate_MK_results(annual_flow_dat, chosen_variable = "Max_3_Day")  %>%
  left_join(stations_sf, by = "STATION_NUMBER")%>%
  mutate(metric = "Peak Flow")

mk_low <- calculate_MK_results(annual_flow_dat, chosen_variable = "Min_7_Day_summer")  %>%
  left_join(stations_sf, by = "STATION_NUMBER")%>%
  mutate(metric = "Low Summer Flow")

mk_freshet <- calculate_MK_results(annual_flow_dat, chosen_variable = "DoY_50pct_TotalQ")  %>%
  left_join(stations_sf, by = "STATION_NUMBER")%>%
  mutate(metric = "Date of Freshet*")

mk_date_low <- calculate_MK_results(annual_flow_dat, chosen_variable = "R2MAD_DoY") %>%
  left_join(stations_sf, by = "STATION_NUMBER")%>%
  mutate(metric = "Start of Low Flow Period")

#combine above results
mk_results_tbl <- bind_rows(mk_annual,
                           mk_peak,
                           mk_low,
                           mk_date_low,
                           mk_freshet)

## Plot results ===============================================================================

## Plot settings - magnitude of flow
colour.scale <- c("> 5% increase"="#CC79A7",
                  "1 - 5% increase"="#D55E00",
                  "No significant trend" = "grey",
                  "< 1% change" = "#0072B2",
                  "1 - 5% decrease"="#009E73",
                  "> 5% decrease"="#ff0000")

## Plot settings - Timing of flow
colour.scale.date <- c("> 2 days later"="#CC79A7",
                       "1 - 2 days later"="#D55E00",
                       "No significant trend" = "grey",
                       "< 1 days change" = "#0072B2",
                       "1 - 2 days earlier"="#009E73",
                       "> 2 days earlier"="#ff0000")

# combine above into single doc
mk_results_all <- bind_rows(mk_annual,
                           mk_low,
                           mk_peak,
                           mk_freshet,
                           mk_date_low) %>%
  dplyr::select(metric, STATION_NUMBER, magnitude_fixed, significant)


# Create summary of sample size in each grouping - Volume metrics
mk_magnitude <- mk_results_all %>%
  filter(metric %in% c("Average Annual Flow",
                       "Low Summer Flow",
                       "Peak Flow"))  %>%
  mutate(magnitude_fixed = factor(case_when(significant == 0.1 ~ "No significant trend",
                                            .default = magnitude_fixed),
                                  levels = c("> 5% increase",
                                             "1 - 5% increase",
                                             "< 1% change",
                                             "1 - 5% decrease",
                                             "> 5% decrease",
                                             "No significant trend"))) %>%
  mutate(magnitude_fixed = fct_relevel(magnitude_fixed,c("> 5% increase",
                                                         "1 - 5% increase",
                                                         "< 1% change",
                                                         "1 - 5% decrease",
                                                         "> 5% decrease",
                                                         "No significant trend"))) %>%
  group_by(metric, magnitude_fixed) %>%
  summarise(n = n()) %>%
  ggplot() +
  geom_col(aes(x = metric, y = n, fill = magnitude_fixed), col = "black", show.legend = TRUE) +
  scale_fill_manual(name = "Change per decade",
                    values = colour.scale,
                    drop = FALSE) +
  labs(x = "", y = "") +
  coord_flip()+
  theme_classic() +
  theme(legend.position = "bottom") +
  ggtitle("Volume")

# Timing metrics
mk_timing <- mk_results_all %>%
  filter(!metric %in% c("Average Annual Flow",
                       "Low Summer Flow",
                       "Peak Flow"))  %>%
  mutate(magnitude_fixed = factor(case_when(significant == 0.1 ~ "No significant trend",
                                            .default = magnitude_fixed),
                                  levels = c("> 2 days later",
                                             "1 - 2 days later",
                                             "< 1 days change",
                                             "1 - 2 days earlier",
                                             "> 2 days earlier",
                                             "No significant trend"))) %>%
  mutate(magnitude_fixed = fct_relevel(magnitude_fixed,c("> 2 days later",
                                                         "1 - 2 days later",
                                                         "< 1 days change",
                                                         "1 - 2 days earlier",
                                                         "> 2 days earlier",
                                                         "No significant trend"))) %>%
  group_by(metric, magnitude_fixed) %>%
  summarise(n = n()) %>%
  ggplot() +
  labs(x = "", y = "Number of Stations") +
  geom_col(aes(x = metric, y = n, fill = magnitude_fixed), col = "black", show.legend = TRUE) +
  scale_fill_manual(name = "Change per decade",
                    values = colour.scale.date,
                    drop = FALSE) +
  coord_flip() +
  theme_classic() +
  theme(legend.position = "bottom") +
  ggtitle("Timing")


# Combine above into single cowplot grid and save
mk_all <- plot_grid(mk_magnitude, mk_timing, nrow = 2, rel_heights = c(1,1))
mk_all

svg_px("./print_ver/out/figs/mk_all.svg", width = 600, height = 600)
plot(mk_all)
dev.off()

# Split by regime ===========================================================================
annual_regime <- mk_annual %>%
  mutate(magnitude_fixed = factor(case_when(significant == 0.1 ~ "No significant trend",
                                            .default = magnitude_fixed),
                                  levels = c("> 5% increase",
                                             "1 - 5% increase",
                                             "< 1% change",
                                             "1 - 5% decrease",
                                             "> 5% decrease",
                                             "No significant trend"))) %>%
  mutate(magnitude_fixed = fct_relevel(magnitude_fixed, c("> 5% increase",
                                                          "1 - 5% increase",
                                                          "< 1% change",
                                                          "1 - 5% decrease",
                                                          "> 5% decrease",
                                                          "No significant trend"))) %>%
  group_by(Regime) %>%
  mutate(n_stations = n()) %>%
  group_by(Regime, magnitude_fixed) %>%
  summarise(n = n(),
            n_stations = unique(n_stations)) %>%
  mutate(percent = n/n_stations) %>%
  ggplot() +
  ggtitle("Mean Annual Flow") +
  geom_col(aes(x = Regime , y = n, fill = magnitude_fixed), col = "black", linewidth = 0.1, show.legend = TRUE) +
  scale_fill_manual(name = "Change per decade",
                    values = colour.scale,
                    drop = FALSE) +
  xlab("") +
  ylab("Number of stations") +
  coord_flip() +
  theme_classic() +
  theme(legend.position = "none",
        plot.margin = unit(c(0.5,0,-2,0), "lines"))

peak_regime <- mk_peak %>%
  mutate(magnitude_fixed = factor(case_when(significant == 0.1 ~ "No significant trend",
                                            .default = magnitude_fixed),
                                  levels = c("> 5% increase",
                                             "1 - 5% increase",
                                             "< 1% change",
                                             "1 - 5% decrease",
                                             "> 5% decrease",
                                             "No significant trend"))) %>%
  mutate(magnitude_fixed = fct_relevel(magnitude_fixed, c("> 5% increase",
                                                          "1 - 5% increase",
                                                          "< 1% change",
                                                          "1 - 5% decrease",
                                                          "> 5% decrease",
                                                          "No significant trend"))) %>%
  group_by(Regime) %>%
  mutate(n_stations = n()) %>%
  group_by(Regime, magnitude_fixed) %>%
  summarise(n = n(),
            n_stations = unique(n_stations)) %>%
  mutate(percent = n/n_stations) %>%
  ggplot() +
  ggtitle("Peak Flow") +
  geom_col(aes(x = Regime , y = n, fill = magnitude_fixed), col = "black", linewidth = 0.1, show.legend = TRUE) +
  scale_fill_manual(name = "Change per decade",
                    values = colour.scale,
                    drop = FALSE) +
  xlab("") +
  ylab("Number of stations") +
  coord_flip() +
  theme_classic() +
  theme(axis.line.x = element_blank(),
        axis.text.x = element_blank(),
        axis.title.x = element_blank(),
        axis.ticks.x = element_blank()) +
  theme(legend.position = "none",
        plot.margin = unit(c(0,0,-2,0), "lines"))

low_regime <- mk_low %>%
  mutate(magnitude_fixed = factor(case_when(significant == 0.1 ~ "No significant trend",
                                            .default = magnitude_fixed),
                                  levels = c("> 5% increase",
                                             "1 - 5% increase",
                                             "< 1% change",
                                             "1 - 5% decrease",
                                             "> 5% decrease",
                                             "No significant trend"))) %>%
  mutate(magnitude_fixed = fct_relevel(magnitude_fixed, c("> 5% increase",
                                                          "1 - 5% increase",
                                                          "< 1% change",
                                                          "1 - 5% decrease",
                                                          "> 5% decrease",
                                                          "No significant trend"))) %>%
  group_by(Regime) %>%
  mutate(n_stations = n()) %>%
  group_by(Regime, magnitude_fixed) %>%
  summarise(n = n(),
            n_stations = unique(n_stations)) %>%
  mutate(percent = n/n_stations) %>%
  ggplot() +
  ggtitle("Summer Low Flow") +
  geom_col(aes(x = Regime , y = n, fill = magnitude_fixed), col = "black", linewidth = 0.1, show.legend = TRUE) +
  scale_fill_manual(name = "Change per decade",
                    values = colour.scale,
                    drop = FALSE) +
  xlab("") +
  ylab("Number of stations") +
  coord_flip() +
  theme_classic() +
  theme(legend.position = "none",
        plot.margin = unit(c(0,0,-2,0), "lines"))

# Timing
freshet_regime <- mk_freshet %>%
  mutate(magnitude_fixed = factor(case_when(significant == 0.1 ~ "No significant trend",
                                            .default = magnitude_fixed),
                                  levels = c("> 2 days later",
                                             "1 - 2 days later",
                                             "< 1 days change",
                                             "1 - 2 days earlier",
                                             "> 2 days earlier",
                                             "No significant trend"))) %>%
  mutate(magnitude_fixed = fct_relevel(magnitude_fixed, c("> 2 days later",
                                                          "1 - 2 days later",
                                                          "< 1 days change",
                                                          "1 - 2 days earlier",
                                                          "> 2 days earlier",
                                                          "No significant trend"))) %>%
  group_by(Regime) %>%
  mutate(n_stations = n()) %>%
  group_by(Regime, magnitude_fixed) %>%
  summarise(n = n(),
            n_stations = unique(n_stations)) %>%
  mutate(percent = n/n_stations) %>%
  ggplot() +
  ggtitle("Date of Freshet") +
  geom_col(aes(x = Regime , y = n, fill = magnitude_fixed), col = "black", linewidth = 0.1, show.legend = TRUE) +
  scale_fill_manual(name = "Change per decade",
                    values = colour.scale.date,
                    drop = FALSE) +
  xlab("") +
  ylab("Number of stations") +
  coord_flip() +
  theme_classic() +
  theme(axis.line.x = element_blank(),
        axis.text.x = element_blank(),
        axis.title.x = element_blank(),
        axis.ticks.x = element_blank()) +
  theme(legend.position = "none",
        plot.margin = unit(c(0.5,0,-2,0), "lines"))

date_low_regime <- mk_date_low %>%
  mutate(magnitude_fixed = factor(case_when(significant == 0.1 ~ "No significant trend",
                                            .default = magnitude_fixed),
                                  levels = c("> 2 days later",
                                             "1 - 2 days later",
                                             "< 1 days change",
                                             "1 - 2 days earlier",
                                             "> 2 days earlier",
                                             "No significant trend"))) %>%
  mutate(magnitude_fixed = fct_relevel(magnitude_fixed, c("> 2 days later",
                                                          "1 - 2 days later",
                                                          "< 1 days change",
                                                          "1 - 2 days earlier",
                                                          "> 2 days earlier",
                                                          "No significant trend"))) %>%
  group_by(Regime) %>%
  mutate(n_stations = n()) %>%
  group_by(Regime, magnitude_fixed) %>%
  summarise(n = n(),
            n_stations = unique(n_stations)) %>%
  mutate(percent = n/n_stations) %>%
  ggplot() +
  ggtitle("Start of Low Flow Period") +
  geom_col(aes(x = Regime , y = n, fill = magnitude_fixed), col = "black", linewidth = 0.1, show.legend = TRUE) +
  scale_fill_manual(name = "Change per decade",
                    values = colour.scale.date,
                    drop = FALSE) +
  xlab("") +
  ylab("Number of stations") +
  coord_flip() +
  theme_classic() +
  theme(legend.position = "none",
        plot.margin = unit(c(0,0,-2,0), "lines"))


# Arrange and create plot for Volume metrics and save
volume_regime <- ggarrange(annual_regime, peak_regime, low_regime,
                          ncol = 1,
                          common.legend = TRUE,
                          legend = "bottom",
                          # align = "v",
                          align = "hv")

svg_px("./print_ver/out/figs/volume_regime.svg", width = 600, height = 600)
plot(volume_regime)
dev.off()


# Do same for timing metrics and save
timing_regime <- ggarrange(freshet_regime, date_low_regime,
                          ncol = 1,
                          common.legend = TRUE,
                          legend = "bottom",
                          # align = "v",
                          align = "hv")

svg_px("./print_ver/out/figs/timing_regime.svg", width = 600, height = 600)
plot(timing_regime)
dev.off()

plot_list <- list(
  All_volume_overall = mk_magnitude + labs(x = "", y = "Number of Stations") + theme(legend.position = "right"),
  All_timing_overall = mk_timing + theme(legend.position = "right"),
  All_volume_river_mean = annual_regime + theme(legend.position = "right", plot.margin = unit(c(0,0,0,0), "lines")),
  All_volume_river_peak = peak_regime + theme(axis.line.x = element_line(),
                                              axis.text.x = element_text(),
                                              axis.title.x = element_text(),
                                              axis.ticks.x = element_line(),
                                              legend.position = "right", plot.margin = unit(c(0,0,0,0), "lines")),
  All_volume_river_low = low_regime + theme(axis.line.x = element_line(),
                                            axis.text.x = element_text(),
                                            axis.title.x = element_text(),
                                            axis.ticks.x = element_line(),
                                            legend.position = "right", plot.margin = unit(c(0,0,0,0), "lines")),
  All_timing_river_freshet = freshet_regime + theme(axis.line.x = element_line(),
                                                    axis.text.x = element_text(),
                                                    axis.title.x = element_text(),
                                                    axis.ticks.x = element_line(),
                                                    legend.position = "right", plot.margin = unit(c(0,0,0,0), "lines")),
  All_timing_river_low_start = date_low_regime + theme(axis.line.x = element_line(),
                                                       axis.text.x = element_text(),
                                                       axis.title.x = element_text(),
                                                       axis.ticks.x = element_line(),
                                                       legend.position = "right", plot.margin = unit(c(0,0,0,0), "lines")))

## Geographic Plots (Appendix?) =====================================================================
# Metrics - Volume
# - Average Annual Flow =============================================================

annual_bar <- mk_annual %>%
  mutate(magnitude_fixed = factor(case_when(significant == 0.1 ~ "No significant trend",
                                            .default = magnitude_fixed),
                                  levels = c("> 5% increase",
                                             "1 - 5% increase",
                                             "< 1% change",
                                             "1 - 5% decrease",
                                             "> 5% decrease",
                                             "No significant trend"))) %>%
  mutate(magnitude_fixed = fct_relevel(magnitude_fixed, c("> 5% increase",
                                                          "1 - 5% increase",
                                                          "< 1% change",
                                                          "1 - 5% decrease",
                                                          "> 5% decrease",
                                                          "No significant trend"))) %>%
  group_by(Sub_Basin) %>%
  mutate(n_stations = n()) %>%
  group_by(Sub_Basin, magnitude_fixed) %>%
  summarise(n = n(),
            n_stations = unique(n_stations),
            region = unique(Major_Basin)) %>%
  mutate(percent = n/n_stations)


# Try plotting each major basin "separately and combining in facet grid
coast <- annual_bar %>%
  filter(region == "Coastal")

coast.map = major_basins %>%
  ggplot() +
  labs(title = "Coastal") +
  theme_void() +
  geom_sf(fill = NA) +
  geom_sf(data = major_basins %>%
            filter(Major_Basin == "Coastal"), fill = "yellow")

 p1 = coast  %>%
   ggplot() +
  # ggtitle("Change in Mean Annual Flow") +
  geom_col(aes(x = fct_reorder(Sub_Basin, region) , y = n, fill = magnitude_fixed), col = "black", linewidth = 0.1, show.legend=TRUE) +
  scale_fill_manual(name = "Change per decade",
                    values = colour.scale,
                    drop = FALSE) +
  xlab("") +
  ylab("Number of stations") +
  coord_flip() +
  theme_classic() +
  theme(legend.position = "none",
        plot.margin = unit(c(0,0,-2,0), "lines")) +
  ylim(0, max(annual_bar$n_stations) + 10) +
  annotation_custom(grob = ggplotGrob(coast.map), ymin = max(annual_bar$n_stations) + 2, ymax = max(annual_bar$n_stations) + 10)

p1

fraser <- annual_bar %>%
  filter(region == "Fraser")

fraser.map <- major_basins %>%
  ggplot() +
  labs(title = "Fraser") +
  theme_void() +
  geom_sf(fill = NA) +
  geom_sf(data = major_basins %>%
            filter(Major_Basin == "Fraser"), fill = "yellow")

p2 <- fraser  %>%
  ggplot() +
  # ggtitle("Change in Mean Annual Flow") +
  geom_col(aes(x = fct_reorder(Sub_Basin, region) , y = n, fill = magnitude_fixed), col = "black", linewidth = 0.1, show.legend=TRUE) +
  scale_fill_manual(name = "Change per decade",
                    values = colour.scale,
                    drop = FALSE) +
  xlab("") +
  ylab("Number of stations") +
  coord_flip() +
  theme_classic() +
  theme(legend.position = "none",
        plot.margin = unit(c(-1,-1,-1,-1), "lines")) +
  theme(axis.line.x = element_blank(),
        axis.text.x = element_blank(),
        axis.title.x = element_blank(),
        axis.ticks.x = element_blank()) +
  ylim(0, max(annual_bar$n_stations) + 10) +
  annotation_custom(grob = ggplotGrob(fraser.map), ymin = max(annual_bar$n_stations) + 2, ymax = max(annual_bar$n_stations) + 10)

p2

Columbia <- annual_bar %>%
  filter(region == "Columbia")

Columbia.map <- major_basins %>%
  ggplot() +
  labs(title = "Columbia") +
  theme_void() +
  geom_sf(fill = NA) +
  geom_sf(data = major_basins %>%
            filter(Major_Basin == "Columbia"), fill = "yellow")

p3 <- Columbia  %>%
  ggplot() +
  # ggtitle("Change in Mean Annual Flow") +
  geom_col(aes(x = fct_reorder(Sub_Basin, region) , y = n, fill = magnitude_fixed), col = "black", linewidth = 0.1, show.legend=TRUE) +
  scale_fill_manual(name = "Change per decade",
                    values = colour.scale,
                    drop = FALSE) +
  xlab("") +
  ylab("Number of stations") +
  coord_flip() +
  theme_classic() +
  theme(legend.position = "none",
        plot.margin = unit(c(-1,-1,-1,-1), "lines")) +
  theme(axis.line.x = element_blank(),
        axis.text.x = element_blank(),
        axis.title.x = element_blank(),
        axis.ticks.x = element_blank()) +
  ylim(0, max(annual_bar$n_stations) + 10) +
  annotation_custom(grob = ggplotGrob(Columbia.map), ymin = max(annual_bar$n_stations) + 2, ymax = max(annual_bar$n_stations) + 10)

p3

Liard <- annual_bar %>%
  filter(region == "Liard")

Liard.map <- major_basins %>%
  ggplot() +
  labs(title = "Liard") +
  theme_void() +
  geom_sf(fill = NA) +
  geom_sf(data = major_basins %>%
            filter(Major_Basin == "Liard"), fill = "yellow")

p4 <- Liard  %>%
  ggplot() +
  # ggtitle("Change in Mean Annual Flow") +
  geom_col(aes(x = fct_reorder(Sub_Basin, region) , y = n, fill = magnitude_fixed), col = "black", linewidth = 0.1, show.legend=TRUE) +
  scale_fill_manual(name = "Change per decade",
                    values = colour.scale,
                    drop = FALSE) +
  xlab("") +
  ylab("Number of stations") +
  coord_flip() +
  theme_classic() +
  theme(legend.position = "none",
        plot.margin = unit(c(-1,-1,-1,-1), "lines")) +
  theme(axis.line.x = element_blank(),
        axis.text.x = element_blank(),
        axis.title.x = element_blank(),
        axis.ticks.x = element_blank()) +
  ylim(0, max(annual_bar$n_stations) + 10) +
  annotation_custom(grob = ggplotGrob(Liard.map), ymin = max(annual_bar$n_stations) + 2, ymax = max(annual_bar$n_stations) + 10)

p4


Northwest <- annual_bar %>%
  filter(region == "Northwest")

Northwest.map <- major_basins %>%
  ggplot() +
  labs(title = "North-West") +
  theme_void() +
  geom_sf(fill = NA) +
  geom_sf(data = major_basins %>%
            filter(Major_Basin == "Northwest"), fill = "yellow")

p5 <- Northwest  %>%
  ggplot() +
  # ggtitle("Change in Mean Annual Flow") +
  geom_col(aes(x = fct_reorder(Sub_Basin, region) , y = n, fill = magnitude_fixed), col = "black", linewidth = 0.1, show.legend=TRUE) +
  scale_fill_manual(name = "Change per decade",
                    values = colour.scale,
                    drop = FALSE) +
  xlab("") +
  ylab("Number of stations") +
  coord_flip() +
  theme_classic() +
  theme(legend.position = "none",
        plot.margin = unit(c(-1,-1,-1,-1), "lines")) +
  theme(axis.line.x = element_blank(),
        axis.text.x = element_blank(),
        axis.title.x = element_blank(),
        axis.ticks.x = element_blank()) +
  ylim(0, max(annual_bar$n_stations) + 10) +
  annotation_custom(grob = ggplotGrob(Northwest.map), ymin = max(annual_bar$n_stations) + 2, ymax = max(annual_bar$n_stations) + 10)

p5

Peace <- annual_bar %>%
  filter(region == "Peace")

Peace.map <- major_basins %>%
  ggplot() +
  labs(title = "Peace") +
  theme_void() +
  geom_sf(fill = NA) +
  geom_sf(data = major_basins %>%
            filter(Major_Basin == "Peace"), fill = "yellow")

p6 <- Peace  %>%
  ggplot() +
  # ggtitle("Change in Mean Annual Flow") +
  geom_col(aes(x = fct_reorder(Sub_Basin, region) , y = n, fill = magnitude_fixed), col = "black", linewidth = 0.1, show.legend=TRUE) +
  scale_fill_manual(name = "Change per decade",
                    values = colour.scale,
                    drop = FALSE) +
  xlab("") +
  ylab("Number of stations") +
  coord_flip() +
  theme_classic() +
  theme(legend.position = "none",
        plot.margin = unit(c(-1,-1,-1,-1), "lines")) +
  theme(axis.line.x = element_blank(),
        axis.text.x = element_blank(),
        axis.title.x = element_blank(),
        axis.ticks.x = element_blank()) +
  ylim(0, max(annual_bar$n_stations) + 10) +
  annotation_custom(grob = ggplotGrob(Peace.map), ymin = max(annual_bar$n_stations) + 2, ymax = max(annual_bar$n_stations) + 10)

p6

plot_list[["Liard_volume_mean"]] <- Liard  %>%
                                          ggplot() +
                                          geom_col(aes(x = fct_reorder(Sub_Basin, region) , y = n, fill = magnitude_fixed), col = "black", linewidth = 0.1, show.legend=TRUE) +
                                          scale_fill_manual(name = "Change per decade",
                                                            values = colour.scale,
                                                            drop = FALSE) +
                                          xlab("") +
                                          ylab("Number of stations") +
                                          coord_flip() +
                                          theme_classic()
plot_list[["Columbia_volume_mean"]] <- Columbia  %>%
                                                 ggplot() +
                                                 geom_col(aes(x = fct_reorder(Sub_Basin, region) , y = n, fill = magnitude_fixed), col = "black", linewidth = 0.1, show.legend=TRUE) +
                                                 scale_fill_manual(name = "Change per decade",
                                                                   values = colour.scale,
                                                                   drop = FALSE) +
                                                 xlab("") +
                                                 ylab("Number of stations") +
                                                 coord_flip() +
                                                 theme_classic()
plot_list[["Fraser_volume_mean"]] <- fraser  %>%
                                             ggplot() +
                                             geom_col(aes(x = fct_reorder(Sub_Basin, region) , y = n, fill = magnitude_fixed), col = "black", linewidth = 0.1, show.legend=TRUE) +
                                             scale_fill_manual(name = "Change per decade",
                                             values = colour.scale,
                                             drop = FALSE) +
                                             xlab("") +
                                             ylab("Number of stations") +
                                             coord_flip() +
                                             theme_classic()
plot_list[["Peace_volume_mean"]] <- Peace  %>%
                                           ggplot() +
                                           geom_col(aes(x = fct_reorder(Sub_Basin, region) , y = n, fill = magnitude_fixed), col = "black", linewidth = 0.1, show.legend=TRUE) +
                                           scale_fill_manual(name = "Change per decade",
                                                             values = colour.scale,
                                                             drop = FALSE) +
                                           xlab("") +
                                           ylab("Number of stations") +
                                           coord_flip() +
                                           theme_classic()
plot_list[["Northwest_volume_mean"]] <- Northwest  %>%
                                                   ggplot() +
                                                   geom_col(aes(x = fct_reorder(Sub_Basin, region) , y = n, fill = magnitude_fixed), col = "black", linewidth = 0.1, show.legend=TRUE) +
                                                   scale_fill_manual(name = "Change per decade",
                                                                     values = colour.scale,
                                                                     drop = FALSE) +
                                                   xlab("") +
                                                   ylab("Number of stations") +
                                                   coord_flip() +
                                                   theme_classic()
plot_list[["Coastal_volume_mean"]] <- coast  %>%
                                             ggplot() +
                                             geom_col(aes(x = fct_reorder(Sub_Basin, region) , y = n, fill = magnitude_fixed), col = "black", linewidth = 0.1, show.legend=TRUE) +
                                             scale_fill_manual(name = "Change per decade",
                                                               values = colour.scale,
                                                               drop = FALSE) +
                                             xlab("") +
                                             ylab("Number of stations") +
                                             coord_flip() +
                                             theme_classic()

annual_bar_plot <- ggarrange(p4, p3, p2, p6, p5, p1, ncol = 1,
                            common.legend = TRUE,
                            legend = "bottom",
                            # align = "v",
                            align = "hv")

annual_bar_plot

svg_px("./print_ver/out/figs/annual_bar.svg", width = 600, height = 600)
plot(annual_bar_plot)
dev.off()

# - Peak Flow ===========================================================================

peak_bar = mk_peak %>%
  mutate(magnitude_fixed = factor(case_when(significant == 0.1 ~ "No significant trend",
                                            .default = magnitude_fixed),
                                  levels = c("> 5% increase",
                                             "1 - 5% increase",
                                             "< 1% change",
                                             "1 - 5% decrease",
                                             "> 5% decrease",
                                             "No significant trend"))) %>%
  mutate(magnitude_fixed = fct_relevel(magnitude_fixed, c("> 5% increase",
                                                          "1 - 5% increase",
                                                          "< 1% change",
                                                          "1 - 5% decrease",
                                                          "> 5% decrease",
                                                          "No significant trend"))) %>%
  group_by(Sub_Basin) %>%
  mutate(n_stations = n()) %>%
  group_by(Sub_Basin, magnitude_fixed) %>%
  summarise(n = n(),
            n_stations = unique(n_stations),
            region = unique(Major_Basin)) %>%
  mutate(percent = n/n_stations)


# Try plotting each major basin "separately and combining in facet grid
coast = peak_bar %>%
  filter(region == "Coastal")

coast.map = major_basins %>%
  ggplot() +
  labs(title = "Coastal") +
  theme_void() +
  geom_sf(fill = NA) +
  geom_sf(data = major_basins %>%
            filter(Major_Basin == "Coastal"), fill = "yellow")

p1 = coast  %>%
  ggplot() +
  # ggtitle("Change in Mean peak Flow") +
  geom_col(aes(x = fct_reorder(Sub_Basin, region) , y = n, fill = magnitude_fixed), col = "black", linewidth = 0.1, show.legend=TRUE) +
  scale_fill_manual(name = "Change per decade",
                    values = colour.scale,
                    drop = FALSE) +
  xlab("") +
  ylab("Number of stations") +
  coord_flip() +
  theme_classic() +
  theme(legend.position = "none",
        plot.margin = unit(c(0,0,-2,0), "lines")) +
  ylim(0, max(peak_bar$n_stations) + 10) +
  annotation_custom(grob = ggplotGrob(coast.map), ymin = max(peak_bar$n_stations) + 2, ymax = max(peak_bar$n_stations) + 10)

p1

fraser = peak_bar %>%
  filter(region == "Fraser")

fraser.map = major_basins %>%
  ggplot() +
  labs(title = "Fraser") +
  theme_void() +
  geom_sf(fill = NA) +
  geom_sf(data = major_basins %>%
            filter(Major_Basin == "Fraser"), fill = "yellow")

p2 = fraser  %>%
  ggplot() +
  # ggtitle("Change in Mean peak Flow") +
  geom_col(aes(x = fct_reorder(Sub_Basin, region) , y = n, fill = magnitude_fixed), col = "black", linewidth = 0.1, show.legend=TRUE) +
  scale_fill_manual(name = "Change per decade",
                    values = colour.scale,
                    drop = FALSE) +
  xlab("") +
  ylab("Number of stations") +
  coord_flip() +
  theme_classic() +
  theme(legend.position = "none",
        plot.margin = unit(c(-1,-1,-1,-1), "lines")) +
  theme(axis.line.x = element_blank(),
        axis.text.x = element_blank(),
        axis.title.x = element_blank(),
        axis.ticks.x = element_blank()) +
  ylim(0, max(peak_bar$n_stations) + 10) +
  annotation_custom(grob = ggplotGrob(fraser.map), ymin = max(peak_bar$n_stations) + 2, ymax = max(peak_bar$n_stations) + 10)

p2

Columbia = peak_bar %>%
  filter(region == "Columbia")

Columbia.map = major_basins %>%
  ggplot() +
  labs(title = "Columbia") +
  theme_void() +
  geom_sf(fill = NA) +
  geom_sf(data = major_basins %>%
            filter(Major_Basin == "Columbia"), fill = "yellow")

p3 = Columbia  %>%
  ggplot() +
  # ggtitle("Change in Mean peak Flow") +
  geom_col(aes(x = fct_reorder(Sub_Basin, region) , y = n, fill = magnitude_fixed), col = "black", linewidth = 0.1, show.legend=TRUE) +
  scale_fill_manual(name = "Change per decade",
                    values = colour.scale,
                    drop = FALSE) +
  xlab("") +
  ylab("Number of stations") +
  coord_flip() +
  theme_classic() +
  theme(legend.position = "none",
        plot.margin = unit(c(-1,-1,-1,-1), "lines")) +
  theme(axis.line.x = element_blank(),
        axis.text.x = element_blank(),
        axis.title.x = element_blank(),
        axis.ticks.x = element_blank()) +
  ylim(0, max(peak_bar$n_stations) + 10) +
  annotation_custom(grob = ggplotGrob(Columbia.map), ymin = max(peak_bar$n_stations) + 2, ymax = max(peak_bar$n_stations) + 10)

p3

Liard = peak_bar %>%
  filter(region == "Liard")

Liard.map = major_basins %>%
  ggplot() +
  labs(title = "Liard") +
  theme_void() +
  geom_sf(fill = NA) +
  geom_sf(data = major_basins %>%
            filter(Major_Basin == "Liard"), fill = "yellow")

p4 = Liard  %>%
  ggplot() +
  # ggtitle("Change in Mean peak Flow") +
  geom_col(aes(x = fct_reorder(Sub_Basin, region) , y = n, fill = magnitude_fixed), col = "black", linewidth = 0.1, show.legend=TRUE) +
  scale_fill_manual(name = "Change per decade",
                    values = colour.scale,
                    drop = FALSE) +
  xlab("") +
  ylab("Number of stations") +
  coord_flip() +
  theme_classic() +
  theme(legend.position = "none",
        plot.margin = unit(c(-1,-1,-1,-1), "lines")) +
  theme(axis.line.x = element_blank(),
        axis.text.x = element_blank(),
        axis.title.x = element_blank(),
        axis.ticks.x = element_blank()) +
  ylim(0, max(peak_bar$n_stations) + 10) +
  annotation_custom(grob = ggplotGrob(Liard.map), ymin = max(peak_bar$n_stations) + 2, ymax = max(peak_bar$n_stations) + 10)

p4


Northwest = peak_bar %>%
  filter(region == "Northwest")

Northwest.map = major_basins %>%
  ggplot() +
  labs(title = "North-West") +
  theme_void() +
  geom_sf(fill = NA) +
  geom_sf(data = major_basins %>%
            filter(Major_Basin == "Northwest"), fill = "yellow")

p5 = Northwest  %>%
  ggplot() +
  # ggtitle("Change in Mean peak Flow") +
  geom_col(aes(x = fct_reorder(Sub_Basin, region) , y = n, fill = magnitude_fixed), col = "black", linewidth = 0.1, show.legend=TRUE) +
  scale_fill_manual(name = "Change per decade",
                    values = colour.scale,
                    drop = FALSE) +
  xlab("") +
  ylab("Number of stations") +
  coord_flip() +
  theme_classic() +
  theme(legend.position = "none",
        plot.margin = unit(c(-1,-1,-1,-1), "lines")) +
  theme(axis.line.x = element_blank(),
        axis.text.x = element_blank(),
        axis.title.x = element_blank(),
        axis.ticks.x = element_blank()) +
  ylim(0, max(peak_bar$n_stations) + 10) +
  annotation_custom(grob = ggplotGrob(Northwest.map), ymin = max(peak_bar$n_stations) + 2, ymax = max(peak_bar$n_stations) + 10)

p5

Peace = peak_bar %>%
  filter(region == "Peace")

Peace.map = major_basins %>%
  ggplot() +
  labs(title = "Peace") +
  theme_void() +
  geom_sf(fill = NA) +
  geom_sf(data = major_basins %>%
            filter(Major_Basin == "Peace"), fill = "yellow")

p6 = Peace  %>%
  ggplot() +
  # ggtitle("Change in Mean peak Flow") +
  geom_col(aes(x = fct_reorder(Sub_Basin, region) , y = n, fill = magnitude_fixed), col = "black", linewidth = 0.1, show.legend=TRUE) +
  scale_fill_manual(name = "Change per decade",
                    values = colour.scale,
                    drop = FALSE) +
  xlab("") +
  ylab("Number of stations") +
  coord_flip() +
  theme_classic() +
  theme(legend.position = "none",
        plot.margin = unit(c(-1,-1,-1,-1), "lines")) +
  theme(axis.line.x = element_blank(),
        axis.text.x = element_blank(),
        axis.title.x = element_blank(),
        axis.ticks.x = element_blank()) +
  ylim(0, max(peak_bar$n_stations) + 10) +
  annotation_custom(grob = ggplotGrob(Peace.map), ymin = max(peak_bar$n_stations) + 2, ymax = max(peak_bar$n_stations) + 10)

p6

plot_list[["Liard_volume_peak"]] <- Liard  %>%
                                           ggplot() +
                                           geom_col(aes(x = fct_reorder(Sub_Basin, region) , y = n, fill = magnitude_fixed), col = "black", linewidth = 0.1, show.legend=TRUE) +
                                           scale_fill_manual(name = "Change per decade",
                                                             values = colour.scale,
                                                             drop = FALSE) +
                                           xlab("") +
                                           ylab("Number of stations") +
                                           coord_flip() +
                                           theme_classic()
plot_list[["Columbia_volume_peak"]] <- Columbia  %>%
                                                 ggplot() +
                                                 geom_col(aes(x = fct_reorder(Sub_Basin, region) , y = n, fill = magnitude_fixed), col = "black", linewidth = 0.1, show.legend=TRUE) +
                                                 scale_fill_manual(name = "Change per decade",
                                                                   values = colour.scale,
                                                                   drop = FALSE) +
                                                 xlab("") +
                                                 ylab("Number of stations") +
                                                 coord_flip() +
                                                 theme_classic()
plot_list[["Fraser_volume_peak"]] <- fraser  %>%
                                             ggplot() +
                                             geom_col(aes(x = fct_reorder(Sub_Basin, region) , y = n, fill = magnitude_fixed), col = "black", linewidth = 0.1, show.legend=TRUE) +
                                             scale_fill_manual(name = "Change per decade",
                                                               values = colour.scale,
                                                               drop = FALSE) +
                                             xlab("") +
                                             ylab("Number of stations") +
                                             coord_flip() +
                                             theme_classic()
plot_list[["Peace_volume_peak"]] <- Peace  %>%
                                           ggplot() +
                                           geom_col(aes(x = fct_reorder(Sub_Basin, region) , y = n, fill = magnitude_fixed), col = "black", linewidth = 0.1, show.legend=TRUE) +
                                           scale_fill_manual(name = "Change per decade",
                                                             values = colour.scale,
                                                             drop = FALSE) +
                                           xlab("") +
                                           ylab("Number of stations") +
                                           coord_flip() +
                                           theme_classic()
plot_list[["Northwest_volume_peak"]] <- Northwest  %>%
                                                   ggplot() +
                                                   geom_col(aes(x = fct_reorder(Sub_Basin, region) , y = n, fill = magnitude_fixed), col = "black", linewidth = 0.1, show.legend=TRUE) +
                                                   scale_fill_manual(name = "Change per decade",
                                                                     values = colour.scale,
                                                                     drop = FALSE) +
                                                   xlab("") +
                                                   ylab("Number of stations") +
                                                   coord_flip() +
                                                   theme_classic()
plot_list[["Coastal_volume_peak"]] <- coast  %>%
                                             ggplot() +
                                             geom_col(aes(x = fct_reorder(Sub_Basin, region) , y = n, fill = magnitude_fixed), col = "black", linewidth = 0.1, show.legend=TRUE) +
                                             scale_fill_manual(name = "Change per decade",
                                                               values = colour.scale,
                                                               drop = FALSE) +
                                             xlab("") +
                                             ylab("Number of stations") +
                                             coord_flip() +
                                             theme_classic()


peak_bar_plot = ggarrange(p4, p3, p2, p6, p5, p1, ncol = 1,
                            common.legend = TRUE,
                            legend = "bottom",
                            # align = "v",
                            align = "hv")

peak_bar_plot

svg_px("./print_ver/out/figs/peak_bar.svg", width = 600, height = 600)
plot(peak_bar_plot)
dev.off()

# - Low Flow ===============================================================================
low_bar = mk_low %>%
  mutate(magnitude_fixed = factor(case_when(significant == 0.1 ~ "No significant trend",
                                            .default = magnitude_fixed),
                                  levels = c("> 5% increase",
                                             "1 - 5% increase",
                                             "< 1% change",
                                             "1 - 5% decrease",
                                             "> 5% decrease",
                                             "No significant trend"))) %>%
  mutate(magnitude_fixed = fct_relevel(magnitude_fixed, c("> 5% increase",
                                                          "1 - 5% increase",
                                                          "< 1% change",
                                                          "1 - 5% decrease",
                                                          "> 5% decrease",
                                                          "No significant trend"))) %>%
  group_by(Sub_Basin) %>%
  mutate(n_stations = n()) %>%
  group_by(Sub_Basin, magnitude_fixed) %>%
  summarise(n = n(),
            n_stations = unique(n_stations),
            region = unique(Major_Basin)) %>%
  mutate(percent = n/n_stations)


# Try plotting each major basin "separately and combining in facet grid
coast = low_bar %>%
  filter(region == "Coastal")

coast.map = major_basins %>%
  ggplot() +
  labs(title = "Coastal") +
  theme_void() +
  geom_sf(fill = NA) +
  geom_sf(data = major_basins %>%
            filter(Major_Basin == "Coastal"), fill = "yellow")

p1 = coast  %>%
  ggplot() +
  # ggtitle("Change in Mean low Flow") +
  geom_col(aes(x = fct_reorder(Sub_Basin, region) , y = n, fill = magnitude_fixed), col = "black", linewidth = 0.1, show.legend=TRUE) +
  scale_fill_manual(name = "Change per decade",
                    values = colour.scale,
                    drop = FALSE) +
  xlab("") +
  ylab("Number of stations") +
  coord_flip() +
  theme_classic() +
  theme(legend.position = "none",
        plot.margin = unit(c(0,0,-2,0), "lines")) +
  ylim(0, max(low_bar$n_stations) + 10) +
  annotation_custom(grob = ggplotGrob(coast.map), ymin = max(low_bar$n_stations) + 2, ymax = max(low_bar$n_stations) + 10)

p1

fraser = low_bar %>%
  filter(region == "Fraser")

fraser.map = major_basins %>%
  ggplot() +
  labs(title = "Fraser") +
  theme_void() +
  geom_sf(fill = NA) +
  geom_sf(data = major_basins %>%
            filter(Major_Basin == "Fraser"), fill = "yellow")

p2 = fraser  %>%
  ggplot() +
  # ggtitle("Change in Mean low Flow") +
  geom_col(aes(x = fct_reorder(Sub_Basin, region) , y = n, fill = magnitude_fixed), col = "black", linewidth = 0.1, show.legend=TRUE) +
  scale_fill_manual(name = "Change per decade",
                    values = colour.scale,
                    drop = FALSE) +
  xlab("") +
  ylab("Number of stations") +
  coord_flip() +
  theme_classic() +
  theme(legend.position = "none",
        plot.margin = unit(c(-1,-1,-1,-1), "lines")) +
  theme(axis.line.x = element_blank(),
        axis.text.x = element_blank(),
        axis.title.x = element_blank(),
        axis.ticks.x = element_blank()) +
  ylim(0, max(low_bar$n_stations) + 10) +
  annotation_custom(grob = ggplotGrob(fraser.map), ymin = max(low_bar$n_stations) + 2, ymax = max(low_bar$n_stations) + 10)

p2

Columbia = low_bar %>%
  filter(region == "Columbia")

Columbia.map = major_basins %>%
  ggplot() +
  labs(title = "Columbia") +
  theme_void() +
  geom_sf(fill = NA) +
  geom_sf(data = major_basins %>%
            filter(Major_Basin == "Columbia"), fill = "yellow")

p3 = Columbia  %>%
  ggplot() +
  # ggtitle("Change in Mean low Flow") +
  geom_col(aes(x = fct_reorder(Sub_Basin, region) , y = n, fill = magnitude_fixed), col = "black", linewidth = 0.1, show.legend=TRUE) +
  scale_fill_manual(name = "Change per decade",
                    values = colour.scale,
                    drop = FALSE) +
  xlab("") +
  ylab("Number of stations") +
  coord_flip() +
  theme_classic() +
  theme(legend.position = "none",
        plot.margin = unit(c(-1,-1,-1,-1), "lines")) +
  theme(axis.line.x = element_blank(),
        axis.text.x = element_blank(),
        axis.title.x = element_blank(),
        axis.ticks.x = element_blank()) +
  ylim(0, max(low_bar$n_stations) + 10) +
  annotation_custom(grob = ggplotGrob(Columbia.map), ymin = max(low_bar$n_stations) + 2, ymax = max(low_bar$n_stations) + 10)

p3

Liard = low_bar %>%
  filter(region == "Liard")

Liard.map = major_basins %>%
  ggplot() +
  labs(title = "Liard") +
  theme_void() +
  geom_sf(fill = NA) +
  geom_sf(data = major_basins %>%
            filter(Major_Basin == "Liard"), fill = "yellow")

p4 = Liard  %>%
  ggplot() +
  # ggtitle("Change in Mean low Flow") +
  geom_col(aes(x = fct_reorder(Sub_Basin, region) , y = n, fill = magnitude_fixed), col = "black", linewidth = 0.1, show.legend=TRUE) +
  scale_fill_manual(name = "Change per decade",
                    values = colour.scale,
                    drop = FALSE) +
  xlab("") +
  ylab("Number of stations") +
  coord_flip() +
  theme_classic() +
  theme(legend.position = "none",
        plot.margin = unit(c(-1,-1,-1,-1), "lines")) +
  theme(axis.line.x = element_blank(),
        axis.text.x = element_blank(),
        axis.title.x = element_blank(),
        axis.ticks.x = element_blank()) +
  ylim(0, max(low_bar$n_stations) + 10) +
  annotation_custom(grob = ggplotGrob(Liard.map), ymin = max(low_bar$n_stations) + 2, ymax = max(low_bar$n_stations) + 10)

p4


Northwest = low_bar %>%
  filter(region == "Northwest")

Northwest.map = major_basins %>%
  ggplot() +
  labs(title = "North-West") +
  theme_void() +
  geom_sf(fill = NA) +
  geom_sf(data = major_basins %>%
            filter(Major_Basin == "Northwest"), fill = "yellow")

p5 = Northwest  %>%
  ggplot() +
  # ggtitle("Change in Mean low Flow") +
  geom_col(aes(x = fct_reorder(Sub_Basin, region) , y = n, fill = magnitude_fixed), col = "black", linewidth = 0.1, show.legend=TRUE) +
  scale_fill_manual(name = "Change per decade",
                    values = colour.scale,
                    drop = FALSE) +
  xlab("") +
  ylab("Number of stations") +
  coord_flip() +
  theme_classic() +
  theme(legend.position = "none",
        plot.margin = unit(c(-1,-1,-1,-1), "lines")) +
  theme(axis.line.x = element_blank(),
        axis.text.x = element_blank(),
        axis.title.x = element_blank(),
        axis.ticks.x = element_blank()) +
  ylim(0, max(low_bar$n_stations) + 10) +
  annotation_custom(grob = ggplotGrob(Northwest.map), ymin = max(low_bar$n_stations) + 2, ymax = max(low_bar$n_stations) + 10)

p5

Peace = low_bar %>%
  filter(region == "Peace")

Peace.map = major_basins %>%
  ggplot() +
  labs(title = "Peace") +
  theme_void() +
  geom_sf(fill = NA) +
  geom_sf(data = major_basins %>%
            filter(Major_Basin == "Peace"), fill = "yellow")

p6 = Peace  %>%
  ggplot() +
  # ggtitle("Change in Mean low Flow") +
  geom_col(aes(x = fct_reorder(Sub_Basin, region) , y = n, fill = magnitude_fixed), col = "black", linewidth = 0.1, show.legend=TRUE) +
  scale_fill_manual(name = "Change per decade",
                    values = colour.scale,
                    drop = FALSE) +
  xlab("") +
  ylab("Number of stations") +
  coord_flip() +
  theme_classic() +
  theme(legend.position = "none",
        plot.margin = unit(c(-1,-1,-1,-1), "lines")) +
  theme(axis.line.x = element_blank(),
        axis.text.x = element_blank(),
        axis.title.x = element_blank(),
        axis.ticks.x = element_blank()) +
  ylim(0, max(low_bar$n_stations) + 10) +
  annotation_custom(grob = ggplotGrob(Peace.map), ymin = max(low_bar$n_stations) + 2, ymax = max(low_bar$n_stations) + 10)

p6



low_bar_plot = ggarrange(p4, p3, p2, p6, p5, p1, ncol = 1,
                            common.legend = TRUE,
                            legend = "bottom",
                            # align = "v",
                            align = "hv")

low_bar_plot

plot_list[["Liard_volume_low"]] <- Liard  %>%
                                          ggplot() +
                                          geom_col(aes(x = fct_reorder(Sub_Basin, region) , y = n, fill = magnitude_fixed), col = "black", linewidth = 0.1, show.legend=TRUE) +
                                          scale_fill_manual(name = "Change per decade",
                                                            values = colour.scale,
                                                            drop = FALSE) +
                                          xlab("") +
                                          ylab("Number of stations") +
                                          coord_flip() +
                                          theme_classic()
plot_list[["Columbia_volume_low"]] <- Columbia  %>%
                                                ggplot() +
                                                geom_col(aes(x = fct_reorder(Sub_Basin, region) , y = n, fill = magnitude_fixed), col = "black", linewidth = 0.1, show.legend=TRUE) +
                                                scale_fill_manual(name = "Change per decade",
                                                                  values = colour.scale,
                                                                  drop = FALSE) +
                                                xlab("") +
                                                ylab("Number of stations") +
                                                coord_flip() +
                                                theme_classic()
plot_list[["Fraser_volume_low"]] <- fraser  %>%
                                            ggplot() +
                                            geom_col(aes(x = fct_reorder(Sub_Basin, region) , y = n, fill = magnitude_fixed), col = "black", linewidth = 0.1, show.legend=TRUE) +
                                            scale_fill_manual(name = "Change per decade",
                                                              values = colour.scale,
                                                              drop = FALSE) +
                                            xlab("") +
                                            ylab("Number of stations") +
                                            coord_flip() +
                                            theme_classic()
plot_list[["Peace_volume_low"]] <- Peace  %>%
                                          ggplot() +
                                          geom_col(aes(x = fct_reorder(Sub_Basin, region) , y = n, fill = magnitude_fixed), col = "black", linewidth = 0.1, show.legend=TRUE) +
                                          scale_fill_manual(name = "Change per decade",
                                                            values = colour.scale,
                                                            drop = FALSE) +
                                          xlab("") +
                                          ylab("Number of stations") +
                                          coord_flip() +
                                          theme_classic()
plot_list[["Northwest_volume_low"]] <- Northwest  %>%
                                                  ggplot() +
                                                  geom_col(aes(x = fct_reorder(Sub_Basin, region) , y = n, fill = magnitude_fixed), col = "black", linewidth = 0.1, show.legend=TRUE) +
                                                  scale_fill_manual(name = "Change per decade",
                                                                    values = colour.scale,
                                                                    drop = FALSE) +
                                                  xlab("") +
                                                  ylab("Number of stations") +
                                                  coord_flip() +
                                                  theme_classic()
plot_list[["Coastal_volume_low"]] <- coast  %>%
                                            ggplot() +
                                            geom_col(aes(x = fct_reorder(Sub_Basin, region) , y = n, fill = magnitude_fixed), col = "black", linewidth = 0.1, show.legend=TRUE) +
                                            scale_fill_manual(name = "Change per decade",
                                                              values = colour.scale,
                                                              drop = FALSE) +
                                            xlab("") +
                                            ylab("Number of stations") +
                                            coord_flip() +
                                            theme_classic()

svg_px("./print_ver/out/figs/low_bar.svg", width = 600, height = 600)
plot(low_bar_plot)
dev.off()

#Metrics - Timing of Flow ===================================================================

# Date of Freshet ======================================================================

freshet_bar = mk_freshet %>%
  mutate(magnitude_fixed = factor(case_when(significant == 0.1 ~ "No significant trend",
                                            .default = magnitude_fixed),
                                  levels = c("> 2 days later",
                                             "1 - 2 days later",
                                             "< 1 days change",
                                             "1 - 2 days earlier",
                                             "> 2 days earlier",
                                             "No significant trend"))) %>%  mutate(magnitude_fixed = fct_relevel(magnitude_fixed, c("> 2 days later",
                                                          "1 - 2 days later",
                                                          "< 1 days change",
                                                          "1 - 2 days earlier",
                                                          "> 2 days earlier",
                                                          "No significant trend"))) %>%
  group_by(Sub_Basin) %>%
  mutate(n_stations = n()) %>%
  group_by(Sub_Basin, magnitude_fixed) %>%
  summarise(n = n(),
            n_stations = unique(n_stations),
            region = unique(Major_Basin)) %>%
  mutate(percent = n/n_stations)


# Try plotting each major basin "separately and combining in facet grid
coast = freshet_bar %>%
  filter(region == "Coastal")

coast.map = major_basins %>%
  ggplot() +
  labs(title = "Coastal") +
  theme_void() +
  geom_sf(fill = NA) +
  geom_sf(data = major_basins %>%
            filter(Major_Basin == "Coastal"), fill = "yellow")

p1 = coast  %>%
  ggplot() +
  # ggtitle("Change in Mean freshet Flow") +
  geom_col(aes(x = fct_reorder(Sub_Basin, region) , y = n, fill = magnitude_fixed), col = "black", linewidth = 0.1, show.legend=TRUE) +
  scale_fill_manual(name = "Change per decade",
                    values = colour.scale.date,
                    drop = FALSE) +
  xlab("") +
  ylab("Number of stations") +
  coord_flip() +
  theme_classic() +
  theme(legend.position = "none",
        plot.margin = unit(c(0,0,-2,0), "lines")) +
  ylim(0, max(freshet_bar$n_stations) + 10) +
  annotation_custom(grob = ggplotGrob(coast.map), ymin = max(freshet_bar$n_stations) + 2, ymax = max(freshet_bar$n_stations) + 10)

p1

fraser = freshet_bar %>%
  filter(region == "Fraser")

fraser.map = major_basins %>%
  ggplot() +
  labs(title = "Fraser") +
  theme_void() +
  geom_sf(fill = NA) +
  geom_sf(data = major_basins %>%
            filter(Major_Basin == "Fraser"), fill = "yellow")

p2 = fraser  %>%
  ggplot() +
  # ggtitle("Change in Mean freshet Flow") +
  geom_col(aes(x = Sub_Basin , y = n, fill = magnitude_fixed), col = "black", linewidth = 0.1, show.legend=TRUE) +
  scale_fill_manual(name = "Change per decade",
                    values = colour.scale.date,
                    drop = FALSE) +  xlab("") +
  ylab("Number of stations") +
  coord_flip() +
  theme_classic() +
  theme(legend.position = "none",
        plot.margin = unit(c(-1,-1,-1,-1), "lines")) +
  theme(axis.line.x = element_blank(),
        axis.text.x = element_blank(),
        axis.title.x = element_blank(),
        axis.ticks.x = element_blank()) +
  ylim(0, max(freshet_bar$n_stations) + 10) +
  annotation_custom(grob = ggplotGrob(fraser.map), ymin = max(freshet_bar$n_stations) + 2, ymax = max(freshet_bar$n_stations) + 10)

p2

Columbia = freshet_bar %>%
  filter(region == "Columbia")

Columbia.map = major_basins %>%
  ggplot() +
  labs(title = "Columbia") +
  theme_void() +
  geom_sf(fill = NA) +
  geom_sf(data = major_basins %>%
            filter(Major_Basin == "Columbia"), fill = "yellow")

p3 = Columbia  %>%
  ggplot() +
  # ggtitle("Change in Mean freshet Flow") +
  geom_col(aes(x = fct_reorder(Sub_Basin, region) , y = n, fill = magnitude_fixed), col = "black", linewidth = 0.1, show.legend=TRUE) +
  scale_fill_manual(name = "Change per decade",
                    values = colour.scale.date,
                    drop = FALSE) +  xlab("") +
  ylab("Number of stations") +
  coord_flip() +
  theme_classic() +
  theme(legend.position = "none",
        plot.margin = unit(c(-1,-1,-1,-1), "lines")) +
  theme(axis.line.x = element_blank(),
        axis.text.x = element_blank(),
        axis.title.x = element_blank(),
        axis.ticks.x = element_blank()) +
  ylim(0, max(freshet_bar$n_stations) + 10) +
  annotation_custom(grob = ggplotGrob(Columbia.map), ymin = max(freshet_bar$n_stations) + 2, ymax = max(freshet_bar$n_stations) + 10)

p3

Liard = freshet_bar %>%
  filter(region == "Liard")

Liard.map = major_basins %>%
  ggplot() +
  labs(title = "Liard") +
  theme_void() +
  geom_sf(fill = NA) +
  geom_sf(data = major_basins %>%
            filter(Major_Basin == "Liard"), fill = "yellow")

p4 = Liard  %>%
  ggplot() +
  # ggtitle("Change in Mean freshet Flow") +
  geom_col(aes(x = fct_reorder(Sub_Basin, region) , y = n, fill = magnitude_fixed), col = "black", linewidth = 0.1, show.legend=TRUE) +
  scale_fill_manual(name = "Change per decade",
                    values = colour.scale.date,
                    drop = FALSE) +  xlab("") +
  ylab("Number of stations") +
  coord_flip() +
  theme_classic() +
  theme(legend.position = "none",
        plot.margin = unit(c(-1,-1,-1,-1), "lines")) +
  theme(axis.line.x = element_blank(),
        axis.text.x = element_blank(),
        axis.title.x = element_blank(),
        axis.ticks.x = element_blank()) +
  ylim(0, max(freshet_bar$n_stations) + 10) +
  annotation_custom(grob = ggplotGrob(Liard.map), ymin = max(freshet_bar$n_stations) + 2, ymax = max(freshet_bar$n_stations) + 10)

p4


Northwest = freshet_bar %>%
  filter(region == "Northwest")

Northwest.map = major_basins %>%
  ggplot() +
  labs(title = "North-West") +
  theme_void() +
  geom_sf(fill = NA) +
  geom_sf(data = major_basins %>%
            filter(Major_Basin == "Northwest"), fill = "yellow")

p5 = Northwest  %>%
  ggplot() +
  # ggtitle("Change in Mean freshet Flow") +
  geom_col(aes(x = fct_reorder(Sub_Basin, region) , y = n, fill = magnitude_fixed), col = "black", linewidth = 0.1, show.legend=TRUE) +
  scale_fill_manual(name = "Change per decade",
                    values = colour.scale.date,
                    drop = FALSE) +  xlab("") +
  ylab("Number of stations") +
  coord_flip() +
  theme_classic() +
  theme(legend.position = "none",
        plot.margin = unit(c(-1,-1,-1,-1), "lines")) +
  theme(axis.line.x = element_blank(),
        axis.text.x = element_blank(),
        axis.title.x = element_blank(),
        axis.ticks.x = element_blank()) +
  ylim(0, max(freshet_bar$n_stations) + 10) +
  annotation_custom(grob = ggplotGrob(Northwest.map), ymin = max(freshet_bar$n_stations) + 2, ymax = max(freshet_bar$n_stations) + 10)

p5

Peace = freshet_bar %>%
  filter(region == "Peace")

Peace.map = major_basins %>%
  ggplot() +
  labs(title = "Peace") +
  theme_void() +
  geom_sf(fill = NA) +
  geom_sf(data = major_basins %>%
            filter(Major_Basin == "Peace"), fill = "yellow")

p6 = Peace  %>%
  ggplot() +
  # ggtitle("Change in Mean freshet Flow") +
  geom_col(aes(x = fct_reorder(Sub_Basin, region) , y = n, fill = magnitude_fixed), col = "black", linewidth = 0.1, show.legend=TRUE) +
  scale_fill_manual(name = "Change per decade",
                    values = colour.scale.date,
                    drop = FALSE) +  xlab("") +
  ylab("Number of stations") +
  coord_flip() +
  theme_classic() +
  theme(legend.position = "none",
        plot.margin = unit(c(-1,-1,-1,-1), "lines")) +
  theme(axis.line.x = element_blank(),
        axis.text.x = element_blank(),
        axis.title.x = element_blank(),
        axis.ticks.x = element_blank()) +
  ylim(0, max(freshet_bar$n_stations) + 10) +
  annotation_custom(grob = ggplotGrob(Peace.map), ymin = max(freshet_bar$n_stations) + 2, ymax = max(freshet_bar$n_stations) + 10)

p6

plot_list[["Liard_timing_freshet"]] <- Liard  %>%
                                              ggplot() +
                                              geom_col(aes(x = fct_reorder(Sub_Basin, region) , y = n, fill = magnitude_fixed), col = "black", linewidth = 0.1, show.legend=TRUE) +
                                              scale_fill_manual(name = "Change per decade",
                                                                values = colour.scale.date,
                                                                drop = FALSE) +  xlab("") +
                                              ylab("Number of stations") +
                                              coord_flip() +
                                              theme_classic()
plot_list[["Columbia_timing_freshet"]] <- Columbia  %>%
                                                    ggplot() +
                                                    geom_col(aes(x = fct_reorder(Sub_Basin, region) , y = n, fill = magnitude_fixed), col = "black", linewidth = 0.1, show.legend=TRUE) +
                                                    scale_fill_manual(name = "Change per decade",
                                                                      values = colour.scale.date,
                                                                      drop = FALSE) +  xlab("") +
                                                    ylab("Number of stations") +
                                                    coord_flip() +
                                                    theme_classic()
plot_list[["Fraser_timing_freshet"]] <- fraser  %>%
                                                ggplot() +
                                                geom_col(aes(x = Sub_Basin , y = n, fill = magnitude_fixed), col = "black", linewidth = 0.1, show.legend=TRUE) +
                                                scale_fill_manual(name = "Change per decade",
                                                                  values = colour.scale.date,
                                                                  drop = FALSE) +  xlab("") +
                                                ylab("Number of stations") +
                                                coord_flip() +
                                                theme_classic()
plot_list[["Peace_timing_freshet"]] <- Peace  %>%
                                              ggplot() +
                                              geom_col(aes(x = fct_reorder(Sub_Basin, region) , y = n, fill = magnitude_fixed), col = "black", linewidth = 0.1, show.legend=TRUE) +
                                              scale_fill_manual(name = "Change per decade",
                                                                values = colour.scale.date,
                                                                drop = FALSE) +  xlab("") +
                                       ylab("Number of stations") +
                                       coord_flip() +
                                       theme_classic()
plot_list[["Northwest_timing_freshet"]] <- Northwest  %>%
                                                      ggplot() +
                                                      geom_col(aes(x = fct_reorder(Sub_Basin, region) , y = n, fill = magnitude_fixed), col = "black", linewidth = 0.1, show.legend=TRUE) +
                                                      scale_fill_manual(name = "Change per decade",
                                                                        values = colour.scale.date,
                                                                        drop = FALSE) +  xlab("") +
                                                      ylab("Number of stations") +
                                                      coord_flip() +
                                                      theme_classic()
plot_list[["Coastal_timing_freshet"]] <- coast  %>%
                                                ggplot() +
                                                geom_col(aes(x = fct_reorder(Sub_Basin, region) , y = n, fill = magnitude_fixed), col = "black", linewidth = 0.1, show.legend=TRUE) +
                                                scale_fill_manual(name = "Change per decade",
                                                                  values = colour.scale.date,
                                                                  drop = FALSE) +
                                                xlab("") +
                                                ylab("Number of stations") +
                                                coord_flip() +
                                                theme_classic()

freshet_bar_plot = ggarrange(p4, p3, p2, p6, p5, p1, ncol = 1,
                            common.legend = TRUE,
                            legend = "bottom",
                            # align = "v",
                            align = "hv")

freshet_bar_plot

svg_px("./print_ver/out/figs/freshet_bar.svg", width = 600, height = 600)
plot(freshet_bar_plot)
dev.off()

# Start of Low Flow Period ==================================================================

date_low_bar = mk_date_low %>%
  mutate(magnitude_fixed = factor(case_when(significant == 0.1 ~ "No significant trend",
                                            .default = magnitude_fixed),
                                  levels = c("> 2 days later",
                                             "1 - 2 days later",
                                             "< 1 days change",
                                             "1 - 2 days earlier",
                                             "> 2 days earlier",
                                             "No significant trend"))) %>%
  mutate(magnitude_fixed = fct_relevel(magnitude_fixed, c("> 2 days later",
                                                          "1 - 2 days later",
                                                          "< 1 days change",
                                                          "1 - 2 days earlier",
                                                          "> 2 days earlier",
                                                          "No significant trend"))) %>%
  group_by(Sub_Basin) %>%
  mutate(n_stations = n()) %>%
  group_by(Sub_Basin, magnitude_fixed) %>%
  summarise(n = n(),
            n_stations = unique(n_stations),
            region = unique(Major_Basin)) %>%
  mutate(percent = n/n_stations)


# Try plotting each major basin "separately and combining in facet grid
coast = date_low_bar %>%
  filter(region == "Coastal")

coast.map = major_basins %>%
  ggplot() +
  labs(title = "Coastal") +
  theme_void() +
  geom_sf(fill = NA) +
  geom_sf(data = major_basins %>%
            filter(Major_Basin == "Coastal"), fill = "yellow")

p1 = coast  %>%
  ggplot() +
  # ggtitle("Change in Mean date_low Flow") +
  geom_col(aes(x = fct_reorder(Sub_Basin, region) , y = n, fill = magnitude_fixed), col = "black", linewidth = 0.1, show.legend=TRUE) +
  scale_fill_manual(name = "Change per decade",
                    values = colour.scale.date,
                    drop = FALSE) +  xlab("") +
  ylab("Number of stations") +
  coord_flip() +
  theme_classic() +
  theme(legend.position = "none",
        plot.margin = unit(c(0,0,-2,0), "lines")) +
  ylim(0, max(date_low_bar$n_stations) + 10) +
  annotation_custom(grob = ggplotGrob(coast.map), ymin = max(date_low_bar$n_stations) + 2, ymax = max(date_low_bar$n_stations) + 10)

p1

fraser = date_low_bar %>%
  filter(region == "Fraser")

fraser.map = major_basins %>%
  ggplot() +
  labs(title = "Fraser") +
  theme_void() +
  geom_sf(fill = NA) +
  geom_sf(data = major_basins %>%
            filter(Major_Basin == "Fraser"), fill = "yellow")

p2 = fraser  %>%
  ggplot() +
  # ggtitle("Change in Mean date_low Flow") +
  geom_col(aes(x = fct_reorder(Sub_Basin, region) , y = n, fill = magnitude_fixed), col = "black", linewidth = 0.1, show.legend=TRUE) +
  scale_fill_manual(name = "Change per decade",
                    values = colour.scale.date,
                    drop = FALSE) +  xlab("") +
  ylab("Number of stations") +
  coord_flip() +
  theme_classic() +
  theme(legend.position = "none",
        plot.margin = unit(c(-1,-1,-1,-1), "lines")) +
  theme(axis.line.x = element_blank(),
        axis.text.x = element_blank(),
        axis.title.x = element_blank(),
        axis.ticks.x = element_blank()) +
  ylim(0, max(date_low_bar$n_stations) + 10) +
  annotation_custom(grob = ggplotGrob(fraser.map), ymin = max(date_low_bar$n_stations) + 2, ymax = max(date_low_bar$n_stations) + 10)

p2

Columbia = date_low_bar %>%
  filter(region == "Columbia")

Columbia.map = major_basins %>%
  ggplot() +
  labs(title = "Columbia") +
  theme_void() +
  geom_sf(fill = NA) +
  geom_sf(data = major_basins %>%
            filter(Major_Basin == "Columbia"), fill = "yellow")

p3 = Columbia  %>%
  ggplot() +
  # ggtitle("Change in Mean date_low Flow") +
  geom_col(aes(x = fct_reorder(Sub_Basin, region) , y = n, fill = magnitude_fixed), col = "black", linewidth = 0.1, show.legend=TRUE) +
  scale_fill_manual(name = "Change per decade",
                    values = colour.scale.date,
                    drop = FALSE) +  xlab("") +
  ylab("Number of stations") +
  coord_flip() +
  theme_classic() +
  theme(legend.position = "none",
        plot.margin = unit(c(-1,-1,-1,-1), "lines")) +
  theme(axis.line.x = element_blank(),
        axis.text.x = element_blank(),
        axis.title.x = element_blank(),
        axis.ticks.x = element_blank()) +
  ylim(0, max(date_low_bar$n_stations) + 10) +
  annotation_custom(grob = ggplotGrob(Columbia.map), ymin = max(date_low_bar$n_stations) + 2, ymax = max(date_low_bar$n_stations) + 10)

p3

Liard = date_low_bar %>%
  filter(region == "Liard")

Liard.map = major_basins %>%
  ggplot() +
  labs(title = "Liard") +
  theme_void() +
  geom_sf(fill = NA) +
  geom_sf(data = major_basins %>%
            filter(Major_Basin == "Liard"), fill = "yellow")

p4 = Liard  %>%
  ggplot() +
  # ggtitle("Change in Mean date_low Flow") +
  geom_col(aes(x = fct_reorder(Sub_Basin, region) , y = n, fill = magnitude_fixed), col = "black", linewidth = 0.1, show.legend=TRUE) +
  scale_fill_manual(name = "Change per decade",
                    values = colour.scale.date,
                    drop = FALSE) +  xlab("") +
  ylab("Number of stations") +
  coord_flip() +
  theme_classic() +
  theme(legend.position = "none",
        plot.margin = unit(c(-1,-1,-1,-1), "lines")) +
  theme(axis.line.x = element_blank(),
        axis.text.x = element_blank(),
        axis.title.x = element_blank(),
        axis.ticks.x = element_blank()) +
  ylim(0, max(date_low_bar$n_stations) + 10) +
  annotation_custom(grob = ggplotGrob(Liard.map), ymin = max(date_low_bar$n_stations) + 2, ymax = max(date_low_bar$n_stations) + 10)

p4


Northwest = date_low_bar %>%
  filter(region == "Northwest")

Northwest.map = major_basins %>%
  ggplot() +
  labs(title = "North-West") +
  theme_void() +
  geom_sf(fill = NA) +
  geom_sf(data = major_basins %>%
            filter(Major_Basin == "Northwest"), fill = "yellow")

p5 = Northwest  %>%
  ggplot() +
  # ggtitle("Change in Mean date_low Flow") +
  geom_col(aes(x = fct_reorder(Sub_Basin, region) , y = n, fill = magnitude_fixed), col = "black", linewidth = 0.1, show.legend=TRUE) +
  scale_fill_manual(name = "Change per decade",
                    values = colour.scale.date,
                    drop = FALSE) +  xlab("") +
  ylab("Number of stations") +
  coord_flip() +
  theme_classic() +
  theme(legend.position = "none",
        plot.margin = unit(c(-1,-1,-1,-1), "lines")) +
  theme(axis.line.x = element_blank(),
        axis.text.x = element_blank(),
        axis.title.x = element_blank(),
        axis.ticks.x = element_blank()) +
  ylim(0, max(date_low_bar$n_stations) + 10) +
  annotation_custom(grob = ggplotGrob(Northwest.map), ymin = max(date_low_bar$n_stations) + 2, ymax = max(date_low_bar$n_stations) + 10)

p5

Peace = date_low_bar %>%
  filter(region == "Peace")

Peace.map = major_basins %>%
  ggplot() +
  labs(title = "Peace") +
  theme_void() +
  geom_sf(fill = NA) +
  geom_sf(data = major_basins %>%
            filter(Major_Basin == "Peace"), fill = "yellow")

p6 = Peace  %>%
  ggplot() +
  # ggtitle("Change in Mean date_low Flow") +
  geom_col(aes(x = fct_reorder(Sub_Basin, region) , y = n, fill = magnitude_fixed), col = "black", linewidth = 0.1, show.legend=TRUE) +
  scale_fill_manual(name = "Change per decade",
                    values = colour.scale.date,
                    drop = FALSE) +  xlab("") +
  ylab("Number of stations") +
  coord_flip() +
  theme_classic() +
  theme(legend.position = "none",
        plot.margin = unit(c(-1,-1,-1,-1), "lines")) +
  theme(axis.line.x = element_blank(),
        axis.text.x = element_blank(),
        axis.title.x = element_blank(),
        axis.ticks.x = element_blank()) +
  ylim(0, max(date_low_bar$n_stations) + 10) +
  annotation_custom(grob = ggplotGrob(Peace.map), ymin = max(date_low_bar$n_stations) + 2, ymax = max(date_low_bar$n_stations) + 10)

p6

plot_list[["Liard_timing_low_start"]] <- Liard  %>%
                                                ggplot() +
                                                geom_col(aes(x = fct_reorder(Sub_Basin, region) , y = n, fill = magnitude_fixed), col = "black", linewidth = 0.1, show.legend=TRUE) +
                                                scale_fill_manual(name = "Change per decade",
                                                                  values = colour.scale.date,
                                                                  drop = FALSE) +  xlab("") +
                                                ylab("Number of stations") +
                                                coord_flip() +
                                                theme_classic()
plot_list[["Columbia_timing_low_start"]] <- Columbia  %>%
                                                      ggplot() +
                                                      geom_col(aes(x = fct_reorder(Sub_Basin, region) , y = n, fill = magnitude_fixed), col = "black", linewidth = 0.1, show.legend=TRUE) +
                                                      scale_fill_manual(name = "Change per decade",
                                                                        values = colour.scale.date,
                                                                        drop = FALSE) +  xlab("") +
                                                      ylab("Number of stations") +
                                                      coord_flip() +
                                                      theme_classic()
plot_list[["Fraser_timing_low_start"]] <- fraser  %>%
                                                  ggplot() +
                                                  geom_col(aes(x = fct_reorder(Sub_Basin, region) , y = n, fill = magnitude_fixed), col = "black", linewidth = 0.1, show.legend=TRUE) +
                                                  scale_fill_manual(name = "Change per decade",
                                                                    values = colour.scale.date,
                                                                    drop = FALSE) +  xlab("") +
                                                  ylab("Number of stations") +
                                                  coord_flip() +
                                                  theme_classic()
plot_list[["Peace_timing_low_start"]] <- Peace  %>%
                                                ggplot() +
                                                geom_col(aes(x = fct_reorder(Sub_Basin, region) , y = n, fill = magnitude_fixed), col = "black", linewidth = 0.1, show.legend=TRUE) +
                                                scale_fill_manual(name = "Change per decade",
                                                                  values = colour.scale.date,
                                                                  drop = FALSE) +  xlab("") +
                                                ylab("Number of stations") +
                                                coord_flip() +
                                                theme_classic()
plot_list[["Northwest_timing_low_start"]] <- Northwest  %>%
                                                        ggplot() +
                                                        geom_col(aes(x = fct_reorder(Sub_Basin, region) , y = n, fill = magnitude_fixed), col = "black", linewidth = 0.1, show.legend=TRUE) +
                                                        scale_fill_manual(name = "Change per decade",
                                                                          values = colour.scale.date,
                                                                          drop = FALSE) +  xlab("") +
                                                        ylab("Number of stations") +
                                                        coord_flip() +
                                                        theme_classic()
plot_list[["Coastal_timing_low_start"]] <- coast  %>%
                                                  ggplot() +
                                                  geom_col(aes(x = fct_reorder(Sub_Basin, region) , y = n, fill = magnitude_fixed), col = "black", linewidth = 0.1, show.legend=TRUE) +
                                                  scale_fill_manual(name = "Change per decade",
                                                                    values = colour.scale.date,
                                                                    drop = FALSE) +  xlab("") +
                                                  ylab("Number of stations") +
                                                  coord_flip() +
                                                  theme_classic()


date_low_bar_plot = ggarrange(p4, p3, p2, p6, p5, p1, ncol = 1,
                            common.legend = TRUE,
                            legend = "bottom",
                            # align = "v",
                            align = "hv")

date_low_bar_plot

svg_px("./print_ver/out/figs/date_low_bar.svg", width = 600, height = 600)
plot(date_low_bar_plot)
dev.off()

## MONTHLY BREAKDOWN ================================================================================

# subset to filtered stations
monthly_flow_dat = monthly_flow_dat %>%
  filter(STATION_NUMBER %in% stations_filt$STATION_NUMBER)

# Average monthly flow
mk_average_monthly = unique(monthly_flow_dat$Month) %>%
  map ( ~ {
    month_dat = monthly_flow_dat %>%
      filter(Month == .x) %>%
      calculate_MK_results(chosen_variable = "Average") %>%
      mutate(Month = .x,
             metric = "Average")

      }) %>%
  bind_rows()

# bar plot and save
monthly_average_bar_plot = mk_average_monthly %>%
  mutate(magnitude_fixed = factor(case_when(significant == 0.1 ~ "No significant trend",
                                     .default = magnitude_fixed),
                                   levels = c("> 5% increase",
                                             "1 - 5% increase",
                                             "< 1% change",
                                             "1 - 5% decrease",
                                             "> 5% decrease",
                                             "No significant trend"))) %>%
  mutate(magnitude_fixed = fct_relevel(magnitude_fixed, c("> 5% increase",
                                                         "1 - 5% increase",
                                                         "< 1% change",
                                                         "1 - 5% decrease",
                                                         "> 5% decrease",
                                                         "No significant trend")),
         Month = factor(month.name[match(Month,month.abb)], levels = month.name)) %>%
  mutate(Month = fct_relevel(Month, "October",
                             "November",
                             "December",
                             "January",
                             "February",
                             "March",
                             "April",
                             "May",
                             "June",
                             "July",
                             "August",
                             "September")) %>%
  group_by(magnitude_fixed, Month) %>%
  summarise(n = n()) %>%
  ggplot() +
  # ggtitle("Monthly Median River Flow")+
  geom_col(aes(x = Month, y = n, fill = magnitude_fixed), col = "black", show.legend=TRUE) +
  scale_fill_manual(name = "Change per decade",
                    values = c("#CC79A7",
                               "#D55E00",
                               "#0072B2",
                               "#009E73",
                               "#ff0000",
                               "grey"),
                    labels = c("> 5% increase",
                               "1 - 5% increase",
                               "< 1% change",
                               "1 - 5% decrease",
                               "> 5% decrease",
                               "No significant trend"),
                    drop = FALSE) +
  ylab("Number of Stations") +
  xlab("") +
  theme_classic() +
  theme(legend.position = "bottom")  +
  guides(fill = guide_legend(nrow = 1))



svg_px("./print_ver/out/figs/monthly_average_bar_plot.svg", width = 800, height = 600)
plot(monthly_average_bar_plot)
dev.off()


# Monthly low flow
mk_low_flow_monthly = unique(monthly_flow_dat$Month) %>%
  map ( ~ {
    month_dat = monthly_flow_dat %>%
      filter(Month == .x) %>%
      calculate_MK_results(chosen_variable = "Min_7_Day") %>%
      mutate(Month = .x,
             metric = "Low Flow")

  }) %>%
  bind_rows()

# bar plot and save
monthly_low_flow_bar_plot = mk_low_flow_monthly %>%
  mutate(magnitude_fixed = factor(case_when(significant == 0.1 ~ "No significant trend",
                                            .default = magnitude_fixed),
                                  levels = c("> 5% increase",
                                             "1 - 5% increase",
                                             "< 1% change",
                                             "1 - 5% decrease",
                                             "> 5% decrease",
                                             "No significant trend"))) %>%
  mutate(magnitude_fixed = fct_relevel(magnitude_fixed, c("> 5% increase",
                                                          "1 - 5% increase",
                                                          "< 1% change",
                                                          "1 - 5% decrease",
                                                          "> 5% decrease",
                                                          "No significant trend")),
         Month = factor(month.name[match(Month,month.abb)], levels = month.name)) %>%
  mutate(Month = fct_relevel(Month,
                             "April",
                             "May",
                             "June",
                             "July",
                             "August",
                             "September",
                             "October",
                             "November",
                             "December",
                             "January",
                             "February",
                             "March")) %>%
  group_by(magnitude_fixed, Month) %>%
  summarise(n = n()) %>%
  ggplot() +
  # ggtitle("Monthly Low Flow")+
  geom_col(aes(x = Month, y = n, fill = magnitude_fixed), col = "black", show.legend = TRUE) +
  scale_fill_manual(name = "Change per decade",
                    values = colour.scale,
                    drop = FALSE) +
  ylab("Number of Stations") +
  xlab("") +
  theme_classic() +
  theme(legend.position = "bottom") +
  guides(fill = guide_legend(nrow = 1))


svg_px("./print_ver/out/figs/monthly_low_flow_bar_plot.svg", width = 800, height = 600)
plot(monthly_low_flow_bar_plot)
dev.off()

# Monthly peak flow
mk_peak_flow_monthly = unique(monthly_flow_dat$Month) %>%
  map ( ~ {
    month_dat = monthly_flow_dat %>%
      filter(Month == .x) %>%
      calculate_MK_results(chosen_variable = "Max_3_Day") %>%
      mutate(Month = .x,
             metric = "Peak Flow")

  }) %>%
  bind_rows()

# bar plot and save
monthly_peak_flow_bar_plot = mk_peak_flow_monthly %>%
  mutate(magnitude_fixed = factor(case_when(significant == 0.1 ~ "No significant trend",
                                            .default = magnitude_fixed),
                                  levels = c("> 5% increase",
                                             "1 - 5% increase",
                                             "< 1% change",
                                             "1 - 5% decrease",
                                             "> 5% decrease",
                                             "No significant trend"))) %>%
  mutate(magnitude_fixed = fct_relevel(magnitude_fixed, c("> 5% increase",
                                                          "1 - 5% increase",
                                                          "< 1% change",
                                                          "1 - 5% decrease",
                                                          "> 5% decrease",
                                                          "No significant trend")),
         Month = factor(month.name[match(Month,month.abb)], levels = month.name))%>%
  mutate(Month = fct_relevel(Month, "October",
                             "November",
                             "December",
                             "January",
                             "February",
                             "March",
                             "April",
                             "May",
                             "June",
                             "July",
                             "August",
                             "September")) %>%
  group_by(magnitude_fixed, Month) %>%
  summarise(n = n()) %>%
  ggplot() +
  # ggtitle("Monthly Low Flow")+
  geom_col(aes(x = Month, y = n, fill = magnitude_fixed), col = "black", show.legend = TRUE) +
  scale_fill_manual(name = "Change per decade",
                    values = colour.scale,
                    drop = FALSE) +
  ylab("Number of Stations") +
  xlab("") +
  theme_classic() +
  theme(legend.position = "bottom") +
  guides(fill = guide_legend(nrow = 1))

plot_list[["All_volume_month_avg_month"]] <- monthly_average_bar_plot + theme(legend.position = "right", axis.text.x = element_text(angle = 45, hjust = 1)) + guides(fill = guide_legend(nrow = 6))
plot_list[["All_volume_month_low_month"]] <- monthly_low_flow_bar_plot + theme(legend.position = "right", axis.text.x = element_text(angle = 45, hjust = 1)) + guides(fill = guide_legend(nrow = 6))
plot_list[["All_volume_month_peak_month"]] <- monthly_peak_flow_bar_plot + theme(legend.position = "right", axis.text.x = element_text(angle = 45, hjust = 1)) + guides(fill = guide_legend(nrow = 6))


svg_px("./print_ver/out/figs/monthly_peak_flow_bar_plot.svg", width = 800, height = 600)
plot(monthly_peak_flow_bar_plot)
dev.off()

# Create table of MK results =================================================================

mk_results_monthly_tbl = bind_rows(mk_average_monthly,
                                   mk_low_flow_monthly,
                                   mk_peak_flow_monthly)

# Save all pertinent plots in single Rdata file
save(mk_all, annual_bar_plot, low_bar_plot, peak_bar_plot, freshet_bar_plot, date_low_bar_plot,
     monthly_average_bar_plot, monthly_low_flow_bar_plot, monthly_peak_flow_bar_plot, timing_regime,
     volume_regime, file = "print_ver/out/figures.RData")

saveRDS(mk_results_tbl, 'print_ver/out/mk_results_tbl.rds')

saveRDS(mk_results_monthly_tbl, 'print_ver/out/mk_results_monthly_tbl.rds')

saveRDS(plot_list, 'app_bar_plots/www/plot_list.rds')

plot_list <- readRDS("app_bar_plots/www/plot_list.rds")
out_dir <- "app_bar_plots/www/plot_pngs"
dir.create(out_dir, showWarnings = FALSE, recursive = TRUE)

for (id in names(plot_list)) {
  p <- plot_list[[id]]
  outfile <- file.path(out_dir, paste0(id, ".png"))
  # skip if already exists
  if (file.exists(outfile)) next
  if (inherits(p, "ggplot")) {
    tryCatch({
      ggplot2::ggsave(filename = outfile, plot = p, width = 10, height = 6, units = "in", dpi = 150)
    }, error = function(e) {
      message("Failed to save: ", id, " -> ", conditionMessage(e))
    })
  } else {
    # for non-ggplot, try to print to file
    tryCatch({
      png(outfile, width = 1000, height = 600, res = 150)
      if (inherits(p, "recordedplot")) replayPlot(p) else print(p)
      dev.off()
    }, error = function(e) {
      try(if (dev.cur() > 1) dev.off(), silent = TRUE)
      message("Failed to render non-ggplot: ", id, " -> ", conditionMessage(e))
    })
  }
}

# Appendices ==================================================================================

# Individual station maps ===============================================================
stations_filt = stations_filt %>%
  mutate(lon = st_coordinates(.)[,1],
         lat = st_coordinates(.)[,2])

stationMaps = vector("list", length(unique(stations_filt$STATION_NUMBER)))
names(stationMaps) = stations_filt %>%
  arrange(Regime, Sub_Basin) %>%
  distinct(STATION_NUMBER) %>%
  pull(STATION_NUMBER)

for(s in names(stationMaps)) {
  station = filter(stations_filt, STATION_NUMBER == s)
  stationMaps[[s]] = leaflet(options =
                               leafletOptions(zoomControl = FALSE)) %>%
    addProviderTiles("OpenStreetMap") %>%
    setView(lng = station$lon, lat = station$lat,
            zoom = 9) %>%
    addCircleMarkers(lng = station$lon, lat = station$lat,
                     fillColor = mypal(station$Regime),
                     opacity = 1,
                     fillOpacity = 1,
                     color = "black",
                     radius = 10,
                     weight = 2,
                     label = station$STATION_NAME,
                     labelOptions = labelOptions(noHide = T)) %>%
    addLegend(position = "topright",
              pal = mypal,
              values = station$Regime)

  library(mapview)
  ifelse(!dir.exists(file.path("print_ver","out", "figs","maps")), dir.create(file.path("print_ver","out", "figs","maps")), FALSE)
  stationMaps[[s]] %>%
    mapview::mapshot(file = paste0("./print_ver/out/figs/maps/",station$STATION_NAME,".png"))
  print(s)
}

# Individual hydrograph plots ==============================================================
for(s in names(stationMaps)) {
  station_name = unique(hydrograph_dat[hydrograph_dat$STATION_NUMBER == s,]$STATION_NAME)

  plotting_df = hydrograph_dat %>%
  ungroup() |>
  filter(STATION_NUMBER == s) %>%
  # Convert from calendar year to 'water year'
  mutate(month_label = factor(Month, levels = c(month.abb[10:12],month.abb[1:9]))) %>%
  arrange(month_label) |>
  # Add labels for the ribbons we'll add to the figure.
  mutate(median_line_label = 'Median Flow') %>%
  mutate(fifty_pct_label = '"Normal" range (50%) of flow') %>%
  mutate(ninety_pct_label = 'Range of 90% of flow')

p = plotting_df %>%
  ggplot() +
  geom_ribbon(aes(x = as.numeric(month_label), ymin = five_perc, ymax = ninetyfive_perc, fill = ninety_pct_label)) +
  geom_ribbon(aes(x = as.numeric(month_label), ymin = twentyfive_perc, ymax = seventyfive_perc, fill = fifty_pct_label)) +
  geom_line(aes(x = as.numeric(month_label), y = median_flow, colour = median_line_label),
            linewidth = 1) +
  geom_hline(aes(yintercept = unique(MAD) * 0.5, linetype = "50% Mean Annual Discharge"), col = "red") +
  scale_colour_manual(values = c("Median Flow" = "#2d7ca1")) +
  scale_linetype_manual(name = "", values = 2, guide = guide_legend(override.aes = list(color = "red"))) +
  scale_fill_manual(values = c("Range of 90% of flow" = "#ceeaed",
                               '"Normal" range (50%) of flow' = 'lightblue')) +
  scale_x_continuous(breaks = c(1:12),
                     labels = plotting_df$month_label[c(1:12)]) +
  labs(y = 'Average Discharge (m<sup>3</sup>/s)',
       x = '',
       title = '*Daily Stream or River Discharge*',
       subtitle = station_name,
       col = '',
       fill = '') +
  theme(axis.title.y = element_markdown(size = 15),
        axis.text.y = element_text(size = 12),
        axis.text.x = element_text(size = 12),
        legend.position = 'top',
        legend.justification = 'left',
        legend.box="vertical",
        legend.box.just = "left",
        legend.margin = unit(0,"cm"),
        plot.title = element_markdown(hjust = 0),
        panel.background = element_rect(fill = 'transparent'),
        panel.grid.major = element_line(colour = 'grey'))

ifelse(!dir.exists(file.path("print_ver","out", "figs","hgraphs")), dir.create(file.path("print_ver","out", "figs","hgraphs")), FALSE)

png(paste0("./print_ver/out/figs/hgraphs/",s,".png"), width = 800, height = 600)
plot(p)
dev.off()
}

# Individual metric trend results ===============================================
for(s in names(stationMaps)) {
  dat = mk_results_tbl %>%
    filter(STATION_NUMBER == s) %>%
    filter(metric %in% c("Average Annual Flow", "Peak Flow", "Low Summer Flow")) %>%
    mutate(magnitude_fixed = factor(case_when(significant == 0.1 ~ "No significant trend",
                                              .default = magnitude_fixed),
                                    levels = c("> 5% increase",
                                               "1 - 5% increase",
                                               "< 1% change",
                                               "1 - 5% decrease",
                                               "> 5% decrease",
                                               "No significant trend"))) %>%
    mutate(magnitude_fixed = fct_relevel(magnitude_fixed, c("> 5% increase",
                                                            "1 - 5% increase",
                                                            "< 1% change",
                                                            "1 - 5% decrease",
                                                            "> 5% decrease",
                                                            "No significant trend")))

  min = -(max(abs(dat$per_change))) * 10
  max = max(abs(dat$per_change)) * 10


  p1 = ggplot(dat) +
    geom_col(aes(x = metric, y = per_change * 10, fill = magnitude_fixed),
             color = "black") +
    theme_classic() +
    ggtitle("Volume of flow events") +
  theme(axis.text = element_text(size = 16),
        axis.title.y = element_blank(),
        plot.title = element_text(size = 20, face = "bold"),
        legend.position = "top",
        legend.text = element_text(size = 16)) +
    scale_fill_manual(name = "Change per decade",
                                        values = c("#CC79A7",
                                                   "#D55E00",
                                                   "#0072B2",
                                                   "#009E73",
                                                   "#ff0000",
                                                   "grey"),
                                        labels = c("> 5% increase",
                                                   "1 - 5% increase",
                                                   "< 1% change",
                                                   "1 - 5% decrease",
                                                   "> 5% decrease",
                                                   "No significant trend"),
                                        drop = FALSE) +
    scale_y_continuous(name = "Percent change per decade",
                       labels = pretty_breaks(n = 6)(c(min,max)),
                       breaks = pretty_breaks(n = 6)(c(min,max)))+
    xlab("") +
    ylab("") +
    ylim(min,max)

  ifelse(!dir.exists(file.path("print_ver","out", "figs","volume_plots")), dir.create(file.path("print_ver","out", "figs","volume_plots")), FALSE)

  png(paste0("./print_ver/out/figs/volume_plots/",s,".png"), width = 800, height = 600)
  plot(p1)
  dev.off()
  print(s)

  dat2 = mk_results_tbl %>%
    filter(STATION_NUMBER == s) %>%
    filter(!metric %in% c("Average Annual Flow", "Peak Flow", "Low Summer Flow")) %>%
    mutate(magnitude_fixed = factor(case_when(significant == 0.1 ~ "No significant trend",
                                              .default = magnitude_fixed),
                                    levels = c("> 2 days later",
                                               "1 - 2 days later",
                                               "< 1 days change",
                                               "1 - 2 days earlier",
                                               "> 2 days earlier",
                                               "No significant trend"))) %>%
    mutate(magnitude_fixed = fct_relevel(magnitude_fixed, c("> 2 days later",
                                                            "1 - 2 days later",
                                                            "< 1 days change",
                                                            "1 - 2 days earlier",
                                                            "> 2 days earlier",
                                                            "No significant trend")))

  min = -(max(abs(dat2$change_timing))) * 10
  max = max(abs(dat2$change_timing)) * 10

  p2 = ggplot(dat2) +
    geom_col(aes(x = metric, y = change_timing * 10, fill = magnitude_fixed),
             color = "black") +
    theme_classic() +
    ggtitle("Timing of flow events") +
    theme(axis.text = element_text(size = 16),
          axis.title.y = element_blank(),
          plot.title = element_text(size = 20, face = "bold"),
          legend.position = "top",
          legend.text = element_text(size = 16)) +
    scale_fill_manual(name = "Change per decade",
                      values = c("#CC79A7",
                                 "#D55E00",
                                 "#0072B2",
                                 "#009E73",
                                 "#ff0000",
                                 "grey"),
                      labels = c("> 2 days later",
                                 "1 - 2 days later",
                                 "< 1 days change",
                                 "1 - 2 days earlier",
                                 "> 2 days earlier",
                                 "No significant trend"),
                      drop = FALSE) +
    scale_y_continuous(name = "Change per decade",
                       labels = pretty_breaks(n = 6)(c(min,max)),
                       breaks = pretty_breaks(n = 6)(c(min,max))) +
    xlab("") +
    ylab("") +
    ylim(min,max)


  ifelse(!dir.exists(file.path("print_ver","out", "figs","timing_plots")), dir.create(file.path("print_ver","out", "figs","timing_plots")), FALSE)

  png(paste0("./print_ver/out/figs/timing_plots/",s,".png"), width = 800, height = 600)
  plot(p2)
  dev.off()
  print(s)
}
