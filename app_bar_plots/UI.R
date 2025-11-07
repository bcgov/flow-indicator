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

library(bslib)
library(ggplot2)
library(grid)
library(lattice)
library(dplyr)

regions <- c("All", "Liard", "Columbia", "Fraser", "Peace", "Northwest", "Coastal")
metric_select_options_tab <- tagList(
  selectInput("region", "Basin", choices = regions, selected = "All"),
  selectInput("main_group", "Select Metric", choices = c("Flow Volume" = "volume", "Flow Timing" = "timing"), selected = "volume"),
  uiOutput("subgroup_ui"),
  uiOutput("detail_ui")
)

ui <- page_sidebar(
  title = NULL,
  sidebar = sidebar(
    wellPanel(metric_select_options_tab)
  ),
  navset_card_tab(
    nav_panel(
      NULL,
      div(style = "width:100%;",
          imageOutput("main_plot", height = "520px", width = "100%")
      )
    )
  )
)
