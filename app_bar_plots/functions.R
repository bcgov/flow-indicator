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

get_plot_index <- function(region, main_group, plot_group, detail_group) {
  if (is.null(region) || is.null(main_group) || is.null(plot_group)) return(NULL)
  region <- as.character(region); main_group <- as.character(main_group)
  plot_group <- as.character(plot_group); detail_group <- if (is.null(detail_group)) "" else as.character(detail_group)

  if (region == "All") {
    if (main_group == "volume") {
      if (plot_group == "overall") return(paste0(region, "_", main_group, "_", plot_group))
      if (plot_group == "river" && detail_group %in% c("mean","peak","low")) return(paste0(region, "_", main_group, "_", plot_group, "_", detail_group))
      if (plot_group == "month" && detail_group %in% c("avg_month","low_month","peak_month")) return(paste0(region, "_", main_group, "_", plot_group, "_", detail_group))
    } else if (main_group == "timing") {
      if (plot_group == "overall") return(paste0(region, "_", main_group, "_", plot_group))
      if (plot_group == "river" && detail_group %in% c("freshet","low_start")) return(paste0(region, "_", main_group, "_", plot_group, "_", detail_group))
    }
    return(NULL)
  } else {
    if (main_group == "volume" && plot_group == "overall" && detail_group %in% c("mean","peak","low")) return(paste0(region, "_", main_group, "_", detail_group))
    if (main_group == "timing" && plot_group == "overall" && detail_group %in% c("freshet","low_start")) return(paste0(region, "_", main_group, "_", detail_group))
    return(NULL)
  }
}
