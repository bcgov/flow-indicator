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

source('UI.R')
source('functions.R')

library(bslib)
library(ggplot2)
library(grid)
library(lattice)
library(dplyr)

server <- function(input, output, session) {

  # load plot_list
  plot_list <- readRDS('www/plot_list.rds')

  # dynamic subgroup UI
  output$subgroup_ui <- renderUI({
    req(input$main_group, input$region)
    if (input$main_group == "volume") {
      if (input$region == "All") {
        selectInput("plot_group", "Select Plot Group", choices = c("Overall" = "overall", "By River" = "river", "By Month" = "month"), selected = "overall")
      } else {
        selectInput("plot_group", "Select Plot Group", choices = c("Overall" = "overall"), selected = "overall")
      }
    } else {
      if (input$region == "All") {
        selectInput("plot_group", "Select Plot Group", choices = c("Overall" = "overall", "By River" = "river"), selected = "overall")
      } else {
        selectInput("plot_group", "Select Plot Group", choices = c("Overall" = "overall"), selected = "overall")
      }
    }
  })

  # dynamic detail UI
  output$detail_ui <- renderUI({
    req(input$main_group, input$plot_group, input$region)
    if (input$region == "All") {
      if (input$main_group == "volume") {
        if (input$plot_group == "river") {
          selectInput("detail_group", "Select Volume Metric", choices = c("Mean Annual Flow" = "mean", "Peak Flow" = "peak", "Summer Low Flow" = "low"), selected = "mean")
        } else if (input$plot_group == "month") {
          selectInput("detail_group", "Select Monthly Metric", choices = c("Average monthly flows" = "avg_month", "Monthly low flows" = "low_month", "Monthly peak flows" = "peak_month"), selected = "avg_month")
        } else return(NULL)
      } else if (input$main_group == "timing") {
        if (input$plot_group == "river") {
          selectInput("detail_group", "Select Timing Metric", choices = c("Date of Freshet" = "freshet", "Start of Low Flow Period" = "low_start"), selected = "freshet")
        } else return(NULL)
      } else return(NULL)
    } else {
      if (input$main_group == "volume" && input$plot_group == "overall") {
        selectInput("detail_group", "Select Volume Metric", choices = c("Mean Annual Flow" = "mean", "Peak Flow" = "peak", "Summer Low Flow" = "low"), selected = "mean")
      } else if (input$main_group == "timing" && input$plot_group == "overall") {
        selectInput("detail_group", "Select Timing Metric", choices = c("Date of Freshet" = "freshet", "Start of Low Flow Period" = "low_start"), selected = "freshet")
      } else return(NULL)
    }
  })

  # main plot renderer
  output$main_plot <- renderImage({
    req(input$region, input$main_group, input$plot_group)
    idx <- get_plot_index(input$region, input$main_group, input$plot_group, input$detail_group)
    pngfile <- file.path("www/plot_pngs", paste0(idx, ".png"))
    if (file.exists(pngfile)) {
      list(src = pngfile, contentType = "image/png", width = session$clientData$output_main_plot_width, height = session$clientData$output_main_plot_height, alt = idx)
    } else {
      # fallback: return a small generated PNG with diagnostic text
      tmp <- tempfile(fileext = ".png")
      png(tmp, width = 800, height = 520, res = 150); plot.new(); text(0.5,0.5, paste("Missing plot:", idx)); dev.off()
      list(src = tmp, contentType = "image/png", width = 800, height = 520, alt = "missing")
    }
  }, deleteFile = FALSE)
  outputOptions(output, "main_plot", suspendWhenHidden = FALSE)
}

shinyApp(ui = ui, server = server)
