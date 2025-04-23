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

ui = shiny::fluidPage(
  tags$head(tags$style(
    HTML('#trend_selector {opacity:0.9;}
         #trend_selector:hover{opacity:1;}'))),
  # Enables us to do some fancy things in javascript...
  useShinyjs(),
  # Include our own styling sheet; defined class 'my_home_button'
  includeCSS('www/bc_button.css'),

  tags$head(tags$style(
    HTML('#trend_selector {opacity:0.9;}
         #trend_selector:hover{opacity:0.9;}'))),
  titlePanel("Flow Indicator"),
  # Throw in our own action button, setting class to 'my_home_button'
  actionButton(
    'abs_button',
    '',
    class = 'my_bc_button'
  ),
  bsTooltip(id = "abs_button",
            title = "Reset Map"),
  titlePanel("Flow Indicator"),
  map_abs_panel,
  trend_select_abs_panel
)

server <- function(input, output, session) {

  source(file.path('Load_Filter_Data.R'), local = T)$value
  source(file.path('Render_UI_elements.R'), local = T)$value

  date_vars = c(
    # "Min_7_Day_summer_DoY",
    # "Max_3_Day_DoY",
    "DoY_50pct_TotalQ",
    # "DoY_90pct_TotalQ",
    'R2MAD_DoY')

  recent_stations = annual_flow_dat %>%
    group_by(STATION_NUMBER) %>%
    summarise(minYear = min(Year)) %>%
    filter(minYear>=1992) %>%
    pull(STATION_NUMBER)

  upstream_stations = stations_sf %>%
    filter(is.na(keep)) %>%
    pull(STATION_NUMBER)

  # Update month selector to show months, if user picks month time-scale
  observeEvent(input$time_scale, {
    if(input$time_scale == 'Monthly'){
      updateSelectizeInput(inputId = 'user_var_choice',
                           choices = c('Average Flow' = 'Average',
                                       "Low Flow" = "Min_7_Day")
      )
    }
    if(input$time_scale == 'Annual'){
      updateSelectizeInput(inputId = 'user_var_choice',
                           choices = c(
                             'Average Flow' = 'Average',
                             'Date of Freshet' = 'DoY_50pct_TotalQ',
                             'Low Summer Flow' = 'Min_7_Day_summer',
                             # 'Date of Low Summer Flow (7-day)' = 'Min_7_Day_summer_DoY',
                             # 'Date of 90% Annual Flow' = 'DoY_90pct_TotalQ',
                             'Date of Low Flow' = 'R2MAD_DoY',
                             'Peak Flow' = 'Max_3_Day'
                             # 'Date of Peak Flow (3-day)' = 'Max_3_Day_DoY'
                           )
      )
    }
  })

  #New BC Button
  observeEvent(input$abs_button, {
    zoom = input$leafmap_zoom
    lat = input$leafmap_center[2]
    lon = input$leafmap_center[1]

    # Change region_rv() to 'All'
    if(basin_rv() != 'All'| zoom != 5 | lat != 50 | lon != -130){
      basin_rv('All')

      # Update map to BC zoom (customizable)
      leafletProxy('leafmap') |>
        set_bc_view()

      updateSelectInput(session = session,
                        'basin_choice',
                        selected = 'All')
    }
  })

  #Reset button
  observeEvent(input$reset_inputs, {
    zoom = input$leafmap_zoom
    lat = input$leafmap_center[2]
    lon = input$leafmap_center[1]

    # Change region_rv() to 'All'
    if(basin_rv() != 'All'| zoom != 5 | lat != 50 | lon != -130){
      basin_rv('All')

      # Update map to BC zoom (customizable)
      leafletProxy('leafmap') |>
        set_bc_view()

      updateSelectInput(session = session,
                        'basin_choice',
                        selected = 'All')
    }
  })

  #Some reactives

  basin_rv = reactiveVal('All')

  # Basin reactives
  basin_drop_rv = reactive({
    input$basin_choice
  })

  observeEvent(input$basin_choice,{

    basin_rv(basin_drop_rv())

  })

  basin_click_rv = reactive({
    input$leafmap_shape_click$id
  })

  observeEvent(input$leafmap_shape_click$id,{
    updateSelectInput(session = session,
                      'basin_choice',
                      selected = input$leafmap_shape_click$id)
    basin_rv(basin_click_rv())
  })

  #Calculate trend results
  mk_results = reactive({
    calculate_MK_results(data = flow_dat_chosen_var(),
                         chosen_variable = input$user_var_choice)
  })

  flow_dat_with_mk = reactive({
    flow_dat_chosen_var() %>%
      left_join(mk_results(), by = join_by(STATION_NUMBER))
  })

  stations_sf_with_trend = reactive({
    dat = stations_sf %>%
      left_join(mk_results(), by = join_by(STATION_NUMBER))

    if(input$recent == FALSE){
      dat = dat %>%
        filter(!(STATION_NUMBER %in% recent_stations))
    }
    else{
      dat = dat
    }

    if(input$upstream == FALSE){
      dat = dat %>%
        filter(!(STATION_NUMBER %in% upstream_stations))
    }
    else{
      dat = dat
    }
    if(basin_rv() == "All"){
      dat = dat
    }
    else {
      dat = dat %>%
        filter(Sub_Basin == basin_rv())
    }

    if(input$user_var_choice %in% date_vars){
      dat %>%
        mutate(trend_sig = factor(trend_sig, levels = c("Significant Trend Earlier",
                                                        'Non-Significant Trend Earlier',
                                                        'No Trend',
                                                        'Non-Significant Trend Later',
                                                        'Significant Trend Later')),
               magnitude_fixed = factor(magnitude_fixed, levels = c("> 2 days earlier",
                                                                    "1 - 2 days earlier",
                                                                    "< 1 days change",
                                                                    "1 - 2 days later",
                                                                    "> 2 days later",
                                                                    "Non-significant trend")))
    } else {
      dat %>%

        mutate(trend_sig = factor(trend_sig, levels = c("Significant Trend Down",
                                                        'Non-Significant Trend Down',
                                                        'No Trend',
                                                        'Non-Significant Trend Up',
                                                        'Significant Trend Up')),
               magnitude_fixed = factor(magnitude_fixed, levels = c("> 5% decrease",
                                                                    "1 - 5% decrease",
                                                                    "< 1% change",
                                                                    "1 - 5% increase",
                                                                    "> 5% increase",
                                                                    "Non-significant trend")))
    }
  })

  # Set up a reactive value that stores a district's name upon user's click
  click_station <- reactiveVal('no_selection')

  # Get Mann-Kendall trend slope and intercept for our station-specific plot.
  senslope_dat = reactive({
    flow_dat_with_mk() %>%
      filter(STATION_NUMBER == click_station()) %>%
      left_join(mk_results()) %>%
      mutate(SlopePreds = Intercept+Slope*Year,
             per_change = (((Intercept + Slope * max(Year))-(Intercept + Slope*min(Year)))/(Intercept + Slope * min(Year)))/(max(Year)-min(Year))*100)
  })

  # Watch for a click on the leaflet map. Once clicked...
  # 1. Update selection.
  observeEvent(input$leafmap_marker_click, {
    # Capture the info of the clicked polygon. We use this for filtering.
    click_station(input$leafmap_marker_click$id)
    shiny::updateTabsetPanel(
      inputId = 'tabset',
      selected = 'Flow Metric Plot')
  })

  output$selected_station = renderText({paste0("Station: ",click_station())})

  date_choice_label = reactive({
    switch(input$time_scale,
           Annual = 'Based on Data from: Entire year',
           Monthly = paste0('Based on Data from: ',month.name[match(input$month_selector,month.abb)])
    )
  })

  output$myplot = renderPlot({
    station_flow_plot(data = flow_dat_chosen_var(),
                      variable_choice = input$user_var_choice,
                      clicked_station = click_station(),
                      stations_shapefile = stations_sf,
                      slopes = senslope_dat(),
                      caption_label = date_choice_label(),
                      user_period_choice = input$user_period_choice)
  })

  output$myhydrograph = renderPlot({
    station_hydrograph_plot(
      dat = hydrograph_data_station(),
      clicked_station = click_station(),
      stations_shapefile = stations_sf)
  })

  mypal = reactive({
    if(input$user_var_choice %in% date_vars){
      colorFactor(palette = c("#ff0000", "#009E73", "#0072B2", "#D55E00", "#CC79A7", "black"),
                  domain = mk_results()$magnitude_fixed,
                  levels = c("> 2 days earlier",
                             "1 - 2 days earlier",
                             "< 1 days change",
                             "1 - 2 days later",
                             "> 2 days later",
                             "Non-significant trend"),
                  ordered = T)
    } else {
      colorFactor(palette = c("#ff0000", "#009E73", "#0072B2", "#D55E00", "#CC79A7", "black"),
                  domain = mk_results()$magnitude_fixed,
                  levels = c("> 5% decrease",
                             "1 - 5% decrease",
                             "< 1% change",
                             "1 - 5% increase",
                             "> 5% increase",
                             "Non-significant trend"),
                  ordered = T)
    }
  })

  # zoom
  boundary_data = reactive({
    if(basin_rv()== "All") {
      sbasins
    }
    else{
      sbasins %>%
        filter(Sub_Basin == basin_rv()) %>%
        st_bbox()
    }
  })

  # polygon
  basin_data = reactive({
    if(basin_rv()== "All") {
      sbasins
    }
    else{
      sbasins %>%
        filter(Sub_Basin == basin_rv())
    }
  })

  output$leafmap <- renderLeaflet({

    map = leaflet() %>%
      addProviderTiles(providers$CartoDB,group = "CartoDB") %>%
      addTiles(group = 'Streets') %>%
      #addProviderTiles(providers$Stamen.Terrain, group = "Terrain") %>% Zhuoyan found that this line can cause disperance of polygons and sites on the map.
      # add_bc_home_button() %>%
      set_bc_view() %>%
      addLayersControl(baseGroups = c("CartoDB","Streets"),
                       options = layersControlOptions(collapsed = F),
                       position = 'bottomright')

    if(basin_rv() == "All"){
      map %>%
        set_bc_view()
    }
    else{
      map %>%
        fitBounds(as.numeric(boundary_data()$xmin)-1, as.numeric(boundary_data()$ymin)-1, as.numeric(boundary_data()$xmax)+1, as.numeric(boundary_data()$ymax)+1)
    }
  })

  observe({
    leafletProxy("leafmap") %>%
      clearMarkers() %>%
      addPolygons(data = bound,
                  color = "grey", fillColor = "white",
                  weight = 1, smoothFactor = 0.5, opacity = 1.0, fillOpacity = 0.2) %>%
      addPolygons(layerId = ~Sub_Basin,
                  data = sbasins,
                  label = ~paste0(Sub_Basin), color = "grey", fillColor = "white",
                  weight = 1, smoothFactor = 0.5, opacity = 1.0, fillOpacity = 0.2,
                  highlightOptions = highlightOptions(color = "#979B9D", weight = 2,
                                                      bringToFront = FALSE)
      ) %>%
      addPolygons(layerId = ~Sub_Basin,
                  data = basin_data(),
                  label = ~paste0(Sub_Basin), color = "black", fillColor = "white",
                  weight = 1, smoothFactor = 0.5, opacity = 1.0, fillOpacity = 0.2,
                  highlightOptions = highlightOptions(color = "black", weight = 2,
                                                      bringToFront = FALSE)
      ) %>%
      # addMarkers(icon = makeIcon(
      #              iconUrl = "http://leafletjs.com/docs/images/leaf-green.png",
      #              iconWidth = 1, iconHeight = 1
      #            ),
      #            data = stations_sf_with_trend(),
      #            group = 'STATION_NAME') %>%
      addCircleMarkers(layerId = ~STATION_NUMBER,
                       color = 'black',
                       fillColor = ~mypal()(magnitude_fixed),
                       radius = ~significant,
                       weight = ~significant*0.3,
                       fillOpacity = 1,
                       #~significant,
                       label = ~paste0(STATION_NAME, " (",STATION_NUMBER,") - ",HYD_STATUS),
                       data = stations_sf_with_trend()) %>%

      removeControl("legend") %>%
      addLegend(pal = mypal(),
                values = ~magnitude_fixed,
                title = 'Change per decade',
                data = stations_sf_with_trend(),
                layerId = 'legend') #%>%
    # addSearchFeatures(
    #   targetGroups = 'STATION_NAME',
    #   options = searchFeaturesOptions(
    #     zoom=12, openPopup = TRUE, firstTipSubmit = TRUE,
    #     autoCollapse = TRUE, hideMarkerOnCollapse = TRUE ,
    #     position = "bottomright"))

  })
}

# Run the application
shinyApp(ui = ui, server = server)
