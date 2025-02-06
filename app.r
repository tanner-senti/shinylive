## Modified app by Tanner Senti (2024-12-31)
# FROM:

### Utah DWQ Lake Profile Dashboard
### Jake Vander Laan, Utah DWQ, jvander@utah.gov
### Version 1.1, Feb 8 2019

library(magrittr)
library(dplyr)
library(leaflet)
library(markdown)
library(ggplot2)
library(gridExtra)

# Custom map function for AR instead of Utah's map function in wqTools:
source("map_fun_redo.R")
# Custom plot function for AR instead of Utah's profilePlot() function in wqTools:
source("plot_fun.R")
# Custom plot function for AR from lake_profiles_graphing project:
source("ADEQ_plot_fun.R")


ui <- fluidPage(
  tags$head(
    tags$script(src = "https://cdnjs.cloudflare.com/ajax/libs/iframe-resizer/3.5.16/iframeResizer.contentWindow.min.js",
                type = "text/javascript")
  ),
  
  # Header
  headerPanel(
    title = tags$a(
      href = 'https://www.adeq.state.ar.us/water/',
      tags$img(
        src = 'adeq_logo.png',
        height = 125,
        width = 100 * 2.85 * 1.75
      ),
      target = "_blank"
    ),
  ),
  # Disclaimer or heading if needed:
  fluidRow(column(p(
    "Lakes Profiles Dashboard"
  ),
  width = 2)),
  # User guide button:
  fluidRow(column(
    2, actionButton("show_guide", "Show User Guide")
  )),
  
  br(),
  
  # Input widgets
  fluidRow(
    column(
      5,
      conditionalPanel(condition = "input.plot_tabs!='User guide'",
                       tabsetPanel(
                         id = "ui_tab",
                         tabPanel(
                           "Map",
                           column(12,
                                  h4("Click a site to display plots")),
                           column(
                             12,
                             shinycssloaders::withSpinner(
                               leaflet::leafletOutput("map", height = "600px"),
                               size = 2,
                               color = "#0080b7"
                             )
                           )
                         ),
                         tabPanel("Table",
                                  column(
                                    12,
                                    h4("Click a site to display plots"),
                                    div(DT::dataTableOutput("table_input"), style = "font-size:70%")
                                  ))
                       )),
      conditionalPanel(condition = "input.plot_tabs=='User guide'",
                       column(12))
    ),
    column(7,
           tabsetPanel(
             id = "plot_tabs",
             tabPanel(
               "Individual profiles",
               fluidRow(column(4, uiOutput("date_select"))),
               fluidRow(column(
                 4,
                 h4("Profile plot:"),
                 plotOutput("ind_prof_plot", height = "500px")
               ),
               column(
                 8,
                 h4("Profile data:"),
                 div(DT::dataTableOutput("profile_table"), style = "font-size:80%")
               ))
             ),
             tabPanel(
               "Site profiles (all dates)",
               fluidRow(column(4, uiOutput("date_slider"))),
               fluidRow(column(
                 8,
                 h4("Parameter profiles:"),
                 plotOutput("site_prof_plot", height = "500px")
               ))
             ),
           ))
  )
)



server <- function(input, output, session) {
  # Loading modal to keep user out of trouble while map draws...
  showModal(
    modalDialog(
      title = "MAP LOADING - PLEASE WAIT...",
      "Please wait for map to draw before proceeding.",
      size = "l",
      footer = NULL
    )
  )
  
  # Remove modal when app is ready
  observe({
    req(map, sites_table)
    removeModal()
  })
  
  # Observer for the user guide button
  observeEvent(input$show_guide, {
    showModal(modalDialog(
      title = "User Guide",
      includeMarkdown('./user_guide/user_guide.rmd'),
      size = "l",
      easyClose = TRUE
    ))
  })
  
  # Data work -------------------------------------------------------------------
  
  # Wide profile data (from lake profile plotting project):
  profiles_wideAR <- read.csv("./data/cleaned_profiles_wide.csv")
  
  # Fix AR wide data
  profiles_wideAR <- profiles_wideAR %>%
    mutate(SiteID = toupper(SiteID)) %>%
    mutate(ActivityIdentifier = paste(SiteID, Date, sep = "-")) %>%
    mutate(Date = as.Date(Date))
  
  # Create AR long data
  profiles_longAR <- profiles_wideAR %>%
    tidyr::pivot_longer(
      cols = c("Temp_Inst", "DO_Inst", "pH_Inst", "Depth"),
      names_to = "Parameter",
      values_to = "IR_Value"
    ) %>%
    mutate(
      Parameter = case_when(
        Parameter == "Depth" ~ "Depth (m)",
        Parameter == "DO_Inst" ~ "DO (mg/L)",
        Parameter == "pH_Inst" ~ "pH",
        Parameter == "Temp_Inst" ~ "Temp (*C)",
        TRUE ~ Parameter
      )
    )
  
  # Temporary fix to issues with plotting and reactive_objects$selectActID -
  # Long term solution needed to preserve tibble and make code work:
  profiles_longAR <- as.data.frame(profiles_longAR)
  profiles_longAR$ActivityIdentifier <-
    as.factor(profiles_longAR$ActivityIdentifier)
  
  # Load Arkansas sites data:
  # (used in buildMapAR function):
  all_sitesAR <- read.csv("./data/all_site_info.csv")
  
  filtered_sitesAR <- all_sitesAR %>%
    filter(MonitoringLocationIdentifier %in% profiles_wideAR$SiteID)
  
  # List of locations with no data (for optional map layer):
  sites_nodataAR <- all_sitesAR %>%
    filter(!(MonitoringLocationIdentifier %in% profiles_wideAR$SiteID))
  
  # New map/site table:
  sites_table <- filtered_sitesAR %>%
    rename(
      StationID = MonitoringLocationIdentifier,
      Lake_Name = MonitoringLocationName,
      Type = MonitoringLocationTypeName,
      Latitude = LatitudeMeasure,
      Longitude = LongitudeMeasure
    )
  
  # Complete data work ----
  
  # Empty reactive values object
  reactive_objects = reactiveValues(
    sel_mlid = NULL,
    selectedActID = NULL,
    sel_profiles = NULL,
    profile_dates = NULL,
    sel_profs_wide = NULL
  )
  observe({
    req(filtered_sitesAR)  # Ensure sites data is available
    if (is.null(reactive_objects$sel_mlid)) {
      # Set default site to the first site in the list
      default_site <-
        filtered_sitesAR$MonitoringLocationIdentifier[1]
      reactive_objects$sel_mlid <- default_site
    }
  })
  
  # Resources for returning site info on click:
  ## https://stackoverflow.com/questions/28938642/marker-mouse-click-event-in-r-leaflet-for-shiny
  ## https://stackoverflow.com/questions/42613984/how-to-implement-inputmap-marker-click-correctly?noredirect=1&lq=1
  
  # Select map set up
  map = leaflet::createLeafletMap(session, 'map')
  
  # Here, buildMapAR() is a function customized from buildMap() in Utah's
  # wqTools package that will build a map from prof_sites data using
  # au_poly polygons (here these are lake polygons)
  session$onFlushed(once = T, function() {
    output$map <- leaflet::renderLeaflet({
      buildMapAR_redo(sites = filtered_sitesAR, sites_nodataAR)
    })
  })
  
  # Table interface:
  output$table_input <- DT::renderDataTable({
    req(sites_table)  # Ensure data is available
    DT::datatable(
      sites_table,
      selection = 'single',
      rownames = FALSE,
      filter = 'top',
      options = list(
        scrollY = '500px',
        paging = FALSE,
        scrollX = TRUE,
        dom = "ltipr"
      )
    )
  })
  
  # Ensure that when switching to the Table tab, the selected site is set
  observeEvent(input$ui_tab, {
    if (input$ui_tab == "Table") {
      # This triggers the selection from the map again (if applicable)
      if (!is.null(input$map_marker_click)) {
        site_click <- input$map_marker_click
        siteid = site_click$id
        reactive_objects$sel_mlid = siteid
      }
    }
  })
  
  
  
  # Map marker click (to identify selected site)
  observe({
    req(profiles_longAR)
    site_click <- input$map_marker_click
    if (is.null(site_click)) {
      return()
    }
    siteid = site_click$id
    reactive_objects$sel_mlid = siteid
  })
  
  # Observer for table row selection to update plots
  observeEvent(input$table_input_rows_selected, {
    req(sites_table)  # Ensure data is available
    selected_row <- input$table_input_rows_selected
    
    if (length(selected_row) > 0) {
      # Get the SiteID of the selected row
      selected_site <- sites_table[selected_row, "StationID"]
      # Update reactive state to reflect selected site
      reactive_objects$sel_mlid <- selected_site
    }
  })
  
  # Observer to zoom map to the selected site when a row is clicked in the table
  observeEvent(input$table_input_rows_selected, {
    req(sites_table)  # Ensure data is available
    selected_row <- input$table_input_rows_selected
    
    if (length(selected_row) > 0) {
      # Get the selected site's latitude and longitude
      selected_site <- sites_table[selected_row,]
      lat <- selected_site$Latitude
      lon <- selected_site$Longitude
      
      # Zoom the map to the selected site
      leafletProxy("map") %>%
        setView(lng = lon,
                lat = lat,
                zoom = 12)  # Adjust zoom level as needed
    }
  })
  
  
  # Select profiles & date options based on selected site ID
  observe({
    req(reactive_objects$sel_mlid)
    reactive_objects$sel_profiles = profiles_longAR[profiles_longAR$SiteID ==
                                                      reactive_objects$sel_mlid, ]
    profile_dates = unique(reactive_objects$sel_profiles$Date)
    profile_dates = profile_dates[order(profile_dates)]
    reactive_objects$profile_dates = profile_dates
  })
  
  # Profile date selection
  output$date_select <- renderUI({
    req(reactive_objects$profile_dates)
    selectInput("date_select",
                "Profile date:",
                reactive_objects$profile_dates)
  })
  
  # Date slider for combined profile plots:
  output$date_slider <- renderUI({
    req(reactive_objects$profile_dates)
    date_min = min(reactive_objects$profile_dates)
    date_max = max(reactive_objects$profile_dates)
    sliderInput(
      "date_slider",
      "Date range:",
      min = date_min,
      max = date_max,
      value = c(date_min, date_max)
    )
  })
  
  # Generate selected aid
  observe({
    req(input$date_select)
    reactive_objects$selectedActID = reactive_objects$sel_profiles[reactive_objects$sel_profiles$Date ==
                                                                     input$date_select, "ActivityIdentifier"][1]
  })
  
  # Profile plot output (uses long data) ----
  output$ind_prof_plot = renderPlot({
    req(reactive_objects$sel_profiles,
        reactive_objects$selectedActID)
    one_profile = reactive_objects$sel_profiles[reactive_objects$sel_profiles$ActivityIdentifier ==
                                                  reactive_objects$selectedActID,]
    
    one_profile = unique(one_profile[, c(#"DataLoggerLine",
      "ActivityIdentifier",
      "Date",
      "Parameter",
      "IR_Value",
      #"IR_Unit",
      "SiteID",
      "Time")])
    
    # OG profilePlot() function from wqTools package by Utah DEQ
    # Modified for AR data setup
    profilePlotAR(
      data = one_profile,
      parameter = "Parameter",
      depth = "Depth (m)",
      do = "DO (mg/L)",
      temp = "Temp (*C)",
      pH = "pH",
      value_var = "IR_Value",
      line_no = "Time"
      #pH_crit = c(6.5, 9),
      #do_crit = do_crit,
      #temp_crit = temp_crit
      
      # customize or remove these:
      #units = "IR_Unit",
      #line_no = "DataLoggerLine",
    )
    box()
  })
  
  #Data table output (uses profiles_Wide):----
  observe({
    req(reactive_objects$selectedActID)
    table_data = profiles_wideAR[profiles_wideAR$ActivityIdentifier ==
                                   reactive_objects$selectedActID, c("SiteID",
                                                                     "Date",
                                                                     "Depth",
                                                                     "DO_Inst",
                                                                     "pH_Inst",
                                                                     "Temp_Inst")]
    reactive_objects$table_data = table_data[order(table_data$Depth), ]
  })
  output$profile_table = DT::renderDataTable({
    req(reactive_objects$table_data)
    DT::datatable(
      reactive_objects$table_data,
      selection = 'multiple',
      options = list(
        scrollY = '500px',
        paging = FALSE,
        scrollX = TRUE,
        searching = F
      )
    ) %>%
      DT::formatStyle("DO_Inst",
                      backgroundColor = DT::styleEqual(1, "orange"))  %>%
      DT::formatStyle("pH_Inst", backgroundColor = DT::styleEqual(1, "orange"))  %>%
      DT::formatStyle("Temp_Inst",
                      backgroundColor = DT::styleEqual(1, "orange"))
  })
  
  # Site profiles (all dates) plotting - uses profiles wide:----
  observe({
    req(reactive_objects$sel_mlid, input$date_slider)
    
    reactive_objects$sel_profs_wide = profiles_wideAR[profiles_wideAR$SiteID == reactive_objects$sel_mlid &
                                                        profiles_wideAR$Date >= input$date_slider[1] &
                                                        profiles_wideAR$Date <= input$date_slider[2]
                                                      ,]
  })
  
  
  output$site_prof_plot = renderPlot({
    req(reactive_objects$sel_profs_wide)
    site_data_wide <- reactive_objects$sel_profs_wide
    
    # plotting function with args
    site_plottingAR(site_data_wide)
    #box()
  })
  
}


# Run the application
shinyApp(ui = ui, server = server)
