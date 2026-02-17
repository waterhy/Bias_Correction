library(shiny)
library(terra)
library(leaflet)
library(ggplot2)
library(ncdf4)
library(dplyr)

#Upload Limit to 500 MB
options(shiny.maxRequestSize = 500 * 1024^2)  # 500 MB

ui <- fluidPage(
  titlePanel("CORDEX NetCDF Time-Series Extractor"),
  h4("Interactive extraction of point time series from rotated climate model grids"),
  sidebarLayout(
    sidebarPanel(
      fileInput("ncfiles", "Choose NetCDF files", multiple = TRUE,
                accept = c(".nc")),
      uiOutput("varselect"),
      
      numericInput("input_lat", "Latitude:", value = NA, step = 0.01),
      numericInput("input_lon", "Longitude:", value = NA, step = 0.01),
      actionButton("set_coords", "Set Coordinates"),
      
      verbatimTextOutput("point_selected"),
      actionButton("run", "Extract from Selected Files"),
      br(),
      downloadButton("downloadData", "Download CSV"),
      
      hr(),
      h5("File Metadata"),
      verbatimTextOutput("file_info"),
      
      hr(),
      p("Created by Aikaterini Lyra | klyra@uth.gr",
        style = "font-size:12px; font-style:italic; color:#333333;")
      
    ),
    
    mainPanel(
      leafletOutput("map", height = 300),
      plotOutput("tsplot"),
      tableOutput("preview")
    )
  )
)

server <- function(input, output, session){
  
  output$file_info <- renderPrint({
    req(input$ncfiles)
    
    f <- input$ncfiles$datapath[1]
    nc <- nc_open(f)
    
    # Safe attribute getter
    get_att <- function(attname){
      val <- tryCatch(ncatt_get(nc, 0, attname)$value,
                      error = function(e) NA)
      if(is.null(val) || val == "") NA else val
    }
    
    # ---- Global Attributes ----
    institution  <- get_att("institution")
    title        <- get_att("title")
    driving_exp  <- get_att("driving_experiment")
    driving_mod  <- get_att("driving_model_id")
    ensemble     <- get_att("driving_model_ensemble_member")
    frequency    <- get_att("frequency")
    institute_id <- get_att("institute_id")
    model_id     <- get_att("model_id")
    domain       <- get_att("CORDEX_domain")
    
    # ---- Grid Dimensions ----
    rlat_len <- if("rlat" %in% names(nc$dim)) nc$dim$rlat$len else NA
    rlon_len <- if("rlon" %in% names(nc$dim)) nc$dim$rlon$len else NA
    
    nc_close(nc)
    
    # ---- Print Clean Output ----
    cat("Institution:", institution, "\n")
    cat("Title:", title, "\n\n")
    
    cat("Driving Experiment:", driving_exp, "\n")
    cat("Driving Model ID:", driving_mod, "\n")
    cat("Ensemble Member:", ensemble, "\n\n")
    
    cat("Frequency:", frequency, "\n")
    cat("Institute ID:", institute_id, "\n")
    cat("Model ID:", model_id, "\n")
    cat("CORDEX Domain:", domain, "\n\n")
    
    cat("Grid Size (Rotated Coordinates):\n")
    cat("  rlat:", rlat_len, "\n")
    cat("  rlon:", rlon_len, "\n")
  })
  
  selected_point <- reactiveVal(NULL)
  
  # --- Variable select based on first selected file ---
  output$varselect <- renderUI({
    req(input$ncfiles)
    nc <- nc_open(input$ncfiles$datapath[1])
    vars <- setdiff(names(nc$var), c("lat","lon"))
    nc_close(nc)
    selectInput("varname", "Select Variable:", choices = vars)
  })
  
  # --- Map extent and grid points based on first file ---
  map_extent <- reactive({
    req(input$ncfiles)
    nc <- nc_open(input$ncfiles$datapath[1])
    lat_vals <- ncvar_get(nc, "lat")
    lon_vals <- ncvar_get(nc, "lon")
    nc_close(nc)
    
    list(
      lat_min = min(lat_vals, na.rm=TRUE),
      lat_max = max(lat_vals, na.rm=TRUE),
      lon_min = min(lon_vals, na.rm=TRUE),
      lon_max = max(lon_vals, na.rm=TRUE),
      lat_vals = lat_vals,
      lon_vals = lon_vals
    )
  })
  
  # --- Initial map render ---
  output$map <- renderLeaflet({
    req(map_extent())
    m <- map_extent()
    leaflet() %>%
      addTiles() %>%
      addRectangles(
        lng1 = m$lon_min, lat1 = m$lat_min,
        lng2 = m$lon_max, lat2 = m$lat_max,
        color = "red", fill = FALSE
      ) %>%
      setView(lng = 24.5,
              lat = 39, zoom = 6)
  })
  
  # --- Show grid points ---
  observe({
    req(map_extent())
    m <- map_extent()
    lat_vec <- as.vector(m$lat_vals)
    lon_vec <- as.vector(m$lon_vals)
    
    leafletProxy("map") %>%
      clearGroup("grid") %>%
      addCircleMarkers(
        lng = lon_vec,
        lat = lat_vec,
        radius = 2,
        color = "blue",
        fill = TRUE,
        fillOpacity = 0.5,
        group = "grid"
      ) %>%
      addLayersControl(
        overlayGroups = c("grid"),
        options = layersControlOptions(collapsed = FALSE)
      )
  })
  
  # --- Click on map to select point and update inputs ---
  observeEvent(input$map_click, {
    click <- input$map_click
    selected_point(c(click$lng, click$lat))
    
    # Update numeric inputs
    updateNumericInput(session, "input_lat", value = click$lat)
    updateNumericInput(session, "input_lon", value = click$lng)
    
    # Update marker
    leafletProxy("map") %>%
      clearGroup("selected") %>%
      addMarkers(
        lng = click$lng,
        lat = click$lat,
        popup = "Selected Point",
        group = "selected"
      )
  })
  
  # --- Update map marker when numeric inputs change ---
  observeEvent({
    input$input_lat
    input$input_lon
  }, {
    req(!is.na(input$input_lat), !is.na(input$input_lon))
    selected_point(c(input$input_lon, input$input_lat))
    
    leafletProxy("map") %>%
      clearGroup("selected") %>%
      addMarkers(
        lng = input$input_lon,
        lat = input$input_lat,
        popup = "Selected Point",
        group = "selected"
      )
  })
  
  output$point_selected <- renderPrint({
    req(selected_point())
    cat("Selected point (WGS84):\n")
    print(selected_point())
  })
  
  # --- Extract from selected files with proper dates ---
  extracted_data <- eventReactive(input$run, {
    req(input$ncfiles, input$varname, selected_point())
    results <- list()
    
    for(i in seq_len(nrow(input$ncfiles))){
      f <- input$ncfiles$datapath[i]
      nc <- nc_open(f)
      var_vals <- ncvar_get(nc, input$varname)
      lat_vals <- ncvar_get(nc, "lat")
      lon_vals <- ncvar_get(nc, "lon")
      time_var <- ncvar_get(nc, "time")
      
      # convert time to dates
      time_units <- ncatt_get(nc, "time", "units")$value
      time_dates <- as.Date(time_var, origin = sub(".*since ", "", time_units))
      
      nc_close(nc)
      
      lat_vec <- as.vector(lat_vals)
      lon_vec <- as.vector(lon_vals)
      dist <- (lat_vec - selected_point()[2])^2 + (lon_vec - selected_point()[1])^2
      idx <- which.min(dist)
      
      dims <- dim(var_vals)
      var_mat <- matrix(var_vals, nrow = dims[1]*dims[2], ncol = dims[3])
      
      ts_vals <- var_mat[idx, ]
      
      period_label <- paste0(min(time_dates), " to ", max(time_dates))
      
      results[[i]] <- data.frame(
        Time = time_dates,
        FileLabel = period_label,
        Value = as.numeric(ts_vals)
      )
    }
    
    bind_rows(results)
  })
  
  # --- Plot with legend showing periods ---
  output$tsplot <- renderPlot({
    req(extracted_data())
    ggplot(extracted_data(), aes(x=Time, y=Value, color=FileLabel)) +
      geom_line() +
      theme_minimal() +
      labs(y=input$varname, color="Period")
  })
  
  # --- Preview ---
  output$preview <- renderTable({
    df <- extracted_data()
    req(df)
    if(nrow(df)==0 || all(is.na(df$Value))){
      return(data.frame(Message="No data available at this point"))
    } else head(df)
  })
  
  # --- Download CSV ---
  output$downloadData <- downloadHandler(
    filename = function(){ paste0("timeseries_", input$varname, ".csv") },
    content = function(file){
      write.csv(extracted_data() %>% select(Time, FileLabel, Value),
                file, row.names = FALSE)
    }
  )
  
}


shinyApp(ui, server)