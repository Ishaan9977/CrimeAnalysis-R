install.packages(c(
  "shiny", "shinydashboard", "leaflet", "ggplot2", 
  "dplyr", "lubridate", "tidyr", "viridis"
))

library(shiny)
library(shinydashboard)
library(sf)
library(leaflet)
library(dplyr)
library(ggplot2)
library(lubridate)
library(leaflet.extras)
library(scales)
library(broom)

# Load and preprocess GeoJSON data
crime_data <- st_read("C:/Users/ishaa/Downloads/Crimes - 2018_20250413.geojson")

crime_data_clean <- crime_data %>%
  filter(!st_is_empty(geometry)) %>%
  mutate(
    latitude = as.numeric(latitude),
    longitude = as.numeric(longitude),
    date = ymd_hms(date),
    hour = hour(date),
    month = month(date, label = TRUE),
    arrested = as.logical(arrest)
  ) %>%
  filter(!is.na(latitude), !is.na(longitude), !is.na(primary_type)) %>%
  distinct()

# UI
ui <- dashboardPage(
  dashboardHeader(title = "Chicago Crime Dashboard"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Crime Map", tabName = "map", icon = icon("map")),
      menuItem("Heatmap", tabName = "heat", icon = icon("fire")),
      menuItem("Clustering", tabName = "cluster", icon = icon("project-diagram")),
      menuItem("Plots", tabName = "plots", icon = icon("chart-bar")),
      menuItem("Trends", tabName = "trends", icon = icon("chart-line")),
      menuItem("Analytics", tabName = "analytics", icon = icon("chart-pie"))
    )
  ),
  dashboardBody(
    tabItems(
      tabItem(tabName = "map",
              fluidRow(box(leafletOutput("crimeMap", height = 600), width = 12))
      ),
      tabItem(tabName = "heat",
              fluidRow(box(leafletOutput("heatMap", height = 600), width = 12))
      ),
      tabItem(tabName = "cluster",
              fluidRow(box(leafletOutput("clusterMap", height = 600), width = 12))
      ),
      tabItem(tabName = "plots",
              fluidRow(
                box(plotOutput("crimeTypePlot", height = 300), width = 6),
                box(plotOutput("crimeHourPlot", height = 300), width = 6)
              ),
              fluidRow(
                box(plotOutput("crimeMonthPlot", height = 300), width = 6)
              )
      ),
      tabItem(tabName = "trends",
              fluidRow(
                box(plotOutput("crimeTrendPlot", height = 300), width = 6),
                box(plotOutput("safestAreasPlot", height = 300), width = 6)  # Updated here
              )
      ),
      tabItem(tabName = "analytics",
              fluidRow(
                box(plotOutput("densityPlot", height = 300), width = 6),
                box(plotOutput("timeTypeBoxplot", height = 300), width = 6)
              ),
              fluidRow(
                box(plotOutput("monthlyTrend", height = 300), width = 6),
                box(plotOutput("arrestVsTotal", height = 300), width = 6)
              ),
              fluidRow(
                box(plotOutput("topCrimePie", height = 300), width = 6)
              )
      )
    )
  )
)

# Server
server <- function(input, output) {
  
  output$crimeMap <- renderLeaflet({
    leaflet(crime_data_clean) %>%
      addTiles() %>%
      addCircleMarkers(lng = ~longitude, lat = ~latitude,
                       radius = 1, color = "red", opacity = 0.5)
  })
  
  output$heatMap <- renderLeaflet({
    leaflet(crime_data_clean) %>%
      addTiles() %>%
      addHeatmap(lng = ~longitude, lat = ~latitude, blur = 20, max = 0.05, radius = 15)
  })
  
  output$clusterMap <- renderLeaflet({
    leaflet(crime_data_clean) %>%
      addTiles() %>%
      addMarkers(lng = ~longitude, lat = ~latitude,
                 clusterOptions = markerClusterOptions())
  })
  
  output$crimeTypePlot <- renderPlot({
    crime_data_clean %>%
      count(primary_type, sort = TRUE) %>%
      top_n(10) %>%
      ggplot(aes(x = reorder(primary_type, n), y = n)) +
      geom_col(fill = "steelblue") +
      coord_flip() +
      labs(title = "Top 10 Crime Types", x = "Crime Type", y = "Count") +
      theme_minimal()
  })
  
  output$crimeHourPlot <- renderPlot({
    crime_data_clean %>%
      count(hour) %>%
      ggplot(aes(x = hour, y = n)) +
      geom_line(color = "purple", size = 1.2) +
      geom_point() +
      labs(title = "Crime by Hour", x = "Hour of Day", y = "Incidents") +
      theme_minimal()
  })
  
  output$crimeMonthPlot <- renderPlot({
    crime_data_clean %>%
      count(month) %>%
      ggplot(aes(x = month, y = n, group = 1)) +
      geom_line(color = "darkorange", size = 1.3) +
      geom_point() +
      labs(title = "Monthly Crime Distribution", x = "Month", y = "Incidents") +
      theme_minimal()
  })
  
  output$crimeTrendPlot <- renderPlot({
    crime_data_clean %>%
      mutate(day = as.Date(date)) %>%
      count(day) %>%
      ggplot(aes(x = day, y = n)) +
      geom_line(color = "navy", size = 1) +
      geom_point(color = "navy", size = 2) +
      labs(title = "Daily Crime Trend", x = "Date", y = "Incidents") +
      theme_minimal()
  })
  
  output$safestAreasPlot <- renderPlot({
    crime_data_clean %>%
      mutate(
        lon_bin = round(longitude, 2),
        lat_bin = round(latitude, 2)
      ) %>%
      count(lat_bin, lon_bin) %>%
      arrange(n) %>%
      slice(1:10) %>%
      ggplot(aes(x = reorder(paste(lat_bin, lon_bin, sep = ", "), n), y = n)) +
      geom_col(fill = "forestgreen") +
      coord_flip() +
      labs(title = "Top 10 Safest Areas (Least Crimes)",
           x = "Lat, Long Grid",
           y = "Number of Crimes") +
      theme_minimal()
  })
  
  output$densityPlot <- renderPlot({
    ggplot(crime_data_clean, aes(x = longitude, y = latitude)) +
      stat_density_2d(aes(fill = after_stat(level)), geom = "polygon", alpha = 0.5) +
      scale_fill_viridis_c() +
      coord_fixed() +
      labs(title = "Crime Density Map", x = "Longitude", y = "Latitude") +
      theme_minimal()
  })
  
  output$timeTypeBoxplot <- renderPlot({
    crime_data_clean %>%
      ggplot(aes(x = primary_type, y = hour)) +
      geom_boxplot(outlier.color = "red") +
      labs(title = "Crime Time Distribution by Type", x = "Crime Type", y = "Hour") +
      theme_minimal() +
      theme(axis.text.x = element_text(angle = 45, hjust = 4)) +
      coord_flip()
  })
  
  output$monthlyTrend <- renderPlot({
    crime_data_clean %>%
      count(month) %>%
      ggplot(aes(x = month, y = n, group = 1)) +
      geom_line(color = "darkorange", size = 1.3) +
      geom_point() +
      labs(title = "Monthly Crime Trend", x = "Month", y = "Incidents") +
      theme_minimal()
  })
  
  output$arrestVsTotal <- renderPlot({
    crime_data_clean %>%
      group_by(arrested) %>%
      summarise(count = n()) %>%
      ggplot(aes(x = arrested, y = count, fill = arrested)) +
      geom_col(width = 0.5) +
      scale_fill_manual(values = c("steelblue", "tomato")) +
      labs(title = "Arrests vs Total Crimes", x = "Arrest Made", y = "Count") +
      theme_minimal()
  })
  
  output$topCrimePie <- renderPlot({
    top10 <- crime_data_clean %>%
      count(primary_type, sort = TRUE) %>%
      slice_max(n, n = 10)
    
    ggplot(top10, aes(x = "", y = n, fill = primary_type)) +
      geom_bar(stat = "identity", width = 1) +
      coord_polar("y") +
      theme_void() +
      labs(title = "Top 10 Crimes (Pie Chart)") +
      theme(legend.position = "right")
  })
  
}

shinyApp(ui, server)
