# --- Step 1: Load Required Libraries ---
# Install packages (do this only once, or comment out after installing)
# install.packages("sf")
# install.packages("leaflet")
# install.packages("dplyr")

# Load the packages
library(sf)
library(leaflet)
library(dplyr)

# --- Step 2: Load GeoJSON Data ---
# Replace with the actual path to your GeoJSON file
crime_data <- st_read("C:/Users/ishaa/Downloads/Crimes - 2018_20250413.geojson")


# --- Step 3: Inspect the Data ---
# View structure and preview first few rows
str(crime_data)
head(crime_data)





# PRE-PROCESSING STARTS HERE

#removing empty geometries
crime_data_clean <- crime_data[!st_is_empty(crime_data), ]


# Convert lat/lon to numeric if needed (they are currently character)
crime_data_clean$latitude <- as.numeric(crime_data_clean$latitude)
crime_data_clean$longitude <- as.numeric(crime_data_clean$longitude)

# Remove rows with missing lat/lon or crime type
crime_data_clean <- crime_data_clean %>%
  filter(!is.na(latitude), !is.na(longitude), !is.na(primary_type))



crime_data_clean <- crime_data_clean %>% distinct()



#Confirming that the cleaning worked---->

summary(crime_data_clean)
str(crime_data_clean)



#Visualisation 

#Interactive Map

leaflet(data = crime_data_clean) %>%
  addTiles() %>%
  addCircleMarkers(
    lng = ~longitude,
    lat = ~latitude,
    radius = 3,
    color = "red",
    stroke = FALSE,
    fillOpacity = 0.5,
    popup = ~paste0("Type: ", primary_type, "<br>",
                    "Location: ", location_description, "<br>",
                    "Date: ", date)
  ) %>%
  setView(lng = -87.65, lat = 41.85, zoom = 10)






#High criminal activity Heat Map

# Install heatmap plugin if needed
# devtools::install_github("rstudio/leaflet.extras")

library(leaflet.extras)

leaflet(data = crime_data_clean) %>%
  addTiles() %>%
  addHeatmap(
    lng = ~longitude,
    lat = ~latitude,
    blur = 20,
    max = 0.05,
    radius = 15
  )


# Top 10 Heatmap of crime locations

library(ggplot2)

crime_data_clean %>%
  count(primary_type, sort = TRUE) %>%
  slice_max(n, n = 10) %>%
  ggplot(aes(x = reorder(primary_type, n), y = n)) +
  geom_bar(stat = "identity", fill = "tomato") +
  coord_flip() +
  labs(title = "Top 10 Crime Types in Chicago (2018)",
       x = "Crime Type", y = "Number of Incidents") +
  theme_minimal()



#Crime by hour of day
library(lubridate)

crime_data_clean <- crime_data_clean %>%
  mutate(hour = hour(date))


crime_data_clean %>%
  count(hour) %>%
  ggplot(aes(x = as.numeric(hour), y = n)) +
  geom_line(color = "blue", size = 1.2) +
  geom_point(color = "black") +
  labs(title = "Crimes by Hour of Day",
       x = "Hour", y = "Number of Crimes") +
  theme_minimal()

#Crimes by primary type:

crime_data_clean %>%
  count(primary_type, sort = TRUE) %>%
  slice_head(n = 10) %>%
  ggplot(aes(x = reorder(primary_type, n), y = n)) +
  geom_col(fill = "tomato") +
  coord_flip() +
  labs(title = "Top 10 Crime Types in Chicago (2018)",
       x = "Crime Type", y = "Count") +
  theme_minimal()

#Crimes by month:
crime_data_clean %>%
  mutate(month = month(date, label = TRUE)) %>%
  count(month) %>%
  ggplot(aes(x = month, y = n)) +
  geom_col(fill = "steelblue") +
  labs(title = "Monthly Distribution of Crimes",
       x = "Month", y = "Number of Crimes") +
  theme_minimal()


#Density Plot
ggplot(crime_data_clean, aes(x = as.numeric(longitude), fill = primary_type)) +
  geom_density(alpha = 0.5) +
  labs(title = "Density of Crimes by Longitude", x = "Longitude", y = "Density") +
  theme_minimal() +
  theme(legend.position = "none")



#Box Plot
crime_data_clean %>%
  mutate(hour = lubridate::hour(date)) %>%
  filter(primary_type %in% c("THEFT", "BATTERY", "ASSAULT")) %>%
  ggplot(aes(x = primary_type, y = hour)) +
  geom_boxplot(fill = "orange") +
  labs(title = "Crime Time Distribution by Type", x = "Crime Type", y = "Hour of Day") +
  theme_minimal()


# Trends over time

crime_data_clean %>%
  mutate(month = lubridate::floor_date(date, "month")) %>%
  count(month) %>%
  ggplot(aes(x = month, y = n)) +
  geom_line(color = "darkgreen", linewidth = 1.3) +
  labs(title = "Monthly Crime Trend", x = "Month", y = "Number of Crimes") +
  theme_minimal()



# K-Means Clustering for Spatial Patterns

coords <- crime_data_clean %>%
  st_drop_geometry() %>%                          # Drop the geometry column first
  select(longitude, latitude) %>%
  mutate(across(everything(), as.numeric)) %>%
  drop_na()


set.seed(123)  # For reproducibility

# Apply K-Means with 5 clusters
kmeans_result <- kmeans(coords, centers = 5)

# Add cluster info back to coords
coords$cluster <- as.factor(kmeans_result$cluster)


library(ggplot2)

ggplot(coords, aes(x = longitude, y = latitude, color = cluster)) +
  geom_point(alpha = 0.5, size = 1) +
  labs(title = "Spatial Clusters of Crimes in Chicago (K-Means)",
       x = "Longitude", y = "Latitude") +
  theme_minimal() +
  scale_color_brewer(palette = "Set1")

library(leaflet)

leaflet(coords) %>%
  addTiles() %>%
  addCircleMarkers(
    lng = ~longitude, lat = ~latitude,
    color = ~cluster,
    radius = 2,
    opacity = 0.7,
    group = ~cluster
  ) %>%
  addLayersControl(
    overlayGroups = unique(coords$cluster),
    options = layersControlOptions(collapsed = FALSE)
  )




#Regression of number of crimes by hour of day

# Extract hour from time
library(lubridate)
crime_data_clean$hour <- hour(crime_data_clean$date)

# Group by hour
hourly_crimes <- crime_data_clean %>%
  filter(!is.na(hour)) %>%
  group_by(hour) %>%
  summarise(count = n())

# Plot with regression line
ggplot(hourly_crimes, aes(x = hour, y = count)) +
  geom_point(color = "darkred", size = 2) +
  geom_smooth(method = "lm", se = FALSE, color = "blue", linewidth = 1.2) +
  labs(title = "Number of Crimes by Hour with Regression Line",
       x = "Hour of Day", y = "Number of Crimes") +
  theme_minimal()

# Aggregate by community area
area_crime_stats <- crime_data_clean %>%
  group_by(community_area) %>%
  summarise(
    total_crimes = n(),
    arrests = sum(arrest == "true", na.rm = TRUE)
  ) %>%
  filter(!is.na(community_area))

# Scatter plot with regression line
ggplot(area_crime_stats, aes(x = total_crimes, y = arrests)) +
  geom_point(color = "forestgreen", size = 2) +
  geom_smooth(method = "lm", se = TRUE, color = "darkblue", linewidth = 1.2) +
  labs(title = "Arrests vs Total Crimes by Community Area",
       x = "Total Crimes", y = "Arrests") +
  theme_minimal()


ggplot(coords, aes(x = longitude, y = latitude)) +
  geom_point(alpha = 0.3, size = 1, color = "gray") +
  geom_smooth(method = "lm", se = FALSE, color = "red", linewidth = 1) +
  labs(title = "Spatial Regression: Latitude vs Longitude",
       x = "Longitude", y = "Latitude") +
  theme_minimal()



#Pie Chart of Crime Type

crime_type_summary <- crime_data_clean %>%
  count(primary_type) %>%
  top_n(10, wt = n)  # Top 10 crime types

ggplot(crime_type_summary, aes(x = "", y = n, fill = primary_type)) +
  geom_col(width = 1) +
  coord_polar(theta = "y") +
  labs(title = "Top 10 Crime Types (Pie Chart)") +
  theme_void()

#  SAVE

saveRDS(crime_data_clean, "crime_data_clean.rds")










