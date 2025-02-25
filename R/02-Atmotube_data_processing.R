############## ATMOTUBE #####################

# INSTRUCTION: ATMOTUBE file name contains slashes (/), which confuses R, because slash(/)
# usually used for directory. Please rename the file name to remove slashes. For example,
# The file I was gives was "CB/1E/CE/F9/B1/BB-09-Oct-2023-00-55-02.csv", I changed it to 
# "BB-09-Oct-2023-00-55-02.csv" and load it in R

#### Import and Process Atmotube Data #### ------------------------------------
Atmotube_data <- read.csv("inputs/D8FCD3DCCC3C_23_Jul_2024_14_58_47.csv")
Atmotube_SN <- "D8FC"  # Removed double quotes

View(Atmotube_data)
# currrent timezone is for east africa, if you move to some other place then we need to change timezone
Atmotube_data$Date <- as.POSIXct(Atmotube_data$Date ,format = "%m/%d/%Y %H:%M", tz = "Africa/Addis_Ababa")

# Convert the local datetime to GMT to match with GPS/accelerometer data
Atmotube_data$Date <- with_tz(Atmotube_data$Date, tz = "GMT")
names(Atmotube_data)

## rename column name
## Android: if the data is downloaded from Android phone: 
colnames(Atmotube_data)[4] <- "Temperature__C"
colnames(Atmotube_data)[5] <- "Humidity__percent"
colnames(Atmotube_data)[8] <- "PM25__ugm3"
colnames(Atmotube_data)[6] <- "Pressure__hPa"

colnames(Atmotube_data)
view(Atmotube_data)
names(Atmotube_data)
Atmotube_data <- Atmotube_data[c("Date", "PM25__ugm3", "Temperature__C","Pressure__hPa","Humidity__percent")]

# run after running the GPS_Accelerometer script
##############. MERGE ATMOTUBE WITH GPS DATA ##############
merged_df <- merge(df_final_GPS, Atmotube_data, by.x = "TimeStamp", by.y = "Date", all.x = TRUE) 
view(merged_df)

# export flat merged data
exportCSV_filename <- paste0(SN, "_", start_time_clean, "_Flat_Merged_Atmotube_and_GPS_data.csv")
write.csv(merged_df, exportCSV_filename)

################## ATMOTUBE PLOTS ##################
## Finally- an updated script that can make html plots of an atmotube file for PM2.5 time-series plot, Temp time series plot and RH time series plot and a Pressure time series plot 

# PM2.5 plot
PM25 <- merged_df |>
  ggplot(aes(x = TimeStamp, y = PM25__ugm3)) +
  geom_line(color = 'blue') +
  ggtitle('PM2.5 concentration') +
  theme(axis.text.x = element_text(size = 14), # change this number if you want to change font size
        axis.text.y = element_text(size = 14))
PM25
plot_filename <- paste0(Atmotube_SN, "_PM2.5 concentration.html")
saveWidget(ggplotly(PM25), file = plot_filename)

# RH
RH <- merged_df |>
  ggplot(aes(x = TimeStamp, y = Humidity__percent)) +
  geom_line(color = 'blue') +
  ggtitle('Humidity (percent)') +
  theme(axis.text.x = element_text(size = 14), # change this number if you want to change font size
        axis.text.y = element_text(size = 14))
RH
plot_filename <- paste0(Atmotube_SN, "_Humidity.html")
saveWidget(ggplotly(RH), file = plot_filename)

# Pressure
pressure <- merged_df |>
  ggplot(aes(x = TimeStamp, y = Pressure__hPa)) +
  geom_line(color = 'blue') +
  ggtitle('Pressure (hPa)') +
  theme(axis.text.x = element_text(size = 14), # change this number if you want to change font size
        axis.text.y = element_text(size = 14))
pressure
plot_filename <- paste0(Atmotube_SN, "_pressure.html")
saveWidget(ggplotly(pressure), file = plot_filename)

################## MAPS ##################
# for map remove rows where GPS is not properly fixed.  good hdop range 1 - 20
merged_df <-merged_df |> 
  filter(hdop__m >= 1, hdop__m <= 20)

# if there is no good GPS fixes then error messages will pop up saying
# "Error in .getNamespace(pkg) : 
#    invalid type/length (symbol/0) in vector allocation"
# but since the maps are the last plots to be made all 
# the other output has already occured (csv files and html time series plots)

view(merged_df)

################## MAPPING FUNTION ##################
generate_map_with_legend <- function(data, column, map_title) {
# Check if there are actual values in the 'cleaned_latitude' column
  if (!all(is.na(data$cleaned_latitude))) {
    unique_values <- unique(data[[column]])
    
    if (length(unique_values) == 1) {
# If there's only one unique value, set a specific color for it (e.g., green)
      map <- leaflet(data = data) %>%
        addProviderTiles(providers$CartoDB.Positron) %>%
        addCircles(lng = ~cleaned_longitude, 
                   lat = ~cleaned_latitude,
                   color = "green",  # Use a specific color (e.g., green)
                   weight = 4,
                   opacity = 1) %>%
        addLegend(position = "bottomright", # Position of the legend
                  colors = "green",  # Color for the legend
                  labels = map_title,  # Label for the legend
                  title = "Legend Title",  # Title of the legend
                  opacity = 1)
      } 
    
    else if(length(unique_values) == 2){
      
      palette <- colorNumeric(c("#00FF00", "#FF0000"), domain = unique_values)
      map <- leaflet(data = data) %>%
        addProviderTiles(providers$CartoDB.DarkMatter) %>%
        addCircles(lng = ~original_longitude, 
                   lat = ~original_latitude,
                   color = ~palette(data[[column]]),  # Use the color palette
                   weight = 4, 
                   opacity = 1) %>%
        addLegend(position = "bottomright",
                  values = c(0, 1),  # Values to be displayed in the legend
                  colors = c("#00FF00", "#FF0000"),  # Specify the legend colors
                  labels = c("Not Worn", "Worn"),  # Labels for the legend
                  title = "Legend Title",  
                  opacity = 1)
      }
    else {
# If there are multiple unique values, use a color ramp
      palette <- colorNumeric("plasma", domain = unique_values)
      
      map <- leaflet(data = data) %>%
        addProviderTiles(providers$CartoDB.DarkMatter) %>%
        addCircles(lng = ~cleaned_longitude, 
                   lat = ~cleaned_latitude,
                   color = ~palette(data[[column]]),  # Use the color palette
                   weight = 4, 
                   opacity = 1) %>%
        addLegend(position = "bottomright",
                  pal = palette,  # Color palette
                  values = unique_values,  # Values to be displayed in the legend
                  title = map_title,  # Title of the legend
                  opacity = 1)
    }
    
  return(map)
    } else {
# No cleaned coordinate data available for mapping
    print("NO cleaned coordinate data available for mapping")
    return(NULL)
  }
}

#__________________________________________________________#
########## call function for maps  ##########

# average movement
average_movement <- generate_map_with_legend(data = merged_df, column = "average_movement", map_title = "Average Movement")
average_movement # Display the map
plot_filename <- paste0(SN, "_", start_time_clean, "_MAP_Average_Movement.html") # Save the map as an HTML file
saveWidget(average_movement, file = plot_filename)

# worn
average_movement <- generate_map_with_legend(data = merged_df, column = "worn", map_title = "Worn")
average_movement # Display the map
plot_filename <- paste0(SN, "_", start_time_clean, "_Map_Worn.html") # Save the map as an HTML file
saveWidget(average_movement, file = plot_filename)

# PM2.5
average_movement <- generate_map_with_legend(data = merged_df, column = "PM25__ugm3", map_title = "PM2.5")
average_movement # Display the map
plot_filename <- paste0(SN, "_", start_time_clean, "_MAP_PM25_Atmotube.html") # Save the map as an HTML file
saveWidget(average_movement, file = plot_filename)

