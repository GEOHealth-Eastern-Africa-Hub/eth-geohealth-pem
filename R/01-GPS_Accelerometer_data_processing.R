###############################################################################
## ATMOTUBE GPS ACCELEROMETER DATA PROCESSING
## Author: Initial script shared by US team (Prof. Steve, Prof. Jack Darby)
## Date: SA 02/09/2024
## Adopted & modified by: Getu GARI - getu.gari@reach-initiatives.org

# Load required libraries

library(tidyverse)
library(readr)
library(lubridate)
library(zoo)
library(leaflet)
library(ggplot2)
library(plotly)
library(htmlwidgets)

#__________________________________________________________#

                    ## READ BEFORE YOU START ###
# this code assumes you kept the Atmotube and GPS data in a same folder.
# the name for GPS data files starts with 'DATA'

## WHAT CHANGES YOU NEED TO MAKE TO RUN THIS CODE ##
# (1) DATA DIRECTOTY: You need to set working directory to the folder where all 
# the data are kept. The code will read all GPS data form the directory, There's no
# need to import file one by one. 

# (2) You need to update the Atmotube data file name in line 227. Android and iPhones have different file naming protocols and annoyingly small changes to the column headings.  
# If you downloaded the Atmotube data on an iPhone then, please rename the default filename by removing all of the slashes (/)
# from the filename. For example,The file "CB/1E/CE/F9/B1/BB-09-Oct-2023-00-55-02.csv" can be renamed
# to "BB-09-Oct-2023-00-55-02.csv". Android downloads don't have slashes. 

# (3) Atmotube records data in local time zone. Currently the time-zone for the data is set for Nairobi.
# If you move from Nairobi enter the new timezone > tz = "Africa/Nairobi"
# Note this code will produce 3 csv files: (1) Flat CSV (1-min) for GPS data, (2) Time worn per day (3) Flat CSV combining GPS and Atmotube data
# On R studio, you will get 3 interactive map (1) Average movement (2) worn (3) PM2.5
#__________________________________________________________#

#### Set Working Directory #### -----------------------------------------------
## Set the working directory to your folder containing the files. You can set WD manually by clicking session>set working directory>set directory
## Example: setwd("/path/to/your/data/folder")
setwd("C:/Users/Getu Gari/Desktop/eth-geohealth-pem/inputs/GCDCData XB-89e6")

#### Import GPS data #### -----------------------------------------------------
# List all files in the folder and filter for filenames starting with 'DATA':
file_list <- list.files(pattern = "DATA", full.names = TRUE)

# Define column names for GPS data
column_names <- c("Time", "Ax", "Ay", "Az", "P", "T", "TOW", "Lat", "Lon", "Height(m)", "MSL(m)", "hdop(m)", "vdop(m)")

# Import and concatenate the filtered files
GPS_data <- data.frame()
for (file_path in file_list) {
  data <- read.csv(file_path, comment = ";", header = FALSE, col.names = column_names)
  GPS_data <- bind_rows(GPS_data, data)
}

# Check summary and view data
summary(GPS_data)
# View(GPS_data)

#### Extract Serial Number and Start Date from Header ####
# we want to use serial number and start time to name the exported files for this code
first_file = substr(file_list[1], 3, nchar(file_list[1]))
# Process the header lines to extract Start_time
header_lines <- readLines(first_file)
# Loop through the header lines to find the Start_time information
for (line in header_lines) {
  if (grepl(";Start_time,", line)) {
    start_time <- gsub("^.*;Start_time,\\s*", "", line)
    break  # Stop searching once found
  }
}

print(paste("Start Time:", start_time))

#### Function to Sanitize File Names ####
sanitize_filename <- function(filename) {
  # Replace colons, commas, and spaces with underscores
  filename <- gsub(":", "_", filename)  # Replace colons
  filename <- gsub(",", "_", filename)  # Replace commas
  filename <- gsub(" ", "_", filename)  # Replace spaces
  return(filename)
}

# Sanitize the start_time to remove invalid characters
start_time_clean <- sanitize_filename(start_time)

# # Loop through the header lines to find the Serial number information
for (line in header_lines) {
  if (grepl("SN:", line)) {
    SN <- gsub("^.*;SN,\\s*", "", line)
    break  # Stop searching once found
  }
}

SN <- substr(SN, nchar(SN) - 3, nchar(SN))
print(paste("Serial Number:", SN))

#### Process GPS Data #### ----------------------------------------------------
# convert UNIX time to GMT datetime format by adjusting  
# the unix time from Jan 1st 1970 by the GPS time fix in unix seconds
GPS_data$Time <- as.numeric(GPS_data$Time)
GPS_data$Time <- as_datetime("1970-01-01 00:00:00") + GPS_data$Time

#  RSS calculation: take Ax, Ay and Az readings- and take the square root of hte sum of the squares-
GPS_data$RSSxyz <-
  sqrt(GPS_data$Ax **2 + GPS_data$Ay **2 + GPS_data$Az **2)

# plot RSS over time
plotRRS <- GPS_data |>
  ggplot(aes(x = Time, y = RSSxyz)) +
  geom_line(color = 'blue') +
  ggtitle("RSS with default frequency") +
  theme(axis.text.x = element_text(size = 14), # change this number if you want to change font size
        axis.text.y = element_text(size = 14))
# review plot on R studio
#plotRRS

# save in HTML and load it in browser for interactive plot
#plot_filename <- paste0(SN, "_", start_time_clean, "RSS_of_X_Y_Z_at_default_frequency.html")
#saveWidget(ggplotly(plotRRS), file = plot_filename)

## then look at the standard deviation of this over each minute
## create a new field with minutes
GPS_data <- GPS_data |>
  mutate(datetime_minute = floor_date(Time, unit = "minute"))

# df group by minutes
GPS_data <- GPS_data |>
  group_by(datetime_minute) |>
  summarize_all(.funs = list(mean, sd), na.rm = TRUE)

# Rename columns for clarity
names(GPS_data)[names(GPS_data) == "RSSxyz_fn2"] <- "SD_of_RSS"

# plot SD over time
plotSD_min <- GPS_data |>
  ggplot(aes(x = datetime_minute, y = SD_of_RSS)) +
  geom_line(color = 'blue') +
  ggtitle("SD of RSS with minute frequency") +
  theme(axis.text.x = element_text(size = 14), # change this number if you want to change font size
        axis.text.y = element_text(size = 14))
# run to see plot in Rstudio
#plotSD_min

# Save HTML Plot
#plot_filename <- paste0(SN, "_", start_time, "_RSS_of_X_Y_Z_SD_of RSS with minute frequency.html")
#saveWidget(ggplotly(plotSD_min), file = plot_filename)

##### Post doc suggested setting threshold in following way: Threshrshold: it was found from the data, SD = 1.6 when no movement
## percentile_20 <- quantile(GPS_data$SD_of_RSS, probs = 0.20, na.rm = TRUE)
## threshold <- percentile_20*3
## However, SNC ust chose time periods in middle of night on all three units and got 
## ave SD during quiet periods of 2.27, 2.46, and 2.60 for accelerometer units A74B, 9AB3 and 3BC2 respectively.
## thus chose worst of the three units for threshold (2.60*3)
threshold <- 2.60 * 3
print(paste("Threshold:", threshold))

# assign boolean value of 1 if std > threshold and 0 if std ≤ threshold 
# (call this variable movement)
GPS_data$movement <- ifelse(GPS_data$SD_of_RSS > threshold, 1, 0)

## plot MOVEMENT VERIABLE
plot_movement <- GPS_data |>
  ggplot(aes(x = datetime_minute, y = movement)) +
  geom_line(color = 'blue') +
  ggtitle("MOVEMENT") +
  theme(axis.text.x = element_text(size = 14), # change this number if you want to change font size
        axis.text.y = element_text(size = 14))
# run to see plot in Rstudio
#plot_movement

# Save HTML Plot
#plot_filename <- paste0(SN, "_", start_time_clean, "_movement.html")
#saveWidget(ggplotly(plot_movement), file = plot_filename)

## Then look at 10 minute moving average (looking backwards) of these movement 
## boolean values (call this average movement)
## Define the window size for the moving average
window_size <- 10
# Reverse the data
reversed_data <- rev(GPS_data)
# Calculate the backward moving average
GPS_data$average_movement <- rollapply(reversed_data$movement, width = window_size, FUN = mean, align = "right", fill = NA)

## plot AVERAGE MOVEMENT VERIABLE
plot_avg_movement <- GPS_data |>
  ggplot(aes(x = datetime_minute, y = average_movement)) +
  geom_line(color = 'blue') +
  ggtitle("AVERAGE MOVEMENT") +
  theme(axis.text.x = element_text(size = 14), # change this number if you want to change font size
        axis.text.y = element_text(size = 14))
# run to see plot in Rstudio
#plot_avg_movement

# Save HTML Plot
#plot_filename <- paste0(SN, "_", start_time_clean, "_average_movement.html")
#saveWidget(ggplotly(plot_avg_movement), file = plot_filename)

## Then if moving average is > 0, assign new boolean value of 1; 
## if = 0 then assign boolean value of 0 (call this variable worn)
GPS_data$worn <- ifelse(GPS_data$average_movement > 0, 1, 0)

## plot WORN VERIABLE
worn <- GPS_data |>
  ggplot(aes(x = datetime_minute, y = worn)) +
  geom_line(color = 'blue') +
  ggtitle("WORN")+
  theme(axis.text.x = element_text(size = 14), # change this number if you want to change font size
        axis.text.y = element_text(size = 14))
# run to see plot in Rstudio
#worn

# Save HTML Plot
#plot_filename <- paste0(SN, "_", start_time_clean, "_worn.html")
#saveWidget(ggplotly(worn), file = plot_filename)

## Sum the worn boolean variable “worn” over each 24 hr period from start of 
## deployment to get minutes worn/day. First 24h runtime would be day 1 and so on
GPS_data <- GPS_data |>
  mutate(day_interval = ceiling(as.numeric(GPS_data$datetime_minute - GPS_data$datetime_minute[1]) / 86400))
GPS_data <- GPS_data[-1,]

#### Calculate Hours Worn Per Day ####
# Min worn per day - create a new csv
df_day <- GPS_data |>
  group_by(day_interval) |>
  summarize(Hours_worn = sum(worn, na.rm = TRUE)/60,
            Total_deployment_hours = sum(day_interval, na.rm = TRUE)/60)
print('Hours Worn Per day')
print(df_day)

# Export hours worn per day with sanitized file name
exportCSV_filename <- paste0(SN, "_", start_time_clean, "_Hours_Worn_Per_day.csv")
write.csv(df_day, exportCSV_filename)

#### Export Flat GPS Data ####
df_final_GPS <- GPS_data |>
  select(datetime_minute, Ax_fn1, Ay_fn1, Az_fn1, RSSxyz_fn1, SD_of_RSS, movement, average_movement, worn, Lat_fn1,  Lon_fn1, 'Height.m._fn1', 'MSL.m._fn1', "hdop.m._fn1", "vdop.m._fn1")
colnames(df_final_GPS) <- c("TimeStamp", "X__m/s2", "Y__m/s2", "Z__m/s2", "RSSxyz__m/s2", "SD_RSS__m/s2", "Movement","average_movement", "worn", "original_latitude", "original_longitude", "Height__m", "MSL__m", "hdop__m", "vdop__m")

#### Clean latitude and longitude based on HDOP
# if HDOP value is >20 or < 0, change it to NULL
df_final_GPS$cleaned_latitude <- ifelse(df_final_GPS$hdop__m > 20 | df_final_GPS$hdop__m < 1, NA, df_final_GPS$original_latitude)
df_final_GPS$cleaned_longitude <- ifelse(df_final_GPS$hdop__m > 20 | df_final_GPS$hdop__m < 1, NA, df_final_GPS$original_longitude)

exportCSV_filename <- paste0(SN, "_", start_time_clean, "_Flat_GPS_Accelerometer_data_minute_average.csv")
write.csv(df_final_GPS, exportCSV_filename)

###############################################################################