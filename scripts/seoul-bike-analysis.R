################################################################################
# MATH 167R — Statistical Programming with R
# Instructor: Andrea Gottlieb
# Group-M — Project
# Date: May 20, 2026
#
# Group Members:
# * Daisy Sandoval
# * Arnav Chawla
# * Omar Adrianne Bapora
#
# Description: Group project analyzing Seoul bike rental demand
# Notes:
# - 
# -
# -
################################################################################

############################
# SETUP
############################

# (Optional but recommended) start clean:
rm(list = ls())

# (Optional) make printing easier to read:
options(stringsAsFactors = FALSE)

# Load packages
library(ggplot2)

# Get current working directory
getwd() 

# Change directory (if necessary)
# setwd() 

# List files inside directory
list.files()

# Read CSV into a data frame
df <- read.csv('data/SeoulBikeData.csv', fileEncoding = "CP949")

############################
# EDA + Cleaning
############################
# Inspection
str(df)

head(df)

summary(df)

# Rename variables
colnames(df)
colnames(df)[2] <- "Count" # RENTAL COUNT
colnames(df)[4] <- "Temp.c"
colnames(df)[5] <- "Humidity"
colnames(df)[6] <- "Wind.Speed.m/s"
colnames(df)[7] <- "Visibility.10m"
colnames(df)[8] <- "Dew.Point.Temp.c"
colnames(df)[9] <- "Solar.Radiation.MJ.m2"
colnames(df)[10] <- "Rainfall.mm"
colnames(df)[11] <- "Snowfall.cm"
colnames(df)

# Correlation inspection
cor(df$Temp.c, df$Count)
cor(df$Humidity, df$Count)

############################
# PLOTS
############################
# Create a new data frame where we group rental count by date and season
daily_df <- aggregate(Count ~ Date + Seasons,
          data=df, sum)

# Inspection
head(daily_df)

# Box plot
box1 <- ggplot(
  daily_df,
  aes(
    x=Seasons,
    y=Count,
    fill=Seasons
  )
) +
  geom_boxplot() + 
  labs(
    title="Daily Bike Rental Count by Season",
    y="Rental Count"
  )
box1
# Save box plot as a png
# ggsave("boxplot1.png", plot = box1)


