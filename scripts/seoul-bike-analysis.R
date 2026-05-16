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
# Load packages
library(ggplot2)

# (Optional but recommended) start clean:
rm(list = ls())

# (Optional) make printing easier to read:
options(stringsAsFactors = FALSE)

# If you use packages, load them here.
# Only load what you actually use.

# Get current working directory
getwd() 

# Change directory (if necessary)
# setwd() 

# List files inside directory
list.files()

# Read CSV into a data frame
df <- read.csv('data/SeoulBikeData.csv', fileEncoding = "CP949")

############################
# EDA
############################
# Inspection
str(df)

head(df)

############################
# PLOTS
############################
daily_df <- aggregate(Rented.Bike.Count ~ Date + Seasons,
          data=df, sum)

head(daily_df)

box1 <- ggplot(
  daily_df,
  aes(
    x=Seasons,
    y=Rented.Bike.Count,
    fill=Seasons
  )
) +
  geom_boxplot() + 
  labs(
    title="Daily Bike Rental Count by Season",
    y="Rental Count"
  )

ggsave("boxplot1.png", plot = box1)
