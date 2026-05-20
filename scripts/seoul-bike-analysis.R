################################################################################
# MATH 167R — Statistical Programming with R
# Instructor: Andrea Gottlieb
# Group-M — Project
# Date: May 20, 2026
#
# Group Members:
# * Daisy Sandoval
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
# install.packages("patchwork")
library(patchwork)

# Get current working directory
getwd() 

# Change directory (if necessary)
# setwd() 

# List files inside directory
list.files()

# Read CSV into a data frame
df <- read.csv('data/SeoulBikeData.csv', fileEncoding = "CP949")

################################################################################################################
# EDA + Cleaning
################################################################################################################
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

# Inspection
str(df)
head(df)
summary(df)

# Correlation inspection
cor(df$Temp.c, df$Count)
cor(df$Humidity, df$Count)

#Date/Week Cleaning
df$Date <- as.Date(df$Date, format="%d/%m/%Y")
df$Weekday <- weekdays(df$Date)

df$Day.Type <- ifelse(
  df$Weekday %in% c("Saturday", "Sunday"),
  "Weekend", "Weekday"
)

df$Day.Holiday <- paste(df$Day.Type, df$Holiday, sep = " - ")

df$Weekday <- factor (
  df$Weekday, levels = c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday", "Sunday")
) 

#Checks
table(df$Weekday)
table(df$Day.Type)
table(df$Holiday)

################################################################################################################
# Summary Statistics
################################################################################################################
aggregate(Count ~ Day.Type, data = df, mean) # mean of bike rentals separated by weekday and weekend
aggregate(Count ~ Holiday, data = df, mean) # means of holidays and non-holidays
avg_weekday <- aggregate(Count ~ Weekday, data = df, mean) # mean of days
aggregate(Count ~ Day.Type + Holiday, data = df, mean) # mean of days and whether they are holidays


################################################################################################################
# PLOTS
################################################################################################################

############################
# WEATHER/SEASON PLOTS
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
# ggsave("boxplot1.png", plot=box1)

# Linear regression plots
# Temperature vs Rental Count
reg1 <- ggplot(
  df,
  aes(
    x=Temp.c,
    y=Count
  )
) +
  geom_point(alpha=0.6, color="steelblue") +
  geom_smooth(method="lm", se=FALSE, color="darkred") +
  labs(
    title="Temperature vs Hourly Rental Count",
    x="Temperature (˚C)",
    y="Rental Count"
  )
reg1
# Save linear regression plot as a png
# ggsave("regplot1.png", plot=reg1)

# Humidity vs Rental Count
reg2 <- ggplot(
  df,
  aes(
    x=Humidity,
    y=Count
  )
) +
  geom_point(alpha=0.6, color="steelblue") +
  geom_smooth(method="lm", se=FALSE, color="darkred") +
  labs(
    title="Humidity vs Hourly Rental Count",
    x="Humidity",
    y="Rental Count"
  )
reg2
# Save linear regression plot as a png
# ggsave("regplot2.png", plot=reg2)

# Display reg1 and reg2 side by side
reg_panel1 = reg1 + reg2
reg_panel1
# ggsave("reg_panel1.png", plot=reg_panel1)


############################
# Weekday/Holiday PLOTS
############################

#boxplot of bike rentals by type of day
box2 <- ggplot(
  df, aes(x = Day.Type, y = Count, fill = Day.Type)
) + 
  geom_boxplot() + 
  labs(
    title = "Hourly Bike Rentals: Type of Day",
    x = "Day Type",
    y = "Rental Count"
  )
box2

#graph of bike rental trends by day of week
trend_dayweek <- ggplot(
  avg_weekday, aes(x = Weekday, y = Count, group = 1)
) + geom_line(color = "steelblue", linewidth = 1) +
  geom_point(size = 3, color = "darkred") +
  labs (
    title = "Bike Rental Trends by Day of the Week",
    x = "Day of Week",
    y = "Average Bike Rental Count"
  )
trend_dayweek

# Bike rents Holidays
box3 <- ggplot(
  df, aes(x = Holiday, y = Count, fill = Holiday)
) + 
  geom_boxplot() + 
  labs(
    title = "Hourly Bike Rentals: Holidays",
    x = "Holiday Status",
    y = "Rental Count"
  )
box3

# Bike rentals Weekdays vs Weekends & Holidays

box4 <- ggplot(
  df, aes(x = Day.Holiday, y = Count, fill = Day.Holiday)
) + 
  geom_boxplot() + 
  labs(
    title = "Hourly Bike Rentals by Day Type & Holiday Status",
    x = "Day Type & Holiday Status",
    y = "Rental Count"
  )
box4

############################
# Temporal Patterns PLOTS
############################


################################################################################################################
# ANALYSIS
################################################################################################################

# 95% confidence interval for the mean hourly rental count
t.test(df$Count)$conf.int

############################
# WEATHER
############################
weather <- lm(
  Count ~ Temp.c + Humidity + Wind.Speed.m/s +
    Visibility.10m + Rainfall.mm + Snowfall.cm,
  data = df
)

summary(weather)

############################
# Holiday/Weekends
############################

#Test whether mean hourly bike rentals differ between Day Types
t.test(Count ~ Day.Type, data = df)
#Test whether mean hourly bike rentals differ between Holiday Status
t.test(Count ~ Holiday, data = df)

#ANOVA###################################
model1 <- aov(Count ~ Day.Type * Holiday, data = df)
summary(model1)




