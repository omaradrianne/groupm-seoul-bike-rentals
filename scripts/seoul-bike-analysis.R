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

################################################################################
# SETUP
################################################################################

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

################################################################################
# EDA + Cleaning
################################################################################
# Rename variables
colnames(df)
colnames(df)[c(2,4:11)] <- c(
  "Count", # RENTAL COUNT
  "Temp.c",
  "Humidity",
  "Wind.Speed.m/s",
  "Visibility.10m",
  "Dew.Point.Temp.c",
  "Solar.Radiation.MJ.m2",
  "Rainfall.mm",
  "Snowfall.cm"
)
colnames(df)

# Inspection
str(df)
head(df)
summary(df)

# Correlation inspection
cor(df$Temp.c, df$Count)
cor(df$Humidity, df$Count)
cor(df$Hour, df$Count)

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
head(df)

################################################################################
# Summary Statistics
################################################################################
#mean of bike rentals by season
avg_Seasons <-aggregate(Count ~ Seasons, data = df, mean) 

# mean visibility by season
avg_Visibility <- aggregate(`Visibility.10m` ~ Seasons, data = df, mean) 

# mean of bike rentals separated by weekday and weekend
aggregate(Count ~ Day.Type, data = df, mean) 

# means rental count by holidays vs non-holidays
aggregate(Count ~ Holiday, data = df, mean) 

# mean rental count by weekday
avg_weekday <- aggregate(Count ~ Weekday, data = df, mean) 

# mean rental count by day type and holiday status
aggregate(Count ~ Day.Type + Holiday, data = df, mean) 

# mean rentals based on time/hour of day
avg_hour <- aggregate(Count ~ Hour, data = df, mean)

# mean rental count by hour and day type
avg_hour_day <- aggregate(Count ~ Hour + Day.Type, data = df, mean)

# top renting hours
avg_hour[order(-avg_hour$Count), ]


################################################################################
# PLOTS
################################################################################

############################
# WEATHER/SEASON PLOTS
############################

# Create a new data frame where we group rental count by date and season
daily_df <- aggregate(Count ~ Date + Seasons,
          data=df, sum)

# Inspection
head(daily_df)

# Box plot: Bike Rentals & Season
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

# Box plot: Visibility & Season
box2 <- ggplot(
  df,
  aes(
    x= Seasons,
    y= Visibility.10m,
    fill=Seasons
  )
) +
  geom_boxplot() + 
  labs(
    title="Visibility by Season",
    x = "Season",
    y = "Visibility (10m)"
  )
box2

# Graph of Visbility by Season
trend_visibilitySeason <- ggplot(
  avg_Visibility, aes(x = Seasons, y = Visibility.10m, group = 1)
) + geom_line(color = "cyan3", linewidth = 1) +
  geom_point(size = 3, color = "coral2") +
  labs (
    title = "Visibility Trends by Season",
    x = "Season",
    y = "Visibility (10m)"
  )
trend_visibilitySeason

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
    x="Humidity (%)",
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

#Boxplot: bike rentals by type of day
box3 <- ggplot(
  df, aes(x = Day.Type, y = Count, fill = Day.Type)
) + 
  geom_boxplot() + 
  labs(
    title = "Hourly Bike Rentals: Type of Day",
    x = "Day Type",
    y = "Rental Count"
  )
box3

#graph of bike rental trends by day of week
trend_dayweek <- ggplot(
  avg_weekday, aes(x = Weekday, y = Count, group = 1)
) + geom_line(color = "cyan3", linewidth = 1) +
  geom_point(size = 3, color = "coral2") +
  labs (
    title = "Hourly Bike Rental Trends by Day of the Week",
    x = "Day of Week",
    y = "Average Hourly Bike Rentals"
  )
trend_dayweek

#Boxplot: Bike rents Holidays
box4 <- ggplot(
  df, aes(x = Holiday, y = Count, fill = Holiday)
) + 
  geom_boxplot() + 
  labs(
    title = "Hourly Bike Rentals: Holidays",
    x = "Holiday Status",
    y = "Rental Count"
  )
box4

#Boxplot: Bike rentals Weekdays vs Weekends & Holidays

box5 <- ggplot(
  df, aes(x = Day.Holiday, y = Count, fill = Day.Holiday)
) + 
  geom_boxplot() + 
  labs(
    title = "Hourly Bike Rentals by Day Type & Holiday Status",
    x = "Day Type & Holiday Status",
    y = "Rental Count"
  )
box5

############################
# Temporal Patterns PLOTS
############################



trend_time <- ggplot(
  avg_hour, aes(x = Hour, y = Count)
) + geom_line(color = "coral2", linewidth = 1) +
  labs(
    title = "Average Hourly Bike Rentals",
    x = "Time of Day",
    y = "Average Rental Count"
  )
trend_time

#trend on hour avg bike rentals, colored by Day Type
trend_hourDay <- ggplot(
  avg_hour_day,aes(x = Hour, y = Count, color = Day.Type)
) + geom_line(linewidth = 1) +
  labs(
    title = "Hourly Rental Trend: Weekdays vs Weekends",
    x = "Time of day",
    y = "Average Rental Count"
  )

trend_hourDay


################################################################################
# ANALYSIS
################################################################################################################

# 95% confidence interval for the mean hourly rental count
t.test(df$Count)$conf.int

############################
# WEATHER
############################

#regression
weather <- lm(
  Count ~ Temp.c + Humidity + `Wind.Speed.m/s` +
    Visibility.10m + Rainfall.mm + Snowfall.cm,
  data = df
)
summary(weather)

# correlation matrix, relationship between variables
cor(
  df[, c(
    "Count",
    "Temp.c",
    "Humidity",
    "Wind.Speed.m/s",
    "Visibility.10m",
    "Rainfall.mm",
    "Snowfall.cm"
  )]
)

#ANOVA to see if visibility varies across seasons
model1 <- aov(Visibility.10m ~ Seasons, data = df)
summary(model1)

############################
# Holiday/Weekends
############################

#Test whether mean hourly bike rentals differ between Day Types
t.test(Count ~ Day.Type, data = df)
#Test whether mean hourly bike rentals differ between Holiday Status
t.test(Count ~ Holiday, data = df)

#ANOVA###################################

#TWO WAY AOV to see if avg rentals are different depending on day type and holiday
model2 <- aov(Count ~ Day.Type * Holiday, data = df)
summary(model2)

#ANOVA to see if avg rentals a different across days
model3 <- aov(Count ~ Weekday, data = df)
summary(model3)


############################
# Temporal (time of day)
############################

#Regression
time <- lm(Count ~ Hour, data = df)
summary(time)

#ANOVA to see if avg rentals are different depending on time
model4 <- aov(Count ~ as.factor(Hour), data = df)
summary(model4)

#############################
# Strongest Associations (Q4)
#############################

predictorModel <- lm(Count ~ 
                       Temp.c + Humidity + `Wind.Speed.m/s`+ Visibility.10m + Rainfall.mm + Snowfall.cm + 
                       Day.Type + Seasons + Holiday + Hour, 
                       data = df)
summary(predictorModel)

# Sorted Strongest Predictors using T-Value from above

predictorStrongest <- summary(predictorModel)$coefficients
predictorStrongest[order(abs(predictorStrongest[, "t value"]), decreasing = TRUE),]



