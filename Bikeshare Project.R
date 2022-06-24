setwd("Z:/Amanda's Stuff/School/Udacity")

getwd()
list.files()

install.packages("ggplot2")
library(ggplot2)

install.packages('ggthemes', dependencies = TRUE)
library(ggthemes)

install.packages('plyr') # Used to append dataframes with differing columns
library(plyr)

install.packages('hms') # Used to manipulate time data
library(hms)

ny = read.csv('new-york-city.csv')
dc = read.csv('washington.csv')
chi = read.csv('chicago.csv')

# Creating unified dataframe for ease of analysis
df = rbind.fill(ny, dc)
df = rbind.fill(df, chi)

# Creating subset of unified df to reduce redundant code
df_sub = subset(df,User.Type == "Customer" | User.Type == "Subscriber")

# Generating summary statistics to determine outliers
summary(df$Trip.Duration/60)
trip_outlier = ((18.92 - 6.48) * 1.5) + 18.92
# Outlier threshold is 37.58,
# plot will use a ylim of 60 as an hour is a more natural time division

# Additional summary statistics
by(df$Trip.Duration/60, df$User.Type, summary)

ggplot(aes(x = User.Type, y = Trip.Duration/60), data = df_sub) +
  geom_boxplot()+
  coord_cartesian(ylim = c(0, 60)) +
  scale_y_continuous(breaks = seq(0, 60, 10)) +
  ggtitle('Trip Duration') +
  xlab('User Type') +
  ylab('Trip Duration (Minutes)') +
  theme_economist()

# Converting Start.Time into POSIXct type
df_sub$Timestamp = format(as.POSIXct(df_sub$Start.Time), format = "%H:%M:%S")
df_sub$Timestamp <- as_hms(df_sub$Timestamp)

ggplot(aes(x = Timestamp), data = df_sub, xaxt = "n") +
  geom_freqpoly(aes(color = User.Type), size = 1.5, bins = 24) +
  axis.Date(1, at = df_sub$Timestamp, labels = format(df_sub$Timestamp, "H%:M%"),
                                               las = 2)
  #scale_x_continuous(breaks =
                       c(0, 6, 12, 18, 24))
