# Project COVID
# Get reliable data on new cases of COVID19 reported on a daily basis in India
# No of cases plotted against "New cases confirmed", "Deaths" and "Recovered"
# Select 8 countries which were impacted most

# Data source https://www.kaggle.com/sudalairajkumar/covid19-in-india

# Loading libraries required
library(ggplot2)
library(gganimate)
library(gifski)
library(dplyr)
library(data.table)
library(grid)
library(png)
library(RCurl)
library(stringr)

# Setting the theme
theme_set(theme_light(base_size = 14))

# Loading data in .csv
final_data <- read.csv('final_data_plot.csv')

# Replacing United Kingdom as UK to save space in x-axis
final_data$Country = str_replace_all(final_data$Country,"United Kingdom","UK")

# Converting the Nature_of_Case into factor
final_data$Nature_of_Case <- as.factor(final_data$Nature_of_Case)

# I couldn't find a way to pass this tibble as a character arrary :(
target <- c("US","Italy","China","Spain","Germany","France","Iran","UK")

# Plotting data
plotme <- ggplot(transform(final_data,
                           Country=factor(Country,
                                          levels=target))) +
  aes(Country, Total_Cases , size = 3, colour = Country) +
  ggtitle('Total cases across countries')+ # Adding a title
  theme(plot.title = element_text(hjust = 0.5), # Centering the title
        #axis.title.x=element_blank(), # Clearing the x-axis title
        axis.ticks.x=element_blank())+ # Clearing the x-axis ticks
  geom_point(alpha = 0.7, show.legend = FALSE) +
  facet_wrap(~Nature_of_Case, nrow = 1) +
  theme(strip.text.x = element_text(
    size = 12, color = "#880808", face = "bold"
  ))
plotme

# Animating the plot date wise
anim <- plotme +
  transition_time(Date)+
  labs(title = 'Date:{frame_time}') +
  #view_follow(fixed_y = TRUE)
  shadow_wake(wake_length = 0.3, alpha = TRUE)
#shadow_mark(alpha = 0.3, size = 0.5)

animation_covid <- animate(anim,duration = 20,fps = 20, end_pause = 5,
        renderer = ffmpeg_renderer("covid19.mp4"),
        height = 700, width = 1000)

anim_save("animation_covid.mp4",animation_covid)