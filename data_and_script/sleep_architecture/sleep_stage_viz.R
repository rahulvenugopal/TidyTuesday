###################################
# This piece of work is inspired by tidy tuesday works from
# Cédric Scherer and Jack Davison on IMDB ratings and avatar dataset

# Author: @Rahul Venugopal
###################################

library(tidyverse)
library(tidytuesdayR)
library(tvthemes)

import_avatar()

# Reading data
data <- read.csv('three_stages.csv')

# Converting data to tidy format
clean_data <- gather(data, "sleep_stage","percentages", 2:4)

clean_data$subj <- c(1:30)
clean_data <- clean_data %>% select(-subj_name)

# Get stage percentage means
clean_data$sleep_stage <- as.factor(clean_data$sleep_stage)

percent_means <- clean_data %>% group_by(sleep_stage) %>% 
  summarise(mean(percentages))

# Creating a new column which has average of each stage from all subjects
clean_data <- clean_data %>% 
  mutate(stage_percentage_mean = case_when(
    sleep_stage %in% c("N1_percentage") ~ as.numeric(percent_means[1,2]),
    sleep_stage %in% c("N2_percentage") ~ as.numeric(percent_means[2,2]),
    sleep_stage %in% c("N3_percentage") ~ as.numeric(percent_means[3,2]))
  )

series_names <- clean_data %>%
  group_by(sleep_stage) %>%
  summarise(percentages = min(percentages),
            halfway = (min(subj) + max(subj) + 1) / 2)

# Visualisation
clean_data %>%
  ggplot(aes(x = subj, y = percentages)) +
  
  # Step plot is invisible due to alpha = 0
  geom_step(aes(y = stage_percentage_mean, x = subj), size = 0.1, alpha = 0) +
  
  # Draw dotted lines starting from mean and ending at actual values
  geom_segment(aes(yend = stage_percentage_mean, xend = subj), linetype = 3) +
  
  # Drawing a thick line for one label of sleep stage
  # Logic is set in such a way that start and end of eacg segment covers on sleep stage
  geom_segment(
    data = clean_data %>% filter(sleep_stage == "N1_percentage"),
    aes(
      x = min(subj),
      xend = max(subj) + 1,
      y = stage_percentage_mean,
      yend = stage_percentage_mean
    ),
    size = 2,
    color = "#006992"
  ) +
  
  # Drawing a thick line for one label of sleep stage
  # Logic is set in such a way that start and end of eacg segment covers on sleep stage
  geom_segment(
    data = clean_data %>% filter(sleep_stage == "N2_percentage"),
    aes(
      x = min(subj),
      xend = max(subj) + 1,
      y = stage_percentage_mean,
      yend = stage_percentage_mean
    ),
    size = 2,
    color = "#00916E"
  ) +
  
  # Drawing a thick line for one label of sleep stage
  # Logic is set in such a way that start and end of eacg segment covers on sleep stage
  geom_segment(
    data = clean_data %>% filter(sleep_stage == "N3_percentage"),
    aes(
      x = min(subj),
      xend = max(subj) + 1,
      y = stage_percentage_mean,
      yend = stage_percentage_mean
    ),
    size = 2,
    color = "#FC440F"
  ) +
  
  # Points colored as per sleep stage
  geom_point(aes(color = sleep_stage)) +
  
  # Add text which comes from sleep stage
  geom_text(
    data = series_names,
    aes(
      y = percentages + .025,
      x = halfway,
      label = sleep_stage,
      color = sleep_stage
    ),
    family = "Slayer",
    vjust = 1,
    hjust = .5,
    size = 4, # Font size of labels
    alpha = 0.2
  ) +
  
  # Adding title, subtitle etc.
  labs(
    x = "",
    y = "Percentage of Sleep",
    title = "Sleep Macroarchitecture",
    subtitle = "Sleet stage percentages \n",
    caption = ""
  ) +
  
  # Setting theme with Slayer font
  theme_avatar(text.font = "Slayer") +
  
  # Customising theme
  theme(
    axis.line.x = element_blank(),
    axis.text.x = element_blank(),
    legend.position = "none",
    panel.grid.minor = element_blank(),
    panel.grid.major.x = element_blank(),
    panel.grid.major.y = element_line(
      color = "#d9cfb5",
      linetype = 3,
      size = 1
    ),
    plot.title = element_text(hjust = .5, size = 20, face = "bold"),
    plot.subtitle = element_text(hjust = .5, size = 10),
    plot.margin = unit(c(1, 1, 1, 1), "cm"),
    plot.caption = element_text(
      hjust = .5,
      size = 8,
      family = "sans",
      color = "#a89567"
    ),
    text = element_text(family = "Slayer")
  ) +
  
  # Setting ranges for y scale
  scale_y_continuous(
    limits = c(0, 100),
    breaks = seq(0, 100,10),
    sec.axis = dup_axis(name = NULL) # To get an axis on right side too
  ) +
  
  # Changinf the color of segments and points
  scale_color_manual(values = c(
    "N1_percentage" = "#006992",
    "N2_percentage" = "#00916E",
    "N3_percentage" = "#FC440F"
  )) +
  
  # Changinf the fill to same color theme
  scale_fill_manual(values = c(
    "N1_percentage" = "#006992",
    "N2_percentage" = "#00916E",
    "N3_percentage" = "#FC440F"
  ))

# Fin.