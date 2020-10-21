# If you don't look at your data, Datasaurus will eat you

# Loading required libraries
library(datasauRus)
library(ggplot2)
library(psych)
library(dplyr)
library(patchwork)
library(ggtext)

# Loading data
data <- datasaurus_dozen
# Converting dataset variable into factor
data$dataset <- as.factor(data$dataset)

# Inspecting data
head(data)
tail(data)
summary_descriptives <- describeBy(data, group =data$dataset)

# Grid plots
scatters <- ggplot(data = data, aes(x = x, y = y, color = dataset)) + 
  geom_point() + 
  geom_smooth(method = lm, se = FALSE, color = "black") +
  facet_wrap(~ dataset, nrow = 3) + 
  theme(plot.background = element_rect(fill = '#fdfff5')) +
  theme_minimal() +
  theme(
    strip.background = element_blank(),
    strip.text = element_textbox(
      size = 8,
      color = "white", fill = "#5D729D", box.color = "#4A618C",
      halign = 0.5, linetype = 1, r = unit(5, "pt"), width = unit(1, "npc"),
      padding = margin(2, 0, 1, 0), margin = margin(3, 3, 3, 3)
    )
  )

violins <- ggplot(data = data, aes(x = x, y = y, color = dataset)) + 
  geom_violin() +
  facet_wrap(~ dataset, nrow = 3) +
  theme(plot.background = element_rect(fill = '#fdfff5')) +
  theme_minimal() +
  theme(
    strip.background = element_blank(),
    strip.text = element_textbox(
      size = 8,
      color = "white", fill = "#5D729D", box.color = "#4A618C",
      halign = 0.5, linetype = 1, r = unit(5, "pt"), width = unit(1, "npc"),
      padding = margin(2, 0, 1, 0), margin = margin(3, 3, 3, 3)
    )
  )

# Patching up plots
scatters + theme(legend.position = "none") + labs (title = "Lies, damned lies, and statistics") + 
  theme(plot.title = element_text(hjust = 0.5)) +
  violins + theme(legend.position = "none") + labs (title = "Now you see me") +
  theme(plot.title = element_text(hjust = 0.5))