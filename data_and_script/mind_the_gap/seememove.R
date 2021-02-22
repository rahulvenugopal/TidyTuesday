# The best stats you have ever seen by Hans Rosling
library(gapminder)
library(ggplot2)
library(gganimate)
library(gifski)
theme_set(theme_bw())

p <- ggplot(gapminder,
            aes(gdpPercap, lifeExp, size = pop, colour = country)) +
  geom_point(alpha = 0.7, show.legend = FALSE) +
  scale_colour_manual(values = country_colors) +
  scale_size(range = c(2, 12)) +
  scale_x_log10() +
  facet_wrap(~continent)

  # Here comes the gganimate specific bits
  p+
  transition_time(year) +
  labs(title = 'Year: {frame_time}') +
  shadow_wake(wake_length = 0.1, alpha = FALSE)
  animate(p, duration = 5, fps = 20, width = 200, height = 200,
          renderer = gifski_renderer())
  anim_save(filename = "test.gif",animation = p)

###################################################################################  
p <-  ggplot(gapminder, aes(gdpPercap, lifeExp, size = pop, colour = country)) +
    geom_point(alpha = 0.7, show.legend = FALSE) +
    scale_colour_manual(values = country_colors) +
    scale_size(range = c(2, 12)) +
    facet_wrap(~continent) +
    scale_x_log10()

anim <- p +
  transition_time(year)+
  labs(title = 'Year:{frame_time}')+
  #view_follow(fixed_y = TRUE)
  shadow_wake(wake_length = 0.1, alpha = FALSE)
  #shadow_mark(alpha = 0.3, size = 0.5)
animate_gapminder <- animate(anim,nframes=100,renderer = ffmpeg_renderer("mindthegap.mp4"),
        height = 700, width = 1000)
anim_save(filename = "mindthegap.mp4",animate_gapminder)

#######################3 video file  
anim <- p +
  transition_time(year)+
  labs(title = 'Year:{frame_time}')+
  #view_follow(fixed_y = TRUE)
  #shadow_wake(wake_length = 0.1, alpha = FALSE)
  shadow_mark(alpha = 0.3, size = 0.5)
animate(anim,nframes=100,renderer = ffmpeg_renderer("okayyo.mp4"),
        height = 700, width = 1000)