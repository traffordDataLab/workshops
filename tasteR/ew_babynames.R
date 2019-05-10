library(tidyverse)

ew_babynames <- read_csv("https://www.trafforddatalab.io/workshops/data/babynames/babynames.csv")

selection <- ew_babynames %>%
  filter(name == "Archie", sex == "Male") 

plot <- ggplot(data = selection, aes(x = year, y = n)) +
  geom_line(colour = "#fc6721", size = 2) +
  labs(title = "Baby boys named Archie",
       subtitle = "England & Wales, 1996 and 2017",
       x = "Year", 
       y = "Frequency",
       caption = "Source: Office for National Statistics",
       color = "") +
  theme_minimal() +
  theme(legend.position = "top")
plot

ggsave(plot, "baby_plot.png", scale = 1, dpi = 300)