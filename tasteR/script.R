## tasteR script ##

# install.packages("tidyverse")
library(tidyverse)

## Import
speedofanimals <- read_csv("data/speed_of_animals.csv")
glimpse(speedofanimals)
speedofanimals <- read_csv("data/speed_of_animals.csv", 
                           col_types = cols(
                             name = col_factor(NULL),
                             habitat = col_factor(NULL),
                             length_cm = col_integer(),
                             mass_kg = col_double(),
                             speed_kph = col_double()
                           ))
speedofanimals

## Tidy
speedofanimals_long <- gather(speedofanimals, measure, value, -name, -habitat)
speedofanimals_long
spread(speedofanimals_long, measure, value)

split <- separate(speedofanimals_long, measure, c("measure", "unit"), sep = "_")
split
unite(split, measure, c(measure, unit), sep = "_")

## Transform
select(speedofanimals, name, speed_kph)
select(speedofanimals, -speed_kph)
select(speedofanimals, 1:3)
select(speedofanimals, starts_with("hab"))
select(speedofanimals, animal = name)

filter(speedofanimals, length_cm < 100)
filter(speedofanimals, name == "Roadrunner")
filter(speedofanimals, habitat != "Air")
filter(speedofanimals, grepl("African", name))
filter(speedofanimals, mass_kg > 1000 & habitat == "Land")
filter(speedofanimals, name %in% c("Coyote", "Roadrunner"))

arrange(speedofanimals, speed_kph)
arrange(speedofanimals, desc(speed_kph))

mutate(speedofanimals, speed_mph = speed_kph * 0.62137)
mutate(speedofanimals, name = factor(name))
mutate(speedofanimals, speed_mph = speed_kph * 0.62137,
       name = factor(name))

## %>% 
speedofanimals %>% 
  filter(habitat == "Air") %>% 
  mutate(speed_mph = speed_kph * 0.62137) %>%
  select(name, speed_mph) %>% 
  arrange(desc(speed_mph))

speedofanimals %>% 
  mutate(speed_mph = speed_kph * 0.62137) %>% 
  group_by(habitat) %>% 
  summarise(mean_speed_mph = round(mean(speed_mph), 1)) %>% 
  arrange(desc(mean_speed_mph))

## Visualise
ggplot()

ggplot(data = speedofanimals,
       aes(x = mass_kg, y = speed_kph))

ggplot(data = speedofanimals,
       aes(x = mass_kg, y = speed_kph)) +
  geom_point()

ggplot(data = speedofanimals,
       aes(x = mass_kg, y = speed_kph)) +
  geom_point(colour = "tomato")

ggplot(data = speedofanimals,
       aes(x = mass_kg, y = speed_kph, fill = habitat)) +
  geom_point(shape = 21, colour = "black", alpha = 0.6)

library(scales)
ggplot(data = speedofanimals,
       aes(x = mass_kg, y = speed_kph, fill = habitat)) +
  geom_point(shape = 21, colour = "black", alpha = 0.6) +
  scale_x_continuous(trans = log_trans(), breaks=c(1,10,100,10000)) +
  scale_y_continuous(trans = log_trans(), breaks=c(1,10,100))

ggplot(data = speedofanimals,
       aes(x = mass_kg, y = speed_kph, fill = habitat)) +
  geom_point(shape = 21, colour = "black", alpha = 0.6) +
  scale_x_continuous(trans = log_trans(), breaks=c(1,10,100,10000)) +
  scale_y_continuous(trans = log_trans(), breaks=c(1,10,100)) +
  labs(title = "Relationship between body mass and speed \namongst selected animals",
       caption = "Source: speedofanimals.com",
       x = "Body mass (kg, log)",
       y = "Top speed (kph, log)",
       fill = "Habitat")

ggplot(data = speedofanimals,
       aes(x = mass_kg, y = speed_kph, fill = habitat)) +
  geom_point(shape = 21, colour = "black", alpha = 0.6) +
  scale_x_continuous(trans = log_trans(), breaks=c(1,10,100,10000)) +
  scale_y_continuous(trans = log_trans(), breaks=c(1,10,100)) +
  labs(title = "Relationship between body mass and speed \namongst selected animals",
       caption = "Source: speedofanimals.com",
       x = "Body mass (kg, log)",
       y = "Top speed (kph, log)",
       fill = "Habitat") +
  theme_minimal() 

ggplot(data = speedofanimals,
       aes(x = mass_kg, y = speed_kph, fill = habitat, size = length_cm)) +
  geom_point(shape = 21, colour = "black", alpha = 0.6) +
  scale_fill_brewer(palette = "Set2") +
  scale_size_continuous(range = c(1, 10)) +
  scale_x_continuous(trans = log_trans(), breaks=c(1,10,100,10000)) +
  scale_y_continuous(trans = log_trans(), breaks=c(1,10,100)) +
  labs(title = "Relationship between body mass and speed \namongst selected animals",
       caption = "Source: speedofanimals.com",
       x = "Body mass (kg, log)",
       y = "Top speed (kph, log)",
       fill = "Habitat",
       size = "Length (cm)") +
  theme_minimal() +
  theme(
    plot.margin = unit(rep(30, 4), "pt"),
    panel.grid.minor = element_blank(),
    axis.title = element_text(size = 8, face = "plain", hjust = 1),
    plot.caption = element_text(size = 8, hjust = 1, margin = margin(t = 15)))
ggsave("scatterplot.png", scale = 1, dpi = 300)

library(ggiraph)
speedofanimals$name <- str_replace_all(speedofanimals$name, "'", "")
p <- ggplot(data = speedofanimals,
            aes(x = mass_kg, y = speed_kph, fill = habitat, size = length_cm)) +
  geom_point_interactive(aes(tooltip = name), shape = 21, colour = "black", alpha = 0.6) +
  scale_fill_brewer(palette = "Set2") +
  scale_size_continuous(range = c(1, 10)) +
  scale_x_continuous(trans = log_trans(), breaks=c(1,10,100,10000)) +
  scale_y_continuous(trans = log_trans(), breaks=c(1,10,100)) +
  labs(title = "Relationship between body mass and speed \namongst selected animals",
       caption = "Source: speedofanimals.com",
       x = "Body mass (kg, log)",
       y = "Top speed (kph, log)",
       fill = "Habitat",
       size = "Length (cm)") +
  theme_minimal() +
  theme(
    plot.margin = unit(rep(30, 4), "pt"),
    panel.grid.minor = element_blank(),
    axis.title = element_text(size = 8, face = "plain", hjust = 1),
    plot.caption = element_text(size = 8, hjust = 1, margin = margin(t = 15)))
girafe(code = print(p))

## Model
speedofanimals <- speedofanimals %>% 
  mutate(body_lengths_s_km = ((speed_kph*1000) / 3600) / (length_cm / 100))
select(speedofanimals, name, body_lengths_s_km) %>% 
  arrange(desc(body_lengths_s_km))

speedofanimals <- speedofanimals %>% 
  mutate(rel_speed_kph = ((body_lengths_s_km * 1.83) * 3600) / 1000)
select(speedofanimals, name, body_lengths_s_km, rel_speed_kph) %>% 
  arrange(desc(rel_speed_kph))
