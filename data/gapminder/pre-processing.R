# Gapminder data, 2018 #

# Source: Gapminder
# Publisher URL: https://www.gapminder.org/data/
# License: Creative Common Attribution License 4.0

library(tidyverse) ; library(readxl)

# Geographies ----------------------
# Source: gapm.io/dl_geo
countries <- read_excel("raw/Data Geographies - v1 - by Gapminder.xlsx", sheet = 2) %>% 
  select(country = name, region = four_regions, iso_a3 = geo) %>% 
  mutate(country = factor(country),
         iso_a3 = factor(toupper(iso_a3)),
         region = factor(gsub("(?<=\\b)([a-z])", "\\U\\1", tolower(region), perl=TRUE)))

# Life expectancy ----------------------
# Source: gapm.io/ilex
lifeExp <- read_csv("raw/life_expectancy_years.csv") %>% 
  gather(year, lifeExp, -country) 

# Income per person ----------------------
# Source: gapm.io/dgdppc
gdpPercap <- read_csv("raw/income_per_person_gdppercapita_ppp_inflation_adjusted.csv") %>% 
  gather(year, gdpPercap, -country) 

# Population ----------------------
# Source: gapm.io/dpop
pop <- read_csv("raw/population_total.csv") %>% 
  gather(year, pop, -country)

countries %>% 
  left_join(pop, by = "country") %>% 
  left_join(gdpPercap, by = c("country", "year")) %>% 
  left_join(lifeExp, by = c("country", "year")) %>% 
  filter(year ==  2018) %>% 
  group_by(country) %>% 
  filter(all(!is.na(pop)) & all(!is.na(gdpPercap)) & all(!is.na(lifeExp))) %>% 
  ungroup() %>% 
  select(country, region, year, lifeExp, pop, gdpPercap) %>% 
  write_csv("gapminder.csv")
