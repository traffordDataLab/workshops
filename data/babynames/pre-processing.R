## Baby names in England and Wales, 1996-2017 ##

# Source: Office of National Statistics 
# Publisher URL: https://www.ons.gov.uk/peoplepopulationandcommunity/birthsdeathsandmarriages/livebirths/bulletins/babynamesenglandandwales/2017/relateddata
# License: Open Government License v3.0

library(tidyverse) ; library(httr) ; library(readxl)

tmp <- tempfile(fileext = ".xls")
GET(url = "https://www.ons.gov.uk/file?uri=/peoplepopulationandcommunity/birthsdeathsandmarriages/livebirths/adhocs/009010babynames1996to2017englandandwales/adhocallbabynames1996to2017.xls",
    write_disk(tmp))

boys <- read_xls(tmp, sheet = 4, skip = 5, na = ":", trim_ws = TRUE) %>% 
  select(seq(1, ncol(.), by = 2)) %>% 
  setNames(c("name", rev(seq(1996,2017,1)))) %>%
  mutate(name = str_to_title(name)) %>% 
  gather(year, n, -name) %>% 
  filter(!is.na(n)) %>% 
  mutate(year = as.integer(year),
         sex = "Male",
         n = as.integer(n)) %>% 
  select(year, sex, name, n) %>% 
  arrange(desc(year)) %>% 
  group_by(year) %>% 
  mutate(rank = min_rank(desc(n)))

girls <- read_xls(tmp, sheet = 5, skip = 5, na = ":", trim_ws = TRUE) %>% 
  select(seq(1, ncol(.), by = 2)) %>% 
  setNames(c("name", rev(seq(1996,2017,1)))) %>%
  mutate(name = str_to_title(name)) %>% 
  gather(year, n, -name) %>% 
  filter(!is.na(n)) %>% 
  mutate(year = as.integer(year),
         sex = "Female",
         n = as.integer(n)) %>% 
  select(year, sex, name, n) %>% 
  arrange(desc(year, name)) %>% 
  group_by(year) %>% 
  mutate(rank = min_rank(desc(n)))

bind_rows(boys, girls) %>% 
  write_csv("babynames.csv")
