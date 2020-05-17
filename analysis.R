library(tidyverse)

shootings <- read.csv("data/shootings-2018.csv", stringsAsFactors = FALSE)

n_shootings <- nrow(shootings)

lives_lost <- shootings %>% 
  select(num_killed) %>% 
  sum()

most_impacted_city <- shootings %>%
  mutate(city_state = paste0(city, ", ", state)) %>% 
  group_by(city_state) %>% 
  summarize(tot_killed = sum(num_killed)) %>% 
  filter(tot_killed == max(tot_killed)) %>% 
  pull(city_state)

most_impacted_state <- shootings %>% 
  group_by(state) %>% 
  summarize(tot_killed = sum(num_killed)) %>% 
  filter(tot_killed == max(tot_killed)) %>% 
  pull(state)

substr(shootings[1, "date"], 1, str_locate(shootings[1, "date"], " ") - 1)

worst_month <- shootings %>% 
  mutate(month = substr(date, 1, str_locate(date, " ") - 1)) %>% 
  group_by(month) %>% 
  summarize(incidents = n()) %>% 
  filter(incidents == max(incidents)) %>% 
  pull(month)
