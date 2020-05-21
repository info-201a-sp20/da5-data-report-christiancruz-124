# -------------------------------- set up -------------------------------------
library(tidyverse)

raw <- read.csv("data/shootings-2018.csv", stringsAsFactors = FALSE)

shootings <- raw %>%
  mutate(city_state = paste0(city, ", ", state)) %>%
  mutate(month = substr(date, 1, str_locate(date, " ") - 1)) %>%
  mutate(total_victims = num_killed + num_injured)

# -------------------------- summary paragraph --------------------------------

sum_shootings <- nrow(shootings)

sum_lost <- shootings %>%
  select(num_killed) %>%
  sum()

sum_city <- shootings %>%
  group_by(city_state) %>%
  summarize(tot_killed = sum(num_killed)) %>%
  filter(tot_killed == max(tot_killed)) %>%
  pull(city_state)

sum_state <- shootings %>%
  group_by(state) %>%
  summarize(tot_killed = sum(num_killed)) %>%
  filter(tot_killed == max(tot_killed)) %>%
  pull(state)

sum_month <- shootings %>%
  group_by(month) %>%
  summarize(incidents = n()) %>%
  filter(incidents == max(incidents)) %>%
  pull(month)

# -------------------------------- table --------------------------------------

table_df <- shootings %>%
  group_by(state) %>%
  summarize(
    tot_victims = sum(total_victims),
    tot_killed = sum(num_killed),
    tot_injured = sum(num_injured),
    perc_killed = round(tot_killed / tot_victims * 100, digits = 0),
    perc_injured = round(tot_injured / tot_victims * 100, digits = 0)
  )

# ------------------------- specific incident ---------------------------------
# Incident chosen: the Trenton, NJ incident on June 17, 2018

spfc_incdnt <- shootings %>%
  filter(num_injured == max(num_injured))

spfc_date <- spfc_incdnt %>%
  select(date) %>%
  pull()

spfc_city_state <- spfc_incdnt %>%
  select(city_state) %>%
  pull()

spfc_city <- spfc_incdnt %>%
  select(city) %>%
  pull()

spfc_address <- spfc_incdnt %>%
  select(address) %>%
  pull()

spfc_injr <- spfc_incdnt %>%
  select(num_injured) %>%
  pull()

spfc_lost <- spfc_incdnt %>%
  select(num_killed) %>%
  pull()

spfc_in_month <- shootings %>%
  filter(month == pull(select(spfc_incdnt, month))) %>%
  filter(state == pull(select(spfc_incdnt, state))) %>%
  nrow()

# ----------------------------- interactive map -------------------------------

# char vector for use in 'index.Rmd'. Map will use this for converting to HTML
# for labels
labels <- lapply(seq(nrow(shootings)), function(x) {
  paste0(
    "<p>Address: ", shootings[x, "address"], "</p>",
    "<p>Deaths: ", shootings[x, "num_killed"], "</p>",
    "<p>Injuries: ", shootings[x, "num_injured"], "</p>"
  )
})

# ------------------------------ choice plot ----------------------------------

# dataframe containing killed/injured ratio for each state
by_state <- shootings %>%
  select(state, num_killed, num_injured) %>%
  gather(key = victim_type, value = total, -state) %>%
  group_by(state, victim_type) %>%
  summarize(total = sum(total))
