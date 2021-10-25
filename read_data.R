# This script reads and processes all the data

# packages ----------------------------------------------------------------
#pacman::p_load(tidyverse, zoo)
library(tidyverse)
library(zoo)


# election results --------------------------------------------------------
df_results <-
  read_csv2(
    "https://www.bundeswahlleiter.de/dam/jcr/2a90d988-7063-48af-b265-dfb9fa14f7e3/btw21_kerg2.csv",
    skip = 9
  ) %>%
  select(
    con_id = Gebietsnummer,
    con_name = Gebietsname,
    superordinate_territory = UegGebietsart,
    superordinate_territory_id = UegGebietsnummer,
    party = Gruppenname,
    tier = Stimme,
    votes = Anzahl
  ) %>%
  mutate(across(
    .cols = one_of("con_id",
                   "superordinate_territory_id",
                   "tier",
                   "votes"),
    as.numeric
  )) %>%
  filter(superordinate_territory == "LAND") %>%
  mutate(
    state = case_when(
      superordinate_territory_id == 1 ~ "Schleswig-Holstein",
      superordinate_territory_id == 2 ~ "Hamburg",
      superordinate_territory_id == 3 ~ "Niedersachsen",
      superordinate_territory_id == 4 ~ "Bremen",
      superordinate_territory_id == 5 ~ "Nordrhein-Westfalen",
      superordinate_territory_id == 6 ~ "Hessen",
      superordinate_territory_id == 7 ~ "Rheinland-Pfalz",
      superordinate_territory_id == 8 ~ "Baden-Württemberg",
      superordinate_territory_id == 9 ~ "Bayern",
      superordinate_territory_id == 10 ~ "Saarland",
      superordinate_territory_id == 11 ~ "Berlin",
      superordinate_territory_id == 12 ~ "Brandenburg",
      superordinate_territory_id == 13 ~ "Mecklenburg-Vorpommern",
      superordinate_territory_id == 14 ~ "Sachsen",
      superordinate_territory_id == 15 ~ "Sachsen-Anhalt",
      superordinate_territory_id == 16 ~ "Thüringen"
    )
  ) %>%
  mutate(tier = case_when(tier == 1 ~ "first", tier == 2 ~ "second", TRUE ~ "both")) %>%
  select(con_id, con_name, state, party, tier, votes) %>%
  pivot_wider(names_from = tier, values_from = votes) %>%
  mutate(first = ifelse(!is.na(both), both, first),
         second = ifelse(!is.na(both), both, second)) %>%
  select(-both) %>%
  bind_rows(tibble(distinct(., con_id, con_name, state),
                   party = "Nichtwählende/Ungültige")) %>%
  group_by(con_id) %>%
  mutate(
    first = ifelse(
      party == "Nichtwählende/Ungültige",
      first[party == "Wahlberechtigte"] -
        first[party == "Wählende"] + first[party == "Ungültige"],
      first
    ),
    second = ifelse(
      party == "Nichtwählende/Ungültige",
      second[party == "Wahlberechtigte"] -
        second[party == "Wählende"] + second[party == "Ungültige"],
      second
    )
  ) %>%
  ungroup() %>%
  filter(party != "Wahlberechtigte",
         party != "Wählende",
         party != "Ungültige",
         party != "Gültige") %>%
  arrange(con_id)

# data for first tier
first_tier <-
  df_results %>%
  select(!c(second)) %>%
  drop_na() %>%
  rename(votes = first) %>%
  group_by(con_id) %>%
  mutate(vote_share = ifelse(party != "Nichtwählende/Ungültige", round(votes / sum(votes[party != "Nichtwählende/Ungültige"]) * 100, 2),
                             0)) %>%
  arrange(desc(vote_share)) %>%
  mutate(closeness = max(votes[party != "Nichtwählende/Ungültige"]) - Rfast::nth(x = votes[party != "Nichtwählende/Ungültige"], k = 2, descending = TRUE)) %>%
  ungroup() %>%
  arrange(con_id) %>%
  mutate(org_votes = votes) %>%
  mutate(diff = 0) %>%
  mutate(
    con_id = as.factor(con_id),
    con_name = as.factor(con_name),
    state = as.factor(state),
    party = as.factor(party)
  )

# data for second tier
second_tier <-
  df_results %>%
  select(!c(con_id, con_name, first)) %>%
  drop_na() %>%
  rename(votes = second) %>%
  group_by(state, party) %>%
  summarize(votes = sum(votes)) %>%
  mutate(vote_share = ifelse(party != "Nichtwählende/Ungültige", round(votes / sum(votes[party != "Nichtwählende/Ungültige"]) * 100, 2),
                             0)) %>%
  arrange(desc(vote_share)) %>%
  ungroup() %>%
  arrange(state) %>%
  mutate(org_votes = votes) %>%
  mutate(diff = 0) %>%
  mutate(state = as.factor(state),
         party = as.factor(party))

# colors parties ----------------------------------------------------------
# colors for parties
df_colors <-
  df_results %>%
  distinct(party) %>%
  mutate(
    group = ifelse(party %in% c("CDU", "CSU"), "CDU/CSU", party),
    color_party = case_when(
      party == "SPD" ~ "red",
      party == "CDU" ~ "black",
      party == "GRÜNE" ~ "green3",
      party == "FDP" ~ "yellow",
      party == "AfD" ~ "blue",
      party == "CSU" ~ "lightblue",
      party == "DIE LINKE" ~ "violetred",
      party == "FREIE WÄHLER" ~ "orange",
      TRUE ~ "grey"
    ),
    color_group = ifelse(group == "CDU/CSU", "black", color_party)
  )


# seats states ------------------------------------------------------------
# how many seats for each state (source: https://www.bundeswahlleiter.de/dam/jcr/bf33c285-ee92-455a-a9c3-8d4e3a1ee4b4/btw21_sitzberechnung.pdf)
df_seats_states <-
  tibble(
    state = c(
      "Schleswig-Holstein",
      "Mecklenburg-Vorpommern",
      "Hamburg",
      "Niedersachsen",
      "Bremen",
      "Brandenburg",
      "Sachsen-Anhalt",
      "Berlin",
      "Nordrhein-Westfalen",
      "Sachsen",
      "Hessen",
      "Thüringen",
      "Rheinland-Pfalz",
      "Bayern",
      "Baden-Württemberg",
      "Saarland"
    ),
    seats = c(22, 13, 13, 59, 5, 20, 17, 24, 127, 32, 43, 16, 30, 93, 77, 7)
  ) %>%
  mutate(state = as.factor(state))


# candidates --------------------------------------------------------------
temp <- tempfile()

download.file(
  "https://www.bundeswahlleiter.de/dam/jcr/90072555-88ec-4601-bf3e-680ce3ef5a46/btw21_kandidaturen_utf8.zip",
  temp
)

df_candidates <-
  read_csv2(unz(temp, "btw21_kandidaturen_utf8.csv"), skip = 8) %>%
  select(
    title = Titel,
    name = Nachname,
    first_name = Vornamen,
    gender = Geschlecht,
    year_birth = Geburtsjahr,
    tier = Gebietsart,
    con_id = Gebietsnummer,
    state = Gebietsname,
    party = Gruppenname,
    list_pos = Listenplatz
  ) %>%
  mutate(
    con_id = ifelse(tier == "Land", NA, con_id),
    state = ifelse(tier == "Wahlkreis", NA, state)
  ) %>%
  select(-tier) %>%
  group_by(title, name, first_name, gender, year_birth, party) %>%
  fill(con_id, state, list_pos, .direction = "downup") %>%
  ungroup() %>%
  distinct() %>%
  relocate(title:year_birth, party, con_id, state, list_pos) %>%
  mutate(con_id = as.factor(con_id))


unlink(temp)


# save data ---------------------------------------------------------------
write_rds(first_tier, "Data/first_tier.rds")
write_rds(second_tier, "Data/second_tier.rds")
write_rds(df_colors, "Data/colors.rds")
write_rds(df_candidates, "Data/candidates.rds")
write_rds(df_seats_states, "Data/seats_states.rds")