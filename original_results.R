# This script calculates the composition of the German Bundestag with the original election results.


# functions ---------------------------------------------------------------
source("functions.R")


# read data ---------------------------------------------------------------
first_tier <-
  read_rds("Data/first_tier.rds") %>%
  filter(party != "Nichtwählende/Ungültige") %>%
  select(con_id, con_name, state, party, votes)
second_tier <-
  read_rds("Data/second_tier.rds") %>%
  filter(party != "Nichtwählende/Ungültige") %>%
  select(state, party, votes)
seats_states <- read_rds("Data/seats_states.rds")
df_cand <- read_rds("Data/candidates.rds")


# composition Bundestag ---------------------------------------------------
# calculate composition of Bundestag
df <- calc_seats(
  first_tier = first_tier,
  second_tier = second_tier,
  seats_states = seats_states,
  seats = 598
)

df_seats <-
  df %>%
  group_by(party) %>%
  summarize(seats = sum(seats))

# store data
write_rds(df_seats, "Data/seats_BT_org.rds")


# candidates elected ------------------------------------------------------
# elected via first tier (party)
winner_first <-
  first_tier %>%
  group_by(con_id, con_name) %>%
  filter(votes == max(votes)) %>%
  ungroup() %>%
  select(con_id, con_name, party)

# elected via second tier (party)
seats_list <-
  df %>%
  select(-c(seats, first))

# directly elected candidates
cand_first <-
  winner_first %>%
  inner_join(df_cand) %>%
  mutate(elected = "Direkt")

# candidates elected via list
cand_second <-
  df_cand %>%
  anti_join(cand_first) %>%
  inner_join(seats_list) %>%
  group_by(party, state) %>%
  arrange(list_pos) %>%
  mutate(new_list_pos = row_number()) %>%
  ungroup() %>%
  filter(new_list_pos <= second) %>%
  select(-c(second, new_list_pos)) %>%
  mutate(elected = "Liste")

# all elected candidates
cand_elected <-
  cand_first %>%
  bind_rows(cand_second) %>%
  mutate(
    con_id = ifelse(elected == "Liste", NA, con_id),
    con_name = ifelse(elected == "Liste", NA, con_name),
    state = ifelse(elected == "Direkt", NA, state),
    list_pos = ifelse(elected == "Direkt", NA, list_pos),
    party = as.factor(party)
  ) %>%
  relocate(title, name, first_name, party, gender, year_birth, elected)

# store data
write_rds(cand_elected, "Data/elected_candidates.rds")
