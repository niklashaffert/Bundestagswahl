# This script contains all the functions that are used in the app.

# packages ----------------------------------------------------------------
library(DT)
library(tidyverse)
library(ggpol)
library(plotly)
library(Rfast)
library(shiny)

# function for exact rounding
round_pre <- function(x) {
  posneg = sign(x)
  z = abs(x) * 10 ^ 0
  z = z + 0.5 + sqrt(.Machine$double.eps)
  z = trunc(z)
  z = z / 10 ^ 0
  z * posneg
}

# finding a divisor between two bounds (prefering a round one)
new_div <- function(div_range, descen) {
  divis <-
    Rfast::nth(
      x = div_range,
      k = 2,
      num.of.nths = 2,
      descending = descen
    ) %>%
    as.vector() %>%
    sort()
  
  divis_med <- median(divis)
  
  digit <- -6
  divis_r <- round(x = divis_med, digits = digit)
  new_div <-  NULL
  
  while (is.null(new_div)) {
    if (divis_r > divis[1] & divis_r <= divis[2]) {
      new_div <- divis_r
    } else{
      digit <- digit + 1
      divis_r <- round(x = divis_med, digits = digit)
    }
  }
  
  return(new_div)
}


saint_lague <- function(votes, seats_tar) {
  divisor <- sum(votes) / seats_tar
  
  seats <- round_pre(votes /  divisor)
  
  while (sum(seats) > seats_tar) {
    divisor_0.5 <- votes / (seats - 0.5)
    divisor_1.5 <- votes / (seats - 1.5)
    divisor_0.5 <- ifelse(divisor_0.5 < 0, NA, divisor_0.5)
    divisor_1.5 <- ifelse(divisor_1.5 < 0, NA, divisor_1.5)
    divisor <- new_div(div_range = c(divisor_0.5, divisor_1.5),
                       descen = FALSE)
    seats <- round_pre(votes / divisor)
  }
  
  while (sum(seats) < seats_tar) {
    divisor_0.5 <- votes / (seats + 0.5)
    divisor_1.5 <- votes / (seats + 1.5)
    divisor_0.5 <- ifelse(divisor_0.5 < 0, NA, divisor_0.5)
    divisor_1.5 <- ifelse(divisor_1.5 < 0, NA, divisor_1.5)
    divisor <- new_div(div_range = c(divisor_0.5, divisor_1.5),
                       descen = TRUE)
    seats <- round_pre(votes / divisor)
  }
  
  return(seats)
}

# distribution of Ueberhangmandate
distribution <- function(votes, min_seats) {
  divisor <- sum(votes) / sum(min_seats)
  seats <- round_pre(votes /  divisor)
  over <- min_seats - seats
  over <- ifelse(over < 0, 0, over)
  
  while (any(over > 3)) {
    divisor_0.5 <- votes / (seats + 0.5)
    divisor_1.5 <- votes / (seats + 1.5)
    divisor_0.5 <- ifelse(divisor_0.5 < 0, NA, divisor_0.5)
    divisor_1.5 <- ifelse(divisor_1.5 < 0, NA, divisor_1.5)
    divisor <- new_div(div_range = c(divisor_0.5, divisor_1.5),
                       descen = TRUE)
    seats <- round_pre(votes / divisor)
    over <- min_seats - seats
    over <- ifelse(over < 0, 0, over)
  }
  
  return(seats)
}

# distribution of seats within parties between states
underdistribution <- function(votes, min_seats, seats_tar) {
  divisor <- sum(votes) / seats_tar
  
  seats <- round_pre(votes / divisor)
  
  seats_max <- pmax(min_seats, seats)
  
  while (sum(seats_max) > seats_tar) {
    pos_0.5 <- (seats - 0.5) > min_seats
    pos_1.5 <- (seats - 1.5) > min_seats
    
    divisor_0.5 <- votes[pos_0.5] / (seats[pos_0.5] - 0.5)
    divisor_1.5 <- votes[pos_1.5] / (seats[pos_1.5] - 1.5)
    divisor_0.5 <- ifelse(divisor_0.5 < 0, NA, divisor_0.5)
    divisor_1.5 <- ifelse(divisor_1.5 < 0, NA, divisor_1.5)
    divisor <- new_div(div_range = c(divisor_0.5, divisor_1.5),
                       descen = FALSE)
    
    seats <- round_pre(votes / divisor)
    seats_max <- pmax(min_seats, seats)
  }
  
  while (sum(seats_max) < seats_tar) {
    divisor_0.5 <- votes / (seats_max + 0.5)
    divisor_1.5 <- votes / (seats_max + 1.5)
    divisor_0.5 <- ifelse(divisor_0.5 < 0, NA, divisor_0.5)
    divisor_1.5 <- ifelse(divisor_1.5 < 0, NA, divisor_1.5)
    divisor <- new_div(div_range = c(divisor_0.5, divisor_1.5),
                       descen = TRUE)
    seats_max <- round_pre(votes / divisor)
  }
  
  return(seats_max)
}

# function to calculate seats in the Bundestag according to the method used by the Bundeswahlleiter
calc_seats <- function(first_tier,
                       second_tier,
                       seats_states,
                       seats) {
  # parties entering Bundestag ----------------------------------------------
  # data frame for winner party in each constituency
  winner_first <-
    first_tier %>%
    group_by(con_id) %>%
    filter(votes == max(votes)) %>%
    ungroup() %>%
    select(con_id, con_name, party)
  
  if (length(winner_first) > (seats / 2)) {
    winner_first <-
      winner_first %>%
      group_by(con_id, con_name) %>%
      sample_n(1) %>%
      ungroup()
  }
  
  # data frame for number if direct mandates per party per state
  direct_mandates <-
    first_tier %>%
    group_by(con_id) %>%
    filter(votes == max(votes)) %>%
    ungroup() %>%
    count(state, party, name = "first")
  
  # determine parties entering Bundestag via three direct mandates
  parties_first <-
    direct_mandates %>%
    group_by(party) %>%
    summarize(first = sum(first)) %>%
    filter(first >= 3) %>%
    pull(party)
  
  # determine parties exceeding the threshold
  parties_second <-
    second_tier %>%
    group_by(party) %>%
    summarize(votes = sum(votes)) %>%
    mutate(vote_share = votes / sum(votes)) %>%
    filter(vote_share >= 0.05 | party == "SSW") %>%
    pull(party)
  
  # all parties entering Bundestag
  parties <-
    c(parties_first,
      parties_second) %>% unique()
  
  # data determining seats --------------------------------------------------
  # data frame with votes (second tier) for all parties entering Bundestag
  df_bt <-
    second_tier %>%
    filter(party %in% parties)
  
  list_mandates <-
    df_bt %>%
    inner_join(seats_states) %>%
    filter(votes > 0) %>%
    group_by(state) %>%
    mutate(second = saint_lague(votes = votes, seats_tar = first(seats))) %>%
    ungroup()
  
  # finding minimum seats per party per state (combining first and second tier)
  min_seats_states <-
    list_mandates %>%
    full_join(direct_mandates) %>%
    mutate(first = as.numeric(first)) %>%
    mutate(
      first = case_when(is.na(first) ~ 0,
                        TRUE ~ first),
      second = case_when(is.na(second) ~ 0,
                         TRUE ~ second)
    ) %>%
    mutate(over = first - second,
           over = ifelse(over < 0, 0, over)) %>%
    mutate(mean_seats = round_pre((second + first) / 2)) %>%
    mutate(min_seats = pmax(first, mean_seats))
  
  # finding Ueberhangmandate
  seats_parties <-
    min_seats_states %>%
    group_by(party) %>%
    summarize(
      first = sum(first),
      second = sum(second),
      over = sum(over),
      min_seats = sum(min_seats)
    ) %>%
    mutate(min_seats = pmax(first, second, min_seats)) %>%
    inner_join(df_bt %>%
                 group_by(party) %>%
                 summarize(votes = sum(votes))) %>%
    mutate(
      seats = distribution(votes = votes, min_seats = min_seats),
      over = min_seats - seats,
      over = ifelse(over < 0, 0, over)
    )
  
  parties_mult <-
    min_seats_states %>%
    group_by(party) %>%
    filter(n() > 1) %>%
    distinct(party) %>%
    pull()
  
  parties_unique <-
    min_seats_states %>%
    group_by(party) %>%
    filter(n() == 1) %>%
    pull(party)
  
  # distribute seats within parties between states
  seats_under <-
    min_seats_states %>%
    select(state, party, votes, min_seats) %>%
    full_join(seats_parties %>% select(party, seats)) %>%
    filter(party %in% parties_mult) %>%
    mutate(votes = ifelse(is.na(votes), 0, votes)) %>%
    group_by(party) %>%
    mutate(seats_state = underdistribution(
      votes = votes,
      min_seats = min_seats,
      seats_tar = first(seats)
    )) %>%
    ungroup() %>%
    select(state, party, seats_up = seats_state)
  
  df_distr <-
    min_seats_states %>%
    select(state, party, votes, min_seats, over_state = over) %>%
    inner_join(seats_under) %>%
    inner_join(seats_parties %>% filter(over > 0) %>% select(party, over_total = over))
  
  if (nrow(df_distr) > 0) {
    df_distr <-
      df_distr %>%
      filter(over_state > 0) %>%
      mutate(
        div_0.5 = votes / (min_seats - 0.5),
        div_1.5 = votes / (min_seats - 1.5),
        div_2.5 = votes / (min_seats - 2.5)
      ) %>%
      group_by(party) %>%
      ungroup() %>%
      mutate(
        min_1 = min(div_0.5),
        min_2 = Rfast::nth(x = c(div_0.5, div_1.5), k = 2),
        min_3 = Rfast::nth(x = c(div_0.5, div_1.5, div_2.5), k = 3)
      ) %>%
      mutate(
        seats_up = ifelse(div_0.5 == min_1, seats_up - 1, seats_up),
        seats_up = ifelse(
          over_total > 1 &
            div_0.5 == min_2 | over_total > 1 & div_1.5 == min_2,
          seats_up - 1,
          seats_up
        ),
        seats_up = ifelse(
          over_total == 3 &
            div_0.5 == min_3 |
            over_total == 3 &
            div_1.5 == min_3 | over_total == 3 & div_2.5 == min_3,
          seats_up - 1,
          seats_up
        )
      )
  }
  
  df_out <-
    seats_under %>%
    rename(seats = seats_up) %>%
    left_join(df_distr %>% select(state, party, seats_new = seats_up)) %>%
    mutate(seats = ifelse(!is.na(seats_new), seats_new, seats)) %>%
    select(-seats_new) %>%
    bind_rows(
      min_seats_states %>%
        filter(party %in% parties_unique) %>%
        select(state, party, seats = min_seats)
    ) %>%
    arrange(state, party) %>%
    left_join(direct_mandates) %>%
    mutate(first = ifelse(is.na(first), 0, first),
           second = seats - first)
  
  return(df_out)
}

# function to determine which candidates are elected
find_mps <- function(first_tier, seats, df_cand) {
  winner_first <-
    first_tier %>%
    group_by(con_id, con_name) %>%
    filter(votes == max(votes)) %>%
    ungroup() %>%
    select(con_id, con_name, party)
  
  seats_list <-
    seats %>%
    select(-c(seats, first))
  
  cand_first <-
    winner_first %>%
    inner_join(df_cand) %>%
    mutate(elected = "Direkt")
  
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
  
  cand_elected <-
    cand_first %>%
    bind_rows(cand_second) %>% mutate(
      con_id = ifelse(elected == "Liste", NA, con_id),
      con_name = ifelse(elected == "Liste", NA, con_name),
      state = ifelse(elected == "Direkt", NA, state),
      list_pos = ifelse(elected == "Direkt", NA, list_pos),
      party = as.factor(party)
    ) %>%
    relocate(title, name, first_name, party, gender, year_birth, elected)
  
  return(cand_elected)
}

# graphics ----------------------------------------------------------------
# function to create a seat plot for the parties in the Bundestag
plot_parl <- function(df, df_seats_org, df_colors) {
  # original data for calculating changes in number of seats
  df_seats_org <-
    df_seats_org %>%
    group_by(party) %>%
    summarize(seats_org = sum(seats))
  
  # combining new and old data and adding colors
  df <-
    df %>%
    mutate(party = as.character(party)) %>%
    group_by(party) %>%
    summarize(seats = sum(seats)) %>%
    mutate(group = ifelse(party %in% c("CDU", "CSU"), "CDU/CSU", party)) %>%
    left_join(df_colors) %>%
    mutate(color_party = ifelse(is.na(color_party), "grey", color_party)) %>%
    inner_join(df_seats_org) %>%
    mutate(diff = seats - seats_org) %>%
    mutate(legend = paste0(party, " (", seats, "/", ifelse(diff > 0, "+", ""), diff, ")")) %>%
    arrange(seats)
  
  # plot
  ggplot(df) +
    geom_parliament(aes(seats = seats, fill =  party), color = "white") +
    scale_fill_manual(values = df$color_party , labels = df$legend) +
    labs(caption = "Zahlen in Klammern zeigen Anzahl der Sitze und Veränderung zum originalen Wahlergebnis.") +
    coord_fixed() +
    theme_void() +
    theme(
      legend.position = "bottom",
      legend.title = element_blank(),
      legend.text = element_text(size = 12),
      plot.caption = element_text(size = 10)
    ) +
    annotate(
      "text",
      x = 0,
      y = 0.4,
      label = paste0(
        "Abg.: ",
        sum(df$seats),
        " (",
        ifelse(sum(df$diff) > 0, "+", ""),
        sum(df$diff),
        ")"
      ),
      colour = "black",
      size = 12
    )
}

# function to create a plot for coalitions
plot_coal <- function(df, df_colors) {
  df <-
    df %>%
    mutate(party = as.character(party)) %>%
    group_by(party) %>%
    summarize(seats = sum(seats)) %>%
    mutate(group = ifelse(party %in% c("CDU", "CSU"), "CDU/CSU", party))
  
  df_group <-
    df %>%
    group_by(group) %>%
    summarize(seats = sum(seats)) %>%
    inner_join(df_colors %>% select(group, color_group) %>% distinct()) %>%
    arrange(desc(seats)) %>%
    mutate(diff = 0)
  
  seats_total <- sum(df_group$seats)
  
  # seats needed for a majority
  seats_need <-
    ifelse(seats_total %% 2 == 1, round(seats_total / 2), seats_total / 2 +
             1)
  
  # possible coalitions
  coalitions <-
    tibble(
      coalition = c(
        rep("GroKo", 2),
        rep("Ampel", 3),
        rep("Jamaika", 3),
        rep("RRG", 3),
        rep("Kenia", 3),
        rep("Deutschland", 3)
      ),
      group = c(
        "SPD",
        "CDU/CSU",
        "SPD",
        "FDP",
        "GRÜNE",
        "CDU/CSU",
        "FDP",
        "GRÜNE",
        "SPD",
        "DIE LINKE",
        "GRÜNE",
        "CDU/CSU",
        "SPD",
        "GRÜNE",
        "CDU/CSU",
        "SPD",
        "FDP"
      )
    )
  
  # final dat with groups, sizes and colors
  df_coalitions <-
    df_group %>%
    inner_join(coalitions) %>%
    group_by(coalition) %>%
    arrange(desc(seats)) %>%
    mutate(group_order = row_number()) %>%
    mutate(coal_size = sum(seats)) %>%
    arrange(desc(coal_size)) %>%
    ungroup()
  
  # plot
  p <-
    ggplot(
      df_coalitions,
      aes(
        x = seats,
        y = reorder(coalition, coal_size),
        group = coalition,
        fill = color_group,
        text = paste("Koalition:",
                     coal_size,
                     "Sitze, davon",
                     group,
                     seats,
                     "Sitze")
      )
    ) +
    geom_bar(stat = "identity") +
    scale_fill_identity() +
    geom_vline(xintercept = seats_need,
               color = "red3",
               size = 0.8) +
    xlab("Sitze") +
    ylab("") +
    theme_minimal() +
    theme(
      axis.text = element_text(size = 12),
      axis.title = element_text(size = 12),
      plot.margin = unit(c(2, 2, 2, 2), "cm")
    )
  
  # making it interactive wtih plotly
  ggplotly(p, tooltip = c("text"))  %>%
    layout(title = list(
      text = paste0(
        'Größen möglicher Koalitionen',
        '<br>',
        '<sup>',
        sprintf(
          "%s (%i) %s",
          "Die rote Linie zeigt die benötigte Sitzzahl",
          seats_need,
          "für die Mehrheit im Bundestag."
        ),
        '</sup>'
      )
    ),
    showlegend = FALSE)
}
