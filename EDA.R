library(tidyverse)

data <- read.csv("Games-Data.csv")

win_counts <- data %>%
  mutate(
    winner = case_when(
      Player.1 == Player.2 ~ "Tie",
      (Player.1 == "R" & Player.2 == "S") |
        (Player.1 == "P" & Player.2 == "R") |
        (Player.1 == "S" & Player.2 == "P") ~ "Player 1",
      (Player.2 == "R" & Player.1 == "S") |
        (Player.2 == "P" & Player.1 == "R") |
        (Player.2 == "S" & Player.1 == "P") ~ "Player 2",
      TRUE ~ NA_character_
    )
  ) %>%
  filter(winner != "Tie") %>%
  count(winner, name = "wins")

win_counts