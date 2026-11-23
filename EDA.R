library(tidyverse)

data <- read.csv("Games-Data.csv")

#how many each player won
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
paste("Player 1 wins:", win_counts$wins[win_counts$winner == "Player 1"])
paste("Player 2 wins:", win_counts$wins[win_counts$winner == "Player 2"])
paste("Ties:", nrow(data) - sum(win_counts$wins))



#how many of each move each player made
move_counts <- data %>%
  pivot_longer(cols = c(Player.1, Player.2), names_to = "player", values_to = "move") %>%
  count(player, move, name = "count")
move_counts_grid <- move_counts %>%
  pivot_wider(names_from = move, values_from = count, values_fill = 0)
move_counts_grid