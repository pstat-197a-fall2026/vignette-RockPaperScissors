library(tidyverse)

data <- read.csv("Games-Data.csv") %>%
  mutate(
    winner = case_when(
      Player.1 == Player.2 ~ "Tie",
      (Player.1 == "R" & Player.2 == "S") |
        (Player.1 == "P" & Player.2 == "R") |
        (Player.1 == "S" & Player.2 == "P") ~ "Player 1",
      TRUE ~ "Player 2"
    ),
    p1_prev_result = lag(winner),
    p1_stayed = Player.1 == lag(Player.1),
    p2_stayed = Player.2 == lag(Player.2)
  )


paste("Player 1 wins:", sum(data$winner == "Player 1"))
paste("Player 2 wins:", sum(data$winner == "Player 2"))
paste("Ties:", sum(data$winner == "Tie"))



#how many of each move each player made
move_counts <- data %>%
  pivot_longer(cols = c(Player.1, Player.2), names_to = "player", values_to = "move") %>%
  count(player, move, name = "count")
move_counts_grid <- move_counts %>%
  pivot_wider(names_from = move, values_from = count, values_fill = 0)
move_counts_grid %>%
  rowwise() %>%
  mutate(p_value = chisq.test(c(P, R, S))$p.value) %>%
  ungroup()

#how many winstreaks of each length. ties filtered out
winstreaks_grid <- data %>%
  filter(winner %in% c("Player 1", "Player 2")) %>%
  mutate(streak_id = consecutive_id(winner)) %>%
  count(streak_id, winner, name = "Streak") %>%
  filter(Streak <= 10) %>%
  count(winner, Streak, name = "count") %>%
  complete(
    Streak = 1:10,
    winner = c("Player 1", "Player 2"),
    fill = list(count = 0)
  ) %>%
  pivot_wider(names_from = winner, values_from = count)
winstreaks_grid

#graph of player 1's wins - player 2's wins
ggplot(data, aes(x = 1:nrow(data))) +
  geom_line(aes(y = cumsum(winner == "Player 1") - cumsum(winner == "Player 2")), color = "blue") +
  labs(title = "Cumulative Wins: Player 1 - Player 2", x = "Game Number", y = "Cumulative Wins Difference") +
  theme_minimal()


#tests
chisq.test(table(lag(data$Player.1), data$Player.1))
chisq.test(table(lag(data$Player.2), data$Player.2))

p2_trans <- table(prev = lag(data$Player.2), curr = data$Player.2)
round(prop.table(p2_trans, margin = 1), 3)
round(chisq.test(p2_trans)$stdres, 2)
