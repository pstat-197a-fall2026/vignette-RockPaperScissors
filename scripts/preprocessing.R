# Run from the project root: Rscript scripts/preprocessing.R
games <- read.csv("data/Games-Data.csv", check.names = FALSE)

# Assume Player 1 is Ryan and Player 2 is Anna; preserve row order.
cleaned_games <- data.frame(
  round = seq_len(nrow(games)),
  ryan_move = toupper(trimws(games[["Player 1"]])),
  anna_move = toupper(trimws(games[["Player 2"]]))
)

# Rock beats scissors, paper beats rock, and scissors beats paper.
ryan_wins <- with(cleaned_games,
  (ryan_move == "R" & anna_move == "S") |
  (ryan_move == "P" & anna_move == "R") |
  (ryan_move == "S" & anna_move == "P")
)
ties <- cleaned_games$ryan_move == cleaned_games$anna_move
cleaned_games$ryan_outcome <- ifelse(ties, "Tie", ifelse(ryan_wins, "Win", "Loss"))
cleaned_games$anna_outcome <- ifelse(ties, "Tie", ifelse(ryan_wins, "Loss", "Win"))

# Shift before splitting so Round 241 retains Round 240's actual history.
cleaned_games$ryan_prev_move <- c(NA, head(cleaned_games$ryan_move, -1))
cleaned_games$anna_prev_move <- c(NA, head(cleaned_games$anna_move, -1))
cleaned_games$ryan_prev_outcome <- c(NA, head(cleaned_games$ryan_outcome, -1))
cleaned_games$anna_prev_outcome <- c(NA, head(cleaned_games$anna_outcome, -1))

# Keep Round 1 in the complete data, but omit it from usable training rows.
train <- subset(cleaned_games, round >= 2 & round <= 240)
test <- subset(cleaned_games, round >= 241 & round <= 300)

write.csv(cleaned_games, "data/cleaned_games.csv", row.names = FALSE, na = "")
write.csv(train, "data/train.csv", row.names = FALSE, na = "")
write.csv(test, "data/test.csv", row.names = FALSE, na = "")
