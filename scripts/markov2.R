#Markov Chain Model - Advanced Edition

train <- read.csv("data/train.csv")

moves <- c("R", "P", "S")
outcomes <- c("Win", "Loss", "Tie")

ryan_counts <- as.matrix(ftable(
  prev_move = factor(train$ryan_prev_move, levels = moves),
  prev_outcome = factor(train$ryan_prev_outcome, levels = outcomes),
  curr = factor(train$ryan_move, levels = moves)
))

anna_counts <- as.matrix(ftable(
  prev_move = factor(train$anna_prev_move, levels = moves),
  prev_outcome = factor(train$anna_prev_outcome, levels = outcomes),
  curr = factor(train$anna_move, levels = moves)
))

alpha <- 1
ryan_smoothed <- ryan_counts + alpha
ryan_cond_mat <- ryan_smoothed / rowSums(ryan_smoothed)

anna_smoothed <- anna_counts + alpha
anna_cond_mat <- anna_smoothed / rowSums(anna_smoothed)

cat("Ryan Conditional Transition Probability Matrix\n")
print(round(ryan_cond_mat, 4))

cat("Anna Conditional Transition Probability Matrix\n")
print(round(anna_cond_mat, 4))

#Verification
ryan_row_sums <- rowSums(ryan_cond_mat)
anna_row_sums <- rowSums(anna_cond_mat)
ryan_row_sums
anna_row_sums
#They all pass with the rows sums equal to 1.


#-------------------------------------------------------------


#Model 2 to use in Part 3.
predict_markov2 <- function(prev_move, prev_outcome, matrix) {
  prev_state <- paste0(prev_move, "_", prev_outcome)
  probs <- as.numeric(matrix[prev_state, ])
  names(probs) <- colnames(matrix)
  return(probs)
}

write.csv(ryan_cond_mat, "results/ryan_conditional_matrix_9x3.csv", row.names = TRUE)
write.csv(anna_cond_mat, "results/anna_conditional_matrix_9x3.csv", row.names = TRUE)


#-------------------------------------------------------------