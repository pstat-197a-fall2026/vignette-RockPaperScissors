#Markov Chain Model - Simplistic Edition

train <- read.csv("data/train.csv")
moves <- c("R", "P", "S")


ryan_counts <- as.matrix(table(
  prev = factor(train$ryan_prev_move, levels = moves),
  curr = factor(train$ryan_move, levels = moves)
))

anna_counts <- as.matrix(table(
  prev = factor(train$anna_prev_move, levels = moves),
  curr = factor(train$anna_move, levels = moves)
))


alpha <- 1
ryan_smoothed <- ryan_counts + alpha
ryan_trans_mat <- ryan_smoothed / rowSums(ryan_smoothed)

anna_smoothed <- anna_counts + alpha
anna_trans_mat <- anna_smoothed / rowSums(anna_smoothed)

cat("Ryan Transition Probability Matrix\n")
print(round(ryan_trans_mat, 4))

cat("Anna Transition Probability Matrix\n")
print(round(anna_trans_mat, 4))

#Verification
ryan_row_sums <- rowSums(ryan_trans_mat)
anna_row_sums <- rowSums(anna_trans_mat)
ryan_row_sums
anna_row_sums
#They all pass with the rows sums equal to 1.


#-------------------------------------------------------------


#Model 1 to use in Part 3.
predict_markov1 <- function(prev_move, matrix) {
  prev_move <- toupper(trimws(prev_move))
  probs <- as.numeric(matrix[prev_move, ])
  names(probs) <- colnames(matrix)
  return(probs)
}

write.csv(ryan_trans_mat, "results/ryan_transition_matrix_3x3.csv", row.names = TRUE)
write.csv(anna_trans_mat, "results/anna_transition_matrix_3x3.csv", row.names = TRUE)


#-------------------------------------------------------------