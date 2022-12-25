start_time <- Sys.time()

# Load packages ----------------------------------------------------------------

source("C:/Users/samh/OneDrive - Centre for Sustainable Energy/R/packages-&-useful-functions.R")

# Load data --------------------------------------------------------------------

deck_of_cards <- read_csv(here("data", "deck-of-cards.csv")) |> 
  clean_names()

# Functions --------------------------------------------------------------------

shuffle_deck <- function(deck_of_cards) {
  random_order <- sample(1:52, size = 52)
  deck_of_cards[random_order, ]
}

# Simulation -------------------------------------------------------------------

N <- 10000000

streak <- integer(N)
streak2 <- integer(N)

for (i in 1:N) {
  
  shuffled_deck <- shuffle_deck(deck_of_cards)
  
  values <- shuffled_deck$value_ace_high
  
  correct <- 0
  
  for (x in 1:51) {
    
    card_val_history <- values[1:x]
    
    high_low_bal <- sum(card_val_history > 8) - sum(card_val_history < 8)
    
    if (values[x] < 8) {
      decision <- "higher"
    } else if (values[x] > 8) {
      decision <- "lower"
    } else if (values[x] == 8 & high_low_bal > 0) {
      decision <- "lower"
    } else if (values[x] == 8 & high_low_bal < 0) {
      decision <- "higher"
    } else if (values[x] == 8 & high_low_bal == 0) {
      decision <- sample(c("lower", "higher"), size = 1)
    } else {
      print("There is an issue")
    }
    
    if (values[x+1] > values[x] & decision == "higher") {
      correct <- correct + 1
      streak[i] <- correct
    } else if (values[x+1] > values[x] & decision == "lower") {
      break
    } else if (values[x+1] < values[x] & decision == "higher") {
      break
    } else if (values[x+1] < values[x] & decision == "lower") {
      correct <- correct + 1
      streak[i] <- correct
    } else if (values[x+1] == values[x]) {
      next
    } else {
      print("There is an issue")
    }
  }

  correct <- 0

  for (x in 1:51) {
    
    if (values[x] < 8) {
      decision <- "higher"
    } else if (values[x] > 8) {
      decision <- "lower"
    } else if (values[x] == 8) {
      decision <- sample(c("lower", "higher"), size = 1)
    } else {
      print("There is an issue")
    }
    
    if (values[x+1] > values[x] & decision == "higher") {
      correct <- correct + 1
      streak2[i] <- correct
    } else if (values[x+1] > values[x] & decision == "lower") {
      break
    } else if (values[x+1] < values[x] & decision == "higher") {
      break
    } else if (values[x+1] < values[x] & decision == "lower") {
      correct <- correct + 1
      streak2[i] <- correct
    } else if (values[x+1] == values[x]) {
      next
    } else {
      print("There is an issue")
    }
  }
}

end_time <- Sys.time()
time_taken <- round(end_time - start_time, 2)
time_taken

# ------------------------------------------------------------------------------

result <- tibble(winning_streak = 1:51, optimum_prob = 0, guess_prob = 0)

for (y in 1:51) {
  result$optimum_prob[y] = sum(streak >= y) / N
  result$guess_prob[y] = sum(streak2 >= y) / N
}

ggplot(result, aes(x = winning_streak, y = optimum_prob)) + 
  geom_line(color = "blue") +
  geom_point()

result <- result |> 
  mutate(prob_ratio = optimum_prob / guess_prob)

ggplot(result, aes(x = winning_streak, y = prob_ratio)) + 
  geom_line(color = "red") +
  geom_point()
