

# ==========================================
# BAYESIAN STATISTICS PROJECT
# Stage 1: Data Exploration
# ==========================================

# Load the dataset
games <- read.csv("games.csv",stringsAsFactors = FALSE)

# Display the first six rows
head(games)

# Display the structure of the dataset
str(games)

# Display the dimensions of the dataset
dim(games)

# Display the column names
names(games)


# Identify all participating engines

engines <- sort(unique(c(
  games$white,
  games$black
)))

engines


# Count the different game outcomes
table(games$result)

# Count games played as White
white_games <- table(games$white)

# Count games played as Black
black_games <- table(games$black)

# Calculate total games played by each engine
games_played <- white_games + black_games

games_played

# Filter games involving SchachMaus
sm_games <- subset(
  games,
  white == "SM" | black == "SM"
)

# Count the recorded outcomes
table(sm_games$result)

# Identify games won by SchachMaus
sm_wins <- sum(
  (sm_games$white == "SM" & sm_games$result == "1-0") |
    (sm_games$black == "SM" & sm_games$result == "0-1")
)

# Identify games lost by SchachMaus
sm_losses <- sum(
  (sm_games$white == "SM" & sm_games$result == "0-1") |
    (sm_games$black == "SM" & sm_games$result == "1-0")
)

# Identify drawn games
sm_draws <- sum(sm_games$result == "1/2-1/2")

# Display the results
c(
  Wins = sm_wins,
  Losses = sm_losses,
  Draws = sm_draws
)


# Function to summarise an engine's performance

engine_performance <- function(engine, games) {
  
  # Select games involving the specified engine
  engine_games <- subset(
    games,
    white == engine | black == engine
  )
  
  # Calculate wins
  wins <- sum(
    (engine_games$white == engine &
       engine_games$result == "1-0") |
      (engine_games$black == engine &
         engine_games$result == "0-1")
  )
  
  # Calculate losses
  losses <- sum(
    (engine_games$white == engine &
       engine_games$result == "0-1") |
      (engine_games$black == engine &
         engine_games$result == "1-0")
  )
  
  # Calculate draws
  draws <- sum(
    engine_games$result == "1/2-1/2"
  )
  
  # Return the summary
  data.frame(
    Engine = engine,
    Games = nrow(engine_games),
    Wins = wins,
    Losses = losses,
    Draws = draws
  )
}

# Apply the function to all engines
performance_summary <- do.call(
  rbind,
  lapply(
    engines,
    engine_performance,
    games = games
  )
)

# Display the summary
performance_summary



# Calculate observed score rates

performance_summary$Score_Rate <- (
  performance_summary$Wins +
    0.5 * performance_summary$Draws
) / performance_summary$Games

# Display the updated summary
performance_summary

# Define the Elo expected-score function

elo_expected_score <- function(RA, RB) {
  1 / (1 + 10^((RB - RA) / 400))
}

# Example 1: Equal ratings
elo_expected_score(2000, 2000)

# Example 2: Engine A is stronger
elo_expected_score(2200, 2000)

# Example 3: Engine A is weaker
elo_expected_score(1800, 2000)

library(cmdstanr)
library(posterior)

# Prepare engine IDs for the Bayesian model

engines <- sort(unique(c(games$white, games$black)))

N_engines <- length(engines)
N_games <- nrow(games)

white_id <- match(games$white, engines)
black_id <- match(games$black, engines)

N_games
head(white_id)
head(black_id)
N_engines


# Encode game outcomes for the Bayesian model

result_id <- ifelse(
  games$result == "1-0", 1,
  ifelse(games$result == "1/2-1/2", 2, 3)
)

# Check the encoded outcomes
table(result_id)


# Connect R to the Stan model

stan_model <- cmdstanr::cmdstan_model("chess_rating.stan")

# Prepare data for Stan

stan_data <- list(
  N_engines = N_engines,
  N_games = N_games,
  white_id = as.integer(white_id),
  black_id = as.integer(black_id),
  result_id = as.integer(result_id)
)


# Fit the Bayesian rating model

fit <- stan_model$sample(
  data = stan_data,
  seed = 123,
  chains = 4,
  parallel_chains = 4,
  iter_warmup = 1000,
  iter_sampling = 1000
)

# Summarise engine ratings and draw parameter

fit$summary(variables = c("rating", "draw_fraction"))



# Extract posterior rating samples

rating_samples <- as_draws_matrix(fit$draws("rating"))

# Calculate probability that each engine is stronger than another

prob_superior <- matrix(
  NA,
  nrow = N_engines,
  ncol = N_engines,
  dimnames = list(engines, engines)
)

for (i in 1:N_engines) {
  for (j in 1:N_engines) {
    prob_superior[i, j] <- mean(
      rating_samples[, i] > rating_samples[, j]
    )
  }
}

# Extract posterior rating summaries

rating_summary <- fit$summary(variables = "rating")

# Create the results table

results_table <- data.frame(
  Engine = engines,
  Mean_Rating = round(rating_summary$mean),
  Lower_90 = round(rating_summary$q5),
  Upper_90 = round(rating_summary$q95)
)

results_table



# Assign engine tiers

results_table$Tier <- ifelse(
  results_table$Engine == "E", "Tier 1",
  ifelse(
    results_table$Engine %in% c("A", "SM"), "Tier 2",
    ifelse(
      results_table$Engine %in% c("B", "C"), "Tier 3",
      "Tier 4"
    )
  )
)

# Arrange by estimated rating

results_table <- results_table[
  order(-results_table$Mean_Rating),
]

results_table

fit$diagnostic_summary()

round(prob_superior, 3)