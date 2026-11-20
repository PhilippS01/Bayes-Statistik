

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

stan_model_tc <- cmdstanr::cmdstan_model("chess_rating_tc.stan")

# Prepare data for Stan

stan_data_tc <- list(
  N_engines = N_engines,
  N_games = N_games,
  white_id = as.integer(white_id),
  black_id = as.integer(black_id),
  result_id = as.integer(result_id),
  tc_scaled = as.vector(games$tc_scaled)
)


# ==========================================
# STAGE 4: FIT TIME-CONTROL BAYESIAN MODEL
# ==========================================

fit_tc <- stan_model_tc$sample(
  data = stan_data_tc,
  seed = 123,
  chains = 4,
  parallel_chains = 4,
  iter_warmup = 1000,
  iter_sampling = 1000
)

# Posterior summary
fit_tc$summary(
  variables = c(
    "rating_intercept",
    "rating_slope",
    "draw_fraction"
  )
)

# Sampling diagnostics
fit_tc$diagnostic_summary()


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

diagnostics <- fit$summary(
  variables = c("rating", "draw_fraction")
)

diagnostics



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

write.csv(
  results_table,
  "final_engine_rankings.csv",
  row.names = FALSE
)

file.exists("final_engine_rankings.csv")







# ==========================================
# STAGE 2: TIME CONTROL PREPARATION
# ==========================================

# Split timecontrol into initial time and increment
tc_parts <- strsplit(games$timecontrol, "+", fixed = TRUE)

# Extract initial time in seconds
initial_time <- as.numeric(
  sapply(tc_parts, function(x) x[1])
)

# Extract increment in seconds
increment <- as.numeric(
  sapply(tc_parts, function(x) x[2])
)

# Calculate expected game duration
games$tc <- initial_time + (40 * increment)

# Display updated dataset
head(games)

# Examine the distribution of time controls
table(games$timecontrol)

# Summarise expected game duration
summary(games$tc)


# ==========================================
# STANDARDISE EXPECTED GAME DURATION
# ==========================================

# Calculate mean and standard deviation
tc_mean <- mean(games$tc)
tc_sd <- sd(games$tc)

# Standardise time control
games$tc_scaled <- (games$tc - tc_mean) / tc_sd

# Examine the standardised values
summary(games$tc_scaled)

# Check mean and standard deviation
mean(games$tc_scaled)
sd(games$tc_scaled)

# ==========================================
# STAGE 5: EXTRACT POSTERIOR SAMPLES
# ==========================================

# Extract baseline rating samples
intercept_samples <- posterior::as_draws_matrix(
  fit_tc$draws("rating_intercept")
)

# Extract time-control slope samples
slope_samples <- posterior::as_draws_matrix(
  fit_tc$draws("rating_slope")
)

# ==========================================
# STAGE 5: EXTRACT POSTERIOR SAMPLES
# ==========================================

# Extract baseline rating samples
intercept_samples <- posterior::as_draws_matrix(
  fit_tc$draws("rating_intercept")
)

# Extract time-control slope samples
slope_samples <- posterior::as_draws_matrix(
  fit_tc$draws("rating_slope")
)

# Examine the dimensions
dim(intercept_samples)
dim(slope_samples)



# Examine the dimensions
dim(intercept_samples)
dim(slope_samples)


# ==========================================
# STAGE 6: CREATE TIME-CONTROL GRID
# ==========================================

# Generate 100 equally spaced durations
tc_grid <- seq(
  from = min(games$tc),
  to = max(games$tc),
  length.out = 100
)

# Standardise the durations
tc_grid_scaled <- (tc_grid - tc_mean) / tc_sd

# Examine the duration range
range(tc_grid)

# Display the first few values
head(tc_grid)


# ==========================================
# STAGE 7: CALCULATE TIME-DEPENDENT RATINGS
# ==========================================

# Create an empty list
rating_curves <- list()

# Calculate ratings for each engine
for (j in 1:N_engines) {
  
  # Convert posterior samples to numeric vectors
  intercept_j <- as.numeric(intercept_samples[, j])
  slope_j <- as.numeric(slope_samples[, j])
  
  # Calculate posterior ratings at each duration
  rating_draws <- outer(
    intercept_j,
    rep(1, length(tc_grid))
  ) +
    outer(
      slope_j,
      tc_grid_scaled
    )
  
  # Calculate posterior summaries
  mean_rating <- colMeans(rating_draws)
  sd_rating <- apply(rating_draws, 2, sd)
  
  lower_90 <- apply(
    rating_draws, 2, quantile, probs = 0.05
  )
  
  upper_90 <- apply(
    rating_draws, 2, quantile, probs = 0.95
  )
  
  # Store results
  rating_curves[[j]] <- data.frame(
    Engine = engines[j],
    tc = tc_grid,
    Mean_Rating = mean_rating,
    SD_Rating = sd_rating,
    Lower_90 = lower_90,
    Upper_90 = upper_90
  )
}

# Combine all engines
rating_curves <- do.call(rbind, rating_curves)

# Check the results
dim(rating_curves)
head(rating_curves)
table(rating_curves$Engine)
 
    

# ==========================================
# INVESTIGATE RATING CALCULATIONS
# ==========================================

# Check standardised durations
head(tc_grid_scaled)

# Check Engine A's intercept and slope
mean(intercept_samples[, 1])
mean(slope_samples[, 1])

# Calculate Engine A's ratings at the first six durations
rating_test <- outer(
  intercept_samples[, 1],
  rep(1, 6)
) +
  outer(
    slope_samples[, 1],
    tc_grid_scaled[1:6]
  )

# Examine the dimensions
dim(rating_test)

# Calculate mean ratings at the six durations
colMeans(rating_test)


# ==========================================
# STAGE 8: TIME-DEPENDENT RATING PLOT
# ==========================================

library(ggplot2)

ggplot(rating_curves,
       aes(x = tc, y = Mean_Rating,
           colour = Engine, fill = Engine)) +
  
  # 90% credible intervals
  geom_ribbon(
    aes(ymin = Lower_90, ymax = Upper_90),
    alpha = 0.12,
    colour = NA
  ) +
  
  # Posterior mean rating curves
  geom_line(linewidth = 1) +
  
  # Required time-control reference lines
  geom_vline(
    xintercept = c(15, 60, 260, 600),
    linetype = "dashed",
    colour = "grey40",
    linewidth = 0.5
  ) +
  
  # Time-control labels
  annotate(
    "text",
    x = c(15, 60, 260, 600),
    y = 3050,
    label = c("15+0", "60+0", "180+2", "600+0"),
    angle = 90,
    vjust = -0.5,
    size = 3
  ) +
  
  # Axis labels and title
  labs(
    title = "Chess Engine Ratings Across Different Time Controls",
    subtitle = "Posterior mean ratings with 90% credible intervals",
    x = "Expected Game Duration (seconds)",
    y = "Estimated Rating",
    colour = "Engine",
    fill = "Engine"
  ) +
  
  theme_minimal() +
  
  theme(
    plot.title = element_text(face = "bold"),
    legend.position = "right"
  )





# =====================================================
# STAGE 9: ALL FOUR TIME-CONTROL RANKINGS
# =====================================================

# Define the four required time controls
time_controls <- c(
  "Ultra Bullet (15+0)" = 15,
  "Bullet (60+0)" = 60,
  "Blitz (180+2)" = 260,
  "Rapid (600+0)" = 600
)

# Create storage for results
all_rankings <- list()
all_superiority <- list()
all_rating_samples <- list()

# Loop through each time control
for (category in names(time_controls)) {
  
  # Get expected game duration
  tc_value <- time_controls[[category]]
  
  # Standardise expected duration
  tc_scaled_value <- (tc_value - tc_mean) / tc_sd
  
  # Calculate posterior ratings
  rating_samples <- matrix(
    NA_real_,
    nrow = nrow(intercept_samples),
    ncol = N_engines
  )
  
  for (j in 1:N_engines) {
    
    rating_samples[, j] <-
      intercept_samples[, j] +
      slope_samples[, j] * tc_scaled_value
  }
  
  # Assign engine names
  colnames(rating_samples) <- engines
  
  # Calculate posterior rating summaries
  rating_summary <- data.frame(
    Engine = engines,
    Mean_Rating = colMeans(rating_samples),
    SD_Rating = apply(rating_samples, 2, sd),
    Lower_90 = apply(
      rating_samples, 2, quantile, probs = 0.05
    ),
    Upper_90 = apply(
      rating_samples, 2, quantile, probs = 0.95
    )
  )
  
  # Rank engines by mean posterior rating
  rating_summary <- rating_summary[
    order(-rating_summary$Mean_Rating),
  ]
  
  # Calculate pairwise superiority probabilities
  superiority <- matrix(
    NA_real_,
    nrow = N_engines,
    ncol = N_engines,
    dimnames = list(engines, engines)
  )
  
  for (i in 1:N_engines) {
    for (j in 1:N_engines) {
      
      superiority[i, j] <- mean(
        rating_samples[, i] > rating_samples[, j]
      )
    }
  }
  
  # Store all results
  all_rankings[[category]] <- rating_summary
  
  all_superiority[[category]] <- round(
    superiority, 3
  )
  
  all_rating_samples[[category]] <- rating_samples
}

# =====================================================
# DISPLAY ALL FOUR RANKINGS AND SUPERIORITY MATRICES
# =====================================================

for (category in names(all_rankings)) {
  
  cat("\n\n========================================\n")
  cat(category, "\n")
  cat("========================================\n")
  
  cat("\nPOSTERIOR RATING SUMMARY\n")
  
  print(
    transform(
      all_rankings[[category]],
      Mean_Rating = round(Mean_Rating, 2),
      SD_Rating = round(SD_Rating, 2),
      Lower_90 = round(Lower_90, 2),
      Upper_90 = round(Upper_90, 2)
    ),
    row.names = FALSE
  )
  
  cat("\nPAIRWISE SUPERIORITY PROBABILITIES\n")
  
  print(all_superiority[[category]])
}

# =====================================================
# CONFIRM THAT ALL FOUR RESULTS WERE CREATED
# =====================================================

cat("\n\nALL FOUR TIME-CONTROL RANKINGS COMPLETED.\n")

names(all_rankings)
names(all_superiority)





### QUESTION 3


# Extract posterior probabilities of game outcomes
outcome_samples <- fit$draws(
  variables = "outcome_prob",
  format = "matrix"
)

# Check the dimensions
dim(outcome_samples)

# Display the first few columns
head(outcome_samples[, 1:6])


# Set seed for reproducibility
set.seed(123)

# Number of posterior samples
n_samples <- nrow(outcome_samples)

# Number of games
n_games <- N_games

# Store simulated outcomes
y_rep <- matrix(
  NA_integer_,
  nrow = n_samples,
  ncol = n_games
)

# Simulate game outcomes
for (s in 1:n_samples) {
  
  for (i in 1:n_games) {
    
    # Extract probabilities for the three outcomes
    probs <- as.numeric(
      outcome_samples[s, c(
        paste0("outcome_prob[", i, ",1]"),
        paste0("outcome_prob[", i, ",2]"),
        paste0("outcome_prob[", i, ",3]")
      )]
    )
    
    # Simulate one game outcome
    y_rep[s, i] <- sample(
      1:3,
      size = 1,
      prob = probs
    )
  }
}

# Check the dimensions
dim(y_rep)


# Actual game outcomes
actual_outcomes <- table(
  factor(result_id, levels = 1:3)
)

names(actual_outcomes) <- c(
  "White Wins",
  "Draws",
  "Black Wins"
)

actual_outcomes


simulated_counts <- t(
  apply(y_rep, 1, function(x) {
    table(factor(x, levels = 1:3))
  })
)
simulated_summary


# ==========================================
# SUMMARISE SIMULATED GAME OUTCOMES
# ==========================================

# Count simulated outcomes in each posterior sample
simulated_counts <- t(
  apply(y_rep, 1, function(x) {
    table(factor(x, levels = 1:3))
  })
)

# Assign outcome names
colnames(simulated_counts) <- c(
  "White Wins",
  "Draws",
  "Black Wins"
)

# Calculate posterior predictive summary
simulated_summary <- data.frame(
  Outcome = colnames(simulated_counts),
  Mean = colMeans(simulated_counts),
  Lower_90 = apply(
    simulated_counts, 2, quantile, probs = 0.05
  ),
  Upper_90 = apply(
    simulated_counts, 2, quantile, probs = 0.95
  )
)

# Display results
print(simulated_summary, row.names = FALSE)


# ==========================================
# POSTERIOR PREDICTIVE CHECK: ORIGINAL MODEL
# GROUPED BAR CHART
# ==========================================

library(ggplot2)
library(dplyr)
library(tidyr)

# 1. Prepare actual game outcomes
actual_df <- data.frame(
  Outcome = names(actual_outcomes),
  Actual = as.numeric(actual_outcomes)
)

# 2. Combine actual and predicted results
comparison_df <- simulated_summary %>%
  left_join(actual_df, by = "Outcome")

# 3. Reshape data for grouped bar chart
comparison_long <- comparison_df %>%
  select(Outcome, Actual, Mean) %>%
  pivot_longer(
    cols = c(Actual, Mean),
    names_to = "Result",
    values_to = "Number_of_Games"
  )

# 4. Create grouped bar chart
ggplot(
  comparison_long,
  aes(
    x = Outcome,
    y = Number_of_Games,
    fill = Result
  )
) +
  geom_col(
    position = position_dodge(width = 0.8),
    width = 0.7
  ) +
  labs(
    title = "Posterior Predictive Check: Original Model (Without Time Control)",
    subtitle = "Actual game outcomes compared with predicted mean counts",
    x = "Game Outcome",
    y = "Number of Games",
    fill = "Result"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(face = "bold"),
    legend.position = "right"
  )






# ==========================================
# STAGE 2: MODEL WITH TIME CONTROL
# ==========================================

# Extract posterior probabilities
outcome_samples_tc <- fit_tc$draws(
  variables = "outcome_prob",
  format = "matrix"
)

# Check dimensions
dim(outcome_samples_tc)

# Display the first few columns
head(outcome_samples_tc[, 1:6])



# ==========================================
# SIMULATE GAME OUTCOMES: MODEL WITH TIME CONTROL
# ==========================================

# Set seed for reproducibility
set.seed(123)

# Number of posterior samples
n_samples_tc <- nrow(outcome_samples_tc)

# Number of games
n_games_tc <- N_games

# Store simulated outcomes
y_rep_tc <- matrix(
  NA_integer_,
  nrow = n_samples_tc,
  ncol = n_games_tc
)

# Simulate game outcomes
for (s in 1:n_samples_tc) {
  
  for (i in 1:n_games_tc) {
    
    # Extract probabilities for the three outcomes
    probs <- as.numeric(
      outcome_samples_tc[s, c(
        paste0("outcome_prob[", i, ",1]"),
        paste0("outcome_prob[", i, ",2]"),
        paste0("outcome_prob[", i, ",3]")
      )]
    )
    
    # Simulate one game outcome
    y_rep_tc[s, i] <- sample(
      1:3,
      size = 1,
      prob = probs
    )
  }
}

# Check dimensions
dim(y_rep_tc)



# ==========================================
# SUMMARISE SIMULATED OUTCOMES: MODEL 2
# ==========================================

# Count simulated outcomes in each posterior sample
simulated_counts_tc <- t(
  apply(y_rep_tc, 1, function(x) {
    table(factor(x, levels = 1:3))
  })
)

# Assign outcome names
colnames(simulated_counts_tc) <- c(
  "White Wins",
  "Draws",
  "Black Wins"
)

# Calculate posterior predictive summary
simulated_summary_tc <- data.frame(
  Outcome = colnames(simulated_counts_tc),
  Mean = colMeans(simulated_counts_tc),
  Lower_90 = apply(
    simulated_counts_tc, 2, quantile, probs = 0.05
  ),
  Upper_90 = apply(
    simulated_counts_tc, 2, quantile, probs = 0.95
  )
)

# Display results
print(simulated_summary_tc, row.names = FALSE)


# ==========================================
# POSTERIOR PREDICTIVE CHECK: MODEL WITH TIME CONTROL
# GROUPED BAR CHART
# ==========================================

# Prepare actual game outcomes
actual_df_tc <- data.frame(
  Outcome = names(actual_outcomes),
  Actual = as.numeric(actual_outcomes)
)

# Combine actual and predicted results
comparison_df_tc <- simulated_summary_tc %>%
  left_join(actual_df_tc, by = "Outcome")

# Reshape data for grouped bar chart
comparison_long_tc <- comparison_df_tc %>%
  select(Outcome, Actual, Mean) %>%
  pivot_longer(
    cols = c(Actual, Mean),
    names_to = "Result",
    values_to = "Number_of_Games"
  )

# Create grouped bar chart
ggplot(
  comparison_long_tc,
  aes(
    x = Outcome,
    y = Number_of_Games,
    fill = Result
  )
) +
  geom_col(
    position = position_dodge(width = 0.8),
    width = 0.7
  ) +
  labs(
    title = "Posterior Predictive Check: Extended Model (With Time Control)",
    subtitle = "Actual game outcomes compared with predicted mean counts",
    x = "Game Outcome",
    y = "Number of Games",
    fill = "Result"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(face = "bold"),
    legend.position = "right"
  )






# ==========================================
# INVESTIGATING WHITE-PIECE ADVANTAGE
# ==========================================

# Count actual game outcomes
white_wins <- sum(result_id == 1)
draws <- sum(result_id == 2)
black_wins <- sum(result_id == 3)

# Calculate percentages of all games
total_games <- length(result_id)

white_percentage <- (white_wins / total_games) * 100
draw_percentage <- (draws / total_games) * 100
black_percentage <- (black_wins / total_games) * 100

# Calculate white's share of decisive games
decisive_games <- white_wins + black_wins

white_decisive_percentage <-
  (white_wins / decisive_games) * 100

black_decisive_percentage <-
  (black_wins / decisive_games) * 100

# Display results
cat("WHITE-PIECE ADVANTAGE ANALYSIS\n\n")

cat("White wins:", white_wins, "\n")
cat("Draws:", draws, "\n")
cat("Black wins:", black_wins, "\n\n")

cat("Percentage of all games:\n")
cat("White wins:", round(white_percentage, 2), "%\n")
cat("Draws:", round(draw_percentage, 2), "%\n")
cat("Black wins:", round(black_percentage, 2), "%\n\n")

cat("Among decisive games:\n")
cat("White wins:", round(white_decisive_percentage, 2), "%\n")
cat("Black wins:", round(black_decisive_percentage, 2), "%\n")



# Compile the revised model
stan_model_white <- cmdstanr::cmdstan_model(
  "chess_rating_white.stan"
)

# Confirm that compilation was successful
stan_model_white$exe_file()

# Check that the executable exists
file.exists(stan_model_white$exe_file())



# ==========================================
# FIT MODEL WITH WHITE-PIECE ADVANTAGE
# ==========================================

fit_white <- stan_model_white$sample(
  data = stan_data,
  seed = 123,
  chains = 4,
  parallel_chains = 4,
  iter_warmup = 1000,
  iter_sampling = 1000
)

# ==========================================
# MODEL WITH WHITE-PIECE ADVANTAGE
# POSTERIOR SUMMARY
# ==========================================

# Display the posterior summary
fit_white$summary(
  variables = c("rating", "white_advantage", "draw_fraction")
)


# ==========================================
# POSTERIOR PREDICTIVE CHECK: REVISED MODEL
# ==========================================

# Extract posterior outcome probabilities
outcome_samples_white <- fit_white$draws(
  variables = "outcome_prob",
  format = "matrix"
)

# Check dimensions
dim(outcome_samples_white)

# Display first few columns
head(outcome_samples_white[, 1:6])




# ==========================================
# SIMULATE GAME OUTCOMES: WHITE ADVANTAGE MODEL
# ==========================================

# Set seed for reproducibility
set.seed(123)

# Number of posterior samples
n_samples_white <- nrow(outcome_samples_white)

# Number of games
n_games_white <- N_games

# Store simulated outcomes
y_rep_white <- matrix(
  NA_integer_,
  nrow = n_samples_white,
  ncol = n_games_white
)

# Simulate game outcomes
for (s in 1:n_samples_white) {
  
  for (i in 1:n_games_white) {
    
    # Extract probabilities for the three outcomes
    probs <- as.numeric(
      outcome_samples_white[s, c(
        paste0("outcome_prob[", i, ",1]"),
        paste0("outcome_prob[", i, ",2]"),
        paste0("outcome_prob[", i, ",3]")
      )]
    )
    
    # Simulate one game outcome
    y_rep_white[s, i] <- sample(
      1:3,
      size = 1,
      prob = probs
    )
  }
}

# Check dimensions
dim(y_rep_white)


# ==========================================
# SUMMARISE SIMULATED OUTCOMES:
# WHITE-ADVANTAGE MODEL
# ==========================================

# Count simulated outcomes in each posterior sample
simulated_counts_white <- t(
  apply(y_rep_white, 1, function(x) {
    table(factor(x, levels = 1:3))
  })
)

# Assign outcome names
colnames(simulated_counts_white) <- c(
  "White Wins",
  "Draws",
  "Black Wins"
)

# Calculate posterior predictive summary
simulated_summary_white <- data.frame(
  Outcome = colnames(simulated_counts_white),
  Mean = colMeans(simulated_counts_white),
  Lower_90 = apply(
    simulated_counts_white, 2, quantile, probs = 0.05
  ),
  Upper_90 = apply(
    simulated_counts_white, 2, quantile, probs = 0.95
  )
)

# Display results
print(simulated_summary_white, row.names = FALSE)



# ==========================================
# POSTERIOR PREDICTIVE CHECK:
# MODEL WITH WHITE-PIECE ADVANTAGE
# GROUPED BAR CHART
# ==========================================

# Prepare actual game outcomes
actual_df_white <- data.frame(
  Outcome = names(actual_outcomes),
  Actual = as.numeric(actual_outcomes)
)

# Combine actual and predicted results
comparison_df_white <- simulated_summary_white %>%
  left_join(actual_df_white, by = "Outcome")

# Reshape data for grouped bar chart
comparison_long_white <- comparison_df_white %>%
  select(Outcome, Actual, Mean) %>%
  pivot_longer(
    cols = c(Actual, Mean),
    names_to = "Result",
    values_to = "Number_of_Games"
  )

# Create grouped bar chart
ggplot(
  comparison_long_white,
  aes(
    x = Outcome,
    y = Number_of_Games,
    fill = Result
  )
) +
  geom_col(
    position = position_dodge(width = 0.8),
    width = 0.7
  ) +
  labs(
    title = "Posterior Predictive Check: Model with White-Piece Advantage",
    subtitle = "Actual game outcomes compared with predicted mean counts",
    x = "Game Outcome",
    y = "Number of Games",
    fill = "Result"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(face = "bold"),
    legend.position = "right"
  )


# ==========================================
# DIAGNOSTICS: MODEL WITH WHITE ADVANTAGE
# ==========================================

# Posterior sampling diagnostics
fit_white$diagnostic_summary()

# Check convergence statistics
fit_white$summary(
  variables = c("rating", "white_advantage", "draw_fraction")
)


fit_white$diagnostic_summary()




# ==========================================
# FINAL COMPARISON OF ALL THREE MODELS
# ==========================================

# Actual game outcomes
actual_df <- data.frame(
  Outcome = names(actual_outcomes),
  Actual = as.numeric(actual_outcomes)
)

# Combine results from all three models
model_comparison <- actual_df %>%
  left_join(
    simulated_summary %>%
      rename(
        Model1_Mean = Mean,
        Model1_Lower = Lower_90,
        Model1_Upper = Upper_90
      ),
    by = "Outcome"
  ) %>%
  left_join(
    simulated_summary_tc %>%
      rename(
        Model2_Mean = Mean,
        Model2_Lower = Lower_90,
        Model2_Upper = Upper_90
      ),
    by = "Outcome"
  ) %>%
  left_join(
    simulated_summary_white %>%
      rename(
        Model3_Mean = Mean,
        Model3_Lower = Lower_90,
        Model3_Upper = Upper_90
      ),
    by = "Outcome"
  )

# Display comparison
print(model_comparison, row.names = FALSE)