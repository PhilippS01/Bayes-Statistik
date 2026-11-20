

data {
  int<lower=1> N_engines;
  int<lower=1> N_games;

  array[N_games] int<lower=1, upper=N_engines> white_id;
  array[N_games] int<lower=1, upper=N_engines> black_id;
  array[N_games] int<lower=1, upper=3> result_id;

  // Standardised expected game duration
  array[N_games] real tc_scaled;
}

parameters {
  // Baseline rating for each engine
  vector[N_engines] rating_intercept;

  // Effect of time control on each engine's rating
  vector[N_engines] rating_slope;

  // Draw parameter
  real<lower=0, upper=1> draw_fraction;
}

transformed parameters {
  array[N_games] simplex[3] outcome_prob;

  for (i in 1:N_games) {
    real white_rating;
    real black_rating;
    real expected_score;
    real draw_prob;

    // Calculate time-dependent ratings
    white_rating =
      rating_intercept[white_id[i]]
      + rating_slope[white_id[i]] * tc_scaled[i];

    black_rating =
      rating_intercept[black_id[i]]
      + rating_slope[black_id[i]] * tc_scaled[i];

    // Calculate expected score
    expected_score = inv_logit(
      log(10) * (white_rating - black_rating) / 400
    );

    // Calculate draw probability
    draw_prob = 2 * fmin(expected_score, 1 - expected_score)
                * draw_fraction;

    // Calculate outcome probabilities
    outcome_prob[i, 1] = expected_score - 0.5 * draw_prob;
    outcome_prob[i, 2] = draw_prob;
    outcome_prob[i, 3] = 1 - expected_score - 0.5 * draw_prob;
  }
}

model {
  // Prior distributions for baseline ratings
  rating_intercept ~ normal(2000, 200);

  // Prior distributions for time-control effects
  rating_slope ~ normal(0, 100);

  // Prior for the draw parameter
  draw_fraction ~ beta(1, 1);

  // Likelihood of observed game results
  for (i in 1:N_games) {
    result_id[i] ~ categorical(outcome_prob[i]);
  }
}
