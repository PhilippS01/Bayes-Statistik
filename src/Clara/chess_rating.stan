

data {
  int<lower=1> N_engines;
  int<lower=1> N_games;
  array[N_games] int<lower=1, upper=N_engines> white_id;
  array[N_games] int<lower=1, upper=N_engines> black_id;
  array[N_games] int<lower=1, upper=3> result_id;
}

parameters {
  vector[N_engines] rating;
  real<lower=0, upper=1> draw_fraction;
}

transformed parameters {
  array[N_games] simplex[3] outcome_prob;

  for (i in 1:N_games) {
    real expected_score;
    real draw_prob;

    expected_score = inv_logit(
      log(10) * (rating[white_id[i]] - rating[black_id[i]]) / 400
    );

    draw_prob = 2 * fmin(expected_score, 1 - expected_score)
                * draw_fraction;

    outcome_prob[i, 1] = expected_score - 0.5 * draw_prob;
    outcome_prob[i, 2] = draw_prob;
    outcome_prob[i, 3] = 1 - expected_score - 0.5 * draw_prob;
  }
}

model {
  // Prior distributions for engine ratings
  rating ~ normal(2000, 200);

  // Prior for the draw parameter
  draw_fraction ~ beta(1, 1);

  // Likelihood of observed game results
  for (i in 1:N_games) {
    result_id[i] ~ categorical(outcome_prob[i]);
  }
}
