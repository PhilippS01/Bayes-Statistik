
data {
  int<lower=1> N;
  int<lower=1> K;

  array[N] int<lower=1, upper=K> white;
  array[N] int<lower=1, upper=K> black;
  array[N] int<lower=1, upper=3> outcome;
}

parameters {
  vector[K] rating;
  real<lower=0, upper=1> draw_tendency;
}

model {
  // Priors
  rating ~ normal(2000, 200);
  draw_tendency ~ beta(1, 1);

  // Likelihood
  for (n in 1:N) {
    real e;
    real p_draw;
    vector[3] p;

    // Expected score from Elo ratings
    e = inv_logit(
      log(10.0) / 400 *
      (rating[white[n]] - rating[black[n]])
    );

    // Draw probability
    p_draw = 2 * draw_tendency * e * (1 - e);

    // Probabilities of all three outcomes
    p[1] = e - 0.5 * p_draw;
    p[2] = p_draw;
    p[3] = 1 - e - 0.5 * p_draw;

    outcome[n] ~ categorical(p);
  }
}
