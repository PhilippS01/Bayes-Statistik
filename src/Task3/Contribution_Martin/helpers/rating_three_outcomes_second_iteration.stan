data {
  int<lower=1> N;
  int<lower=1> K;

  array[N] int<lower=1, upper=K> white;
  array[N] int<lower=1, upper=K> black;
  array[N] int<lower=1, upper=3> outcome;
  
  int<lower=1> P;
  array[N] int<lower=1, upper=P> pair_id;
  array[N] int<lower=-1, upper=1> matchup_direction;
}

parameters {
  vector[K] rating;
  real<lower=0, upper=1> draw_tendency;
  real white_advantage;
  
  vector[P] pair_raw;
  real<lower=0> matchup_sd;
}

transformed parameters {
  vector[P] pair_effect;

  pair_effect = matchup_sd * pair_raw;
}

model {
  // Priors
  rating ~ normal(2000, 200);
  draw_tendency ~ beta(1, 1);
  white_advantage ~ normal(0, 50);
  
  pair_raw ~ std_normal();
  matchup_sd ~ normal(0, 50);

  // Likelihood
  for (n in 1:N) {
    real e;
    real p_draw;
    real delta;
    vector[3] p;

    // Expected score for Elo (formula given in the task)
    delta = rating[black[n]] - (rating[white[n]] + white_advantage + matchup_direction[n] * pair_effect[pair_id[n]]);
    e = 1 / (1 + pow(10.0, (delta) / 400));

    // Draw probability
    p_draw = 2 * draw_tendency * e * (1 - e);

    // Probabilities of all three outcomes
    p[1] = e - 0.5 * p_draw;       // White wins
    p[2] = p_draw;                 // Draw
    p[3] = 1 - e - 0.5 * p_draw;   // Black wins

    outcome[n] ~ categorical(p);
  }
}
