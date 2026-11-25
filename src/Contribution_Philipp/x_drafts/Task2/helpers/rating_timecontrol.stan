data {
  int<lower=1> N;
  int<lower=1> K;

  array[N] int<lower=1, upper=K> white;
  array[N] int<lower=1, upper=K> black;
  array[N] int<lower=1, upper=3> outcome;

  vector[N] x;  // Standardized time control
}

parameters {
  vector[K] intercept;  // Base ratings
  vector[K] slope;      // Time-control effects

  real<lower=0, upper=1> draw_tendency;
}

model {
  // Priors
  intercept ~ normal(2000, 200);
  slope ~ normal(0, 100);
  draw_tendency ~ beta(1, 1);

  for (n in 1:N) {
    real rating_white;
    real rating_black;
    real e;
    real p_draw;
    vector[3] p;

    // Time-dependent ratings based on the linear model described in the Rmd
    rating_white = intercept[white[n]] + slope[white[n]] * x[n];

    rating_black = intercept[black[n]] + slope[black[n]] * x[n];

    // Same Elo expected-score formula as Task 1
    e = 1 / (1 + pow(10.0, (rating_black - rating_white) / 400));

    // Same draw model as Task 1
    p_draw = 2 * draw_tendency * e * (1 - e);

    p[1] = e - 0.5 * p_draw;       // White wins
    p[2] = p_draw;                 // Draw
    p[3] = 1 - e - 0.5 * p_draw;   // Black wins

    outcome[n] ~ categorical(p);
  }
}
