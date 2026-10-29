
data {
  int<lower=1> N; // Number of decisive games (782)
  int<lower=1> K; // Number of engines (6)

  // Engine IDs for each game (1 to K)
  array[N] int<lower=1, upper=K> white;
  array[N] int<lower=1, upper=K> black;

  // Observed result: 1 = white wins, 0 = black wins
  array[N] int<lower=0, upper=1> white_win;
}

parameters {
  // One unknown rating for each of the K engines
  vector[K] rating;
}

model {
  // PRIOR
  rating ~ normal(
    2000, // Prior mean: specified in the assignment
    200   // Prior SD: reasonable initial assumption (as in the hint)
  );

  // LIKELIHOOD
  for (n in 1:N) {

    real logit_p;

    // Elo formula given in the task
    logit_p = 1 / (1 + pow(10.0, (rating[black[n]] - rating[white[n]]) / 400));

    // Binary game-outcome model
    white_win[n] ~ bernoulli_logit(logit_p);
  }
}
