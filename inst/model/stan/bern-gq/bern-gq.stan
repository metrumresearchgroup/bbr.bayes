data {
  int<lower=0> N;
  array[N] int<lower=0, upper=1> y;
}

parameters {
  real<lower=0, upper=1> theta;
}

generated quantities {
  int y_rep[N] = bernoulli_rng(rep_vector(theta, N));
}
