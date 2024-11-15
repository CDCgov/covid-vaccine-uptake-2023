data {
  int<lower=0> N;
  vector[N] x;
  vector[N] y;
  vector[N] std;
  real prior_intercept_sd;
  real prior_slope_sd;
}

parameters {
  real intercept;
  real slope;
}

transformed parameters {
  vector[N] yhat;
  yhat = slope * x + intercept;
}

model {
  intercept ~ normal(0, prior_intercept_sd);
  slope ~ normal(0, prior_slope_sd);
  yhat ~ normal(y, std);
}
