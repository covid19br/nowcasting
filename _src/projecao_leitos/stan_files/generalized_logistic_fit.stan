functions {
  real[] logisticgrowth(real t, real[] y, real[] theta, real[] x_r, int[] x_i) {
    real dydt[1];
    dydt[1] = theta[1] * y[1]^theta[3] * (1-y[1]/theta[2]);
    return dydt;
  }
}

data {
  int<lower=0> T; // numero de dias
  int<lower=0> T_p; // numero de dias a prever
  real      y[T];    // casos por dia
  real      ts[T];   // tempos de obsercacao
  real      ts_p[T_p];   // tempos de obsercacao
  real      initial_y;
  real      K_prior;
}
transformed data {
  real x_r[0];
  int x_i[0];
  real y0[1];
  real t0;
  
  t0 = 0;
  y0[1] = initial_y;
}

parameters {
  real<lower=0> r;
  real<lower=0> K;
  real<lower=0, upper=1> p;
  real<lower=0> sigma_B;
  real<lower=0> sigma_A;
}
model {
  real y_hat[T, 1];
  real theta[3];
  
  r ~ normal(1,5);
  K ~ cauchy(K_prior, 3);
  p ~ beta(1.3, 1.3);

  theta[1] = r;
  theta[2] = K;
  theta[3] = p;
  y_hat = integrate_ode_rk45(logisticgrowth, y0, t0, ts, theta, x_r, x_i);

  for (t in 1:T) {
    y[t] ~ normal(y_hat[t,1], sigma_B*y_hat[t,1] + sigma_A);
  }
  sigma_B ~ student_t(3,0,1);
  sigma_A ~ normal(0, 0.1);
}
generated quantities{
  real y_pred[T_p, 1];
  real cases_pred[T_p];
  real theta[3];
  real sigma[T_p];
  theta[1] = r;
  theta[2] = K;
  theta[3] = p;
  
  y_pred = integrate_ode_rk45(logisticgrowth, y0, t0, ts_p, theta, x_r, x_i );
  for (t in 1:T_p) {
    sigma[t] = sigma_B*y_pred[t,1] + sigma_A;
    cases_pred[t] = normal_rng(y_pred[t,1], sigma[t] );
  }
}
