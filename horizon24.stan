 data {
  int N_games;
  int fourfour[N_games];
  real ind_use[N_games];
  real pop_use[N_games];
  int ind[N_games];
  int age[N_games];
  real ind_use_x_ind_use_win[N_games];
  real ind_use_x_ind_win[N_games];
  real ind_use_win[N_games];
  real pop_use_x_pop_use_win[N_games];
  real pop_use_x_ind_win[N_games];
  real pop_use_win[N_games];
  real komi[N_games];
  int N_ind;
  int N_ages;
}

transformed data {
  vector[3] vary_ind_mu[N_ind];
  for (i in 1:N_ind) vary_ind_mu[i] = rep_vector(0, 3);
  vector[2] vary_age_mu[N_ages];
  for (i in 1:N_ages) vary_age_mu[i] = rep_vector(0, 2);
}

parameters {
  real a;
  real b_ind_use;
  real b_ind_use_x_ind_use_win;
  real b_ind_use_x_ind_win;
  real b_ind_use_win;
  real b_pop_use;
  real b_pop_use_x_pop_use_win;
  real b_pop_use_x_ind_win;
  real b_pop_use_win;
  real b_komi;

  vector[3] vary_ind[N_ind];
  real<lower=0> sigma_a_ind;
  real<lower=0> sigma_ind_use_ind;
  real<lower=0> sigma_pop_use_ind;
  cholesky_factor_corr[3] L_Omega_ind;

  vector[2] vary_age[N_ages];
  real<lower=0> sigma_ind_use_age;
  real<lower=0> sigma_pop_use_age;
  cholesky_factor_corr[2] L_Omega_age;

}

transformed parameters {
  vector[3] sigma_ind = [sigma_a_ind, sigma_ind_use_ind, sigma_pop_use_ind]';
  vector[2] sigma_age = [sigma_ind_use_age, sigma_pop_use_age]';
}

model {
  a ~ normal(0, 1);
  b_ind_use ~ normal(0, 5);
  b_ind_use_x_ind_use_win ~ normal(0, 10);
  b_ind_use_x_ind_win ~ normal(0, 10);
  b_ind_use_win ~ normal(0, 5);
  b_pop_use ~ normal(0, 5);
  b_pop_use_x_pop_use_win ~ normal(0, 10);
  b_pop_use_x_ind_win ~ normal(0, 10);
  b_pop_use_win ~ normal(0, 5);
  b_komi ~ normal(0, 10);

  sigma_a_ind ~ exponential(1);
  sigma_ind_use_ind ~ exponential(1);
  sigma_pop_use_ind ~ exponential(1);
  L_Omega_ind ~ lkj_corr_cholesky(2);
  vary_ind ~ multi_normal_cholesky(vary_ind_mu, diag_pre_multiply(sigma_ind, L_Omega_ind));

  sigma_ind_use_age ~ exponential(1);
  sigma_pop_use_age ~ exponential(1);
  L_Omega_age ~ lkj_corr_cholesky(2);
  vary_age ~ multi_normal_cholesky(vary_age_mu, diag_pre_multiply(sigma_age, L_Omega_age));

  for (i in 1:N_games) {
    real theta = a +
      vary_ind[ind[i], 1] +
      vary_ind[ind[i], 2] * ind_use[i] +
      vary_ind[ind[i], 3] * pop_use[i] +
      vary_age[age[i], 1] * ind_use[i] +
      vary_age[age[i], 2] * pop_use[i] +
      b_ind_use * ind_use[i] +
      b_ind_use_x_ind_use_win * ind_use_x_ind_use_win[i] +
      b_ind_use_x_ind_win * ind_use_x_ind_win[i] +
      b_ind_use_win * ind_use_win[i] +
      b_pop_use * pop_use[i] +
      b_pop_use_x_pop_use_win * pop_use_x_pop_use_win[i] +
      b_pop_use_x_ind_win * pop_use_x_ind_win[i] +
      b_pop_use_win * pop_use_win[i] +
      b_komi * komi[i];
    target += bernoulli_logit_lpmf(fourfour[i] | theta);
  }
}
