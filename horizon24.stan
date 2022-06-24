 data{
  int N_games;
  int fourfour[N_games];
  real ind_use[N_games];
  real pop_use[N_games];
  int ind[N_games];
  int age_group[N_games];
  real ind_use_x_ind_use_win[N_games];
  real ind_use_x_ind_win[N_games];
  real ind_use_win[N_games];
  real pop_use_x_pop_use_win[N_games];
  real pop_use_x_ind_win[N_games];
  real pop_use_win[N_games];
  real komi[N_games];
  int bin_total[N_games];
  int N_ind;
  int N_age_group;
}

transformed data{
  vector[3] zeros_pid;
  vector[2] zeros_age_group;
  for (i in 1:3) zeros_pid[i] = 0;
  for (i in 1:2) zeros_age_group[i] = 0;
}

parameters{
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
  cov_matrix[3] Sigma_ind;
  vector[2] vary_age_group[N_age_group];
  cov_matrix[2] Sigma_age_group;
}

model{
  real vary[N_games];
  real glm[N_games];
  // Priors
  a ~ normal(0, 100);
  b_ind_use ~ normal(0, 100);
  b_ind_use_x_ind_use_win ~ normal(0, 100);
  b_ind_use_x_ind_win ~ normal(0, 100);
  b_ind_use_win ~ normal(0, 100);
  b_pop_use ~ normal(0, 100);
  b_pop_use_x_pop_use_win ~ normal(0, 100);
  b_pop_use_x_ind_win ~ normal(0, 100);
  b_pop_use_win ~ normal(0, 100);
  b_komi ~ normal(0, 100);
  // Varying effects
  for (j in 1:N_ind) vary_ind[j] ~ multi_normal(zeros_pid, Sigma_ind);
  for (j in 1:N_age_group) vary_age_group[j] ~ multi_normal(zeros_age_group,
    Sigma_age_group);
  // Fixed effects
  for (i in 1:N_games) {
    vary[i] = vary_ind[ind[i],1]
        + vary_ind[ind[i],2] * ind_use[i]
        + vary_ind[ind[i],3] * pop_use[i]
        + vary_age_group[age_group[i],1]
        + vary_age_group[age_group[i],2] * pop_use[i];
    glm[i] = vary[i] + a
        + b_ind_use * ind_use[i]
        + b_ind_use_x_ind_use_win * ind_use_x_ind_use_win[i]
        + b_ind_use_x_ind_win * ind_use_x_ind_win[i]
        + b_ind_use_win * ind_use_win[i]
        + b_pop_use * pop_use[i]
        + b_pop_use_x_pop_use_win * pop_use_x_pop_use_win[i]
        + b_pop_use_x_ind_win * pop_use_x_ind_win[i]
        + b_pop_use_win * pop_use_win[i]
        + b_komi * komi[i];
    glm[i] = inv_logit(glm[i]);
  }
  fourfour ~ binomial(bin_total, glm);
}

generated quantities{
  real dev;
  real vary[N_games];
  real glm[N_games];
  dev = 0;
  for (i in 1:N_games) {
    vary[i] = vary_ind[ind[i],1]
        + vary_ind[ind[i],2] * ind_use[i]
        + vary_ind[ind[i],3] * pop_use[i]
        + vary_age_group[age_group[i],1]
        + vary_age_group[age_group[i],2] * pop_use[i];
    glm[i] = vary[i] + a
        + b_ind_use * ind_use[i]
        + b_ind_use_x_ind_use_win * ind_use_x_ind_use_win[i]
        + b_ind_use_x_ind_win * ind_use_x_ind_win[i]
        + b_ind_use_win * ind_use_win[i]
        + b_pop_use * pop_use[i]
        + b_pop_use_x_pop_use_win * pop_use_x_pop_use_win[i]
        + b_pop_use_x_ind_win * pop_use_x_ind_win[i]
        + b_pop_use_win * pop_use_win[i]
        + b_komi * komi[i];
    dev = dev + (-2) * binomial_lpmf(fourfour[i] | bin_total[i], inv_logit(glm[i]));
  }
}

