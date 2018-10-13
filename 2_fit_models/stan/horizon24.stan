// generated by glmer2stan 
data{
  int N;
  int fourfour[N];
  real b_44[N];
  real pop_44[N];
  int PB_id[N];
  int b_age_group[N];
  real b_44xb_win_44[N];
  real b_44xb_win[N];
  real b_win_44[N];
  real pop_44xpop_win_44[N];
  real pop_44xb_win[N];
  real pop_win_44[N];
  real komi[N];
  int bin_total[N];
  int N_PB_id;
  int N_b_age_group;
}

transformed data{
  vector[3] zeros_PB_id;
  vector[2] zeros_b_age_group;
  for (i in 1:3) zeros_PB_id[i] = 0;
  for (i in 1:2) zeros_b_age_group[i] = 0;
}

parameters{
  real Intercept;
  real beta_b_44;
  real beta_b_44xb_win_44;
  real beta_b_44xb_win;
  real beta_b_win_44;
  real beta_pop_44;
  real beta_pop_44xpop_win_44;
  real beta_pop_44xb_win;
  real beta_pop_win_44;
  real beta_komi;
  vector[3] vary_PB_id[N_PB_id];
  cov_matrix[3] Sigma_PB_id;
  vector[2] vary_b_age_group[N_b_age_group];
  cov_matrix[2] Sigma_b_age_group;
}

model{
  real vary[N];
  real glm[N];
  // Priors
  Intercept ~ normal(0, 100);
  beta_b_44 ~ normal(0, 100);
  beta_b_44xb_win_44 ~ normal(0, 100);
  beta_b_44xb_win ~ normal(0, 100);
  beta_b_win_44 ~ normal(0, 100);
  beta_pop_44 ~ normal(0, 100);
  beta_pop_44xpop_win_44 ~ normal(0, 100);
  beta_pop_44xb_win ~ normal(0, 100);
  beta_pop_win_44 ~ normal(0, 100);
  beta_komi ~ normal(0, 100);
  // Varying effects
  for (j in 1:N_PB_id) vary_PB_id[j] ~ multi_normal(zeros_PB_id, Sigma_PB_id);
  for (j in 1:N_b_age_group) vary_b_age_group[j] ~ multi_normal(zeros_b_age_group,
    Sigma_b_age_group);
  // Fixed effects
  for (i in 1:N) {
    vary[i] = vary_PB_id[PB_id[i],1]
        + vary_PB_id[PB_id[i],2] * b_44[i]
        + vary_PB_id[PB_id[i],3] * pop_44[i]
        + vary_b_age_group[b_age_group[i],1]
        + vary_b_age_group[b_age_group[i],2] * pop_44[i];
    glm[i] = vary[i] + Intercept
        + beta_b_44 * b_44[i]
        + beta_b_44xb_win_44 * b_44xb_win_44[i]
        + beta_b_44xb_win * b_44xb_win[i]
        + beta_b_win_44 * b_win_44[i]
        + beta_pop_44 * pop_44[i]
        + beta_pop_44xpop_win_44 * pop_44xpop_win_44[i]
        + beta_pop_44xb_win * pop_44xb_win[i]
        + beta_pop_win_44 * pop_win_44[i]
        + beta_komi * komi[i];
    glm[i] = inv_logit(glm[i]);
  }
  fourfour ~ binomial(bin_total, glm);
}

generated quantities{
  real dev;
  real vary[N];
  real glm[N];
  dev = 0;
  for (i in 1:N) {
    vary[i] = vary_PB_id[PB_id[i],1]
        + vary_PB_id[PB_id[i],2] * b_44[i]
        + vary_PB_id[PB_id[i],3] * pop_44[i]
        + vary_b_age_group[b_age_group[i],1]
        + vary_b_age_group[b_age_group[i],2] * pop_44[i];
    glm[i] = vary[i] + Intercept
        + beta_b_44 * b_44[i]
        + beta_b_44xb_win_44 * b_44xb_win_44[i]
        + beta_b_44xb_win * b_44xb_win[i]
        + beta_b_win_44 * b_win_44[i]
        + beta_pop_44 * pop_44[i]
        + beta_pop_44xpop_win_44 * pop_44xpop_win_44[i]
        + beta_pop_44xb_win * pop_44xb_win[i]
        + beta_pop_win_44 * pop_win_44[i]
        + beta_komi * komi[i];
    dev = dev + (-2) * binomial_lpmf(fourfour[i] | bin_total[i], inv_logit(glm[i]));
  }
}
 
