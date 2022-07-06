
d <- read.csv("first_moves.csv")

stopifnot(nrow(d) == 31756)

n_age_groups <- length(unique(d$age_group))

print("fit model to data")

# fit the 24 month model

stan_data <- list(
  N_games = nrow(d),
  fourfour = d$fourfour,
  ind_use = d$ind_use,
  pop_use = d$pop_use,
  ind = d$ind,
  age = d$age_group,
  ind_use_x_ind_use_win = d$ind_use_x_ind_use_win,
  ind_use_x_ind_win = d$ind_use_x_ind_win,
  ind_use_win = d$ind_use_win,
  pop_use_x_pop_use_win = d$pop_use_x_pop_use_win,
  pop_use_x_ind_win = d$pop_use_x_ind_win,
  pop_use_win = d$pop_use_win,
  komi = d$komi,
  N_ind = length(unique(d$ind)),
  N_ages = n_age_groups
)

fit <- cmdstan_models[["horizon24"]]$sample(parallel_chains = n_chains, chains = n_chains,
  iter_warmup = floor(n_iter/2), iter_sampling = n_iter, adapt_delta = adapt_delta,
  max_treedepth = 15, data = stan_data, step_size = 0.1,
  refresh = 100)

fit$save_object(file = paste0("horizon24.RDS"))

diagnostics <- extract_diagnostics(fit)
diagnostics$fit_name <- "horizon24"
diagnostics$machine_name <- machine_name
diagnostics$project_seed <- project_seed
diagnostics$n_ind <- stan_data$N_ind
diagnostics$n_obs <- stan_data$N_games
diagnostics$n_iter <- n_iter
diagnostics$chains <- n_chains
diagnostics$adapt_delta <- adapt_delta
diag_filename <- paste0("figures/diag_", diagnostics$fit_name, ".csv")
write.csv(as.data.frame(diagnostics), diag_filename, row.names = FALSE)
