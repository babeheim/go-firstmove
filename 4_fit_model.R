
if (scaffold) {
  rm(list = ls())
  source("project_support.R")
}

d <- read.csv("first_moves.csv")

stopifnot(nrow(d) == 31133)

n_age_groups <- length(unique(d$age_group))

# n_obs <- 100
# keep <- sample(1:nrow(d), n_obs)
# d <- d[keep,]
# d$ind <- match(d$ind, unique(d$ind))

print("fit model to data")

# fit the 24 month model

dat_list <- list(
  N_games = nrow(d),
  fourfour = d$fourfour,
  ind_use = d$ind_use,
  pop_use = d$pop_use,
  ind = d$ind,
  age_group = d$age_group,
  ind_use_x_ind_use_win = d$ind_use_x_ind_use_win,
  ind_use_x_ind_win = d$ind_use_x_ind_win,
  ind_use_win = d$ind_use_win,
  pop_use_x_pop_use_win = d$pop_use_x_pop_use_win,
  pop_use_x_ind_win = d$pop_use_x_ind_win,
  pop_use_win = d$pop_use_win,
  komi = d$komi,
  bin_total = rep(1, nrow(d)),
  N_ind = length(unique(d$ind)),
  N_age_group = n_age_groups
)

horizon24 <- stan(file = "horizon24.stan", data = dat_list,
  iter = n_iter, chains = 3, cores = 3)

save(horizon24, file = "horizon24.RData")
