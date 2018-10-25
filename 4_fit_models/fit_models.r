
if (scaffold) {
  rm(list = ls())
  source("../project_support.r")
}

dir_init("./temp")

d <- read.csv("./inputs/fourfour_final.csv", stringsAsFactors = FALSE)

print("fit model to data")

# fit the 24 month model

dat_list <- list(
  N = nrow(d),
  fourfour = d$fourfour,
  b_44 = d$b_44,
  pop_44 = d$pop_44,
  PB_id = d$PB_id,
  b_age_group = d$b_age_group,
  b_44xb_win_44 = d$b_44xb_win_44,
  b_44xb_win = d$b_44xb_win,
  b_win_44 = d$b_win_44,
  pop_44xpop_win_44 = d$pop_44xpop_win_44,
  pop_44xb_win = d$pop_44xb_win,
  pop_win_44 = d$pop_win_44,
  komi = d$komi,
  bin_total = rep(1, nrow(d)),
  N_PB_id = length(unique(d$PB_id)),
  N_b_age_group = length(unique(d$b_age_group))
)

horizon24 <- stan(file = "./stan/horizon24.stan", data = dat_list,
  iter = n_iter, chains = 3, cores = 3)

save(horizon24, file = "./temp/horizon24.RData")

if (save_output) {
  dir_init("./output")
  file.copy("./temp/horizon24.RData", "./output")
}

if (!save_temp) unlink("./temp", recursive = TRUE)
