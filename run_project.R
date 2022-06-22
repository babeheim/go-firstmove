
rm(list = ls())

start_time <- Sys.time()

source("./project_support.R")

dir_init("./RData")
dir_init("./figures")

tic.clearlog()

##############

tic("prep games")
source("./1_prep_games.R")
toc(log = TRUE)

##############

tic("explore games")
source("./2_explore_games.R")
toc(log = TRUE)

##############

tic("build regression dataframe")
source("./3_prep_first_moves.R")
toc(log = TRUE)

##############

tic("fit regression model")
source("./4_fit_model.R")
toc(log = TRUE)

##############

tic("describe models")
source("./5_explore_fit.R")
toc(log = TRUE)
