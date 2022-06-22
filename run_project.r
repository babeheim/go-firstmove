
rm(list = ls())

start_time <- Sys.time()

source("./project_support.r")

tic.clearlog()

##############

tic("clean data")
dir_init("./1_prep_games/inputs")
files <- list.files("./data", full.names = TRUE)
file.copy(files, "./1_prep_games/inputs")
setwd("./1_prep_games")
source("./prep_games.r")
setwd("..")
toc(log = TRUE)

##############

tic("explore data")
dir_init("./2_explore_games/inputs")
file.copy("./1_prep_games/output/games.csv", "./2_explore_games/inputs")
setwd("./2_explore_games")
source("./explore_games.r")
setwd("..")
toc(log = TRUE)

##############

tic("build regression dataframe")
dir_init("./3_prep_first_moves/inputs")
file.copy("./1_prep_games/output/games.csv", "./3_prep_first_moves/inputs")
setwd("./3_prep_first_moves")
source("./prep_first_moves.r")
setwd("..")
toc(log = TRUE)

##############

tic("fit regression models")
dir_init("./4_fit_model/inputs")
file.copy("./3_prep_first_moves/output/first_moves.csv", "./4_fit_model/inputs")
setwd("./4_fit_model")
source("./fit_model.r")
setwd("..")
toc(log = TRUE)

##############

tic("describe models")
dir_init("./5_explore_fit/inputs")
files <- list.files("./4_fit_model/output", full.names = TRUE)
file.copy(files, "./5_explore_fit/inputs")
file.copy("./3_prep_first_moves/output/first_moves.csv", "./5_explore_fit/inputs")
setwd("./5_explore_fit")
source("./explore_fit.r")
setwd("..")
toc(log = TRUE)

##############

tic("prepare manuscript")
dir_init("./6_prepare_manuscript/inputs")
files <- list.files("./2_explore_games/output", full.names = TRUE)
file.copy(files, "./6_prepare_manuscript/inputs")
files <- list.files("./5_explore_fit/output", full.names = TRUE)
file.copy(files, "./6_prepare_manuscript/inputs")
setwd("./6_prepare_manuscript")
source("./prepare_manuscript.r")
setwd("..")
toc(log = TRUE)

##############

dir_init("./output")

tic.log(format = TRUE)
msg_log <- unlist(tic.log())
msg_log <- paste0("- ", msg_log)
if (!exists("start_time")) start_time <- NA
header <- c(
  "project: Go first-move evolution analysis",
  paste("start_time:", start_time),
  paste("finish_time:", Sys.time()),
  "events:")
msg_log <- c(header, msg_log)
writeLines(msg_log, "./output/log.txt")

files <- list.files(
  c("./6_prepare_manuscript/output"), full.names = TRUE)

file.copy(files, "./output")
