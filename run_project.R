
rm(list = ls())

source("0_init_project.R")

tic("run go-firstmove project")

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

toc(log = TRUE)

###########

tic.log(format = TRUE)
msg_log <- unlist(tic.log())

task <- msg_log
task <- gsub(":.*$", "", task)

time_min <- msg_log
time_min <- gsub("^.*: ", "", time_min)
time_min <- gsub(" sec elapsed", "", time_min)
time_min <- round(as.numeric(time_min)/60, 2)

report <- data.frame(
  project_seed = project_seed,
  n_iter = n_iter,
  machine = machine_name,
  task = task,
  time_min = time_min
)

write.csv(report, file.path("figures/timing-report.csv"), row.names = FALSE)
