
rm(list = ls())

restore_environment <- function(project = ".") {

  project <- normalizePath(
    project,
    winslash = "/",
    mustWork = TRUE
  )

  lockfile <- file.path(
    project,
    "renv.lock"
  )

  activate_file <- file.path(
    project,
    "renv",
    "activate.R"
  )

  if (!file.exists(lockfile)) {
    stop(
      "renv.lock was not found: ",
      lockfile
    )
  }

  if (!file.exists(activate_file)) {
    stop(
      "renv activation script was not found: ",
      activate_file
    )
  }

  old_wd <- getwd()

  on.exit(
    setwd(old_wd),
    add = TRUE
  )

  setwd(project)

  # Bootstrap and activate the project's own renv environment.
  #
  # renv/activate.R can bootstrap renv itself on a fresh machine,
  # so renv does not need to be installed globally beforehand.
  source(
    activate_file,
    local = .GlobalEnv
  )

  if (!requireNamespace(
    "renv",
    quietly = TRUE
  )) {
    stop(
      "renv could not be bootstrapped from renv/activate.R."
    )
  }

  # Restore exactly the package versions recorded in renv.lock.
  renv::restore(
    project = project,
    lockfile = lockfile,
    prompt = FALSE,
    retry = FALSE
  )

  # Explicitly ensure that this running R process is using the
  # restored project library.
  renv::load(
    project = project,
    quiet = TRUE
  )

  invisible(TRUE)
}


project_root <- normalizePath(
  getwd(),
  winslash = "/",
  mustWork = TRUE
)

restore_environment(project_root)

source("./0_init_project.R")

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
