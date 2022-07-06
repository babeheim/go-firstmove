
library(rethinking)   # github.com/rmcelreath/rethinking
library(kaya)         # github.com/babeheim/kaya
library(cmdstanr)
library(posterior)
library(RColorBrewer)
library(tictoc)
library(digest)

stopifnot(capabilities("png"))
stopifnot(capabilities("cairo"))

machine_name <- "mpi-mac-mini"
n_iter <- 1000
n_chains <- 4
adapt_delta <- 0.9
project_seed <- 1

set.seed(project_seed)

options(warnPartialMatchDollar=TRUE)

source("R/misc_functions.R")

cmdstan_models <- list()
cmdstan_models[["horizon24"]] <- cmdstan_model("horizon24.stan")
