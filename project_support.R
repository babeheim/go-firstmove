
library(rethinking)   # github.com/rmcelreath/kaya
library(kaya)         # github.com/babeheim/kaya
library(rstan)
library(RColorBrewer)
library(tictoc)
library(digest)

stopifnot(capabilities("png"))
stopifnot(capabilities("cairo"))

scaffold <- FALSE
n_iter <- 2000
machine_name <- "mpi-mac-mini"

source("R/misc_functions.R")
