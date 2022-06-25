
library(rethinking)   # needed for model exploration
library(rstan)
library(RColorBrewer) # needed for figures
library(tictoc)
library(kaya) # github.com/babeheim/kaya
library(digest)

stopifnot(capabilities("png"))
stopifnot(capabilities("cairo"))

scaffold <- FALSE
n_iter <- 2000

source("R/misc_functions.R")
