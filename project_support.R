
library(rethinking)   # needed for model exploration
library(rstan)
library(RColorBrewer) # needed for figures
library(tictoc)
library(knitr)
library(testthat)
library(kaya) # github.com/babeheim/kaya
library(digest)

expect_true(capabilities("png"))
expect_true(capabilities("cairo"))

scaffold <- TRUE
n_iter <- 2000

source("R/misc_functions.R")
