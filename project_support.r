
library(rethinking)   # needed for model exploration
library(rstan)
library(RColorBrewer) # needed for figures
library(tictoc)
library(knitr)
library(testthat)

expect_true(capabilities("png"))
expect_true(capabilities("cairo"))

expect_silent(system("xelatex -v"))
expect_silent(system("bibtex -v"))
expect_silent(system("pandoc -v"))

scaffold <- FALSE
save_temp <- FALSE
save_output <- TRUE
n_iter <- 2000

texttab <- function(input.matrix, alignment = NA,
  hlines = NA, caption = "", scale = NA) {
  output <- character(nrow(input.matrix))
  for (i in 1:nrow(input.matrix)) {
    add.amps <- paste(input.matrix[i, ], collapse = " & ")
    output[i] <- paste(add.amps, "\\\\", sep = " ")
  }
  if (all(!is.na(hlines))) {
    for (i in 1:length(hlines)) {
      output <- append(output, "\\hline", hlines[i] + (i - 1))
    }
  }
  return(output)
}

dir_init <- function(path, verbose = FALSE, overwrite = TRUE) {
  if (substr(path, 1, 2) != "./") stop("path argument must be formatted
    with './' at beginning")
  contents <- dir(path, recursive = TRUE)
  if (dir.exists(path)) {
    if (overwrite) {
      if (verbose) {
        if (length(contents) == 0) print(paste("folder ", path, " created.", sep = ""))
        if (length(contents) > 0) print(paste("folder ", path,
          " wiped of ", length(contents), " files/folders.", sep = ""))
      }
      if (dir.exists(path)) unlink(path, recursive = TRUE)
      dir.create(path)
    }
  } else {
    if (verbose) {
      print(paste("folder ", path, " created.", sep = ""))
    }
    dir.create(path)
  }
}

col_alpha <- function(acol, alpha = 0.2) {
  acol <- col2rgb(acol)
  acol.red <- acol["red", ] / 255
  acol.green <- acol["green", ] / 255
  acol.blue <- acol["blue", ] / 255
  acol <- mapply(
    function(red, green, blue, alphas) {
      rgb(red, green, blue, alphas)
    },
    acol.red, acol.green, acol.blue, alpha
  )
  return(as.character(acol))
}

