
library(rethinking)   # needed for model exploration
library(rstan)
library(RColorBrewer) # needed for figures
library(tictoc)
library(knitr)

scaffold <- FALSE
save_temp <- FALSE

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

modal <- function(data) {
  mode <- NA
  if (length(data) > 0 & !all(is.na(data))) {
    mode <- names(sort(table(data), decreasing = TRUE))[1]
    options(warn = (-1))
    if (!is.na(as.numeric(mode))) {
      mode <- as.numeric(mode)
    }
    options(warn = 0)
  }
  return(mode)
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
