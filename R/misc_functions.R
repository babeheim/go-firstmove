
prep_latex_variables <- function(named_list) {
  out <- character()
  for (i in 1:length(named_list)) {
    out[i] <- paste0("\\newcommand{\\", names(named_list)[i], "}{", named_list[[i]], "}")
  }
  return(out)
}

create_database_fast <- function(sgf_paths, num_cores = 3) {
  data_list <- parallel::mclapply(seq_along(sgf_paths), function(z) {
    game_data <- read_sgf(sgf_paths[z], rotate = FALSE)
    if (class(game_data) != "try-error") {
      game_data$m1 <- game_data$moves$coord_sgf[1]
      game_data$m2 <- game_data$moves$coord_sgf[2]
      game_data$filename <- sgf_paths[z]
      game_data <- game_data[-which(names(game_data) %in% c("AB", "AW", "moves"))]
      if (z %% 100 == 0) print(z)
      return(game_data)
    }
  }, mc.cores = num_cores)
  output <- as.data.frame(bind_rows(data_list))
  return(output)
}

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

