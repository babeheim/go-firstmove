
if (scaffold) {
  rm(list = ls())
  source("project_support.R")
}


print("load game data")

d <- read.csv("games.csv")

stopifnot(nrow(d) == 49789)

print("exclude players with less than 50 games")

# drop all games by black players who have less than 50 games to their names
black_game_counts <- sort(table(d$PB), decreasing = TRUE)
black_game_counts <- black_game_counts[black_game_counts > 49]
black_players_studied <- names(black_game_counts)

stopifnot(length(black_players_studied) == 209)

drop <- which(!d$PB %in% black_players_studied)
stopifnot(length(drop) == 15140)
d <- d[-drop, ]



print("prepared predictors based on a retrospective time horizon")

d$DT <- as.Date(d$DT)

# for this approach to work, games must be ordered from oldest (top) to newest (bottom)
stopifnot(all(diff(d$DT) >= 0))

period_length <- 2

one_period_before <- as.Date(d$DT) - (365) * period_length

n_games <- nrow(d)
game_index <- 1:n_games

d$first_row <- NA
date_pointer <- 1
for (i in 1:n_games) {
  while (one_period_before[i] > d$DT[date_pointer]) date_pointer <- date_pointer + 1
  d$first_row[i] <- date_pointer
}

d$ind_use <- NA
d$ind_win <- NA
d$ind_use_win <- NA
d$pop_use <- NA
d$pop_use_win <- NA
d$pop_win <- NA

for (i in 1:n_games) {
  valid_range <- game_index %in% d$first_row[i]:(i - 1)

  d$pop_use[i] <- mean(d$fourfour[valid_range]) - 0.5
  d$pop_use_win[i] <- mean(d$black_won[valid_range & d$fourfour]) -
    mean(d$black_won[valid_range & !d$fourfour])
  d$pop_win[i] <- mean(d$black_won[valid_range])
  
  focal_player <- d$PB[i]
  focal_as_black <- d$PB == focal_player
  focal_as_white <- d$PW == focal_player
  
  d$ind_use[i] <- mean(d$fourfour[focal_as_black & valid_range]) - 0.5
  d$ind_win[i] <- mean(d$black_won[focal_as_black & valid_range]) -
    mean(d$black_won[valid_range])
  d$ind_use_win[i] <- mean(d$black_won[focal_as_black & d$fourfour &
    valid_range]) - mean(d$black_won[focal_as_black & valid_range])
  # note that personal 44 win rate is relative to personal average
  if (i %% 100 == 0) print(i)
}

print("reduce table and simplify variable names")

d <- rename(d, age = black_age)
d <- select(d, DT, PB, BN, komi, black_won, fourfour, age, ind_use, ind_win, ind_use_win, pop_use, pop_use_win, pop_win)

print("drop all remaining games outside horizon cutoffs, or missing values")

# using a 2-year cutoff, starting the calculations at 1954, we need games from 1956 onwards
years <- as.numeric(substr(d$DT, 1, 4))
drop <- which(years < 1956)
d <- d[-drop, ]

# drop any games that have missing values for any predictors
drop <- which(apply(d, 1, function(z) any(is.na(z))))
d <- d[-drop, ]



print("create remaining predictors for STAN model")

# create integer player IDs for STAN
PB_list <- sort(unique(d$PB))
d$ind <- match(d$PB, PB_list)

# create integer ages starting at 1 for varying effects in STAN
age_list <- sort(unique(d$age))
d$age_group <- match(d$age, age_list)

# create interaction terms
d$ind_use_x_ind_use_win <- d$ind_use * d$ind_use_win
d$pop_use_x_pop_use_win <- d$pop_use * d$pop_use_win
d$pop_use_x_ind_win <- d$pop_use * d$ind_win
d$ind_use_x_ind_win <- d$ind_use * d$ind_win

stopifnot(nrow(d) == 31756)

stopifnot(!any(is.na(d)))

stopifnot(all(c("DT", "PB", "BN", "komi", "black_won", "fourfour", "age", "ind_win", "ind_use", "ind_use_win", "pop_use", "pop_use_win", "pop_win", "ind", "age_group", "ind_use_x_ind_use_win", "pop_use_x_pop_use_win", "pop_use_x_ind_win", "ind_use_x_ind_win") %in% colnames(d)))

cor_check <- c("black_won", "fourfour", "age", "ind_win", "ind_use", "ind_use_win", "pop_use", "pop_use_win", "pop_win", "ind", "ind_use_x_ind_use_win", "pop_use_x_pop_use_win", "pop_use_x_ind_win", "ind_use_x_ind_win")
cors <- cor(d[,cor_check])
stopifnot(!any(abs(cors[lower.tri(cors)]) > 0.9))

print("save data to file")

write.csv(d, "first_moves.csv", row.names = FALSE)
