
if (scaffold) {
  rm(list = ls())
  source("../project_support.r")
}

dir_init("./temp")

#######


print("load game data")

d <- read.csv("./inputs/gogod_cleaned.csv", stringsAsFactors = FALSE)

print("exclude players with less than 50 games")

# drop all games by black players who have less than 50 games to their names
black_game_counts <- sort(table(d$PB), decreasing = TRUE)
black_game_counts <- black_game_counts[black_game_counts > 49]
black_players_studied <- names(black_game_counts)

drop <- which(!d$PB %in% black_players_studied) # 13834 games to drop
d <- d[-drop, ] # 34536 games remaining



print("prepared predictors based on a retrospective time horizon")

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

d$b_use_cfrq_1p <- NA
d$b_win_cfrq_1p <- NA
d$b_use_win_cfrq_1p <- NA
d$use_cfrq_1p <- NA
d$use_win_cfrq_1p <- NA
d$pop_win_cfrq_1p <- NA

for (i in 1:n_games) {
  valid_range <- game_index %in% d$first_row[i]:(i - 1)

  d$use_cfrq_1p[i] <- mean(d$fourfour[valid_range]) - 0.5
  d$use_win_cfrq_1p[i] <- mean(d$black_won[valid_range & d$fourfour]) -
    mean(d$black_won[valid_range & !d$fourfour])
  d$pop_win_cfrq_1p[i] <- mean(d$black_won[valid_range])
  
  focal_player <- d$PB[i]
  focal_as_black <- d$PB == focal_player
  focal_as_white <- d$PW == focal_player
  
  d$b_use_cfrq_1p[i] <- mean(d$fourfour[focal_as_black & valid_range]) - 0.5
  d$b_win_cfrq_1p[i] <- mean(d$black_won[focal_as_black & valid_range]) -
    mean(d$black_won[valid_range])
  d$b_use_win_cfrq_1p[i] <- mean(d$black_won[focal_as_black & d$fourfour &
    valid_range]) - mean(d$black_won[focal_as_black & valid_range])
  # note that personal 44 win rate is relative to personal average
}



# this is clunky, i should streamline this
print("reduce table and simplify variable names")

keep <- c("DT", "PB", "BN", "komi", "BR", "black_won", "fourfour",
  "black_age", "b_win_cfrq_1p", "b_use_cfrq_1p", "b_use_win_cfrq_1p",
  "use_cfrq_1p", "use_win_cfrq_1p", "pop_win_cfrq_1p")

d <- d[, keep]

colnames(d) <- c("DT", "PB", "BN", "komi", "BR", "black_won", "fourfour",
  "b_age", "b_win", "b_44", "b_win_44", "pop_44", "pop_win_44", "pop_win")



print("drop all remaining games outside horizon cutoffs, or missing values")

# using a 2-year cutoff, starting the calculations at 1954, we need games from 1956 onwards
years <- as.numeric(substr(d$DT, 1, 4))
drop <- which(years < 1956) # 155 games to drop
d <- d[-drop, ] # 34380 games remaining

# drop any games that have missing values for any predictors
drop <- which(apply(d, 1, function(z) any(is.na(z)))) # 3487 games to drop
d <- d[-drop, ] # 30893 games remaining



print("create remaining predictors for STAN model")

# create integer player IDs for STAN
PB_list <- sort(unique(d$PB))
d$PB_id <- match(d$PB, PB_list)

# create integer ages starting at 1 for varying effects in STAN
b_age_list <- sort(unique(d$b_age))
d$b_age_group <- match(d$b_age, b_age_list)

# create interaction terms
d$b_44xb_win_44 <- d$b_44 * d$b_win_44
d$pop_44xpop_win_44 <- d$pop_44 * d$pop_win_44
d$pop_44xb_win <- d$pop_44 * d$b_win
d$b_44xb_win <- d$b_44 * d$b_win



print("save data to file")

write.csv(d, "./temp/fourfour_final.csv", row.names = FALSE) # 30893 remaining



#######

if (save_output) {
  dir_init("./output")
  file.copy("./temp/fourfour_final.csv", "./output")
}

if (!save_temp) unlink("./temp", recursive = TRUE)
