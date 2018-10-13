
if (scaffold) {
  rm(list = ls())
  source("../project_support.r")
}

dir_init("./temp")

gogod <- read.csv("./inputs/gogod_gamedata_c.csv", stringsAsFactors = FALSE)
# 60337 x 18

print("loaded game data")

years <- as.numeric(substr(gogod[, "DT"], 1, 4))
keep <- which(years >= 1954)

gogod <- gogod[keep, ] # 51203

drop <- which(!is.na(gogod[, "HA"]))
gogod <- gogod[-drop, ]  # 49818 games___well now 50049

# drop games with inappropriate first move formats:
drop <- which(substr(gogod[, "move.string"], 1, 2) != ";B")

gogod <- gogod[-drop, ]

# first move is a pass: # game 373-453887744, file name "1998/1998-04-21a_sgf"
drop <- which(gogod[, "filename"] == "./temp/1998-04-21a.sgf")
# 373-454033408 ??
# from the game comments, it sounds like this game involved some
# kind of novelty rules with playing cards - DROP
gogod <- gogod[-drop, ]

# fix RE
gogod[which(gogod[, "RE"] == "w+R"), "RE"] <- "W+R"
gogod[which(gogod[, "RE"] == "w+0.25"), "RE"] <- "W+0.25"
gogod[gogod[, "filename"] == "./temp/1960-02-03b_sgf", "RE"] <- "V"
# "087-567214713", filed under "1960-69/1960-02-03b_sgf"
# the game was declared void after one player (Kitani) was
# under doctor"s orders to stop...it"s not "?" because there was no winner
# it was void! the hashes are different
RE_data <- substr(gogod[, "RE"], 1, 1)
drop <- which( RE_data == "?" | RE_data == "U")

gogod <- gogod[-drop, ]

# drop games with named amateurs?
BR <- gogod[, "BR"]
WR <- gogod[, "WR"]
drop <- sort(unique(c(grep("ama", BR), grep("Ama", BR),
  grep("ama", WR), grep("Ama", WR))))

gogod <- gogod[-drop, ]

# drop all games by black players who have less than 50 games to their names

pb_data <- gogod[, "PB"]
pw_data <- gogod[, "PW"]

pb_counts <- sort(table(pb_data), decreasing = TRUE)
pb_counts <- pb_counts[pb_counts > 49]
pb_list_good <- names(pb_counts)

drop <- which(!gogod[, "PB"] %in% pb_list_good)
gogod <- gogod[-drop, ]
# 34379

drop <- which(gogod[, "PW"] == "?")
gogod <- gogod[-drop, ]

# now add in Black"s age and nationality
bio_data <- read.csv("./inputs/PB Biographical Data.csv", as.is = TRUE)
drop <- which(bio_data$Birth.Year == "??" |
  bio_data$Player.Name == "Kikuchi Yasuro")
# drop kikuchi yasuro because he"s a super-strong amateur but not a pro - meh
# I"ll have NA"s, so what?
bio_data <- bio_data[-drop, ]
bio_data$Birth.Year <- as.numeric(bio_data$Birth.Year)
bio_data$Nationality <- gsub(" ", "", bio_data$Nationality)

gogod$BA <- as.numeric(substr(gogod$DT, 1, 4)) -
  bio_data$Birth.Year[match(gogod$PB, bio_data$Player.Name)]
gogod$BN <- bio_data$Nationality[match(gogod$PB, bio_data$Player.Name)]

# build final regression table

period_length <- 2

DT <- gogod[, "DT"]
years <- as.numeric(substr(gogod[, "DT"], 1, 4))
n_games <- nrow(gogod)
game_index <- 1:n_games
PB <- as.character(gogod[, "PB"])
PW <- as.character(gogod[, "PW"])
BN <- gogod[, "BN"]
BR <- gogod[, "BR"]
b_age <- gogod[, "BA"]
player_list <- sort(unique(c(PB, PW)))
n_players <- length(player_list)
black_won <- as.numeric(substr(gogod[, "RE"], 1, 1) == "B")
white_won <- as.numeric(substr(gogod[, "RE"], 1, 1) == "W")
## what about ties? ignore them, I guess no one won!
komi <- as.numeric(gogod[, "KM"])
komi <- komi - modal(komi)


# ok, let"s get those first moves

raw_hits <- substr(gogod[, "move.string"], 4, 5)
fourfour <- as.numeric(raw_hits %in% c("pd", "dp", "dd", "pp"))

dates <- as.Date(gogod[, "DT"])
dates_char <- as.character(dates)

dates_1p <- dates - (365) * period_length
dates_1p <- as.Date(dates_1p)

first_row <- NA
date_pointer <- 1
for (i in 1:n_games) {
  while (dates_1p[i] > dates[date_pointer]) date_pointer <- date_pointer + 1
  first_row[i] <- date_pointer
}


print("identify date ranges")

# ok, using the "valid range" calculate period-specific predictors

# calculate how many games the two players have had together where Black is Black,
# and the win rate of Black

# how about an 0/1 indicator:
# the last time they saw the 44 as a white player, did they lose?

b_use_cfrq_1p <- NA
b_win_cfrq_1p <- NA
b_use_win_cfrq_1p <- NA
use_cfrq_1p <- NA
use_win_cfrq_1p <- NA
pop_win_cfrq_1p <- NA

for (i in 1:n_games) {

  valid_range <- game_index %in% first_row[i]:(i - 1)

  use_cfrq_1p[i] <- mean(fourfour[valid_range]) - 0.5
  use_win_cfrq_1p[i] <- mean(black_won[valid_range & fourfour]) -
    mean(black_won[valid_range & !fourfour])
  pop_win_cfrq_1p[i] <- mean(black_won[valid_range])

  focal_player <- PB[i]
  focal_as_black <- PB == focal_player
  focal_as_white <- PW == focal_player

  b_use_cfrq_1p[i] <- mean(fourfour[focal_as_black & valid_range]) - 0.5
  b_win_cfrq_1p[i] <- mean(black_won[focal_as_black & valid_range]) -
    mean(black_won[valid_range])
  b_use_win_cfrq_1p[i] <- mean(black_won[focal_as_black & fourfour &
    valid_range]) - mean(black_won[focal_as_black & valid_range])
  # personal use win rate is relative to personal average
}

print("calculate predictors based on dates")


regression_table <- cbind(
  DT,
  fourfour,
  first_row,
  b_age,
  b_win_cfrq_1p,
  b_use_cfrq_1p,
  b_use_win_cfrq_1p,
  use_cfrq_1p,
  use_win_cfrq_1p,
  pop_win_cfrq_1p,
  PB,
  BN,
  komi,
  BR,
  black_won
)

write.csv(regression_table, "./temp/fourfour_regression_table_24m.csv",
  row.names = FALSE) # 34406 x 17

dir_init("./output")

file.copy("./temp/fourfour_regression_table_24m.csv", "./output")

if (!save_temp) unlink("./temp", recursive = TRUE)
