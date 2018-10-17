
if (scaffold) {
  rm(list = ls())
  source("../project_support.r")
}

dir_init("./temp")

print("load game data")

games <- read.csv("./inputs/gogod_gamedata_c.csv", stringsAsFactors = FALSE)

games$year <- as.numeric(substr(games$DT, 1, 4))

# drop games before 1954
games$year <- as.numeric(substr(games$DT, 1, 4))
drop <- which(games$year < 1954) # 8642 games to drop
games <- games[-drop, ] # 51203 games remaining

# drop handicapped games
drop <- which(!is.na(games$HA)) # 1154 games to drop
games <- games[-drop, ]  # 50049 games remaning

# drop games with inappropriate first move formats
drop <- which(substr(games$move.string, 1, 2) != ";B") # 17 games to drop
games <- games[-drop, ] # 50038 games remaining

# drop this one bad game; first move is a pass, comments imply invalid
drop <- which(games$filename == "./temp/1998-04-21a.sgf")
games <- games[-drop, ]

# drop games with unusual results
drop <- which(substr(games$RE, 1, 1) %in% c("?", "U")) # 38 games to drop
games <- games[-drop, ] # 49999 games remaining

# drop games played by ranked amateurs
drop <- sort(unique(c(grep("ama|Ama", games$BR), grep("ama|Ama", games$WR)))) # 1629 games to drop
games <- games[-drop, ] # 48370 games remaining


first_move <- substr(games$move.string, 4, 5)
games$fourfour <- as.numeric(first_move %in% c("dd", "dp", "pd", "pp"))
games$threefour <- as.numeric(first_move %in% c(c("cd", "dc"), c("cp", "dq"),
  c("pc", "qd"), c("pq", "qp")))
games$threethree <- as.numeric(first_move %in% c("cc", "cq", "qc", "qq"))

year <- 1958:2009

drop <- which(!games$year %in% year)
if(length(drop) > 0) games <- games[-drop, ]

games$black_won <- substr(games$RE, 1, 1) %in% "B"


d <- data.frame(year)

col_alpha <- function (acol, alpha = 0.2){
  acol <- col2rgb(acol)
  acol.red <- acol["red",]/255
  acol.green <- acol["green",]/255
  acol.blue <- acol["blue",]/255
  acol <- mapply(function(red, green, blue, alphas) rgb(red, green, blue, alphas), acol.red, acol.green, acol.blue, alpha)
  return(as.character(acol))
}


d$fourfour_frq <- tapply(games$fourfour, games$year, mean)[as.character(d$year)]
d$threefour_frq <- tapply(games$threefour, games$year, mean)[as.character(d$year)]
d$threethree_frq <- tapply(games$threethree, games$year, mean)[as.character(d$year)]

tar <- 1:nrow(games)
d$black_win_frq <- tapply(games$black_won[tar], games$year[tar], mean)[as.character(d$year)]

tar <- which(games$fourfour == 1)
d$fourfour_win_frq <- tapply(games$black_won[tar], games$year[tar], mean)[as.character(d$year)]

tar <- which(games$threefour == 1)
d$threefour_win_frq <- tapply(games$black_won[tar], games$year[tar], mean)[as.character(d$year)]

tar <- which(games$threethree == 1)
d$threethree_win_frq <- tapply(games$black_won[tar], games$year[tar], mean)[as.character(d$year)]


plot(d$year, d$fourfour_win_frq - d$threefour_win_frq,
  type = "l", col = "#3690C0", lwd = 2, ylim = c(-0.4, 0.2),
  frame.plot = FALSE, axes = FALSE, xlab = "year", ylab = "win rate\n vs ThreeFour")
points(1968:1973, d$threethree_win_frq[1:6] - d$threefour_win_frq[1:6],
  type = "l", col = "black")
points(d$year, d$black_win_frq - d$threefour_win_frq,
  type = "l", lty = 2, col = "gray", lwd = 2)
abline(h = 0, col = "#f46d43", lwd = 2)

abline(v = c(1967, 1977, 1984, 1996, 2006), lty = 2, col = "gray")

axis(1, at = 1960 + 10 * 0:5)
axis(2, at = c(-0.4, -0.2, 0, 0.2), las = 1)

# the published average line looks wrong...


plot(d$year, d$fourfour_frq, axes = FALSE, frame.plot = FALSE,
  xlab = "year", ylab = "Frequency\n of Games", type = "n",
  ylim = c(0, 0.83), xlim = c(1958, 2009))

abline(v = c(1967, 1977, 1984, 1996, 2006), lty = 2, col = "gray")

polygon(c(d$year, rev(d$year)), c(d$threefour_frq, rep(0, nrow(d))),
  border = NA, col = col_alpha("#f46d43", 190/255))
points(d$year, d$threefour_frq, type = "l", lwd = 1.5)

polygon(c(d$year, rev(d$year)), c(d$fourfour_frq, rep(0, nrow(d))),
  border = NA, col = col_alpha("#3690C0", 221/255))
points(d$year, d$fourfour_frq, type = "l", lwd = 1.5)

polygon(c(d$year, rev(d$year)), c(d$threethree_frq, rep(0, nrow(d))),
  border = NA, col = col_alpha("#2c383f", 253/255))
points(d$year, d$threethree_frq, type = "l", lwd = 1.5)

axis(1, at = 1960 + 10 * 0:5)
axis(2, at = seq(0, 1, by = 0.2), las = 1)

