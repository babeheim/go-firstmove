
if (scaffold) {
  rm(list = ls())
  source("project_support.R")
}

#######

print("load game data")

games <- read.csv("games.csv")

stopifnot(nrow(games) == 49789)



print("identify move variants")

first_move <- games$m1
second_move <- games$m2

games$fourfour <- as.numeric(first_move %in% c("dd", "dp", "pd", "pp"))
games$threefour <- as.numeric(first_move %in% c(c("cd", "dc"), c("cp", "dq"),
  c("pc", "qd"), c("pq", "qp")))
games$threethree <- as.numeric(first_move %in% c("cc", "cq", "qc", "qq"))

games$second_dd <- first_move == "pd" & second_move == "dd" |
                   first_move == "dd" & second_move == "dp" |
                   first_move == "dp" & second_move == "pp" |
                   first_move == "pp" & second_move == "pd"

games$second_dp <- first_move == "pd" & second_move == "dp" |
                   first_move == "dd" & second_move == "pp" |
                   first_move == "dp" & second_move == "pd" |
                   first_move == "pp" & second_move == "dd"

games$second_cq <- first_move == "pd" & second_move == "cq" |
                   first_move == "dd" & second_move == "qq" |
                   first_move == "dp" & second_move == "qc" |
                   first_move == "pp" & second_move == "cc"

games$second_dc <- first_move == "pd" & second_move == "dc" |
                   first_move == "dd" & second_move == "cp" |
                   first_move == "dp" & second_move == "pq" |
                   first_move == "pp" & second_move == "qd"

games$black_won <- substr(games$RE, 1, 1) %in% "B"



print("calculate annual frequencies")

year <- 1958:2009

drop <- which(!games$year %in% year)
games <- games[-drop, ]

d <- data.frame(year)

d$fourfour_frq <- tapply(games$fourfour, games$year,
  mean)[as.character(d$year)]
d$threefour_frq <- tapply(games$threefour, games$year,
  mean)[as.character(d$year)]
d$threethree_frq <- tapply(games$threethree, games$year,
  mean)[as.character(d$year)]

tar <- 1:nrow(games)
d$black_win_frq <- tapply(games$black_won[tar], games$year[tar],
  mean)[as.character(d$year)]

tar <- which(games$fourfour == 1)
d$fourfour_win_frq <- tapply(games$black_won[tar], games$year[tar],
  mean)[as.character(d$year)]
tar <- which(games$threefour == 1)
d$threefour_win_frq <- tapply(games$black_won[tar], games$year[tar],
  mean)[as.character(d$year)]
tar <- which(games$threethree == 1)
d$threethree_win_frq <- tapply(games$black_won[tar], games$year[tar],
  mean)[as.character(d$year)]

tar <- which(games$fourfour == 1)
d$second_dd_frq <- tapply(games$second_dd[tar], games$year[tar],
  mean)[as.character(d$year)]
d$second_dp_frq <- tapply(games$second_dp[tar], games$year[tar],
  mean)[as.character(d$year)]
d$second_cq_frq <- tapply(games$second_cq[tar], games$year[tar],
  mean)[as.character(d$year)]
d$second_dc_frq <- tapply(games$second_dc[tar], games$year[tar],
  mean)[as.character(d$year)]

tar <- which(games$fourfour == 1)
d$second_avg_win_frq <- tapply(1 - games$black_won[tar],
  games$year[tar], mean)[as.character(d$year)]

tar <- which(games$second_dd == 1)
d$second_dd_win_frq <- tapply(1 - games$black_won[tar],
  games$year[tar], mean)[as.character(d$year)]

tar <- which(games$second_dp == 1)
d$second_dp_win_frq <- tapply(1 - games$black_won[tar],
  games$year[tar], mean)[as.character(d$year)]

tar <- which(games$second_cq == 1)
d$second_cq_win_frq <- tapply(1 - games$black_won[tar],
  games$year[tar], mean)[as.character(d$year)]

tar <- which(games$second_dc == 1)
d$second_dc_win_frq <- tapply(1 - games$black_won[tar],
  games$year[tar], mean)[as.character(d$year)]


print("calculate essential descriptive statistics of dataset")

calcs <- list()

tar <- which(d$year %in% 1967:2009)
frq_win_cor <- cor(d$fourfour_frq[tar], d$fourfour_win_frq[tar] - d$threefour_win_frq[tar])
calcs$corFourFourFrqWin <- sprintf("%.2f", frq_win_cor)
calcs$nGames <- format(nrow(games), big.mark = ",", trim = TRUE)

calcs$nGamesTakemiya <- format(sum(games$PB == "Takemiya Masaki"), big.mark = ",", trim = TRUE)
calcs$nGamesTakemiyaNoFourFour <- format(sum(games$PB == "Takemiya Masaki" & games$fourfour == 0), big.mark = ",", trim = TRUE)

win_rate_black_takemiya <- mean(games$black_won[which(games$PB == "Takemiya Masaki" & games$year %in% 1968:1972)])

calcs$winRateBlackTakemiya <- sprintf("%1.0f", 100 * win_rate_black_takemiya)

win_rate_black <- mean(games$black_won[which(games$year %in% 1968:1972)])

calcs$winRateBlack <- sprintf("%1.0f", 100 * win_rate_black)



writeLines(prep_latex_variables(calcs), "figures/gameDescriptives.tex")



print("plot frequencies of different first moves")

png("./figures/first_move_frqs.png", res = 300,
  units = "in", height = 4, width = 5)

par(mar = c(5.1, 6.1, 4.1, 2.1))

plot(d$year, d$fourfour_frq, axes = FALSE, frame.plot = FALSE,
  xlab = "year", ylab = "Frequency\n of Games", type = "n",
  ylim = c(0, 0.83), xlim = c(1958, 2009))

abline(v = c(1967, 1977, 1984, 1996, 2006), lty = 2, col = "gray")

polygon(c(d$year, rev(d$year)), c(d$threefour_frq, rep(0, nrow(d))),
  border = NA, col = col_alpha("#f46d43", 190 / 255))
points(d$year, d$threefour_frq, type = "l", lwd = 1.5)

polygon(c(d$year, rev(d$year)), c(d$fourfour_frq, rep(0, nrow(d))),
  border = NA, col = col_alpha("#3690C0", 221 / 255))
points(d$year, d$fourfour_frq, type = "l", lwd = 1.5)

polygon(c(d$year, rev(d$year)), c(d$threethree_frq, rep(0, nrow(d))),
  border = NA, col = col_alpha("#2c383f", 253 / 255))
points(d$year, d$threethree_frq, type = "l", lwd = 1.5)

axis(1, at = 1960 + 10 * 0:5)
axis(2, at = seq(0, 1, by = 0.2), las = 1)

dev.off()



print("plot win frequencies of different first moves")

png("./figures/first_move_win_frqs.png", res = 300,
  units = "in", height = 4, width = 5)

par(mar = c(5.1, 6.1, 4.1, 2.1))

plot(d$year, d$fourfour_win_frq - d$threefour_win_frq,
  type = "l", col = "#3690C0", lwd = 2, ylim = c(-0.4, 0.2),
  frame.plot = FALSE, axes = FALSE, xlab = "year",
  ylab = "win rate\n vs ThreeFour")
points(1968:1973, d$threethree_win_frq[1:6] - d$threefour_win_frq[1:6],
  type = "l", col = "black")
points(d$year, d$black_win_frq - d$threefour_win_frq,
  type = "l", lty = 2, col = "gray", lwd = 2)
abline(h = 0, col = "#f46d43", lwd = 2)

abline(v = c(1967, 1977, 1984, 1996, 2006), lty = 2, col = "gray")

axis(1, at = 1960 + 10 * 0:5)
axis(2, at = c(-0.4, -0.2, 0, 0.2), las = 1)

dev.off()

# the published average line looks wrong...



print("plot second move frequencies in response to a 44")

png("./figures/second_move_frqs.png", res = 300,
  units = "in", height = 4, width = 5)

par(mar = c(5.1, 6.1, 4.1, 2.1))

plot(d$year, d$second_dd_frq,
  axes = FALSE, frame.plot = FALSE,
  xlab = "year", ylab = "Frequency\n of Games", type = "n",
  ylim = c(0, 0.83), xlim = c(1964, 2009))

abline(v = c(1967, 1977, 1984, 1996, 2006), lty = 2, col = "gray")

polygon(c(d$year, rev(d$year)), c(d$second_dc_frq, rep(0, nrow(d))),
  border = NA, col = col_alpha("#85ceb7", 255 / 255))
points(d$year, d$second_dc_frq, type = "l", lwd = 1.5)

polygon(c(d$year, rev(d$year)), c(d$second_dp_frq, rep(0, nrow(d))),
  border = NA, col = col_alpha("#7bc3dd", 178 / 255))
points(d$year, d$second_dp_frq, type = "l", lwd = 1.5)

polygon(c(d$year, rev(d$year)), c(d$second_dd_frq, rep(0, nrow(d))),
  border = NA, col = col_alpha("#f46d43", 163 / 255))
points(d$year, d$second_dd_frq, type = "l", lwd = 1.5)

polygon(c(d$year, rev(d$year)), c(d$second_cq_frq, rep(0, nrow(d))),
  border = NA, col = col_alpha("#fee08b", 190 / 255))
points(d$year, d$second_cq_frq, type = "l", lwd = 1.5)

axis(1, at = 1960 + 10 * 0:5)
axis(2, at = seq(0, 1, by = 0.2), las = 1)

dev.off()



print("plot win frequencies of responses to a 44")

png("./figures/second_move_win_frqs.png", res = 300,
  units = "in", height = 4, width = 5)

par(mar = c(5.1, 6.1, 4.1, 2.1))

plot(d$year, d$second_dc_win_frq - d$second_avg_win_frq,
  type = "l", col = "#85ceb7", lwd = 2,
  xlim = c(1964, 2009), ylim = c(-0.4, 0.4),
  frame.plot = FALSE, axes = FALSE, xlab = "year",
  ylab = "Relative Performance")
points(d$year, d$second_dp_win_frq - d$second_avg_win_frq,
  type = "l", col = "#7bc3dd", lwd = 2)
points(d$year, d$second_dd_win_frq - d$second_avg_win_frq,
  type = "l", col = "#f46d43", lwd = 2)
points(d$year, d$second_cq_win_frq - d$second_avg_win_frq,
  type = "l", col = "#fee08b", lwd = 2)
abline(h = 0, lwd = 2)

abline(v = c(1967, 1977, 1984, 1996, 2006), lty = 2, col = "gray")

axis(1, at = 1960 + 10 * 0:5)
axis(2, at = c(-0.4, 0, 0.4), las = 1)

dev.off()
