
if (scaffold) {
  rm(list = ls())
  source("../project_support.r")
}

dir_init("./temp")

print("load fitted models")

load("./inputs/horizon24.robj")
d <- read.csv("./inputs/fourfour_final.csv", stringsAsFactors = FALSE)
p <- extract(horizon24)

print("extract posterior estimates and create Table 1")

beta_post <- as.data.frame(p[1:10])
colnames(beta_post) <- c("Intercept", "b_44", "b_44xb_win_44",
  "b_44xb_win", "b_win_44", "pop_44", "pop_44xpop_win_44",
  "pop_44xb_win", "pop_win_44", "komi")

my_names <- c("Intercept", "Personal Fourfour Use Rate ($\\beta$)",
  "$\\times$ Personal Fourfour Win Rate",
  "$\\times$ Personal Win Rate",
  "Personal Fourfour Win Rate",
  "Population Fourfour Use Rate ($\\gamma$)",
  "$\\times$ Population Fourfour Win Rate",
  "$\\times$ Personal Win Rate",
  "Population Fourfour Win Rate",
  "Handicap (komi)")

my_means <- sprintf("%.2f", colMeans(beta_post))
my_ses <- sprintf("%.2f", apply(beta_post, 2, sd))

tab1 <- cbind(my_names, my_means, my_ses)

tab1 <- rbind(tab1[2:nrow(tab1), ], tab1[1, ])

tab1 <- rbind(c("Fixed Effects", "Est.", "S.E."), tab1)
tab1 <- rbind(tab1, c("Varying Effects", "", ""))

playerID_mean <- sprintf("%.2f", mean(p$Sigma_PB_id[ , 1, 1]))
playerID_se <- sprintf("%.2f", sd(p$Sigma_PB_id[ , 1, 1]), 2)

tab1 <- rbind(tab1, c("Player$_j$", playerID_mean, playerID_se))

playerIDxpersonal_mean <- sprintf("%.2f", mean(p$Sigma_PB_id[ , 2, 2]), 2)
playerIDxpersonal_se <- sprintf("%.2f", sd(p$Sigma_PB_id[ , 2, 2]), 2)

tab1 <- rbind(tab1, c("$\\times$ Personal Fourfour Use Rate",
  playerIDxpersonal_mean, playerIDxpersonal_se))

playerIDxpop_mean <- sprintf("%.2f", mean(p$Sigma_PB_id[ , 3, 3]), 2)
playerIDxpop_se <- sprintf("%.2f", sd(p$Sigma_PB_id[ , 3, 3]), 2)

tab1 <- rbind(tab1, c("$\\times$ Population Fourfour Use Rate",
  playerIDxpop_mean, playerIDxpop_se))

agexpersonal_mean <- sprintf("%.2f", mean(p$Sigma_b_age_group[ , 1, 1]), 2)
agexpersonal_se <- sprintf("%.2f", sd(p$Sigma_b_age_group[ , 1, 1]), 2)

tab1 <- rbind(tab1, c("Age$_k$ $\\times$ Personal Fourfour Use Rate",
  agexpersonal_mean, agexpersonal_se))

agexpop_mean <- sprintf("%.2f", mean(p$Sigma_b_age_group[ , 2, 2]), 2)
agexpop_se <- sprintf("%.2f", sd(p$Sigma_b_age_group[ , 2, 2]), 2)

tab1 <- rbind(tab1, c("Age$_k$ $\\times$ Population Fourfour Use Rate",
  agexpop_mean, agexpop_se))

output <- texttab(tab1, hlines = c(1, 11, 12, 17))

writeLines(output, "./temp/table1.txt")

print("create Table 2, nationality scores")

# players averaged by nationality...

JP_cols <- sort(unique(d$PB_id[d$BN == "Japanese"]))
CH_cols <- sort(unique(d$PB_id[d$BN == "Chinese"]))
SK_cols <- sort(unique(d$PB_id[d$BN == "Korean"]))
TW_cols <- sort(unique(d$PB_id[d$BN == "Taiwanese"]))

japanese_beta_intercepts <- colMeans(p$vary_PB_id[ , JP_cols, 2])
japanese_gamma_intercepts <- colMeans(p$vary_PB_id[ , JP_cols, 3])

chinese_beta_intercepts <- colMeans(p$vary_PB_id[ , CH_cols, 2])
chinese_gamma_intercepts <- colMeans(p$vary_PB_id[ , CH_cols, 3])

korean_beta_intercepts <- colMeans(p$vary_PB_id[ , SK_cols, 2])
korean_gamma_intercepts <- colMeans(p$vary_PB_id[ , SK_cols, 3])

taiwanese_beta_intercepts <- colMeans(p$vary_PB_id[ , TW_cols, 2])
taiwanese_gamma_intercepts <- colMeans(p$vary_PB_id[ , TW_cols, 3])

ja_n <- length(japanese_beta_intercepts)
ja_beta_mean <- sprintf("%.2f", mean(japanese_beta_intercepts), 2)
ja_beta_se <- sprintf("%.2f", sd(japanese_beta_intercepts) / sqrt(ja_n), 2)
ja_gamma_mean <- sprintf("%.2f", mean(japanese_gamma_intercepts), 2)
ja_gamma_se <- sprintf("%.2f", sd(japanese_gamma_intercepts) / sqrt(ja_n), 2)

ch_n <- length(chinese_beta_intercepts)
ch_beta_mean <- sprintf("%.2f", mean(chinese_beta_intercepts), 2)
ch_beta_se <- sprintf("%.2f", sd(chinese_beta_intercepts) / sqrt(ch_n), 2)
ch_gamma_mean <- sprintf("%.2f", mean(chinese_gamma_intercepts), 2)
ch_gamma_se <- sprintf("%.2f", sd(chinese_gamma_intercepts) / sqrt(ch_n), 2)

ko_n <- length(korean_beta_intercepts)
ko_beta_mean <- sprintf("%.2f", mean(korean_beta_intercepts), 2)
ko_beta_se <- sprintf("%.2f", sd(korean_beta_intercepts) / sqrt(ko_n), 2)
ko_gamma_mean <- sprintf("%.2f", mean(korean_gamma_intercepts), 2)
ko_gamma_se <- sprintf("%.2f", sd(korean_gamma_intercepts) / sqrt(ko_n), 2)

ta_n <- length(taiwanese_beta_intercepts)
ta_beta_mean <- sprintf("%.2f", mean(taiwanese_beta_intercepts), 2)
ta_beta_se <- sprintf("%.2f", sd(taiwanese_beta_intercepts) / sqrt(ta_n), 2)
ta_gamma_mean <- sprintf("%.2f", mean(taiwanese_gamma_intercepts), 2)
ta_gamma_se <- sprintf("%.2f", sd(taiwanese_gamma_intercepts) / sqrt(ta_n), 2)

n <- c(ch_n, ja_n, ko_n, ta_n)
beta_mean <- c(ch_beta_mean, ja_beta_mean, ko_beta_mean, ta_beta_mean)
beta_se <- c(ch_beta_se, ja_beta_se, ko_beta_se, ta_beta_se)
gamma_mean <- c(ch_gamma_mean, ja_gamma_mean, ko_gamma_mean, ta_gamma_mean)
gamma_se <- c(ch_gamma_se, ja_gamma_se, ko_gamma_se, ta_gamma_se)
nationality <- c("Chinese", "Japanese", "South Korean", "Taiwanese")

tab2 <- cbind(nationality, n, beta_mean, beta_se, gamma_mean, gamma_se)

tab2 <- rbind(c("Nationality", "$n$", "$\\beta_j$", "(S.E.)",
  "$\\gamma_j$", "(S.E.)"), tab2)

output <- texttab(tab2, alignment = "{lrrrrr}", hlines = c(1, 5))

writeLines(output, "./temp/table2.txt")

print("create figures")

pdf("./temp/FigPr44xPop_COL.pdf", width = 3.5, height = 3.5)

ci_weight <- 0.6

color_high <- "orange"
color_med <- "lightblue2"
color_low <- "royalblue"

par(mar = c(5.1, 4.1, 0, 0))

par(family = "Times")

xs <- seq(-0.5, 0.5, length = 100)

# pane 2
plot(c(-0.5, 0.5), c(0, 1), col = "white", xlab = "recent population 44 use",
  ylab = "pr(use 44)", xaxt = "n", las = 1, ylim = c(-0.1, 1.1))
axis(1, at = c(-0.5, -0.25, 0, 0.25, 0.5),
  labels = c(-0.5, -0.25, 0, 0.25, 0.5) + 0.5)

my_col <- color_high
per_use <- 0
perf <- -0.1
my_mean <- NA
lb <- NA
ub <- NA
for (i in 1:length(xs)) {
  x <- xs[i]
  est <- as.numeric(beta_post$Intercept + beta_post$pop_44 * x +
    beta_post$b_44 * per_use + beta_post$pop_win_44 * perf +
    beta_post$pop_44xpop_win_44 * x * perf)
  my_mean[i] <- logistic(mean(est))
  lb[i] <- logistic(HPDI(est)[1])
  ub[i] <- logistic(HPDI(est)[2])
}
points(my_mean ~ xs, type = "l", lty = 2)
polygon(c(xs, rev(xs)), c(ub, rev(lb)), border = NA,
  col = col.alpha(my_col, ci_weight))

text(0.13, 0.38, "-10% perf.", col = my_col)

my_col <- color_med
per_use <- 0
perf <- 0
my_mean <- NA
lb <- NA
ub <- NA
for (i in 1:length(xs)) {
  x <- xs[i]
  est <- as.numeric(beta_post$Intercept + beta_post$pop_44 * x +
    beta_post$b_44 * per_use + beta_post$pop_win_44 * perf +
    beta_post$pop_44xpop_win_44 * x * perf)
  my_mean[i] <- logistic(mean(est))
  lb[i] <- logistic(HPDI(est)[1])
  ub[i] <- logistic(HPDI(est)[2])
}
points(my_mean ~ xs, type = "l", lty = 2)
polygon(c(xs, rev(xs)), c(ub, rev(lb)), border = NA,
  col = col.alpha(my_col, ci_weight))

my_col <- color_low
per_use <- 0
perf <-  + 0.1
my_mean <- NA
lb <- NA
ub <- NA
for (i in 1:length(xs)) {
  x <- xs[i]
  est <- as.numeric(beta_post$Intercept + beta_post$pop_44 * x +
    beta_post$b_44 * per_use + beta_post$pop_win_44 * perf +
    beta_post$pop_44xpop_win_44 * x * perf)
  my_mean[i] <- logistic(mean(est))
  lb[i] <- logistic(HPDI(est)[1])
  ub[i] <- logistic(HPDI(est)[2])
}
points(my_mean ~ xs, type = "l", lty = 2)
polygon(c(xs, rev(xs)), c(ub, rev(lb)), border = NA,
  col = col.alpha(my_col, ci_weight))

text(-0.05, 0.74, " + 10% perf.", col = my_col)

d$fourfour <- as.numeric(d$fourfour)

these <- sample(1:nrow(d), 2000)
points(d$pop_44[these], jitter(d$fourfour[these], factor = 0.5),
  col = col.alpha("gray", 0.5), pch = 20, cex = 0.5)

dev.off()



# effect of player ID

pdf("./temp/FigPlayerInt.pdf", height = 7, width = 7)

my_col <- heat.colors(300)

par(mfrow = c(3, 1))
par(mar = c(3.1, 4.1, 1.1, 2.1))

Pb_int <- p$vary_PB_id[ , , 1]
PBxb_44 <- p$vary_PB_id[ , , 2]
PBxpop_44 <- p$vary_PB_id[ , , 3]

Pb_int <- apply(Pb_int, 2, function(z) z + p$Intercept)
my_means <-  logistic(apply(Pb_int, 2, mean))
o <- order(my_means)
my_means <- my_means[o]
n_pl <- length(my_means)
my_HPDI <- logistic(apply(Pb_int, 2, HPDI))
my_HPDI <- my_HPDI[ , o]
plot(1:n_pl, my_means, ylim = c(0, 1), xaxt = "n", las = 1,
  ylab = "Pr(use 44)", xlab = "player", pch = 20, cex = 0.5, frame.plot = FALSE)
for (i in 1:n_pl) {
  lines(c(i, i), c(my_HPDI[1, i], my_HPDI[2, i]), col = my_col[i])
}
points(1:n_pl, my_means, pch = 20)
abline(h = logistic(mean(p$Intercept)), col = "gray", lty = 2)
abline(h = logistic(HPDI(as.numeric(p$Intercept))), col = "gray")

PBxb_44 <- apply(PBxb_44, 2, function(z) z + p$beta_b_44)
my_b_means <- apply(PBxb_44, 2, mean)
  o <- order(my_means)
my_b_means <- my_b_means[o]
my_b_HPDI <- apply(PBxb_44, 2, HPDI)
my_b_HPDI <- my_b_HPDI[ , o]
plot(1:n_pl, my_b_means, ylim = c(min(my_b_HPDI[1, ]),
  max(my_b_HPDI[2, ])), xaxt = "n", las = 1, ylab = "beta + beta_j",
  xlab = "player", pch = 20, cex = 0.5, frame.plot = FALSE)
for (i in 1:n_pl) {
  lines(c(i, i), c(my_b_HPDI[1, i], my_b_HPDI[2, i]), col = my_col[i])
}
abline(h = (mean(p$beta_b_44)), col = "gray", lty = 2)
abline(h = (HPDI(as.numeric(p$beta_b_44))), col = "gray")
points(1:n_pl, my_b_means, pch = 20)
abline(h = 0)

PBxpop_44 <- apply(PBxpop_44, 2, function(z) z + p$beta_pop_44)
my_pop_means <- apply(PBxpop_44, 2, mean)
o <- order(my_pop_means)
my_pop_means <- my_pop_means[o]
my_pop_HPDI <- apply(PBxpop_44, 2, HPDI)
my_pop_HPDI <- my_pop_HPDI[ , o]
plot(1:n_pl, my_pop_means, ylim = c(min(my_pop_HPDI[1, ]),
  max(my_pop_HPDI[2, ])), xaxt = "n", las = 1, ylab = "gamma + gamma_j",
  xlab = "player", pch = 20, cex = 0.5, frame.plot = FALSE)
for (i in 1:n_pl) {
  lines(c(i, i), c(my_pop_HPDI[1, i], my_pop_HPDI[2, i]), col = my_col[i])
}
abline(h = (mean(p$beta_pop_44)), col = "gray", lty = 2)
abline(h = (HPDI(as.numeric(p$beta_pop_44))), col = "gray")
points(1:n_pl, my_pop_means, pch = 20)
abline(h = 0)

dev.off()

pdf("./temp/FigPlayerAgeBetaGamma_allplayers.pdf", width = 3.75, height = 3.28)

plot(c(-4, 10), c(-10, 16), col = "white", pch = 20,
  xlab = "reliance on individual information",
  ylab = "reliance on social information", las = 1)

thin <- sample(1:5000, 1000)

points(p$beta_b_44[thin], p$beta_pop_44[thin], pch = 20,
  col = col.alpha("black", 0.3))

abline(h = 0, lty = 2, col = col.alpha("black", 0.6))
abline(v = 0, lty = 2, col = col.alpha("black", 0.6))
abline(0, 1, lty = 1, col = gray(0.3))

player_beta_effects <- colMeans(p$vary_PB_id[ , , 2]) + mean(p$beta_b_44)
player_gamma_effects <- colMeans(p$vary_PB_id[ , , 3]) + mean(p$beta_pop_44)
points(player_beta_effects, player_gamma_effects)

targets <- c("Peng Quan", "Kato Atsushi", "Takemiya Masaki",
  "Hashimoto Shoji", "Yi Seong-chae", "Cho Hun-hyeon")
target_ids <- unique(d$PB_id[d$PB %in% targets])

points(player_beta_effects[target_ids], player_gamma_effects[target_ids],
  pch = 20, col = "red")

dev.off()



pdf("./temp/FigPlayerAgeBetaGamma_COL.pdf", width = 7.5, height = 3.28)

par(mfrow = c(1, 2))
par(mar = c(5.1, 4.1, 0, 2.1))
par(family = "Times")

my_cols <- brewer.pal(6, "Spectral")

set.seed(1000)
thin <- sample(1:3000, 1000)

plot(c(-4, 10), c(-10, 16), col = "white", pch = 20,
  xlab = "reliance on individual information",
  ylab = "reliance on social information", las = 1)

target_id_list <- integer(0)
target_name_list <- character(0)

target <- "Peng Quan"
target_col <- my_cols[1]
target_id <- unique(d$PB_id[d$PB == target])
i <- as.numeric(target_id)
target_name_list <- c(target_name_list, target)
target_id_list <- c(target_id_list, target_id)
points(p$vary_PB_id[thin, i, 2] + mean(p$beta_b_44[thin]),
  p$vary_PB_id[thin, i, 3] + mean(p$beta_pop_44[thin]),
  col = col.alpha(target_col, 0.3), pch = 20)

target <- "Kato Atsushi"
target_col <- my_cols[2]
target_id <- unique(d$PB_id[d$PB == target])
i <- target_id
target_name_list <- c(target_name_list, target)
target_id_list <- c(target_id_list, target_id)
points(p$vary_PB_id[thin, i, 2] +
  mean(p$beta_b_44[thin]), p$vary_PB_id[thin, i, 3] +
  mean(p$beta_pop_44[thin]), col = col.alpha(target_col, 0.3), pch = 20)

target <- "Takemiya Masaki"
target_col <- my_cols[3]
target_id <- unique(d$PB_id[d$PB == target])
i <- target_id
target_name_list <- c(target_name_list, target)
target_id_list <- c(target_id_list, target_id)
points(p$vary_PB_id[thin, i, 2] +
  mean(p$beta_b_44[thin]), p$vary_PB_id[thin, i, 3] +
  mean(p$beta_pop_44[thin]), col = col.alpha(target_col, 0.3), pch = 20)

target <- "Hashimoto Shoji"
target_col <- my_cols[4]
target_id <- unique(d$PB_id[d$PB == target])
i <- target_id
target_name_list <- c(target_name_list, target)
target_id_list <- c(target_id_list, target_id)
points(p$vary_PB_id[thin, i, 2] +
  mean(p$beta_b_44[thin]), p$vary_PB_id[thin, i, 3] +
  mean(p$beta_pop_44[thin]), col = col.alpha(target_col, 0.3), pch = 20)

target <- "Yi Se-tol"
target_col <- my_cols[5]
target_id <- unique(d$PB_id[d$PB == target])
i <- target_id
target_name_list <- c(target_name_list, target)
target_id_list <- c(target_id_list, target_id)
points(p$vary_PB_id[thin, i, 2] +
  mean(p$beta_b_44[thin]), p$vary_PB_id[thin, i, 3] +
  mean(p$beta_pop_44[thin]), col = col.alpha(target_col, 0.3), pch = 20)

target <- "Cho Hun-hyeon"
target_col <- my_cols[6]
target_id <- unique(d$PB_id[d$PB == target])
i <- target_id
target_name_list <- c(target_name_list, target)
target_id_list <- c(target_id_list, target_id)
points(p$vary_PB_id[thin, i, 2] +
  mean(p$beta_b_44[thin]), p$vary_PB_id[thin, i, 3] +
  mean(p$beta_pop_44[thin]), col = col.alpha(target_col, 0.3), pch = 20)

target_name_list[target_name_list == "Takemiya Masaki"] <- "Takemiya\nMasaki"
target_name_list[target_name_list == "Yi Se-tol"] <- "Lee\nSedol"
target_name_list[target_name_list == "Cho Hun-hyeon"] <- "   Cho\n    Hunhyun"
target_name_list[target_name_list == "Hashimoto Shoji"] <- "   Hashimoto\n Shoji"
target_name_list[target_name_list == "Kato Atsushi"] <- "Kato\nAtsushi"
target_name_list[target_name_list == "Peng Quan"] <- "Peng\nQuan"

points(p$beta_b_44[thin], p$beta_pop_44[thin], pch = 20,
  col = col.alpha("black", 0.3))

abline(h = 0, lty = 2, col = col.alpha("black", 0.6))
abline(v = 0, lty = 2, col = col.alpha("black", 0.6))
abline(0, 1, lty = 1, col = gray(0.3))

for (i in 1:length(target_id_list)) {
  text(mean(p$vary_PB_id[ , target_id_list[i], 2] +
    p$beta_b_44), mean(p$vary_PB_id[ , target_id_list[i], 3] +
    p$beta_pop_44), target_name_list[i])
}

# age 8 is "1", age 9 is "2", etc.
n_ages <- length(unique(d$b_age_group))

my_pop_means <- apply(p$vary_b_age_group[ , , 2], 2, mean) + mean(p$beta_pop_44)
my_pop_HPDI <- apply(p$vary_b_age_group[ , , 2], 2, HPDI) + mean(p$beta_pop_44)
plot(my_pop_means, type = "p", ylim = c(-2, 9), xaxt = "n",
  ylab = "reliance on social information", xlab = "age (years)",
  xlim = c(3, 63), las = 1, col = "black", cex = 0.5, pch = 20)
axis(1, at = seq(3, 78, by = 10), labels =  seq(3, 78, by = 10) + 7)
for (i in 1:n_ages) lines(c(i, i), c(my_pop_HPDI[1, i], my_pop_HPDI[2, i]),
  col = "black")
abline(h = mean(p$beta_pop_44), lty = 2, col = "black")

dev.off()

if (save_output) {
  dir_init("./output")
  files <- list.files("./temp", full.names = TRUE)
  file.copy(files, "./output")
}

if (!save_temp) unlink("./temp", recursive = TRUE)
