
if (scaffold) {
  rm(list = ls())
  source("project_support.R")
}

######

print("load fitted models")

d <- read.csv("first_moves.csv")
stopifnot(nrow(d) == 31756)

load("horizon24.RData")
sam <- extract(horizon24)




print("calculate key numerical results and save to disk")

calcs <- list()

# model intercept of 53.2%
post_logodds <- sam$a
calcs$prIndUseBaseline <- sprintf("%.1f", mean(logistic(post_logodds) * 100))

# recent personal use predicts current use; 60% use outcome is 58.7% (OR 9.39 ????) ceteris paribus
post_logodds <- sam$a + sam$b_ind_use * 0.1
calcs$prIndUseSixty <- sprintf("%.1f", mean(logistic(post_logodds) * 100))
calcs$orIndUse <- sprintf("%.2f", mean(sam$b_ind_use))

# recent win rate predicts current use, OR of 2.77
post_logodds <- sam$a + sam$b_ind_use * 0.1
calcs$orIndUseWin <- sprintf("%.2f", mean(exp(sam$b_ind_use_win)))

# recent personal use and win rate interaction effect exists

# recent pop use of 60% means expected prob is 59%, OR 10.475
post_logodds <- sam$a + sam$b_pop_use * 0.1
calcs$prPopUseSixty <- sprintf("%.1f", mean(logistic(post_logodds) * 100))
calcs$orPopUse <- sprintf("%.2f", mean(exp(sam$b_pop_use)))

# recent pop use of 60% and +10% performance, its 61.4%
# is this right? what *are* the centerings
post_logodds <- sam$a + sam$b_pop_use * 0.1 + sam$b_pop_use_win * 0.1 + sam$b_pop_use_x_pop_use_win * 0.1 * 0.1
calcs$prPopUseSixtyWinTen <- sprintf("%.1f", mean(logistic(post_logodds) * 100))

# social knowledge is 1.05x more important
social_indy_ratio <- sam$b_pop_use / sam$b_ind_use
calcs$socialIndyRatio <- sprintf("%.2f", mean(social_indy_ratio))

# varying effect sigma of 4.00 for player intecepts
calcs$playerIndyVaref <- sprintf("%.2f", mean(sam$Sigma_ind[ , 2, 2]))

# varying effect sigma of 9.77 for player slopes on pop knowledge
calcs$playerSocialVaref <- sprintf("%.2f", mean(sam$Sigma_ind[ , 3, 3]))

# save(calcs, file = "./RData/model_result_list.RData")

writeLines(prep_latex_variables(calcs), "./figures/keyModelCalcs.tex")



print("extract posterior estimates and create table of model estimates")

# b_post <- as.data.frame(sam[1:10])
b_post <- as.data.frame(sam[c("a", "b_ind_use", "b_ind_use_x_ind_use_win",
  "b_ind_use_x_ind_win", "b_ind_use_win", "b_pop_use", "b_pop_use_x_pop_use_win",
  "b_pop_use_x_ind_win", "b_pop_use_win", "b_komi")])
# this seems unnecessary

my_names <- c("a", "Personal Fourfour Use Rate ($\\beta$)",
  "$\\times$ Personal Fourfour Win Rate",
  "$\\times$ Personal Win Rate",
  "Personal Fourfour Win Rate",
  "Population Fourfour Use Rate ($\\gamma$)",
  "$\\times$ Population Fourfour Win Rate",
  "$\\times$ Personal Win Rate",
  "Population Fourfour Win Rate",
  "Handicap (komi)")

my_means <- sprintf("%.2f", colMeans(b_post))
my_ses <- sprintf("%.2f", apply(b_post, 2, sd))

tab1 <- cbind(my_names, my_means, my_ses)

tab1 <- rbind(tab1[2:nrow(tab1), ], tab1[1, ])

tab1 <- rbind(c("Fixed Effects", "Est.", "S.E."), tab1)
tab1 <- rbind(tab1, c("Varying Effects", "", ""))

playerID_mean <- sprintf("%.2f", mean(sam$Sigma_ind[ , 1, 1]))
playerID_se <- sprintf("%.2f", sd(sam$Sigma_ind[ , 1, 1]))

tab1 <- rbind(tab1, c("Player$_j$", playerID_mean, playerID_se))

playerIDxpersonal_mean <- sprintf("%.2f", mean(sam$Sigma_ind[ , 2, 2]))
playerIDxpersonal_se <- sprintf("%.2f", sd(sam$Sigma_ind[ , 2, 2]))

tab1 <- rbind(tab1, c("$\\times$ Personal Fourfour Use Rate",
  playerIDxpersonal_mean, playerIDxpersonal_se))

playerIDxpop_mean <- sprintf("%.2f", mean(sam$Sigma_ind[ , 3, 3]))
playerIDxpop_se <- sprintf("%.2f", sd(sam$Sigma_ind[ , 3, 3]))

tab1 <- rbind(tab1, c("$\\times$ Population Fourfour Use Rate",
  playerIDxpop_mean, playerIDxpop_se))

agexpersonal_mean <- sprintf("%.2f", mean(sam$Sigma_age_group[ , 1, 1]))
agexpersonal_se <- sprintf("%.2f", sd(sam$Sigma_age_group[ , 1, 1]))

tab1 <- rbind(tab1, c("Age$_k$ $\\times$ Personal Fourfour Use Rate",
  agexpersonal_mean, agexpersonal_se))

agexpop_mean <- sprintf("%.2f", mean(sam$Sigma_age_group[ , 2, 2]))
agexpop_se <- sprintf("%.2f", sd(sam$Sigma_age_group[ , 2, 2]))

tab1 <- rbind(tab1, c("Age$_k$ $\\times$ Population Fourfour Use Rate",
  agexpop_mean, agexpop_se))

output <- texttab(tab1, hlines = c(1, 11, 12, 17))

writeLines(output, "./figures/pr44_logistic_coefs.txt")



print("create nationality scores")

JP_cols <- sort(unique(d$ind[d$BN == "Japanese"]))
CH_cols <- sort(unique(d$ind[d$BN == "Chinese"]))
SK_cols <- sort(unique(d$ind[d$BN == "Korean"]))
TW_cols <- sort(unique(d$ind[d$BN == "Taiwanese"]))

japanese_b_intercepts <- colMeans(sam$vary_ind[ , JP_cols, 2])
japanese_gamma_intercepts <- colMeans(sam$vary_ind[ , JP_cols, 3])

chinese_b_intercepts <- colMeans(sam$vary_ind[ , CH_cols, 2])
chinese_gamma_intercepts <- colMeans(sam$vary_ind[ , CH_cols, 3])

korean_b_intercepts <- colMeans(sam$vary_ind[ , SK_cols, 2])
korean_gamma_intercepts <- colMeans(sam$vary_ind[ , SK_cols, 3])

taiwanese_b_intercepts <- colMeans(sam$vary_ind[ , TW_cols, 2])
taiwanese_gamma_intercepts <- colMeans(sam$vary_ind[ , TW_cols, 3])

ja_n <- length(japanese_b_intercepts)
ja_b_mean <- sprintf("%.2f", mean(japanese_b_intercepts))
ja_b_se <- sprintf("%.2f", sd(japanese_b_intercepts) / sqrt(ja_n))
ja_gamma_mean <- sprintf("%.2f", mean(japanese_gamma_intercepts))
ja_gamma_se <- sprintf("%.2f", sd(japanese_gamma_intercepts) / sqrt(ja_n))

ch_n <- length(chinese_b_intercepts)
ch_b_mean <- sprintf("%.2f", mean(chinese_b_intercepts))
ch_b_se <- sprintf("%.2f", sd(chinese_b_intercepts) / sqrt(ch_n))
ch_gamma_mean <- sprintf("%.2f", mean(chinese_gamma_intercepts))
ch_gamma_se <- sprintf("%.2f", sd(chinese_gamma_intercepts) / sqrt(ch_n))

ko_n <- length(korean_b_intercepts)
ko_b_mean <- sprintf("%.2f", mean(korean_b_intercepts))
ko_b_se <- sprintf("%.2f", sd(korean_b_intercepts) / sqrt(ko_n))
ko_gamma_mean <- sprintf("%.2f", mean(korean_gamma_intercepts))
ko_gamma_se <- sprintf("%.2f", sd(korean_gamma_intercepts) / sqrt(ko_n))

ta_n <- length(taiwanese_b_intercepts)
ta_b_mean <- sprintf("%.2f", mean(taiwanese_b_intercepts))
ta_b_se <- sprintf("%.2f", sd(taiwanese_b_intercepts) / sqrt(ta_n))
ta_gamma_mean <- sprintf("%.2f", mean(taiwanese_gamma_intercepts))
ta_gamma_se <- sprintf("%.2f", sd(taiwanese_gamma_intercepts) / sqrt(ta_n))

n <- c(ch_n, ja_n, ko_n, ta_n)
b_mean <- c(ch_b_mean, ja_b_mean, ko_b_mean, ta_b_mean)
b_se <- c(ch_b_se, ja_b_se, ko_b_se, ta_b_se)
gamma_mean <- c(ch_gamma_mean, ja_gamma_mean, ko_gamma_mean, ta_gamma_mean)
gamma_se <- c(ch_gamma_se, ja_gamma_se, ko_gamma_se, ta_gamma_se)
nationality <- c("Chinese", "Japanese", "South Korean", "Taiwanese")

tab2 <- cbind(nationality, n, b_mean, b_se, gamma_mean, gamma_se)

tab2 <- rbind(c("Nationality", "$n$", "$\\b_j$", "(S.E.)",
  "$\\gamma_j$", "(S.E.)"), tab2)

output <- texttab(tab2, alignment = "{lrrrrr}", hlines = c(1, 5))

writeLines(output, "./figures/national_coef_averages.txt")



print("create model-derived figures")

pdf("./figures/pr44xPop.pdf", width = 3.5, height = 3.5)

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
  est <- as.numeric(sam$a + sam$b_pop_use * x +
    sam$b_ind_use * per_use + sam$b_pop_use_win * perf +
    sam$b_pop_use_x_pop_use_win * x * perf)
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
  est <- as.numeric(sam$a + sam$b_pop_use * x +
    sam$b_ind_use * per_use + sam$b_pop_use_win * perf +
    sam$b_pop_use_x_pop_use_win * x * perf)
  my_mean[i] <- logistic(mean(est))
  lb[i] <- logistic(HPDI(est)[1])
  ub[i] <- logistic(HPDI(est)[2])
}
points(my_mean ~ xs, type = "l", lty = 2)
polygon(c(xs, rev(xs)), c(ub, rev(lb)), border = NA,
  col = col.alpha(my_col, ci_weight))

my_col <- color_low
per_use <- 0
perf <- 0.1
my_mean <- NA
lb <- NA
ub <- NA
for (i in 1:length(xs)) {
  x <- xs[i]
  est <- as.numeric(sam$a + sam$b_pop_use * x +
    sam$b_ind_use * per_use + sam$b_pop_use_win * perf +
    sam$b_pop_use_x_pop_use_win * x * perf)
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
points(d$pop_use[these], jitter(d$fourfour[these], factor = 0.5),
  col = col.alpha("gray", 0.5), pch = 20, cex = 0.5)

dev.off()



pdf("./figures/playerAgeBetaGamma.pdf", width = 7.5, height = 3.28)

par(mfrow = c(1, 2))
par(mar = c(5.1, 4.1, 0, 2.1))
par(family = "Times")

set.seed(1000)
thin <- sample(1:3000, 1000)

targets <- data.frame(
  PB = c("Peng Quan", "Kato Atsushi", "Takemiya Masaki", "Hashimoto Shoji", "Yi Se-tol", "Cho Hun-hyeon"),
  print_name = c("Peng\nQuan", "Kato\nAtsushi", "Takemiya\nMasaki", "   Hashimoto\n Shoji", "Lee\nSedol", "   Cho\n    Hunhyun"),
  color = brewer.pal(6, "Spectral")
)
targets$ind <- d$ind[match(targets$PB, d$PB)]

plot(c(-4, 10), c(-10, 16), col = "white", pch = 20,
  xlab = "reliance on individual information",
  ylab = "reliance on social information", las = 1)

for (i in 1:nrow(targets)) {
  if (!is.na(targets$ind[i])) {
  my_xs <- sam$vary_ind[thin, targets$ind[i], 2] + mean(sam$b_ind_use[thin])
  my_ys <- sam$vary_ind[thin, targets$ind[i], 3] + mean(sam$b_pop_use[thin])
  points(my_xs, my_ys, col = col.alpha(targets$color[i], 0.3), pch = 20)
  }
}

points(sam$b_ind_use[thin], sam$b_pop_use[thin], pch = 20,
  col = col.alpha("black", 0.3))

abline(h = 0, lty = 2, col = col.alpha("black", 0.6))
abline(v = 0, lty = 2, col = col.alpha("black", 0.6))
abline(0, 1, lty = 1, col = gray(0.3))

for (i in 1:nrow(targets)) {
  if (!is.na(targets$ind[i])) {
    my_x <- mean(sam$vary_ind[ , targets$ind[i], 2] +
      sam$b_ind_use)
    my_y <- mean(sam$vary_ind[ , targets$ind[i], 3] +
      sam$b_pop_use)
    text(my_x, my_y, targets$print_name[i])
  }
}

# age 8 is "1", age 9 is "2", etc.
n_ages <- length(unique(d$age_group))

my_pop_means <- apply(sam$vary_age_group[ , , 2], 2, mean) + mean(sam$b_pop_use)
my_pop_HPDI <- apply(sam$vary_age_group[ , , 2], 2, HPDI) + mean(sam$b_pop_use)
plot(my_pop_means, type = "p", ylim = c(-2, 9), xaxt = "n",
  ylab = "reliance on social information", xlab = "age (years)",
  xlim = c(3, 63), las = 1, col = "black", cex = 0.5, pch = 20)
axis(1, at = seq(3, 78, by = 10), labels =  seq(3, 78, by = 10) + 7)
for (i in 1:n_ages) lines(c(i, i), c(my_pop_HPDI[1, i], my_pop_HPDI[2, i]),
  col = "black")
abline(h = mean(sam$b_pop_use), lty = 2, col = "black")

dev.off()
