
######

print("load fitted models")

d <- read.csv("first_moves.csv")
stopifnot(nrow(d) == 31756)

fit <- readRDS("horizon24.RDS")
sam <- as_draws_rvars(fit$draws())



print("calculate key numerical results and save to disk")

calcs <- list()

calcs$nGamesExcluded <- format(nrow(d), big.mark = ",", trim = TRUE)

calcs$nIterations <- format(n_iter, big.mark = ",", trim = TRUE)
calcs$nBurnIn <- format(floor(n_iter/2), big.mark = ",", trim = TRUE)

calcs$nChinesePlayers <- length(unique(d$PB[which(d$BN == "Chinese")]))
calcs$nJapanesePlayers <- length(unique(d$PB[which(d$BN == "Japanese")]))
calcs$nKoreanPlayers <- length(unique(d$PB[which(d$BN == "Korean")]))
calcs$nTaiwanesePlayers <- length(unique(d$PB[which(d$BN == "Taiwanese")]))

post_logodds <- draws_of(sam$a)
calcs$prIndUseBaseline <- sprintf("%.1f", mean(logistic(post_logodds) * 100))

calcs$betaIndUse <- sprintf("%.2f", mean(draws_of(sam$b_ind_use)))

post_logodds <- draws_of(sam$a) + draws_of(sam$b_ind_use) * 0.1
calcs$prIndUseSixty <- sprintf("%.1f", mean(logistic(post_logodds) * 100))
calcs$orIndUse <- sprintf("%.2f", mean(draws_of(sam$b_ind_use)))

post_logodds <- draws_of(sam$a) + draws_of(sam$b_ind_use) * 0.1
calcs$orIndUseWin <- sprintf("%.2f", mean(exp(draws_of(sam$b_ind_use_win))))

post_logodds <- draws_of(sam$a) + draws_of(sam$b_pop_use) * 0.1
calcs$prPopUseSixty <- sprintf("%.1f", mean(logistic(post_logodds) * 100))
calcs$orPopUse <- sprintf("%.2f", mean(exp(draws_of(sam$b_pop_use))))

post_logodds <- draws_of(sam$a) + draws_of(sam$b_pop_use) * 0.1 + draws_of(sam$b_pop_use_win) * 0.1 + draws_of(sam$b_pop_use_x_pop_use_win) * 0.1 * 0.1
calcs$prPopUseSixtyWinTen <- sprintf("%.1f", mean(logistic(post_logodds) * 100))

social_indy_ratio <- draws_of(sam$b_pop_use) / draws_of(sam$b_ind_use)
calcs$socialIndyRatio <- sprintf("%.2f", mean(social_indy_ratio))

calcs$playerIndyVaref <- sprintf("%.2f", mean(draws_of(sam$sigma_ind_use_ind)))

calcs$playerSocialVaref <- sprintf("%.2f", mean(draws_of(sam$sigma_pop_use_ind)))

writeLines(prep_latex_variables(calcs), "./figures/keyModelCalcs.tex")


# ugh, just start over agian once u have the cmdstan model...

# print("extract posterior estimates and create table of model estimates")

b_post <- data.frame(
  a = draws_of(sam["a"]),
  b_ind_use = draws_of(sam["b_ind_use"]),
  b_ind_use_x_ind_use_win = draws_of(sam["b_ind_use_x_ind_use_win"]),
  b_ind_use_x_ind_win = draws_of(sam["b_ind_use_x_ind_win"]),
  b_ind_use_win = draws_of(sam["b_ind_use_win"]),
  b_pop_use = draws_of(sam["b_pop_use"]),
  b_pop_use_x_pop_use_win = draws_of(sam["b_pop_use_x_pop_use_win"]),
  b_pop_use_x_ind_win = draws_of(sam["b_pop_use_x_ind_win"]),
  b_pop_use_win = draws_of(sam["b_pop_use_win"]),
  b_komi = draws_of(sam["b_komi"])
)

my_names <- c("Intercept",
  "Personal Fourfour Use Rate ($\\beta$)",
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

playerID_mean <- sprintf("%.2f", mean(sam$sigma_a_ind))
playerID_se <- sprintf("%.2f", sd(sam$sigma_a_ind))

tab1 <- rbind(tab1, c("Player$_j$", playerID_mean, playerID_se))

playerIDxpersonal_mean <- sprintf("%.2f", mean(sam$sigma_ind_use_ind))
playerIDxpersonal_se <- sprintf("%.2f", sd(sam$sigma_ind_use_ind))

tab1 <- rbind(tab1, c("$\\times$ Personal Fourfour Use Rate",
  playerIDxpersonal_mean, playerIDxpersonal_se))

playerIDxpop_mean <- sprintf("%.2f", mean(sam$sigma_pop_use_ind))
playerIDxpop_se <- sprintf("%.2f", sd(sam$sigma_pop_use_ind))

tab1 <- rbind(tab1, c("$\\times$ Population Fourfour Use Rate",
  playerIDxpop_mean, playerIDxpop_se))

agexpersonal_mean <- sprintf("%.2f", mean(sam$sigma_ind_use_age))
agexpersonal_se <- sprintf("%.2f", sd(sam$sigma_ind_use_age))

tab1 <- rbind(tab1, c("Age$_k$ $\\times$ Personal Fourfour Use Rate",
  agexpersonal_mean, agexpersonal_se))

agexpop_mean <- sprintf("%.2f", mean(sam$sigma_pop_use_age))
agexpop_se <- sprintf("%.2f", sd(sam$sigma_pop_use_age))

tab1 <- rbind(tab1, c("Age$_k$ $\\times$ Population Fourfour Use Rate",
  agexpop_mean, agexpop_se))

output <- texttab(tab1, hlines = c(1, 11, 12, 17))

writeLines(output, "./figures/pr44_logistic_coefs.txt")



print("create nationality scores")

grps <- c("Chinese", "Japanese", "Korean", "Taiwanese")
grp_labels <- c("Chinese", "Japanese", "South Korean", "Taiwanese")

for (i in 1:length(grps)) {
  grp_cols <- sort(unique(d$ind[d$BN == grps[i]]))
  grp_n <- length(grp_cols)
  grp_beta_intercepts <- colMeans(draws_of(sam$vary_ind[grp_cols, 2]))
  grp_gamma_intercepts <- colMeans(draws_of(sam$vary_ind[grp_cols, 3]))

  add <- data.frame(
    label = grp_labels[i],
    n = length(grp_cols),
    beta_mean = sprintf("%.2f", mean(grp_beta_intercepts)),
    beta_se = sprintf("%.2f", sd(grp_beta_intercepts) / sqrt(grp_n)),
    gamma_mean = sprintf("%.2f", mean(grp_gamma_intercepts)),
    gamma_se = sprintf("%.2f", sd(grp_gamma_intercepts) / sqrt(grp_n))
  )
  if (i == 1) {
    out <- add
  } else {
    out <- bind_rows(out, add)
  }

}

out <- rbind(c("Nationality", "$n$", "$\\beta_j$", "(S.E.)",
  "$\\gamma_j$", "(S.E.)"), out)
out <- texttab(out, alignment = "{lrrrrr}", hlines = c(1, 5))

writeLines(out, "./figures/national_coef_averages.txt")



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
  est <- as.numeric(draws_of(sam$a) + draws_of(sam$b_pop_use) * x +
    draws_of(sam$b_ind_use) * per_use + draws_of(sam$b_pop_use_win) * perf +
    draws_of(sam$b_pop_use_x_pop_use_win) * x * perf)
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
  est <- as.numeric(draws_of(sam$a) + draws_of(sam$b_pop_use) * x +
    draws_of(sam$b_ind_use) * per_use + draws_of(sam$b_pop_use_win) * perf +
    draws_of(sam$b_pop_use_x_pop_use_win) * x * perf)
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
  est <- as.numeric(draws_of(sam$a) + draws_of(sam$b_pop_use) * x +
    draws_of(sam$b_ind_use) * per_use + draws_of(sam$b_pop_use_win) * perf +
    draws_of(sam$b_pop_use_x_pop_use_win) * x * perf)
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
  my_xs <- draws_of(sam$vary_ind[targets$ind[i], 2])[thin] + mean(draws_of(sam$b_ind_use)[thin])
  my_ys <- draws_of(sam$vary_ind[targets$ind[i], 3])[thin] + mean(draws_of(sam$b_pop_use)[thin])
  points(my_xs, my_ys, col = col.alpha(targets$color[i], 0.3), pch = 20)
  }
}

points(draws_of(sam$b_ind_use)[thin], draws_of(sam$b_pop_use)[thin], pch = 20,
  col = col.alpha("black", 0.3))

abline(h = 0, lty = 2, col = col.alpha("black", 0.6))
abline(v = 0, lty = 2, col = col.alpha("black", 0.6))
abline(0, 1, lty = 1, col = gray(0.3))

for (i in 1:nrow(targets)) {
  if (!is.na(targets$ind[i])) {
    my_x <- mean(draws_of(sam$vary_ind)[ , targets$ind[i], 2] +
      draws_of(sam$b_ind_use))
    my_y <- mean(draws_of(sam$vary_ind)[, targets$ind[i], 3] +
      draws_of(sam$b_pop_use))
    text(my_x, my_y, targets$print_name[i])
  }
}

# age 8 is "1", age 9 is "2", etc.
n_ages <- length(unique(d$age_group))

my_pop_means <- apply(draws_of(sam$vary_age)[ , , 2], 2, mean) + mean(draws_of(sam$b_pop_use))
my_pop_HPDI <- apply(draws_of(sam$vary_age)[ , , 2], 2, HPDI) + mean(draws_of(sam$b_pop_use))
plot(my_pop_means, type = "p", ylim = c(-2, 9), xaxt = "n",
  ylab = "reliance on social information", xlab = "age (years)",
  xlim = c(3, 63), las = 1, col = "black", cex = 0.5, pch = 20)
axis(1, at = seq(3, 78, by = 10), labels =  seq(3, 78, by = 10) + 7)
for (i in 1:n_ages) lines(c(i, i), c(my_pop_HPDI[1, i], my_pop_HPDI[2, i]),
  col = "black")
abline(h = mean(draws_of(sam$b_pop_use)), lty = 2, col = "black")

dev.off()

