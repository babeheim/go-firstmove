
if (scaffold) {
  rm(list = ls())
  source("../project_support.r")
}

dir_init("./temp")

d <- read.csv("./inputs/fourfour_regression_table_24m.csv")

colnames(d) <- c("DT", "fourfour", "first_row", "b_age", "b_win",
  "b_44", "b_win_44", "pop_44", "pop_win_44", "pop_win", "PB",
  "BN", "komi", "BR", "black_won")

# 2-year cutoff
years <- as.numeric(substr(d[, "DT"], 1, 4))
drop <- which(years < 1956)
d <- d[-drop, ]

drop <- which(apply(d, 1, function(z) any(is.na(z))))
d <- d[-drop, ]

PB_list <- sort(unique(d$PB))
PB_id_list <- 1:length(PB_list)
d$PB_id <- PB_id_list[match(d$PB, PB_list)]

BR <- as.character(d[, "BR"])

BR[BR %in% c("Gisung", "Insei", "Kisei", "Meijin", "Mingren")] <- "9d"
BR[is.na(BR)] <- "5d"
BR[BR == "0d"] <- "1d"
d$BR_id <- as.integer(substr(BR, 1, 1))

BN_id <- as.character(d$BN)
BN_id[BN_id == "Chinese"] <- 1
BN_id[BN_id == "Japanese"] <- 2
BN_id[BN_id == "Korean"] <- 3
BN_id[BN_id == "Taiwanese"] <- 4
d$BN_id <- as.integer(BN_id)

# create integer ages starting at 1 for fixed and random effects
b_age_list <- sort(unique(d$b_age))
d$b_age_group <- as.integer(match(d$b_age, b_age_list))

d$b_decade <- floor(d$b_age / 10)
d$b_decade[d$b_decade < 1] <- 1
d$b_decade[d$b_decade > 6] <- 6

# convert age from years to decades, and center on the median age of 30 yo
b_age <- d$b_age
b_age <- b_age / 10
b_age <- b_age - median(b_age, na.rm = TRUE)
b_age[is.na(b_age)] <- 0
d$b_age <- b_age

d$b_winxb_age <- d$b_win * d$b_age
d$b_44xb_win_44 <- d$b_44 * d$b_win_44
d$b_44xb_age <- d$b_44 * d$b_age
d$pop_44xpop_win_44 <- d$pop_44 * d$pop_win_44

d$pop_44xb_win <- d$pop_44 * d$b_win
d$pop_44xb_age <- d$pop_44 * d$b_age

d$b_age_sq <- d$b_age ^ 2
d$b_44xb_age_sq <- d$b_44 * d$b_age_sq
d$pop_44xb_age_sq <- d$pop_44 * d$b_age_sq

d$b_age_thr <- d$b_age ^ 3
d$b_44xb_age_thr <- d$b_44 * d$b_age_thr
d$pop_44xb_age_thr <- d$pop_44 * d$b_age_thr
d$fourfourxpop_win_44 <- d$fourfour * d$pop_win_44
d$fourfourxb_win_44 <- d$fourfour * d$b_win_44

d$b_44xb_win <- d$b_44 * d$b_win

colnames(d) <- gsub("\\.", "_", colnames(d))

d$bin_total <- 1

write.csv(d, "./temp/fourfour_final.csv", row.names = FALSE)

print("prepped final data table for regression analysis")


# fit the 24 month model

dat_list <- list(
  N = nrow(d),
  fourfour = d$fourfour,
  b_44 = d$b_44,
  pop_44 = d$pop_44,
  PB_id = d$PB_id,
  b_age_group = d$b_age_group,
  b_44xb_win_44 = d$b_44xb_win_44,
  b_44xb_win = d$b_44xb_win,
  b_win_44 = d$b_win_44,
  pop_44xpop_win_44 = d$pop_44xpop_win_44,
  pop_44xb_win = d$pop_44xb_win,
  pop_win_44 = d$pop_win_44,
  komi = d$komi,
  bin_total = d$bin_total,
  N_PB_id = length(unique(d$PB_id)),
  N_b_age_group = length(unique(d$b_age_group))
)

horizon24 <- stan(file = "./stan/horizon24.stan", data = dat_list,
  iter = 2000, chains = 3, cores = 3)

save(horizon24, file = "./temp/horizon24.robj")

dir_init("./output")

files <- c("./temp/horizon24.robj",
    "./temp/fourfour_final.csv")

file.copy(files, "./output")

if (!save_temp) unlink("./temp", recursive = TRUE)
