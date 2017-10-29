rm(list=ls())

source('./code/project_functions.r')

dir_init('./temp')

start.time <- Sys.time()

d <- read.csv("./inputs/fourfour_regression_table_24m.csv")

colnames(d) <- c("DT", "fourfour", "threefour", "threethree", "first.row", "b.age", "b.win", "b.44", "b.win.44", "pop.44", "pop.win.44", "pop.win", "PB", "BN", "komi", "BR", "black.won") 

# 2-year cutoff
years <- as.numeric(substr(d[,"DT"], 1, 4))
drop <- which(years < 1956)
d <- d[-drop,] 

drop <- which(apply(d, 1, function(z) any(is.na(z))))
d <- d[-drop,]

PB.list <- sort(unique(d$PB))
PB.id.list <- 1:length(PB.list)
d$PB.id <- PB.id.list[match(d$PB, PB.list)]

BR <- as.character(d[,"BR"])

BR[BR %in% c("Gisung", "Insei", "Kisei" , "Meijin", "Mingren")] <- "9d"
BR[is.na(BR)] <- "5d"
BR[BR=="0d"] <- "1d"
d$BR.id <- as.integer(substr(BR, 1, 1))

BN.id <- as.character(d$BN)
BN.id[BN.id=="Chinese"] <- 1
BN.id[BN.id=="Japanese"] <- 2
BN.id[BN.id=="Korean"] <- 3
BN.id[BN.id=="Taiwanese"] <- 4
d$BN.id <- as.integer(BN.id)

# create integer ages starting at 1 for fixed and random effects
b.age.list <- sort(unique(d$b.age))
d$b.age.group <- as.integer(match(d$b.age, b.age.list))

d$b.decade <- floor(d$b.age/10)
d$b.decade[d$b.decade < 1] <- 1
d$b.decade[d$b.decade > 6] <- 6

# convert age from years to decades, and center on the median age of 30 yo
b.age <- d$b.age
b.age <- b.age/10
b.age <- b.age - median(b.age, na.rm=T)
b.age[is.na(b.age)] <- 0
d$b.age <- b.age

d$b.winxb.age <- d$b.win*d$b.age
d$b.44xb.win.44 <- d$b.44*d$b.win.44
d$b.44xb.age <- d$b.44*d$b.age
d$pop.44xpop.win.44 <- d$pop.44*d$pop.win.44

d$pop.44xb.win <- d$pop.44*d$b.win
d$pop.44xb.age <- d$pop.44*d$b.age

d$b.age.sq <- d$b.age^2
d$b.44xb.age.sq <- d$b.44*d$b.age.sq
d$pop.44xb.age.sq <- d$pop.44*d$b.age.sq

d$b.age.thr <- d$b.age^3
d$b.44xb.age.thr <- d$b.44*d$b.age.thr
d$pop.44xb.age.thr <- d$pop.44*d$b.age.thr
d$fourfourxpop.win.44 <- d$fourfour*d$pop.win.44
d$fourfourxb.win.44 <- d$fourfour*d$b.win.44

d$b.44xb.win <- d$b.44 * d$b.win

colnames(d) <- gsub("\\.", "_", colnames(d))

d$bin_total <- 1

write.csv(d, './temp/fourfour_final.csv', row.names=FALSE)

# fit the 24 month model
           
# horizon24 <- glmer2stan(
#    fourfour ~ (1 + b_44 + pop_44 | PB_id) + (0 + b_44 + pop_44 | b_age_group) +
#     b_44 +
#     b_44xb_win_44 +
#     b_44xb_win +
#     b_win_44 +
#     pop_44 +
#     pop_44xpop_win_44 +
#     pop_44xb_win +
#     pop_win_44 +
#     komi,
#    data=d,
#    family="binomial",
#    warmup=1000,
#    iter=6000,
#    calcDIC=TRUE,
#    initmethod="zero"
# ) # maybe write a map2stan version?

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

horizon24 <- stan(file="./inputs/horizon24.stan", data=dat_list, iter = 2000, chains=3)

save(horizon24, file="./temp/horizon24.robj")  

stop.time <- Sys.time()

cat(task.timer("Fit 24-month model."), file="./temp/fitmodels_log.txt")

dir_init('./output')
files <- c('./temp/horizon24.robj', 
    './temp/fitmodels_log.txt',
    './temp/fourfour_final.csv')
file.copy(files, './output')

if(!save_temp) unlink('./temp', recursive=TRUE)