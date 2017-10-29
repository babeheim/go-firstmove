rm(list=ls())

source('./code/project_functions.r')
source('./code/project_variables.r')

dir_init('./temp')

start.time <- Sys.time()

load("./inputs/horizon24.robj")

d <- read.csv('./inputs/fourfour_final.csv', as.is=TRUE)

## model analysis

p <- extract(horizon24)

beta.post <- as.data.frame(p[1:10])
colnames(beta.post) <- c("Intercept", "b.44", "b.44xb.win.44", "b.44xb.win", "b.win.44", "pop.44", "pop.44xpop.win.44", "pop.44xb.win", "pop.win.44", "komi")

my.names <- c("Intercept", "Personal Fourfour Use Rate ($\\beta$)", 
	"$\\times$ Personal Fourfour Win Rate", 
	"$\\times$ Personal Win Rate",
	"Personal Fourforu Win Rate",
	"Population Fourfour Use Rate ($\\gamma$)",
	"$\\times$ Population Fourfour Win Rate",
	"$\\times$ Personal Win Rate",
	"Population Fourfour Win Rate",
	"Handicap (komi)")
my.means <- sprintf('%.2f', colMeans(beta.post))
my.ses <- sprintf('%.2f', apply(beta.post, 2, sd))

tab1 <- cbind(my.names, my.means, my.ses)

tab1 <- rbind(tab1[2:nrow(tab1),], tab1[1,])

tab1 <- rbind(c("Fixed Effects", "Est.", "S.E."), tab1)
tab1 <- rbind(tab1, c("Varying Effects", "", ""))

playerID.mean <- sprintf('%.2f', mean(p$Sigma_PB_id[,1,1]))
playerID.se <- sprintf('%.2f', sd(p$Sigma_PB_id[,1,1]), 2)

tab1 <- rbind(tab1, c("Player$_j$", playerID.mean, playerID.se))

playerIDxpersonal.mean <- sprintf('%.2f', mean(p$Sigma_PB_id[,2,2]), 2)
playerIDxpersonal.se <- sprintf('%.2f', sd(p$Sigma_PB_id[,2,2]), 2)

tab1 <- rbind(tab1, c("$\\times$ Personal Fourfour Use Rate", playerIDxpersonal.mean, playerIDxpersonal.se))

playerIDxpop.mean <- sprintf('%.2f', mean(p$Sigma_PB_id[,3,3]), 2)
playerIDxpop.se <- sprintf('%.2f', sd(p$Sigma_PB_id[,3,3]), 2)

tab1 <- rbind(tab1, c("$\\times$ Population Fourfour Use Rate", playerIDxpop.mean, playerIDxpop.se))

agexpersonal.mean <- sprintf('%.2f', mean(p$Sigma_b_age_group[,1,1]), 2)
agexpersonal.se <- sprintf('%.2f', sd(p$Sigma_b_age_group[,1,1]), 2)

tab1 <- rbind(tab1, c("Age$_k$ $\\times$ Personal Fourfour Use Rate", agexpersonal.mean, agexpersonal.se))

agexpop.mean <- sprintf('%.2f', mean(p$Sigma_b_age_group[,2,2]), 2)
agexpop.se <- sprintf('%.2f', sd(p$Sigma_b_age_group[,2,2]), 2)

tab1 <- rbind(tab1, c("Age$_k$ $\\times$ Population Fourfour Use Rate", agexpop.mean, agexpop.se))

output <- texttab(tab1, hlines=c(1, 11, 12, 17))

writeLines(output, './temp/table1.txt')

# players averaged by nationality...

JP.cols <- sort(unique(d$PB_id[d$BN=="Japanese"]))
CH.cols <- sort(unique(d$PB_id[d$BN=="Chinese"]))
SK.cols <- sort(unique(d$PB_id[d$BN=="Korean"]))
TW.cols <- sort(unique(d$PB_id[d$BN=="Taiwanese"]))

japanese.beta.intercepts <- colMeans(p$vary_PB_id[,JP.cols,2])
japanese.gamma.intercepts <- colMeans(p$vary_PB_id[,JP.cols,3])

chinese.beta.intercepts <- colMeans(p$vary_PB_id[,CH.cols,2])
chinese.gamma.intercepts <- colMeans(p$vary_PB_id[,CH.cols,3])

korean.beta.intercepts <- colMeans(p$vary_PB_id[,SK.cols,2])
korean.gamma.intercepts <- colMeans(p$vary_PB_id[,SK.cols,3])

taiwanese.beta.intercepts <- colMeans(p$vary_PB_id[,TW.cols,2])
taiwanese.gamma.intercepts <- colMeans(p$vary_PB_id[,TW.cols,3])

ja.n <- length(japanese.beta.intercepts)
ja.beta.mean <- sprintf('%.2f', mean(japanese.beta.intercepts), 2)
ja.beta.se <- sprintf('%.2f', sd(japanese.beta.intercepts)/sqrt(ja.n), 2)
ja.gamma.mean <- sprintf('%.2f', mean(japanese.gamma.intercepts), 2)
ja.gamma.se <- sprintf('%.2f', sd(japanese.gamma.intercepts)/sqrt(ja.n), 2)

ch.n <- length(chinese.beta.intercepts)
ch.beta.mean <- sprintf('%.2f', mean(chinese.beta.intercepts), 2)
ch.beta.se <- sprintf('%.2f', sd(chinese.beta.intercepts)/sqrt(ch.n), 2)
ch.gamma.mean <- sprintf('%.2f', mean(chinese.gamma.intercepts), 2)
ch.gamma.se <- sprintf('%.2f', sd(chinese.gamma.intercepts)/sqrt(ch.n), 2)

ko.n <- length(korean.beta.intercepts)
ko.beta.mean <- sprintf('%.2f', mean(korean.beta.intercepts), 2)
ko.beta.se <- sprintf('%.2f', sd(korean.beta.intercepts)/sqrt(ko.n), 2)
ko.gamma.mean <- sprintf('%.2f', mean(korean.gamma.intercepts), 2)
ko.gamma.se <- sprintf('%.2f', sd(korean.gamma.intercepts)/sqrt(ko.n), 2)

ta.n <- length(taiwanese.beta.intercepts)
ta.beta.mean <- sprintf('%.2f', mean(taiwanese.beta.intercepts), 2)
ta.beta.se <- sprintf('%.2f', sd(taiwanese.beta.intercepts)/sqrt(ta.n), 2)
ta.gamma.mean <- sprintf('%.2f', mean(taiwanese.gamma.intercepts), 2)
ta.gamma.se <- sprintf('%.2f', sd(taiwanese.gamma.intercepts)/sqrt(ta.n), 2)

n <- c(ch.n, ja.n, ko.n, ta.n)
beta.mean <- c(ch.beta.mean, ja.beta.mean, ko.beta.mean, ta.beta.mean)
beta.se <- c(ch.beta.se, ja.beta.se, ko.beta.se, ta.beta.se)
gamma.mean <- c(ch.gamma.mean, ja.gamma.mean, ko.gamma.mean, ta.gamma.mean)
gamma.se <- c(ch.gamma.se, ja.gamma.se, ko.gamma.se, ta.gamma.se)
nationality <- c("Chinese", "Japanese", "South Korean", "Taiwanese")

tab2 <- cbind(nationality, n, beta.mean, beta.se, gamma.mean, gamma.se)

tab2 <- rbind(c("Nationality", "$n$", "$\\beta_j$", "(S.E.)", "$\\gamma_j$", "(S.E.)"), tab2)

output <- texttab(tab2, alignment="{lrrrrr}", hlines=c(1, 5))

writeLines(output, './temp/table2.txt')



pdf("./temp/FigPr44xPop_COL.pdf", width=3.5, height=3.5)

ci.weight <- 0.6

color.high <- "orange"
color.med <- "lightblue2"
color.low <- "royalblue"

par(mar=c(5.1, 4.1, 0, 0))

par(family="Times")
# windowsFonts(Times=windowsFont("TT Times New Roman"))

xs <- seq(-0.5, 0.5, length=100)

	# pane 2
plot(c(-0.5,0.5), c(0,1), col="white", xlab="recent population 44 use", ylab="pr(use 44)", xaxt="n", las=1, ylim=c(-0.1,1.1))
axis(1, at=c(-0.5, -0.25, 0, 0.25, 0.5), labels=c(-0.5, -0.25, 0, 0.25, 0.5)+0.5)

my.col=color.high
per.use = 0
perf <- -0.1
my.mean <- NA
lb <- NA
ub <- NA
for(i in 1:length(xs)){
	x <- xs[i]
	est <- as.numeric(beta.post$Intercept + beta.post$pop.44*x + beta.post$b.44*per.use + beta.post$pop.win.44*perf + beta.post$pop.44xpop.win.44*x*perf)
	my.mean[i] <- logistic(mean(est))
	lb[i] <- logistic(HPDI(est)[1])
	ub[i] <- logistic(HPDI(est)[2])
}
points(my.mean ~ xs, type="l", lty=2)
polygon(c(xs, rev(xs)), c(ub, rev(lb)), border=NA, col=col.alpha(my.col, ci.weight))

text(0.13, 0.38, "-10% perf.", col=my.col)

my.col=color.med
per.use = 0
perf <- 0
my.mean <- NA
lb <- NA
ub <- NA
for(i in 1:length(xs)){
	x <- xs[i]
	est <- as.numeric(beta.post$Intercept + beta.post$pop.44*x + beta.post$b.44*per.use + beta.post$pop.win.44*perf + beta.post$pop.44xpop.win.44*x*perf)
	my.mean[i] <- logistic(mean(est))
	lb[i] <- logistic(HPDI(est)[1])
	ub[i] <- logistic(HPDI(est)[2])
}
points(my.mean ~ xs, type="l", lty=2)
polygon(c(xs, rev(xs)), c(ub, rev(lb)), border=NA, col=col.alpha(my.col, ci.weight))


my.col=color.low
per.use = 0
perf <- +0.1
my.mean <- NA
lb <- NA
ub <- NA
for(i in 1:length(xs)){
	x <- xs[i]
	est <- as.numeric(beta.post$Intercept + beta.post$pop.44*x + beta.post$b.44*per.use + beta.post$pop.win.44*perf + beta.post$pop.44xpop.win.44*x*perf)
	my.mean[i] <- logistic(mean(est))
	lb[i] <- logistic(HPDI(est)[1])
	ub[i] <- logistic(HPDI(est)[2])
}
points(my.mean ~ xs, type="l", lty=2)
polygon(c(xs, rev(xs)), c(ub, rev(lb)), border=NA, col=col.alpha(my.col, ci.weight))

text(-0.05, 0.74, "+10% perf.", col=my.col)

d$fourfour <- as.numeric(d$fourfour)

these <- sample(1:nrow(d), 2000)
points(d$pop_44[these], jitter(d$fourfour[these], factor=0.5), col=col.alpha("gray", 0.5), pch=20, cex=0.5)

dev.off()



# effect of player ID

pdf("./temp/FigPlayerInt.pdf", height=7, width=7)

my.col <- heat.colors(300)

par(mfrow=c(3,1))
par(mar=c(3.1, 4.1, 1.1, 2.1))
PB.int <- p$vary_PB_id[,,1]
PBxb.44 <- p$vary_PB_id[,,2]
PBxpop.44 <- p$vary_PB_id[,,3]

PB.int <- apply(PB.int, 2, function(z) z + p$Intercept)
my.means <-  logistic(apply(PB.int, 2, mean))
o <- order(my.means)
my.means <- my.means[o]
n.pl <- length(my.means)
my.HPDI <- logistic(apply(PB.int, 2, HPDI))
my.HPDI <- my.HPDI[,o]
plot(1:n.pl, my.means, ylim=c(0,1), xaxt="n", las=1, ylab="Pr(use 44)", xlab="player", pch=20, cex=0.5, frame.plot=F)
for(i in 1:n.pl){
	lines(c(i,i), c(my.HPDI[1,i], my.HPDI[2,i]), col=my.col[i])
}
points(1:n.pl, my.means, pch=20)
abline(h=logistic(mean(p$Intercept)), col="gray", lty=2)
abline(h=logistic(HPDI(as.numeric(p$Intercept))), col="gray")

PBxb.44 <- apply(PBxb.44, 2, function(z) z + p$beta_b_44)
my.b.means <- apply(PBxb.44, 2, mean)
	o <- order(my.means)
my.b.means <- my.b.means[o]
my.b.HPDI <- apply(PBxb.44, 2, HPDI)
my.b.HPDI <- my.b.HPDI[,o]

plot(1:n.pl, my.b.means, ylim=c(min(my.b.HPDI[1,]), max(my.b.HPDI[2,])), xaxt="n", las=1, ylab="beta + beta_j", xlab="player", pch=20, cex=0.5, frame.plot=F)
for(i in 1:n.pl){
	lines(c(i,i), c(my.b.HPDI[1,i], my.b.HPDI[2,i]), col=my.col[i])
}
abline(h=(mean(p$beta_b_44)), col="gray", lty=2)
abline(h=(HPDI(as.numeric(p$beta_b_44))), col="gray")
points(1:n.pl, my.b.means, pch=20)
abline(h=0)

PBxpop.44 <- apply(PBxpop.44, 2, function(z) z + p$beta_pop_44)
my.pop.means <- apply(PBxpop.44, 2, mean)
o <- order(my.pop.means)
my.pop.means <- my.pop.means[o]
my.pop.HPDI <- apply(PBxpop.44, 2, HPDI)
my.pop.HPDI <- my.pop.HPDI[,o]

plot(1:n.pl, my.pop.means, ylim=c(min(my.pop.HPDI[1,]), max(my.pop.HPDI[2,])), xaxt="n", las=1, ylab="gamma + gamma_j", xlab="player", pch=20, cex=0.5, frame.plot=F)
for(i in 1:n.pl){
	lines(c(i,i), c(my.pop.HPDI[1,i], my.pop.HPDI[2,i]), col=my.col[i])
}
abline(h=(mean(p$beta_pop_44)), col="gray", lty=2)
abline(h=(HPDI(as.numeric(p$beta_pop_44))), col="gray")
points(1:n.pl, my.pop.means, pch=20)
abline(h=0)

dev.off()

pdf("./temp/FigPlayerAgeBetaGamma_allplayers.pdf", width=3.75, height=3.28)
	
plot(c(-4, 10), c(-10, 16), col="white", pch=20, xlab="reliance on individual information", ylab="reliance on social information", las=1)

thin <- sample(1:5000, 1000)

points(p$beta_b_44[thin], p$beta_pop_44[thin], pch=20, col=col.alpha("black", 0.3))

abline(h=0, lty=2, col=col.alpha("black", 0.6))	
abline(v=0, lty=2, col=col.alpha("black", 0.6))	
abline(0,1, lty=1, col=gray(0.3))

player.beta.effects <- colMeans(p$vary_PB_id[,,2]) + mean(p$beta_b_44)
player.gamma.effects <- colMeans(p$vary_PB_id[,,3]) + mean(p$beta_pop_44)
points(player.beta.effects, player.gamma.effects)

targets <- c("Peng Quan", "Kato Atsushi", "Takemiya Masaki", "Hashimoto Shoji", "Yi Seong-chae", "Cho Hun-hyeon")
target.ids <- unique(d$PB_id[d$PB %in% targets])

points(player.beta.effects[target.ids], player.gamma.effects[target.ids], pch=20, col='red')

dev.off()

library(RColorBrewer)

pdf("./temp/FigPlayerAgeBetaGamma_COL.pdf", width=7.5, height=3.28)
	
par(mfrow=c(1,2))
par(mar=c(5.1, 4.1, 0, 2.1))	
par(family="Times")
	
my.cols <- brewer.pal(6, "Spectral")

set.seed(1000)
thin <- sample(1:3000, 1000) 

plot(c(-4, 10), c(-10, 16), col="white", pch=20, xlab="reliance on individual information", ylab="reliance on social information", las=1)

target.id.list <- integer(0)
target.name.list <- character(0)

target <- "Peng Quan"
target.col <- my.cols[1]
target.id <- unique(d$PB_id[d$PB==target])
i <- as.numeric(target.id)
target.name.list <- c(target.name.list, target)
target.id.list <- c(target.id.list, target.id)
points(p$vary_PB_id[thin,i,2]+mean(p$beta_b_44[thin]), p$vary_PB_id[thin,i,3]+mean(p$beta_pop_44[thin]), col=col.alpha(target.col, 0.3), pch=20)

target <- "Kato Atsushi"
target.col <- my.cols[2]
target.id <- unique(d$PB_id[d$PB==target])
i <- target.id
target.name.list <- c(target.name.list, target)
target.id.list <- c(target.id.list, target.id)
points(p$vary_PB_id[thin,i,2]+mean(p$beta_b_44[thin]), p$vary_PB_id[thin,i,3]+mean(p$beta_pop_44[thin]), col=col.alpha(target.col, 0.3), pch=20)

target <- "Takemiya Masaki"
target.col <- my.cols[3]
target.id <- unique(d$PB_id[d$PB==target])
i <- target.id
target.name.list <- c(target.name.list, target)
target.id.list <- c(target.id.list, target.id)
points(p$vary_PB_id[thin,i,2]+mean(p$beta_b_44[thin]), p$vary_PB_id[thin,i,3]+mean(p$beta_pop_44[thin]), col=col.alpha(target.col, 0.3), pch=20)

target <- "Hashimoto Shoji"
target.col <- my.cols[4]
target.id <- unique(d$PB_id[d$PB==target])
i <- target.id
target.name.list <- c(target.name.list, target)
target.id.list <- c(target.id.list, target.id)
points(p$vary_PB_id[thin,i,2]+mean(p$beta_b_44[thin]), p$vary_PB_id[thin,i,3]+mean(p$beta_pop_44[thin]), col=col.alpha(target.col, 0.3), pch=20)

target <- "Yi Se-tol"
target.col <- my.cols[5]
target.id <- unique(d$PB_id[d$PB==target])
i <- target.id
target.name.list <- c(target.name.list, target)
target.id.list <- c(target.id.list, target.id)
points(p$vary_PB_id[thin,i,2]+mean(p$beta_b_44[thin]), p$vary_PB_id[thin,i,3]+mean(p$beta_pop_44[thin]), col=col.alpha(target.col, 0.3), pch=20)

target <- "Cho Hun-hyeon"
target.col <- my.cols[6]
target.id <- unique(d$PB_id[d$PB==target])
i <- target.id
target.name.list <- c(target.name.list, target)
target.id.list <- c(target.id.list, target.id)
points(p$vary_PB_id[thin,i,2]+mean(p$beta_b_44[thin]), p$vary_PB_id[thin,i,3]+mean(p$beta_pop_44[thin]), col=col.alpha(target.col, 0.3), pch=20)

target.name.list[target.name.list=="Takemiya Masaki"] <- "Takemiya\nMasaki"
target.name.list[target.name.list=="Yi Se-tol"] <- "Lee\nSedol"
target.name.list[target.name.list=="Cho Hun-hyeon"] <- "   Cho\n    Hunhyun"
target.name.list[target.name.list=="Hashimoto Shoji"] <- "   Hashimoto\n Shoji"
target.name.list[target.name.list=="Kato Atsushi"] <- "Kato\nAtsushi"
target.name.list[target.name.list=="Peng Quan"] <- "Peng\nQuan"

points(p$beta_b_44[thin], p$beta_pop_44[thin], pch=20, col=col.alpha("black", 0.3))

abline(h=0, lty=2, col=col.alpha("black", 0.6))	
abline(v=0, lty=2, col=col.alpha("black", 0.6))	
abline(0,1, lty=1, col=gray(0.3))

for(i in 1:length(target.id.list)){

	text(mean(p$vary_PB_id[,target.id.list[i],2]+ p$beta_b_44), mean(p$vary_PB_id[,target.id.list[i],3]+p$beta_pop_44), target.name.list[i])
}

# age 8 is "1", age 9 is "2", etc.
n.ages <- length(unique(d$b_age_group))
	
my.pop.means <- apply(p$vary_b_age_group[,,2], 2, mean)+mean(p$beta_pop_44)
my.pop.HPDI <- apply(p$vary_b_age_group[,,2], 2, HPDI)+mean(p$beta_pop_44)
plot(my.pop.means, type="p", ylim=c(-2,9), xaxt="n", ylab="reliance on social information", xlab="age (years)", xlim=c(3, 63), las=1, col="black", cex=0.5, pch=20)
axis(1, at=seq(3, 78, by=10), labels= seq(3, 78, by=10)+7)
for(i in 1:n.ages) lines(c(i,i), c(my.pop.HPDI[1,i],  my.pop.HPDI[2,i]), col="black")	
abline(h=mean(p$beta_pop_44), lty=2, col="black")
# abline(h=HPDI(as.numeric(p$beta_pop_44)), lty=1, col="gray")
	
dev.off()

stop.time <- Sys.time()
cat(task.timer("Create tables and figures."), file="./temp/describemodels_log.txt")

dir_init('./output')
files <- list.files('./temp', full.names=TRUE)
file.copy(files, './output')

if(!save_temp) unlink('./temp', recursive=TRUE)