rm(list=ls())

source('./code/project_functions.r')
source('./code/project_variables.r')

dir_init('./temp')

start.time <- Sys.time()

#########################
#### Final Data Cleaning ####
#########################

gogod <- read.csv("./inputs/gogod_gamedata_c.csv", as.is=TRUE) # 60337 x 18

years <- as.numeric(substr(gogod[,"DT"], 1, 4))
keep <- which(years >= 1954)

gogod <- gogod[keep,] # 51203

drop <- which(!is.na(gogod[,"HA"]))
gogod <- gogod[-drop,]  # 49818 games...well now 50049

# drop games with inappropriate first move formats:
drop <- which(substr(gogod[,"move.string"], 1,2)!=";B")

gogod <- gogod[-drop,]

# first move is a pass: # game 373-453887744, file name "1998/1998-04-21a.sgf"
drop <- which(gogod[,"filename"]=="./temp/1998-04-21a.sgf")  # fuck, now it's 373-454033408 ??
# from the game comments, it sounds like this game was some kind of novelty rules with playing cards - DROP
gogod <- gogod[-drop,]

# fix RE
gogod[which(gogod[,"RE"]=="w+R"), "RE"] <- "W+R"
gogod[which(gogod[,"RE"]=="w+0.25"), "RE"] <- "W+0.25"
gogod[gogod[,"filename"]=="./temp/1960-02-03b.sgf", "RE"] <- "V" # "087-567214713", filed under "1960-69/1960-02-03b.sgf"...the game was declared void after one player (Kitani) was under doctor's orders to stop...it's not "?" because there was no winner...it was void! ...dammit man! the hashes are different...
RE.data <- substr(gogod[,"RE"], 1, 1)
drop <- which( RE.data == "?" | RE.data == "U")  

gogod <- gogod[-drop,]

# drop games with named amateurs?
BR <- gogod[,"BR"]
WR <- gogod[,"WR"]
drop <- sort(unique(c(grep("ama", BR), grep("Ama", BR), grep("ama", WR), grep("Ama", WR))))

gogod <- gogod[-drop,]

# drop all games by black players who have less than 50 games to their names

pb.data <- gogod[,"PB"]
pw.data <- gogod[,"PW"]

pb.counts <- sort(table(pb.data), decreasing=TRUE)
pb.counts <- pb.counts[pb.counts > 49]
pb.list.good <- names(pb.counts)

drop <- which(!gogod[,"PB"] %in% pb.list.good) 
gogod <- gogod[-drop,]
# 34379

drop <- which(gogod[,"PW"]=="?")
gogod <- gogod[-drop,]

# now add in Black's age and nationality
bio.data <- read.csv("./inputs/PB Biographical Data.csv", as.is=TRUE)
drop <- which(bio.data$Birth.Year=="??" | bio.data$Player.Name=="Kikuchi Yasuro")   # drop kikuchi yasuro because he's a super-strong amateur but not a pro...you know what, fuck it.  I'll have NA's, so what?  
bio.data <- bio.data[-drop,]
bio.data$Birth.Year <- as.numeric(bio.data$Birth.Year)
bio.data$Nationality <- gsub(" ", "", bio.data$Nationality)

gogod$BA <- as.numeric(substr(gogod$DT, 1, 4)) - bio.data$Birth.Year[match(gogod$PB, bio.data$Player.Name)]
gogod$BN <- bio.data$Nationality[match(gogod$PB, bio.data$Player.Name)]

# build final regression table

period.length <- 2

DT <- gogod[,"DT"]
years <- as.numeric(substr(gogod[,"DT"], 1, 4))
n.games <- nrow(gogod)
game.index <- 1:n.games
PB <- as.character(gogod[,"PB"])
PW <- as.character(gogod[,"PW"])
BN <- gogod[,"BN"]
BR <- gogod[,"BR"]
b.age <- gogod[,"BA"]
player.list <- sort(unique(c(PB, PW)))
n.players <- length(player.list)
black.won <- as.numeric(substr(gogod[,"RE"], 1, 1)=="B")
white.won <- as.numeric(substr(gogod[,"RE"], 1, 1)=="W")
## what about ties? ignore them, I guess...no one won!
komi <- as.numeric(gogod[,"KM"])
komi <- komi - modal(komi)


# ok, let's get those first moves

raw.hits <- substr(gogod[,"move.string"], 4, 5)
trans.hits <- sapply_pb(raw.hits, function(z) mirror.mirror(z)[1])
fourfour <- as.numeric(trans.hits=="pd")
threefour <- as.numeric(trans.hits=="qd")
threethree <- as.numeric(trans.hits=="qc")
fourfive <- as.numeric(trans.hits=="pe")
threefive <- as.numeric(trans.hits=="qe")

dates <- as.Date(gogod[,"DT"])
dates.char <- as.character(dates)

dates.1p <- dates - (365)*period.length
dates.1p <- as.Date(dates.1p)

first.row <- NA
date.pointer <- 1
for(i in 1:n.games){
	while(dates.1p[i] > dates[date.pointer]) date.pointer <- date.pointer + 1
	first.row[i] <- date.pointer
}


### ok, using the "valid range" calculate period-specific predictors

# calculate how many games the two players have had together where Black is Black, and the win rate of Black

# how about an 0/1 indicator: the last time they saw the 44 as a white player, did they lose?

b.use.cfrq.1p <- NA
b.win.cfrq.1p <- NA
b.use.win.cfrq.1p <- NA
use.cfrq.1p <- NA
use.win.cfrq.1p <- NA
pop.win.cfrq.1p <- NA

for(i in 1:n.games){

	valid.range <- game.index %in% first.row[i]:(i-1)
	
	use.cfrq.1p[i] <- mean(fourfour[valid.range]) - 0.5
	use.win.cfrq.1p[i] <- mean(black.won[valid.range & fourfour]) - mean(black.won[valid.range & !fourfour])	  # population use win is relative to Threefour, not to average....does this difference matter? I'll have to change this and find out...
	pop.win.cfrq.1p[i] <- mean(black.won[valid.range])
		
	focal.player <- PB[i]
	focal.as.black <- PB==focal.player
	focal.as.white <- PW==focal.player

	b.use.cfrq.1p[i] <- mean(fourfour[focal.as.black & valid.range]) - 0.5
    b.win.cfrq.1p[i] <- mean(black.won[focal.as.black & valid.range]) - mean(black.won[valid.range])
	b.use.win.cfrq.1p[i] <- mean(black.won[focal.as.black & fourfour & valid.range]) - mean(black.won[focal.as.black & valid.range]) # personal use win rate is relative to personal average
	
	# if(i %% 50 == 0) print(i)
		
}
	
regression.table <- cbind(

	DT,

	fourfour,
	threefour,
	threethree,
	
	first.row, 

	b.age,
	b.win.cfrq.1p,
	b.use.cfrq.1p,
	b.use.win.cfrq.1p,

	use.cfrq.1p,
	use.win.cfrq.1p,
	pop.win.cfrq.1p,

	PB,
	BN,
	komi,
	BR,
	black.won
	

)

write.csv(regression.table, "./temp/fourfour_regression_table_24m.csv", row.names=FALSE) # 34406 x 17

stop.time <- Sys.time()
cat(task.timer("Build regression tables."), file="./temp/buildregressiontable_log.txt")

dir_init('./output')
files <- c('./temp/fourfour_regression_table_24m.csv',
	'./temp/buildregressiontable_log.txt')
file.copy(files, './output')

if(!save_temp) unlink('./temp', recursive=TRUE)