
if (scaffold) {
  rm(list = ls())
  source("../project_support.r")
}

dir_init("./temp")



print("load game data")

# i can probably do more to show how we constructed this file right here...

d <- read.csv("./inputs/gogod_gamedata_c.csv", stringsAsFactors = FALSE)
# 59851 games in initial "clean" set



print("exclude games outside scope of analysis")

# drop games before 1954
d$year <- as.numeric(substr(d$DT, 1, 4))
drop <- which(d$year < 1954) # 8642 games to drop
d <- d[-drop, ] # 51203 games remaining

# drop handicapped games
drop <- which(!is.na(d$HA)) # 1154 games to drop
d <- d[-drop, ]  # 50049 games remaning

# drop games with inappropriate first move formats
drop <- which(substr(d$move.string, 1, 2) != ";B") # 17 games to drop
d <- d[-drop, ] # 50038 games remaining

# drop this one bad game; first move is a pass, comments imply invalid
drop <- which(d$filename == "./temp/1998-04-21a.sgf")
d <- d[-drop, ]

# drop games with unusual results
drop <- which(substr(d$RE, 1, 1) %in% c("?", "U")) # 38 games to drop
d <- d[-drop, ] # 49999 games remaining

# drop games played by ranked amateurs
drop <- sort(unique(c(grep("ama|Ama", d$BR), grep("ama|Ama", d$WR)))) # 1629 games to drop
d <- d[-drop, ] # 48370 games remaining

# drop a game with unknown player
drop <- which(d$PW == "?")
d <- d[-drop, ]


print("save cleaned dataset to file")

write.csv(d, "./temp/gogod_cleaned.csv", row.names = FALSE)

if (save_output){
  dir_init("./output")
  file.copy("./temp/gogod_cleaned.csv", "./output")
}

if (!save_temp) unlink("./temp", recursive = TRUE)
