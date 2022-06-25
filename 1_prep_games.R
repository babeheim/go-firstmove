
if (scaffold) {
  rm(list = ls())
  source("project_support.R")
}

# this script requires access to the 2009 GoGoD CD, here stored as a zip file
have_raw_dataset <- file.exists("raw_data/GoGoD CD 2009.zip")
if (have_raw_dataset) source("prep_raw_data.R")
# creates raw_data/gogod_database.csv

d <- read.csv("raw_data/gogod_database.csv")

stopifnot(nrow(d) == 60350)


cat("clean komi\n")

d$KM[which(d$KM == "2.5PC[Beijing")] <- 2.5 
d$KM[which(d$KM == "275")] <- "2.75"
d$KM[which(d$KM == "5.5;")] <- "5.5"
d$KM[which(d$KM == "?")] <- NA
d$KM <- as.numeric(d$KM)


cat("clean player names\n")

d$PB[which(d$PB == "Fujisawa Kuranosuke")] <- "Fujisawa Hosai"
d$PW[which(d$PW == "Fujisawa Kuranosuke")] <- "Fujisawa Hosai"
# "Fujisawa Hosai" and "Fujisawa Kuranosuke" are the same person - it appears he suffered at the hands of Go Seigen so much he resigned from the Nihon Ki-in and then came back under a different name.  For the purposes of analysis, we should pick one name and use it....I"ll use "Hosai".


#  "Takagawa Kaku" and "Takagawa Shukaku" are the same person
d$PB[which(d$PB == "Takagawa Kaku")] <- "Takagawa Shukaku"
d$PW[which(d$PW == "Takagawa Kaku")] <- "Takagawa Shukaku"

tar <- which(d$date == "1979-01-12" & d$PB == "Yang Yi")
d$PB[tar] <- "Yang Yi (o)"

# A pro named Yang Yi was born in 1984 and began his games in 1996. This is obviously not the same person, so I added an '(o)' to his name.

tar <- which(d$date == "1971-05-01" & d$PW == "Cho Hye-yeon")
d$PW[tar] <- "Cho Hun-hyeon"

# this is a guess on my part - it can"t be who it originally says (Cho Hye-yeon) because she wasn"t born yet!  it"s game 1970-75/1971-05-01a.sgf


cat("clean dates\n")

d$DT <- gsub("^Published ", "", d$DT)
d$DT <- gsub("^Publihsed ", "", d$DT)
d$DT <- gsub("^published ", "", d$DT)
d$DT <- gsub("^Publsihed ", "", d$DT)
d$DT <- gsub("^Recorded ", "", d$DT)
d$DT <- gsub("^Broadcast ", "", d$DT)
d$DT <- gsub("^Broadcast", "", d$DT)

d$DT[which(d$DT == "Spring 1960")] <- "1960-04-15"
d$DT[which(d$DT == "Spring 1994")] <- "1960-04-15"
d$DT[which(d$DT == "Summer 1994")] <- "1960-06-15"
d$DT[which(d$DT == "on 1981-01-04,11")] <- "1981-01-04"

d$year <- as.numeric(substr(d$DT, 1, 4))

d$date <- NA

tar <- which(!is.na(d$year)) # warnings
d$date[tar] <- d$DT[tar]
d$date <- substr(d$date, 1, 10)

d$date <- gsub("~$", "", d$date)
d$date <- gsub("\\+$", "", d$date)
d$date <- gsub("\\?$", "", d$date)
d$date <- gsub(" \\(\\?\\)$", "", d$date)

d$date[d$date == "196"] <- "0196-07-01"
d$date[d$date == "1670s"] <- "1675-07-01"
d$date[d$date == "1640~1650"] <- "1655-07-01"
d$date[d$date == "2014-11-8"] <- "2014-11-08"

add <- which(nchar(d$date) == 4)
d$date[add] <- paste0(d$date[add], "-07-01")

add <- which(nchar(d$date) == 7)
d$date[add] <- paste0(d$date[add], "-15")

add <- grep("\\d{4}\\s", d$date)
d$date[add] <- paste0(substr(d$date[add], 1, 4), "-07-01")

tar <- grep("\\d{4}-\\d{2}\\s", d$date)
d$date[tar] <- paste0(substr(d$date[tar], 1, 7), "-15")
tar <- which(substr(d$date, 8, 8) == "~")
d$date[tar] <- paste0(substr(d$date[tar], 1, 7), "-15")

d$date[which(d$date == "1950, end")] <- "1950-12-01"
d$date[which(d$date == "1932-12,19")] <- "1932-12-19"
d$date[which(d$date == "2001-02-29")] <- "2001-02-28"
d$date[which(d$date == "1999-02-29")] <- "1999-02-28"

d$date[which(d$date == "1968-00-00")] <- "1968-07-01"
d$date[which(d$date == "1970-00-00")] <- "1970-07-01"
d$date[which(d$date == "1989-04-00")] <- "1989-04-15"
d$date[which(d$date == "1989-04-00")] <- "1989-04-15"
d$date[which(d$date == "1999-09-00")] <- "1999-09-15"
d$date[which(d$date == "2005, Spri")] <- "2005-04-15"

d$DT <- as.Date(d$date)

cat("drop games outside our target sample\n")
drop <- which(is.na(d$DT))
stopifnot(length(drop) == 925) # 2% bad
d <- d[-drop,]

# drop games before 1954
drop <- which(as.numeric(d$year) < 1954)
d <- d[-drop,]

# drop games with unusual results
drop <- which(substr(d$RE, 1, 1) %in% c("?", "U"))
d <- d[-drop,]

# drop handicapped games
drop <- which(!is.na(d$HA))
d <- d[-drop, ]




cat("construct additional variables for analysis\n")

d$black_won <- substr(d$RE, 1, 1) == "B"
d$komi <- as.numeric(d$KM) - 5.5 # re-centering on modal komi amount
d$fourfour <- as.numeric(d$m1 %in% c("pd", "dp", "dd", "pp"))

stopifnot(nrow(d) == 49789)




print("add player-specific predictors")

players <- read.csv("./raw_data/players.csv", stringsAsFactors = FALSE)

link <- match(d$PB, players$Player.Name)
d$BN <- players$Nationality[link]
d$black_birth_year <- players$birth_year[link]
d$black_age <- d$year - d$black_birth_year



print("reorder games by date")
o <- order(d$DT)
d <- d[o,]


# checks

stopifnot(all(c("PB", "BR", "DT", "year", "KM", "RE", "HA", "GC", "filename", "BN", "black_age", "black_won", "komi", "fourfour") %in% colnames(d)))

stopifnot(nrow(d) == 49789)

stopifnot(mean(is.na(d$black_age)) < 0.31)
stopifnot(!any(is.na(as.Date(d$DT))))
stopifnot(!any(is.na(d$fourfour)))
stopifnot(min(d$year) == 1954)
stopifnot(max(d$year) == 2009)

print("save cleaned dataset to file")

write.csv(d, "games.csv", row.names = FALSE)
