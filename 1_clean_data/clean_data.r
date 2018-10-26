
if (scaffold) {
  rm(list = ls())
  source("../project_support.r")
}

dir_init("./temp")



print("load game data")

d <- read.csv("./inputs/gogod_database.csv", stringsAsFactors = FALSE)



print("clean game dates")

plusnum <- which(substr(d$DT,1,1) %in% c(0:9)) 
# index values in original database

no.letter <- d$DT[plusnum] #only ones starting with numbers

minusten <- nchar(no.letter) != 10

new <- as.character(no.letter[minusten]) # removes all with ten characters

minusseven <- nchar(new) != 7
newer <- as.character(new[minusseven]) # removes all with seven characters

minusfour <- nchar(newer) != 4
newest <- newer[minusfour] # removes all with four characters

dates <- substr(newest,1,(regexpr("[^0-9,-]",newest) - 1))
# removes all characters beyond the date (from no.letter)

# only problem with dates is first entry - "196" (corrected below)
dates[1] <- 196
newer[minusfour] <- dates
new[minusseven] <- newer
no.letter[minusten] <- new
d$DT[plusnum] <- no.letter

# only problems left are dash-years and 17th c./17/1600

pluslett <- which(regexpr("[[:alpha:],[:punct:],[:blank:]]", d$DT) == 1)
lett <- d$DT[pluslett] # only ones starting with letters or (
lettless <- substr(lett,(regexpr("[[:digit:]]",lett)),nchar(lett)) 
# removes initial word or phrase

lessten <- nchar(lettless) != 10
lettten <- lettless[lessten] # removes all with ten characters

lessseven <- nchar(lettten) != 7
lettseven <- lettten[lessseven] # removes all with seven characters

lessfour <- nchar(lettseven) != 4
lettfour <- lettseven[lessfour] # removes all with four characters

anom <- lettfour[c(1:13, 22)]

lettclean <- substr(lettfour, 1, (regexpr("[^0-9,-]", lettfour) - 1)) # removes all characters beyond date (from lett)

lettclean[c(1:13, 22)] <- anom
lettseven[lessfour] <- lettclean
lettten[lessseven] <- lettseven
lettless[lessten] <- lettten
d$DT[pluslett] <- lettless

# there"s a mistake in "GOGOD CD Archive/Database/1996/1996-04-00a.sgf" - the game is dated to 1995 but it should be 1996 according to the filename (at least, thats what Im going to go with)

this <- which(d[,"filename"] == "./temp/1996-04-00a.sgf")
d$DT[this] <- "1996-04"

# another mistake: the game called "1985/1985-04-30j.sgf" has the date 1984-04-30...
this <- which(d[,"filename"] == "./temp/1985-04-30j.sgf")
d$DT[this] <- "1985-04-30"


# Here"s a list of games with improper formatting:

# "1810-00" ... "167-230744064"
# "1814-00" ... "131-389076992"
# "1913-03-00" ... "160-724597760"
# "1918-09~10" ... "276-740425728"
# "1961-03;04" ... "171-689518592"
# "1968-00-00" ... "174-624742400"
# "1970-00-00" ... "255-449642496"
# "1989-04-00" ... "277-356646912"
# "1996-06~09" ... "223-291176448"
# "1999-02-29" ... "159-619907072"
# "1999-09-00" ... "136-702837248"
# "2001-02-29" ... "136-747127808"
# "2004-12.06" ... "117-660176512"

drop <- which(is.na(d$ID))
d <- d[-drop, ]

drop <- which(is.na(d$DT))
d <- d[-drop, ]

d$DT[which(d$ID == "167-230744064")] <- "1810-01-15"
d$DT[which(d$ID == "131-389076992")] <- "1814-01-15"
d$DT[which(d$ID == "160-724597760")] <- "1913-03-01"
d$DT[which(d$ID == "276-740425728")] <- "1918-09-10"
d$DT[which(d$ID == "171-689518592")] <- "1961-03-04"
d$DT[which(d$ID == "174-624742400")] <- "1968-01-01"
d$DT[which(d$ID == "255-449642496")] <- "1970-01-01"
d$DT[which(d$ID == "277-356646912")] <- "1989-04-01"
d$DT[which(d$ID == "223-291176448")] <- "1996-06-09"
d$DT[which(d$ID == "159-619907072")] <- "1999-02-28"
d$DT[which(d$ID == "136-702837248")] <- "1999-09-01"
d$DT[which(d$ID == "136-747127808")] <- "2001-02-28"
d$DT[which(d$ID == "117-660176512")] <- "2004-12-06"

# my final convention: all dates with unknown months get -07-01.  AL dates with unknown days gets -15.

tar <- which(nchar(d$DT) == 7)
d$DT[tar] <- paste(d$DT[tar], "-15", sep = "")

tar <- which(nchar(d$DT) %in% c(3, 4))
d$DT[tar] <- paste(d$DT[tar], "-07-01", sep = "")

d$DT <- as.Date(d$DT)
drop <- which(is.na(d$DT))
d <- d[-drop, ]

#####
# the database should be organized chronologically!
o <- order(d$DT)
d <- d[o, ]
#####

### Left to do:

# I don"t trust the 664-whatever series of "1650~"
# 17th c./1600 etc.

print("clean results")

RE <- d[,"RE"]

ziless <- which(regexpr("zi",RE)>0) #index value for those with "zi"
nozi <- substr(RE[ziless],1,regexpr("zi",RE[ziless])-2) #gets rid of "zi""s
RE[ziless] <- nozi
parenless <- which(regexpr("[(]",RE)>0) #index value for those with "(" in them.
noparen <- substr(RE[parenless],1,regexpr("[(]",RE[parenless])-2) #gets rid of those with parenthesis after result
RE[parenless] <-noparen
curlyless <- which(regexpr("[{]",RE)>0) #index values for those with curly braces
nocurly <- substr(RE[curlyless],1,regexpr("[{]",RE[curlyless])-2) #gets rid of those with curly braces
RE[curlyless] <-nocurly
leftless <- which(regexpr("[L,l]",RE)>0) #index for those that say "left unfinished"
noleft <- c(1:length(leftless))
noleft[1:length(leftless)] <- "?" #made "left unfinished" into "?"
RE[leftless] <- noleft
lettfree <- which(regexpr("[:alpha:]",RE)>3) #index of those with letters after the result
absentlett <- substr(RE[lettfree],1,regexpr("[[:lower:]]",RE[lettfree])-2) #gets rid of those with letters after result
RE[lettfree] <- absentlett

### Crucial: #######
d[,"RE"] <- RE

d$RE[which(d$RE == "w+R")] <- "W+R"
d$RE[which(d$RE == "w+0.25")] <- "W+0.25"
d$RE[d$filename == "./temp/1960-02-03b.sgf"] <- "V"



print("clean player names (PW and PB)")

PB <- d[,"PB"]
PW <- d[,"PW"]

PB[PB == "Fujisawa Kuranosuke"] <- "Fujisawa Hosai"
PW[PW == "Fujisawa Kuranosuke"] <- "Fujisawa Hosai"
# "Fujisawa Hosai" and "Fujisawa Kuranosuke" are the same person - it appears he suffered at the hands of Go Seigen so much he resigned from the Nihon Ki-in and then came back under a different name.  For the purposes of analysis, we should pick one name and use it....I"ll use "Hosai".

#  "Takagawa Kaku" and "Takagawa Shukaku" are the same person
PB[PB == "Takagawa Kaku"] <- "Takagawa Shukaku"
PW[PW == "Takagawa Kaku"] <- "Takagawa Shukaku"

# Game "147-905162748" says PB is "Yang Yi", but this pro was born in 1984!  I assume that it"s just a different player with the same name...filename 1976-79/1979-01-12a.sgf
PB[d[,"filename"] == "./temp/1979-01-12a.sgf"] <- "Yang Yi (o)"
d[d[,"filename"] == "./temp/1979-01-12a.sgf", "GC"] <- "A pro named Yang Yi was born in 1984 and began
  his games in 1996.  This is obviously not the same person, so I added an '(o)' to his name."

# this is a guess on my part - it can"t be who it originally says (Cho Hye-yeon) because she wasn"t born yet!  it"s game 1970-75/1971-05-01a.sgf
PW[d[,"filename"] == "./temp/1971-05-01a.sgf"] <- "Cho Hun-hyeon"

### crucial ####
d[,"PB"] <- PB
d[,"PW"] <- PW
#################

# Currently have 60350 games



print("drop games with sketchy comments")

# using the game comments, I"ve marked the following games for removal:
# my criteria was: if Fairbarn and Hall say the game is corrupt, or a duplicate,
# or clearly the same as another game but there"s recording differences, etc.

# ugh, it should have started with the year folders...not "GOGOD CD Archive/Database/"

drop <- which(d[,"filename"] == "./temp/1253-02-00a.sgf")
drop <- c(drop, which(d[,"filename"] == "./temp/1561-00-00a.sgf"))
drop <- c(drop, which(d[,"filename"] == "./temp/1566-00-00a.sgf"))
drop <- c(drop, which(d[,"filename"] == "./temp/1620-00-00bf.sgf"))
drop <- c(drop, which(d[,"filename"] == "./temp/1740hcem253.sgf"))
drop <- c(drop, which(d[,"filename"] == "./temp/1740hcem295.sgf"))
drop <- c(drop, which(d[,"filename"] == "./temp/1813-03-05a.sgf"))
drop <- c(drop, which(d[,"filename"] == "./temp/1850-06-04OLD.sgf"))
drop <- c(drop, which(d[,"filename"] == "./temp/1931-03-04u.sgf"))
drop <- c(drop, which(d[,"filename"] == "./temp/1988-05-17e.sgf"))
drop <- c(drop, which(d[,"filename"] == "./temp/1995-03-12g.sgf"))
drop <- c(drop, which(d[,"filename"] == "./temp/1997-03-13d.sgf"))
drop <- c(drop, which(d[,"filename"] == "./temp/2004-10-07fVR.sgf"))

d <- d[-drop,]

# 60337 left, because 13 were removed!

 # [1] "Allegedly the oldest recorded game in Japan. Probably a forgery by Hayashi Genbi."
 # [2] "Allegedly the second oldest game in Japan; but often assumed to be a forgery by Hayashi Genbi - though there is a record in the Sanada-ki that Masayuki was a good player. Rankado Kiwa has date Eiroku 9 with matching cyclic date. 151 is missing from the original; as is 196."
 # [3] "This game may be a forgery. The date is suspect anyway because the era year and cyclic date are said not to match; though they do in the Rankado Kiwa version."
 # [4] "There are some obvious but uncorrectable recording errors"
 # [5] "The same game as 1740HCEM233 (Fan Xiping vs Bian Liyan) except that a6-a4 is added here; and the other game gives a result - W+7.5."
 # [6] "Same as the five-stone game 1740hcem287 by Fan Xiping with different orientation (result there is given as B+4.5 zi)"
 # [7] "Result may be questionable. Fukui"s edition of Inseki"s games gives only 93 moves - up to 95 here with the 66-67 exchange omitted and several changes in move order in the fight on the centre right;but with B+R. He also has date Bunka 10 IV. Araki"s collection gives both versions."
 # [8] "Appears in Hikaru no Go; Vol. 8; Chap. 62. This is the old version of the game which is said to be riddled with mistakes. The latest collected games shows only 157 moves and has some changes."
 # [9] "The source is corrupt even before this point but now becomes unintelligible"
# [10] "Source was corrupt: this is as published; with no attempt at correction"
# [11] "As given but the record is clearly corrupt after about move 130"
# [12] "The source is badly corrupt and illogically says 179 moves were played."
# [13] "This is a variant file of 2004-10-07f because the moves after 250 in two published versions vary after 150. This version appears less correct but is included in the Korean Yearbook."



print("clean the komi data")

komi <- d[,"KM"]
komi[which(komi == "2.5PC[Beijing")] <- 2.5 #switched from "2.5PC[Beijing" to "2.5"
komi[which(komi == "275")] <- "2.75"
komi[which(komi == "5.5;")] <- "5.5"
komi <- as.numeric(komi)

## crucial###
d[,"KM"] <- komi



#######

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



print("prepared predictors based on demographic data")

bio_data <- read.csv("./inputs/PB Biographical Data.csv", stringsAsFactors = FALSE)
bio_data$Birth.Year <- as.numeric(bio_data$Birth.Year)
bio_data$Nationality <- gsub(" ", "", bio_data$Nationality)

d$BN <- bio_data$Nationality[match(d$PB, bio_data$Player.Name)]

d$black_birth_year <- bio_data$Birth.Year[match(d$PB, bio_data$Player.Name)]
d$black_age <- d$year - d$black_birth_year

player_list <- sort(unique(c(d$PB, d$PW)))
n_players <- length(player_list)
d$black_won <- as.numeric(substr(d$RE, 1, 1) == "B")
# note that we are ignoring ties
d$komi <- as.numeric(d$KM) - 5.5 # re-centering on modal komi amount

first_move <- substr(d$move.string, 4, 5)
d$fourfour <- as.numeric(first_move %in% c("pd", "dp", "dd", "pp"))



print("save cleaned dataset to file")

write.csv(d, "./temp/gogod_cleaned.csv", row.names = FALSE)

if (save_output){
  dir_init("./output")
  file.copy("./temp/gogod_cleaned.csv", "./output")
}

if (!save_temp) unlink("./temp", recursive = TRUE)
