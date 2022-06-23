
if (scaffold) {
  rm(list = ls())
  source("project_support.R")
}

print("extract raw data")

have_raw_dataset <- file.exists("raw_data/GoGoD CD 2009.zip")

have_raw_dataset <- FALSE

if (have_raw_dataset) {

  # this script requires access to the 2009 GoGoD CD, here stored as a zip file

  dir_init("./temp")

  # database from 2009
  unzip("./raw_data/GoGoD CD 2009.zip", exdir = "./temp")

  #####

  # gogod issues:

  # - 1813-04-11a.sgf has an unmatched bracket inside the DT tag which causes an error for me
  # - 1866-04-18b.sgf has an unmatched bracket inside the DT tag which causes an error for me
  # - 2018-09-05d.sgf has an unmatched bracket inside the EV tag which causes an error for me

  # - the following SGF have duplicated PC tags, 1996-04-15o.sgf, 1996-04-11l.sgf, 2007-01-09a.sgf, 1996-04-17d.sgf
  # - the RU tag appears twice in 2006-08-21c.sgf
  # - the RO tag appears twice in 1933-04-19c.sgf

  my_path <- "./temp/Go CD/Database/1930-39/1933-04-19c.sgf"
  txt <- readLines(my_path)
  txt <- txt[-8]
  writeLines(txt, my_path)
  stopifnot(validate_sgfs(my_path) == "sgf is valid")

  my_path <- "./temp/Go CD/Database/2006/2006-08-21c.sgf"
  txt <- readLines(my_path)
  txt[10] <- "RU[Ing or Chinese]"
  txt <- txt[-11]
  writeLines(txt, my_path)
  stopifnot(validate_sgfs(my_path) == "sgf is valid")

  my_path <- "./temp/Go CD/Database/2007/2007-01-09a.sgf"
  txt <- readLines(my_path)
  txt <- txt[-9]
  writeLines(txt, my_path)
  stopifnot(validate_sgfs(my_path) == "sgf is valid")

  my_path <- "./temp/Go CD/Database/1996/1996-04-17d.sgf"
  txt <- readLines(my_path)
  txt <- txt[-9]
  writeLines(txt, my_path)
  stopifnot(validate_sgfs(my_path) == "sgf is valid")

  my_path <- "./temp/Go CD/Database/1996/1996-04-11l.sgf"
  txt <- readLines(my_path)
  txt <- txt[-9]
  writeLines(txt, my_path)
  stopifnot(validate_sgfs(my_path) == "sgf is valid")

  my_path <- "./temp/Go CD/Database/1996/1996-04-15o.sgf"
  txt <- readLines(my_path)
  txt <- txt[-9]
  writeLines(txt, my_path)
  stopifnot(validate_sgfs(my_path) == "sgf is valid")

  my_path <- "./temp/Go CD/Database/1850-99/1866-04-18b.sgf"
  txt <- readLines(my_path)
  txt[6] <- "DT[1866-04-18 (Keio 2 III 4)]"
  writeLines(txt, my_path)
  stopifnot(validate_sgfs(my_path) == "sgf is valid")

  my_path <- "./temp/Go CD/Database/1800-49/1813-04-11a.sgf"
  txt <- readLines(my_path)
  txt[5] <- "DT[1813-04-11 Bunka 10 III (11)]"
  writeLines(txt, my_path)
  stopifnot(validate_sgfs(my_path) == "sgf is valid")

  # these issues arose in cleaning the 2009 data, but didn't appear in the 2020 version?

  # 1992-01-20e.sgf - GC tag appears twice

  my_path <- "./temp/Go CD/Database/1992/1992-01-20e.sgf"
  txt <- readLines(my_path)
  txt[13] <- "GC[Page106, Another source has 1989]"
  txt <- txt[-15]
  writeLines(txt, my_path)
  stopifnot(validate_sgfs(my_path) == "sgf is valid")

  # 1934-04-18e.sgf - tag missing, value present

  my_path <- "./temp/Go CD/Database/1930-39/1934-04-18e.sgf"
  txt <- readLines(my_path)
  txt[6] <- "EV[Oteai, Section B]"
  writeLines(txt, my_path)
  stopifnot(validate_sgfs(my_path) == "sgf is valid")


  # 1934-04-11g.sgf - i think EV has a tab character in it

  my_path <- "./temp/Go CD/Database/1930-39/1934-04-11g.sgf"
  txt <- readLines(my_path)
  txt[6] <- "EV[Oteai, Section B]"
  writeLines(txt, my_path)
  stopifnot(validate_sgfs(my_path) == "sgf is valid")

  # 0750.sgf - broken up comment

  my_path <- "./temp/Go CD/Database/0196-1699/0750.sgf"
  txt <- readLines(my_path)
  txt[8] <- "GC[Ming Huang is a popular name for the Tang Emperor Xuan Zong (Li Longji, r.712-756), who was the emperor infatuated with Yang Guifei. 8, 12, 31, 32 are omitted in the original, there are two 47s (one put at 49, 51 is unmarked, 61 and 63 are transposed from the original). The colours have been assigned speculatively.]"
  txt <- txt[-c(9, 10, 11)]
  writeLines(txt, my_path)
  stopifnot(validate_sgfs(my_path) == "sgf is valid")


  # 2009-02-14a.sgf - dunno whats up here

  my_path <- "./temp/Go CD/Database/2009/2009-02-14a.sgf"
  txt <- readLines(my_path)
  txt[13] <- "GC[Black won because the rules were the KGS version of Chinese rules, incorrectly counting handicap stones. Komi and the result are in KGS form. MFG was a version using Monte Carlo with 32 cores.]"
  txt <- txt[-c(14, 15)]
  txt[38] <- gsub(";$", "", txt[38])
  writeLines(txt, my_path)
  stopifnot(validate_sgfs(my_path) == "sgf is valid")

  # 1930-08-30.sgf - double closing bracket in 1930-08-30

  my_path <- "./temp/Go CD/Database/1930-39/1930-08-30.sgf"
  txt <- readLines(my_path)
  txt[7] <- "RO[Game 75 (Go on 1 win; replay of tied game)]"
  writeLines(txt, my_path)
  stopifnot(validate_sgfs(my_path) == "sgf is valid")



  ###

  raw_sgfs <- list.files("./temp/Go CD/Database", full.names = TRUE, recursive = TRUE, pattern = "*\\.sgf$")

  expect_true(length(raw_sgfs) == 60350)

  renamed_sgfs <- rep(NA, length(raw_sgfs))
  for (i in seq_along(raw_sgfs)) {
    renamed_sgfs[i] <- digest(file = raw_sgfs[i], algo = "sha1")
    if (i %% 1000 == 0) print(i)
  }
  renamed_sgfs <- paste0(renamed_sgfs, ".sgf")

  stopifnot(sum(duplicated(renamed_sgfs)) == 0)

  dir_init("./temp/clean_sgfs")
  file.copy(raw_sgfs, file.path("temp", "clean_sgfs", renamed_sgfs), overwrite = TRUE)

  # validate all games

  sgfs <- list.files("./temp/clean_sgfs", full.names = TRUE)

  stopifnot(length(sgfs) == 60350)
  # minus one from the flagged duplication

  # valid <- validate_sgfs(sgfs)

  db <- create_database_fast(sgfs, num_cores = 8)

  db$filename <- gsub("./temp/clean_sgfs", "", db$filename)

  sgfs <- gsub("./temp/clean_sgfs/", "", sgfs)

  db$original_filename <- raw_sgfs[match(sgfs, renamed_sgfs)]
  db$original_filename <- gsub("./temp/Go CD/", "", db$original_filename)

  # two duplicated hashes:
  # "a7b097871ef8b71f272" "edeb257511048c22967"
  # both of these are annotated already in GoGoD, and can't do much damage anyway

  write.csv(db, "raw_data/gogod_database.csv", row.names = FALSE)
  unlink("temp", recursive = TRUE)

}



# now clean raw data

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




print("prepared predictors based on demographic data")

bio_data <- read.csv("./raw_data/players.csv", stringsAsFactors = FALSE)

d$BN <- bio_data$Nationality[match(d$PB, bio_data$Player.Name)]

d$black_birth_year <- bio_data$Birth.Year[match(d$PB, bio_data$Player.Name)]
d$black_age <- d$year - d$black_birth_year

# reorder by date
o <- order(d$DT)
d <- d[o,]




# checks

stopifnot(all(c("PB", "BR", "DT", "year", "KM", "RE", "HA", "GC", "filename", "BN", "black_birth_year", "black_age", "black_won", "komi", "fourfour") %in% colnames(d)))

stopifnot(nrow(d) == 49789)

stopifnot(mean(is.na(d$black_age)) < 0.31)
stopifnot(!any(is.na(as.Date(d$DT))))
stopifnot(!any(is.na(d$fourfour)))
stopifnot(min(d$year) == 1954)
stopifnot(max(d$year) == 2009)

print("save cleaned dataset to file")

write.csv(d, "games.csv", row.names = FALSE)
