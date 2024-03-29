
# this script requires access to the 2009 GoGoD CD, here stored as a zip file

print("extract raw data")

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

stopifnot(length(raw_sgfs) == 60350)

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

db <- create_database_fast(sgfs, num_cores = 6)

db$filename <- gsub("./temp/clean_sgfs", "", db$filename)

sgfs <- gsub("./temp/clean_sgfs/", "", sgfs)

db$original_filename <- raw_sgfs[match(sgfs, renamed_sgfs)]
db$original_filename <- gsub("./temp/Go CD/", "", db$original_filename)

# two duplicated hashes:
# "a7b097871ef8b71f272" "edeb257511048c22967"
# both of these are annotated already in GoGoD, and can't do much damage anyway

write.csv(db, "raw_data/gogod_database.csv", row.names = FALSE)
unlink("temp", recursive = TRUE)
