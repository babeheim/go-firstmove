
if (file.exists("games.csv")) file.remove("games.csv")
if (file.exists("first_moves.csv")) file.remove("first_moves.csv")
if (file.exists("horizon24.RData")) file.remove("horizon24.RData")
if (file.exists("horizon24")) file.remove("horizon24")

source("project_support.R")

tic.clearlog()

dir_init("./figures", verbose = TRUE)
