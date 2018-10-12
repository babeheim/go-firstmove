
if (scaffold) {
  rm(list = ls())
  source("../project_support.r")
}

dir_init("./temp")

file.copy("./tex/Go Paper.tex", "./temp")
files <- list.files("./inputs", full.names = TRUE)
file.copy(files, "./temp")
files <- list.files("./assets", full.names = TRUE)
file.copy(files, "./temp")

setwd("./temp")
system("pdflatex 'Go Paper.tex'")
setwd("..")

dir_init("./output")
file.copy("./temp/Go Paper.pdf", "./output")

if(!save_temp) unlink("./temp", recursive = TRUE)