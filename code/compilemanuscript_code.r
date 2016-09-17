
rm(list=ls())

source('./code/project_functions.r')
source('./code/project_variables.r')

dir_init('./temp')

start.time <- Sys.time()

file.copy('./code/Go Paper.tex', './temp')
files <- list.files('./inputs', full.names=TRUE)
file.copy(files, './temp')

setwd('./temp')
system("pdflatex 'Go Paper.tex'")
setwd('..')

dir_init('./output')
file.copy('./temp/Go Paper.pdf', './output')

stop.time <- Sys.time()
cat(task.timer("compile manuscript"), file="./temp/compilemanuscript_log.txt")

dir_init('./output')
files <- c('./temp/Go Paper.pdf')
file.copy(files, './output')

if(!save_temp) unlink('./temp', recursive=TRUE)