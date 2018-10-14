
rm(list = ls())

start_time <- Sys.time()

source("./project_support.r")

tic.clearlog()

##############

tic("build regression dataframe")
dir_init("./1_build_dataframe/inputs")
file.copy("./data/gogod_gamedata_c.csv", "./1_build_dataframe/inputs")
file.copy("./data/PB Biographical Data.csv", "./1_build_dataframe/inputs")
setwd("./1_build_dataframe")
source("./build_dataframe.r")
setwd("..")
toc(log = TRUE)

##############

tic("fit regression models")
dir_init("./2_fit_models/inputs")
file.copy("./1_build_dataframe/output/fourfour_final.csv", "./2_fit_models/inputs")
setwd("./2_fit_models")
source("./fit_models.r")
setwd("..")
toc(log = TRUE)

##############

tic("describe models")
dir_init("./3_describe_models/inputs")
files <- list.files("./2_fit_models/output", full.names = TRUE)
file.copy(files, "./3_describe_models/inputs")
file.copy("./1_build_dataframe/output/fourfour_final.csv", "./3_describe_models/inputs")
setwd("./3_describe_models")
source("./describe_models.r")
setwd("..")
toc(log = TRUE)

##############

tic("prepare manuscript")
dir_init("./4_prepare_manuscript/inputs")
files <- list.files("./3_describe_models/output", full.names = TRUE)
file.copy(files, "./4_prepare_manuscript/inputs")
setwd("./4_prepare_manuscript")
source("./prepare_manuscript.r")
setwd("..")
toc(log = TRUE)

##############

dir_init("./output")

tic.log(format = TRUE)
msg_log <- unlist(tic.log())
msg_log <- paste0("- ", msg_log)
if (!exists("start_time")) start_time <- NA
header <- c(
  "project: Go first-move evolution analysis",
  paste("start_time:", start_time),
  paste("finish_time:", Sys.time()),
  "events:")
msg_log <- c(header, msg_log)
writeLines(msg_log, "./output/log.txt")

files <- list.files(
  c("./4_prepare_manuscript/output"), full.names = TRUE)

file.copy(files, "./output")
