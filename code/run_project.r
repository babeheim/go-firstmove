rm(list=ls())

source('./code/project_functions.r')
module_init('./build_table')
files <- c('./code/project_functions.r',
	'./code/build_table.r')
file.copy(files, './build_table/code')
files <- list.files('./inputs', pattern=".csv", full.names=TRUE)
file.copy(files, './build_table/inputs')
setwd('./build_table')
source('./code/build_table.r')
setwd('..')
# input gogod_gamedata_c.csv and the biographical information
# output various regression tables

source('./code/project_functions.r')
module_init('./fit_models')
files <- c('./code/project_functions.r', 
	'./code/fit_models.r')
file.copy(files, './fit_models/code')
files <- list.files('./build_table/output', full.names=TRUE, pattern='.csv')
files <- c(files, "./inputs/horizon24.stan")
file.copy(files, './fit_models/inputs')
setwd('./fit_models')
source('./code/fit_models.r')
setwd('..')
# input regression tables
# output .robj files holding STAN models

source('./code/project_functions.r')
module_init('./describe_models')
files <- c('./code/project_functions.r', 
	'./code/describe_models.r')
file.copy(files, './describe_models/code')
files <- list.files('./fit_models/output', full.names=TRUE)
file.copy(files, './describe_models/inputs')
setwd('./describe_models')
source('./code/describe_models.r')
setwd('..')
# input .robj files holding STAN models, and the fourfour_final table to get player
# random effects ID's
# output various figures

source('./code/project_functions.r')
module_init('./compile_manuscript')
files <- c('./code/project_functions.r', 
	'./code/compile_manuscript.r',
	'./code/Go Paper.tex')
file.copy(files, './compile_manuscript/code')
files <- list.files(c('./inputs', './describe_models/output'), full.names=TRUE)
tar <- grep('log', files)
if(length(tar)>0) files <- files[-tar]
file.copy(files, './compile_manuscript/inputs')
setwd('./compile_manuscript')
source('./code/compile_manuscript.r')
setwd('..')
# input all publication outputs
# output the pdf of the manuscript

source('./code/project_functions.r')
module_init('./log')
file.copy('./code/log.r', './log/code')
files <- list.files('.', pattern='*log.txt', full.names=TRUE, recursive=TRUE)
file.copy(files, './log/inputs')
setwd('./log')
source('./code/log.r')
setwd('..')

# pass final outputs
source('./code/project_functions.r')
dir_init('./output')
files <- './compile_manuscript/output/Go Paper.pdf'
file.copy(files, './output')
files <- list.files('./log/output', full.names=TRUE)
file.copy(files, './output')

# final housekeeping
source('./code/project_functions.r')
if(!save_temp){
    folders <- c(
        './build_table',
        './fit_models', 
        './describe_models',
        './compile_manuscript',
        './log'
    )
    if(length(folders)>0) unlink(folders, recursive=TRUE)
}