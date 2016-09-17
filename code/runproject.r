rm(list=ls())

library(glmer2stan)
library(rethinking)

source('./code/project_functions.r')
dir_init('./buildregressiontable')
dir_init('./buildregressiontable/code')
files <- c('./code/project_functions.r', 
	'./code/project_variables.r',
	'./code/buildregressiontable_code.r')
file.copy(files, './buildregressiontable/code')
dir_init('./buildregressiontable/inputs')
files <- list.files('./inputs', pattern=".csv", full.names=TRUE)
file.copy(files, './buildregressiontable/inputs')
setwd('./buildregressiontable')
source('./code/buildregressiontable_code.r')
setwd('..')
# input gogod_gamedata_c.csv and the biographical information
# output various regression tables

source('./code/project_functions.r')
dir_init('./fitmodels')
dir_init('./fitmodels/code')
files <- c('./code/project_functions.r', 
	'./code/project_variables.r',
	'./code/fitmodels_code.r')
file.copy(files, './fitmodels/code')
dir_init('./fitmodels/inputs')
files <- list.files('./buildregressiontable/output', full.names=TRUE, pattern='.csv')
file.copy(files, './fitmodels/inputs')
setwd('./fitmodels')
source('./code/fitmodels_code.r')
setwd('..')
# input regression tables
# output .robj files holding STAN models

source('./code/project_functions.r')
dir_init('./describemodels')
dir_init('./describemodels/code')
files <- c('./code/project_functions.r', 
	'./code/project_variables.r',
	'./code/describemodels_code.r')
file.copy(files, './describemodels/code')
dir_init('./describemodels/inputs')
files <- list.files('./fitmodels/output', full.names=TRUE)
file.copy(files, './describemodels/inputs')
setwd('./describemodels')
source('./code/describemodels_code.r')
setwd('..')
# input .robj files holding STAN models, and the fourfour_final table to get player
# random effects ID's
# output various figures

source('./code/project_functions.r')
dir_init('./compilemanuscript')
dir_init('./compilemanuscript/code')
files <- c('./code/project_functions.r', 
	'./code/project_variables.r',
	'./code/compilemanuscript_code.r',
	'./code/Go Paper.tex')
file.copy(files, './compilemanuscript/code')
dir_init('./compilemanuscript/inputs')
files <- list.files(c('./inputs', './describemodels/output'), full.names=TRUE)
tar <- grep('log', files)
if(length(tar)>0) files <- files[-tar]
file.copy(files, './compilemanuscript/inputs')
setwd('./compilemanuscript')
source('./code/compilemanuscript_code.r')
setwd('..')
# input all publication outputs
# output the pdf of the manuscript

source('./code/project_functions.r')
dir_init('./log')
dir_init('./log/code')
file.copy('./code/log_code.r', './log/code')
dir_init('./log/inputs')
files <- list.files('.', pattern='*log.txt', full.names=TRUE, recursive=TRUE)
file.copy(files, './log/inputs')
setwd('./log')
source('./code/log_code.r')
setwd('..')

# pass final outputs
source('./code/project_functions.r')
dir_init('./output')
files <- './compilemanuscript/output/Go Paper.pdf'
file.copy(files, './output')
files <- list.files('./log/output', full.names=TRUE)
file.copy(files, './output')

# final housekeeping
source('./code/project_variables.r')
if(!save_temp){
    folders <- c(
        './buildregressiontable',
        './fitmodels', 
        './describemodels',
        './compilemanuscript',
        './log'
    )
    if(length(folders)>0) unlink(folders, recursive=TRUE)
}