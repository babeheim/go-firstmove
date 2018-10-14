

if (scaffold == TRUE) {
  rm(list = ls())
  source("../project_support.r")
}

# stage 1: Rmarkdown to markdown

file.copy("./Rmarkdown/manuscript.Rmd", ".", overwrite = TRUE)
knit("./manuscript.Rmd")
file.remove("./manuscript.Rmd")

dir_init("./markdown")
file.copy("./manuscript.md", "./markdown")
file.remove("./manuscript.md")

# Stage 2: markdown to tex

dir_init("./tex")
system("pandoc ./markdown/manuscript.md --latex-engine=xelatex --template=./assets/go-template.tex -o ./tex/manuscript.tex")

# Stage 3: compile tex into a pdf and docx

dir_init("./temp")
my_files <- list.files("./inputs", full.names = TRUE)
my_files <- c(my_files, list.files("./assets", full.names = TRUE))
my_files <- c(my_files, list.files("./tex", full.names = TRUE))
file.copy(my_files, "./temp")
setwd("./temp")
system("xelatex manuscript")
system("bibtex manuscript")
system("xelatex manuscript")
system("xelatex manuscript")

setwd("..")

if (save_output) {
  dir_init("./output", overwrite = FALSE)
  file.copy("./temp/manuscript.pdf", "./output", overwrite = TRUE)
}

if (!save_temp) {
  unlink("./markdown", recursive = TRUE)
  unlink("./tex", recursive = TRUE)
  unlink("./temp", recursive = TRUE)
}
