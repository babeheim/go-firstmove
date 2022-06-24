go-firstmove
============

Analysis script for "Strategic social learning and the population dynamics of human behavior: the game of Go", by Bret Beheim, Calvin Thigpen, and Richard McElreath, in Evolution and Human Behavior, Volume 35, Issue 5, pp.351-357
DOI: http://dx.doi.org/10.1016/j.evolhumbehav.2014.04.001

# Requirements

- R (3.3.1 or greater) https://cran.r-project.org/
- rethinking package (v1.59 or greater), http://xcelab.net/rm/software/
- rstan package
- tictoc package (for timing analysis steps)
- RColorBrewer package (for figures)
- testthat package (for reproducibility diagnostics)
- LaTeX, https://www.latex-project.org/

# Instructions:

In R, set the working directory to that containing this readme file. For example, on a Mac or Linux machine, you might type into the command prompt

```
  setwd('~/Desktop/go-firstmove')
```

if the folder containing the project is named 'go-firstmove' and on your Desktop. You can tell if you are in the right place by typing in `dir()` and seeing this readme.txt file.

The `raw_data/` folder contains two files from which all calculations are derived:

- `gogod_database.csv` - game information for the professional Go games used in this analysis, drawn from the [Games of Go on Disk](https://gogodonline.co.uk/) (GoGoD) database, 2009 edition. This database is commercially available for a small fee.

- `players.csv` - a list biographical details of the professional Go players in our database, compiled from [Sensei's Library](https://senseis.xmp.net/) in 2010-2011.

The analysis itself is broken up into independent modules that pass outputs to each other. The whole process runs by typing one command into R,

```r
  source('./run_project.R')
```

with the project folder as the working directory. If all goes well, each step of the analysis will execute in sequence, and write the final tables and figures into the `figures/` folder, along with a runtime log.

If one has access to the original GoGoD database of Smart Go Format (SGF) files, the `1_prep_games.R` script will draw from those to produce the `raw_data/gogod_database.csv`. Specifically, the code will look for a ZIP file called `GoGoD CD 2009.zip` in the `raw_data/` folder.

The total time until completion will vary by machine but should take several hours.

The project is maintained by Bret Beheim (beheim@gmail.com) and is hosted at https://github.com/babeheim/go-firstmove.
