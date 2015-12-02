# Use line profiler from Hadley Wickham
# cf. http://adv-r.had.co.nz/Profiling.html

# This code will not work if package "parallel" is used

# devtools::install_github("hadley/lineprof")
library(lineprof)
source("matvsmob.R")

l <- lineprof(get.mat.mob.parallel(moves.results.df))
l

shine(l) # stop this by ctrl-c or esc

