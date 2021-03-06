# =======================================================================
# Names:Luis Urrechaga, Pablo Irigoyen  
# Group Number: B
# Assignment:TheFeetMaze  
# Date:
# =======================================================================
# 1. Be sure to include, with this template, any necessary files
#    for execution, including datasets (problem.R, methodXXX.R, ...)
#    (submission of the entire template folder is recommended)
# 2. If you use a function of a certain package, do not forget to include the
#    corresponding call to the "library ()" function
# 3. Do not forget to comment on the code, especially those non-trivial commands
#    (remember that part of the rating depends on the cleaning of the code)
# 4. It is strongly recommended to test any implemented function in order to 
#    check for its proper operation
# =======================================================================
# (This is a general code, you must adapt it)
# =======================================================================
# Configuring the Environment
rm(list=ls())
cat("\014")
graphics.off()
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
getwd()
dir()

# LIBRARIES (add any needed library here)
library(rstudioapi)
library(ggplot2)
library(gridExtra)

# ADDITIONAL FUNCTIONS (add any used method/problem here)
source("../problems/problem TheFeetMaze.R")
source("../methods/Breadth First Search.R")
source("../methods/Depth First Search.R")
#source("../methods/YYYYYYYYYYY.R")

# And here, there are additional (needed) functions
source("../methods/Expand Node.R")
source("../methods/Analyze Results.R")
source("../methods/Plot Results.R")
# =======================================================================
# Check the proper operation of implemented function here!



# =======================================================================
# Solving of the problem (you have to adapt it)
#problem = initialize.problem("../data/map.txt", "../data/map_Up.txt", "../data/map_Down.txt", "../data/map_Right.txt", "../data/map_Left.txt")
problem = initialize.problem("../data/map.csv", "../data/map_Up.csv", "../data/map_Down.csv", "../data/map_Right.csv", "../data/map_Left.csv")
print(problem)
res1 = Breadth.First.Search(problem, count.limit = 1000, graph.search = T) 
res2 = Depth.First.Search(problem,count.limit = 10000, graph.search = T)
all = list(res1, res2)
analyze.results(list(res1,res2),problem)

