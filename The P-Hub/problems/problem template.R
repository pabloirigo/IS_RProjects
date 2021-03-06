# =======================================================================
# Names: Pablo Irigoyen, Luis Urrechaga
# Group Number: B
# Assignment: P-Hub
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

# This function must return a list with the information needed to 
# solve the problem.
# (Depending on the problem, it should receive or not parameters)
initialize.problem = function(file, P){
  problem = list()
  #problem$actions = read.csv(file, header=F, skip=2, dec="", sep=" ")
  problem$state.initial = read.csv(file, header = F, skip = 2, dec = ".", sep = "")
  problem$actions.possible = data.frame(direction = c("move"), stringsAsFactors = F)
  problem$name = paste0("P-Hub - [", file, "]")
  problem$mainAirports = P
  problem$rows = nrow(problem$state.initial)
  problem$columns = ncol(problem$state.initial)
  problem$randomMainAirports = sample(1:problem$rows, problem$mainAirports, replace= F)
  problem$mainAirportsCombinations = list(problem$randomMainAirports)
  problem$maxCombinations = as.integer((factorial(problem$rows)/(factorial(problem$mainAirports) * factorial(problem$rows - problem$mainAirports))))
  problem$totalCost = 0
  return(problem)
}

#get.principalAirports = function (){
#m1 <- problem$mainAirports[1]
#m2 <- problem$mainAirports[2]

#initAirport <- sample(1:10, 1)
#}


# =======================================================================
# Must return TRUE or FALSE according with if the action can be done or not
# over the specific state
is.applicable = function (state, action, airports, mainAirports){
  originAirport = airports[[1]]
  destinationAirport = airports[[2]]
  
  # <insert code here in order to calculate result value>
  if (action == "move"){
    result = originAirport %in% mainAirports || (!(originAirport %in% mainAirports) && destinationAirport %in% mainAirports)
  }
  
  return(result)
}

contains = function(listToCheck, listWhereToCheck){
  sameAirport <- F
  for (list in listWhereToCheck){
    if(all(listToCheck %in% list)){
      sameAirport <- T
      break
    }
  }
  return (sameAirport)
}

bestAirport = function(state, mainAirports, origAirport, destinAirport){
  bestCost = -1
  for (mainAirport in mainAirports){
    if(bestCost == -1){
      bestCost <- state[origAirport, mainAirport] + state[mainAirport, destinAirport]
    }else{
      sum <- state[origAirport, mainAirport] + state[mainAirport, destinAirport]
      if(sum<bestCost)
        bestCost<-sum
    }
  }
  return(bestCost)
}

# =======================================================================
# Must return the state resulting on applying the action over the state
effect = function (state, action, mainAirports, mainAirportsCombinations, count, totalCombinations){
  #result = state
  # <insert code here in order to modify the resulting state> 
  if(action == "move"){
    i<-1
    cost = 0
    for(row in state){
      j<-1
      for(column in row){
        if(i != j){
          if(i %in% mainAirports || j %in% mainAirports){
            cost<-cost+state[i,j]
          }else{
            bestAir<-bestAirport(state, mainAirports, i, j)
            cost<-cost + bestAir
          }
        }
        j<-j+1
      }
      i<-i+1
    }
    if(count<totalCombinations){
      newMainAirports<-sample(1:problem$rows, problem$mainAirports, replace = F)
      while (contains(newMainAirports, mainAirportsCombinations)){
        newMainAirports<sample(1:problem$rows, problem$mainAirports, replace = F)
      }
    }else{
      newMainAirports<-0
    }
  }
  mainAirportsCombinations<-append(mainAirportsCombinations, list(newMainAirports))
  returnValue(list("cost" = cost, "mainAirports" = mainAirports, "newMainAirports" = newMainAirports, "mainAirportsCombinations" = mainAirportsCombinations))
  
  return(returnValue)
}


# =======================================================================
# Must return TRUE or FALSE according with the state is final or not
# * In case the final state is stablished by a condition, second argument
#   could be omited
is.final.state = function (state, count, nAirports){
  result = F
  # <insert code here in order to modify the resulting state> 
  if(count>problem$maxCombinations){
    result = T
  }
  return(result)
}

# =======================================================================
# Must print the state in console (in a legible way)
to.string = function (state){
  # <insert code here to print the state> 
  print(state + "/n")
}

# =======================================================================
# Return the cost of applying an action over a state
get.cost = function (action,state){
  # Return the cost of applying an action over a state
  return(1)
}

# =======================================================================
# (Used for Informed Algorithms)
# Heuristic function used in Informed algorithms
get.evaluation = function(state,problem){
  return(1)
}