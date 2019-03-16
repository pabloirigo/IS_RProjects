# =======================================================================
# Names:Luis Urrechaga, Pablo Irigoyen
# Group Number: B 
# Assignment: TheFeetMaze
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
# With the following two commands we clean the work environment and the console
#rm(list = ls())
#cat("\014")
#setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
#getwd()
#dir()

# This function must return a list with the information needed to 
# solve the problem.
# (Depending on the problem, it should receive or not parameters)
initialize.problem = function(file, fileUp, fileDown, fileRight, fileLeft){
  
  problem = list()
  #problem$map = read.delim(file, header = F)
  problem$map = read.csv(file, header = F)
  #problem$map_Up = read.delim(fileUp, header = F)
  problem$map_Up = read.csv(fileUp, header = F)
  #problem$map_Down = read.delim(fileDown, header = F)
  problem$map_Down = read.csv(fileDown, header = F)
  #problem$map_Right = read.delim(fileRight, header = F)
  problem$map_Right = read.csv(fileRight, header = F)
  #problem$map_Left = read.delim(fileLeft, header = F)
  problem$map_Left = read.csv(fileLeft, header = F)
  problem$state.initial = c(7, 1)
  problem$state.final = c(1, 7)
  problem$state = problem$state.initial
  problem$actions.possible = data.frame(direction=c("Up", "Down", "Right", "Left"), stringsAsFactors = F)
  problem$name = "TheFeetMaze"
  
  return(problem)
}

# =======================================================================
# Must return TRUE or FALSE according with if the action can be done or not
# over the specific state
is.applicable = function (state,action,problem){
  
  result = F
  row = state[1]
  column = state[2]
  
  
  #Action Up
  if (action == "Up" && row>1){
    #Checking if in the map_Up there is a F in that position, in that case there is a wall above
    if(problem$map_Up[[row]][column] == T){
      #If the one above is not the same type("R" or "L"), it can move -> opposite foot
      result = !(problem$map[[row]][column] == problem$map[[row-1]][column])
    }
  }
  
  #Action Down
  if (action == "Down" && row<7){
    #Checking if in the map_Down there is a F in that position, in that case there is a wall above
    if(problem$map_Down[[row]][column] == T){
      #If the one under is not the same type("R" or "L"), it can move -> opposite foot
      result = !(problem$map[[row]][column] == problem$map[[row+1]][column])
    }
  }
  
  #Action Right
  if (action == "Right" && column<7){
    #Checking if in the map_Right there is a F in that position, in that case there is a wall above
    if(problem$map_Right[[row]][column] == T){
      #If the one to the right is not the same type("R" or "L"), it can move -> opposite foot
      result = !(problem$map[[row]][column] == problem$map[[row]][column+1])
    }
  }
  
  #Action Left
  if (action == "Left" && column>1){
    #Checking if in the map_Left there is a F in that position, in that case there is a wall above
    if(problem$map_Left[[row]][column] == T){
      #If the one to the left is not the same type("R" or "L"), it can move -> opposite foot
      result = !(problem$map[[row]][column] == problem$map[[row]][column-1])
    }
  }
  
  # <insert code here in order to calculate result value>
  #print(result)
  return(result)
}

# =======================================================================
# Must return the state resulting on applying the action over the state
effect = function (state,action){
  
  pos = which(state == problem$state, arr.ind = TRUE)
  row = pos[1]
  column = pos[2]
  result = state
  
  #Going Up
  if (action == "Up" ){
    result[1] = state[1]-1
  }
  
  #Going Down
  if (action == "Down" ){
    result[1] = state[1]+1
  } 
  
  #Going Right
  if (action == "Right" ){
    result[2] = state[2]+1
  }
  
  #Going Left
  if (action == "Left"){
    result[2] = state[2]-1
  }
  
  # <insert code here in order to modify the resulting state> 
  #print(result)
  return(result)
}


# =======================================================================
# Must return TRUE or FALSE according with the state is final or not
# * In case the final state is stablished by a condition, second argument
#   could be omited
is.final.state = function (state, finalState = problem$state.final){
  
  result= FALSE
  
  #If the column and the row match the ones in final.state then it finishes
  if(state[1] == finalState[1] && state[2] == finalState[2]){
    result= TRUE
  }
  
  
  # <insert code here in order to modify the resulting state> 
  return(result)
}

# =======================================================================
# Must print the state in console (in a legible way)
to.string = function (state){
  print(state)
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
