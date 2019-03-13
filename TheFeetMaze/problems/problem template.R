# =======================================================================
# Names:Luis Urrechaga, Pablo Irigoyen
# Group Number: Group B 
# Assignment: The Feet Maze
# Date: 11/03/19
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
rm(list = ls())
cat("\014")
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
getwd()
dir()

initFeet<-"L"
x<-matrix(initFeet,nrow=7, ncol=7)
x[c(1), c(2,4,5,6)]<-"R"
x[c(2), c(1,3,4)]<-"R"
x[c(3), c(2,4,6)]<-"R"
x[c(4), c(1,2,3,4,5,7)]<-"R"
x[c(5), c(4,7)]<-"R"
x[c(6), c(1,2,3,7)]<-"R"
x[c(7), c(2,4,6)]<-"R"
x

initTrue = "true"

leftWall<-matrix(initTrue,nrow=7,ncol=7)
#leftWall[c(1), c()]<-"false"
leftWall[c(2), c(3)]<-"false"
leftWall[c(3), c(3, 6)]<-"false"
#leftWall[c(4), c()]<-"false"
leftWall[c(5), c(5)]<-"false"
leftWall[c(6), c(4)]<-"false"
#leftWall[c(7), c()]<-"false"
leftWall

rightWall<-matrix(initTrue,nrow=7,ncol=7)
#rightWall[c(1), c()]<-"false"
rightWall[c(2), c(2)]<-"false"
rightWall[c(3), c(2,5)]<-"false"
#rightWall[c(4), c()]<-"false"
rightWall[c(5), c(4)]<-"false"
rightWall[c(6), c(3)]<-"false"
#rightWall[c(7), c()]<-"false"
rightWall

downWall<-matrix(initTrue,nrow=7,ncol=7)
downWall[c(1), c(3)]<-"false"
#downWall[c(2), c()]<-"false"
downWall[c(3), c(6)]<-"false"
#downWall[c(4), c()]<-"false"
downWall[c(5), c(3)]<-"false"
#downWall[c(6), c()]<-"false"
#downWall[c(7), c()]<-"false"
downWall

upWall<-matrix(initTrue,nrow=7,ncol=7)
#upWall[c(1), c()]<-"false"
upWall[c(2), c(3)]<-"false"
#upWall[c(3), c()]<-"false"
upWall[c(4), c(6)]<-"false"
#upWall[c(5), c()]<-"false"
upWall[c(6), c(3)]<-"false"
#upWall[c(7), c()]<-"false"
upWall

#[c()]
#datamat<-(()
#is.matrix(x)
#x[]

# This function must return a list with the information needed to 
# solve the problem.
# (Depending on the problem, it should receive or not parameters)
initialize.problem = function(){
  problem = list()
  problem$state.initial = c(7,1)
  problem$columns = ncol(x)
  problem$row = nrow(x)
  problem$state.final   = c(1,7)
  problem$actions.possible =data.frame(direction=c("Up","Down","Rigth","Left"),cost=1)
  problem$name = "The Feet Maze"
  problem$matrix = x
  problem$upWall = upWall
  problem$downWall = downWall
  problem$leftWall = leftWall
  problem&rightWall = rightWall
  return(problem)
}

# =======================================================================
# Must return TRUE or FALSE according with if the action can be done or not
# over the specific state
is.applicable = function (state,action,problem){
  result = FALSE
  
  if (action == "Up" && upWall(state) == "true"){
    result = row!=1
  }
  if (action == "Down" && downWall(state) == "true"){
    result = row!=problem$rows
  }
  if (action == "Left" && leftWall(state) == "true"){
    result = col!=1
  }
  if (action == "Right"  && rightWall(state) == "true"){
    result = col!=problem$columns
  }
  # <insert code here in order to calculate result value>
  return(result)
}

# =======================================================================
# Must return the state resulting on applying the action over the state
effect = function (state,action){
  result = state
  i<-integer()
  
  if (action == "Up" ){
    result[row-1,col] = state[row,col]
    result[row,col] = state[row-1,col]
    i+1
  }
  if (action == "Down" ){
    result[row+1,col] = state[row,col]
    result[row,col] = state[row+1,col]
    i+1
  } 
  if (action == "Left" ){
    result[row,col-1] = state[row,col]
    result[row,col] = state[row,col-1]
    i+1
  }
  if (action == "Right"){
    result[row,col+1] = state[row,col]
    result[row,col] = state[row,col+1]
    i+1
  }
  # <insert code here in order to modify the resulting state> 
  return(result)
}


# =======================================================================
# Must return TRUE or FALSE according with the state is final or not
# * In case the final state is stablished by a condition, second argument
#   could be omited
is.final.state = function (state, finalstate = problem$state.final){
  
  if(state == finalstate){
    result= TRUE
  }else
    result= FALSE
  
  # <insert code here in order to modify the resulting state> 
  return(result)
}

# =======================================================================
# Must print the state in console (in a legible way)
to.string = function (state){
  for (i in 1:nrow(state)){
    print(state[i,])
  }
}

# =======================================================================
# Return the cost of applying an action over a state
get.cost = function (action,state){
  # Return the cost of applying an action over a state
  return(action$cost)
}

# =======================================================================
# (Used for Informed Algorithms)
# Heuristic function used in Informed algorithms
get.evaluation = function(state,problem){
  
  return(1)
}

