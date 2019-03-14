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
x[1, c(2,4,5,6)]<-"R"
x[2, c(1,3,4)]<-"R"
x[3, c(2,4,6)]<-"R"
x[4, c(1,2,3,4,5,7)]<-"R"
x[5, c(4,7)]<-"R"
x[6, c(1,2,3,7)]<-"R"
x[7, c(2,4,6)]<-"R"
x

initTrue = T

leftWall<-matrix(initTrue,nrow=7,ncol=7)
#leftWall[c(1), c()]<-"false"
leftWall[2, 3]<-F
leftWall[3, c(3, 6)]<-F
#leftWall[c(4), c()]<-"false"
leftWall[5, 5]<-F
leftWall[6, 4]<-F
#leftWall[c(7), c()]<-"false"
leftWall

rightWall<-matrix(initTrue,nrow=7,ncol=7)
#rightWall[c(1), c()]<-"false"
rightWall[2, 2]<-F
rightWall[3, c(2,5)]<-F
#rightWall[c(4), c()]<-"false"
rightWall[5, 4]<-F
rightWall[6, 3]<-F
#rightWall[c(7), c()]<-"false"
rightWall

downWall<-matrix(initTrue,nrow=7,ncol=7)
downWall[1, 3]<-F
#downWall[c(2), c()]<-"false"
downWall[3, 6]<-F
#downWall[c(4), c()]<-"false"
downWall[5, 3]<-F
#downWall[c(6), c()]<-"false"
#downWall[c(7), c()]<-"false"
downWall

upWall<-matrix(initTrue,nrow=7,ncol=7)
#upWall[c(1), c()]<-"false"
upWall[2, 3]<-F
#upWall[c(3), c()]<-"false"
upWall[4, 6]<-F
#upWall[c(5), c()]<-"false"
upWall[6, 3]<-F
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
  problem$state.initial = x(7,1)
  problem$columns = ncol(x)
  problem$row = nrow(x)
  problem$state.final   = x(1,7)
  problem$actions.possible =data.frame(direction=c("Up","Down","Rigth","Left"),cost=1)
  problem$name = "The Feet Maze"
  problem$matrix = x
  problem$upWall = upWall
  problem$downWall = downWall
  problem$leftWall = leftWall
  problem$rightWall = rightWall
  return(problem)
}

# =======================================================================
# Must return TRUE or FALSE according with if the action can be done or not
# over the specific state
is.applicable = function (state,action,problem){
  result = F
  action <- action$action
  
  #Going UP
  if (action == "Up" && upWall(state) == T && (state[row,col] != state[row-1,col]) && row>1){
    if(upWall(state) == "true" && state[row, col] != state[row-1, col] && row=!1){
      result = row!=1
    }
  }
  
  #Going Down
  if (action == "Down" && downWall(state) == "true"){
    result = row!=problem$rows
  }
  
  #Going Left
  if (action == "Left" && leftWall(state) == "true"){
    result = col!=1
  }
  
  #Going Right
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

