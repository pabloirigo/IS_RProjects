Hill.Climber = function(problem,
                                count.limit = 100, 
                                count.print = 100, 
                                trace = FALSE, 
                                graph.search = FALSE){
  
  name.method = paste0("Hill.Climber", ifelse(graph.search," + GS",""))
  state.initial    = problem$state.initial
  #state.final      = problem$state.final
  actions.possible = problem$actions.possible
  nAirports = problem$rows
  main.Airports = problem$mainAirports
  main.Airports.Combinations = problem$mainAirportsCombinations
  
  node = list(parent=c(),
              state=state.initial,
              actions=c(),
              depth=0,
			        cost=1,
			        main_Airports=main.Airports,
			        main_Airports_Combinations=main.Airports.Combinations)
  
  #frontier = list(node)
  successor <- node

  if (graph.search){
    expanded = list()     
  }
  count = 1
  end.reason = 0
  report = data.frame(iteration=numeric(),
                      depth.of.expanded=numeric())
  
  
  while (count<=count.limit){
    if (count%%count.print==0){
      print(paste0("Count: ",count), quote = F)
    }
    
    if(graph.search){
      expanded = append(expanded,list(successor))
    }
    
    if (trace){
      print(" ",quote = F)
      print("------------------------------", quote = F)
      print("State extracted from frontier:", quote = F)
      to.string(successor$state)
      print(paste0("(depth=",successor$depth,", cost=",successor$depth,")"),quote = F)
    }
    
    if (is.final.state(successor$state,as.integer(count), nAirports)){
      end.reason = "Solution"
      break
    }
    
    newNode = expand.node(successor, actions.possible, main.Airports.Combinations, main.Airports, as.integer(count), problem$maxCombinations)
    main.Airports <- newNode$newMainAirports
    main.Airports.Combinations <- newNode$main.Airports.Combinations
    
    if(successor$cost==-1){
      successor<-newNode
    }
    
    if(successor$cost>newNode$cost){
      successor<-newNode
    }

    report = rbind(report,
                   data.frame(iteration = count),
                   depth.of.expanded = successor$depth)
    count = count+1
  }
  
  result = list()
  result$report = report
  result$name = name.method

  # Show the obtained (or not) final solution
  if (end.reason == "Solution"){
    print("Solution found!!", quote = F)
    length(successor$mainAirports)
    toString(susccessor$mainAirports)
    result$state.final = successor
  }
  
  #plot.results(report,name.method,problem)
  
  return(result)
}
