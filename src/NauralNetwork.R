hiddenNeuronsCount <- 14
evolutionIterations <- 500
numberOfTestGames <- 1000
neededForEval <- list("hiddenNeuronsCount", "NNvsRandomPlayerGame", "GenerateEmptyBoard", "board.size",
          "IsMovePossible", "Move", "RunAI", "InitNN", "EvaluateBoard", "RunNN", "winningSeries",
          "FlipMatrix", "EvaluateBoardSmall", "GetSubBoard", "GetFieldsO", "GetFieldsX")


# Creating an empty neural network which we represent as a matrix of weights
# Neural network should have 2*n^2 input neurons, h hidden neurons and n^2 output neurons
# In matrix we store weights of connections between neurons
# (2*n*n)x(hiddenNeuronsCount) between input layer and hidden layer
# (n*n)x(hiddenNeuronsCount) between hidden layer and output layer
# For every neuron sum of input connections equals 1
InitNN <- function(){
  n <- board.size
  n.weights <- 3*n*n*hiddenNeuronsCount
  nn <- matrix(runif(n.weights, min=0, max=1),ncol=hiddenNeuronsCount)

  # normalize input and output connections
  # for each neuron in hidden layer sum of input connections must be equal to 1
  # as well as output connections (sum must equals 1) 
  NormalizeConnections(nn)
}

NormalizeConnections <- function(nn){
  n <- board.size
  #connections to hidden layer
  nn[1:(2*n*n),] <- apply(nn[1:(2*n*n),], 2, function(col){ sapply(col, function(y)y/sum(col)) })
  #connections to output layer
  nn[(2*n*n + 1):(3*n*n),] <- t(apply(nn[(2*n*n + 1):(3*n*n),], 1, function(row){ sapply(row, function(y)y/sum(row)) }))
  nn
}

GetFieldsO <- function(side){
  length(which(side == 1))
}

GetFieldsX <- function(side){
  length(which(side == -1))
}

# Calculating the network
RunNN <- function(nn, board){
  n <- board.size
  board.vector <- as.vector(board)

  # input layer - n neurons of O and n neurons of X
  network.in <- c(sapply(board.vector, FUN=GetFieldsO), sapply(board.vector, FUN=GetFieldsX))
  a <- nn[1:(2*n*n),];

  # hidden layer - multply input layer vector by part of neural network matrix
  network.hidden <- network.in %*% a
  a <- nn[(2*n*n + 1):(3*n*n),];

  # output layer - multply hidden layer vector by part of neural network matrix
  a %*% t(network.hidden)
}


# Running AI to choose best move
RunAI <- function(ai, board, side){
  if(side == -1) {
    board <- sapply(board, function(x) -x)
  }
  res <- RunNN(ai,board)

  # We don't want the AI to cheat
  res[board!=0] = -Inf

  # We choose the best one
  which.max(res)
}

Move <- function(ai, board, side){
  move <- RunAI(ai, board, side)
  board[[move]] <- side

  board
}

IsMovePossible <- function(board){
  length(which(board==0)) > 0
}

# AI with neural network plays against rantom player
# if AI wins it gains +1 point, if there is a draw 0 points
# AI gains -2 points in case of loss
NNvsRandomPlayerGame <- function(tic.ai){
  # AI side
  aiSide <- sample(c(-1,1),1)

  # side which starts the game
  side <- sample(c(-1,1),1)

  board <- GenerateEmptyBoard() 
  eval <- 0

  # play a game
  while(eval == 0 && IsMovePossible(board)){
    if(side==aiSide){
      board <- Move(tic.ai, board, side)
    } else {
      # Make a valid move completely at random
      move <- sample(which(board==0),1)
      board[[move]] <- side
    }
    eval <- EvaluateBoard(board)
    side <- side * -1
  }

  # return result
  if(eval == aiSide) return(3)
  if(eval == 0) return(2)
  return(0)
}

# AI with neural network plays against another one
# if first one wins it gains +1 point, if there is a draw 0 points
# AI gains -2 points in case of loss
NNvsNNGame <- function(first, second){
  # side of first player
  firstPlayerSide <- sample(c(-1,1),1)

  # side which starts the game
  side <- sample(c(-1,1),1)

  board <- GenerateEmptyBoard() 
  eval <- 0

  # play a game
  while(eval == 0 && IsMovePossible(board)){
    if(side==firstPlayerSide){
      board <- Move(first, board, side)
    } else {
      board <- Move(second, board, side)
    }
    eval <- EvaluateBoard(board)
    side <- side * -1
  }

  # return result
  if(eval == firstPlayerSide) return(3)
  if(eval == 0) return(2)
  return(0)
}

NNvsNN2Games <- function(first, second){
  # side of first player
  firstPlayerSide <- 1
  
  # side which starts the game
  side <- 1
  
  board <- GenerateEmptyBoard() 
  eval <- 0
  
  # play a game
  while(eval == 0 && IsMovePossible(board)){
    if(side==firstPlayerSide){
      board <- Move(first, board, side)
    } else {
      board <- Move(second, board, side)
    }
    eval <- EvaluateBoard(board)
    side <- side * -1
  }
  
  side <- -1
  board <- GenerateEmptyBoard() 
  eval2 <- 0
  
  # play a game
  while(eval2 == 0 && IsMovePossible(board)){
    if(side==firstPlayerSide){
      board <- Move(first, board, side)
    } else {
      board <- Move(second, board, side)
    }
    eval2 <- EvaluateBoard(board)
    side <- side * -1
  }
  
  # return result
  if(eval+eval2 > 0) return(3)
  if(eval+eval2 == 0) return(2)
  return(0)
}

TrainAI <- function(){  
  #coevolutionary strategy
  populationA <- InitPopulation()
  populationB <- InitPopulation()
  for (i in 1:evolutionIterations ) {
    individualWinsA <- Compete(populationA, populationB)
    individualWinsB <- Compete(populationB, populationA)
    populationA <- NextGeneration(populationA, individualWinsA)
    populationB <- NextGeneration(populationB, individualWinsB)
  }

  # choose best individual from population A and B
  individualWinsA <- Compete(populationA, populationB)
  individualWinsB <- Compete(populationB, populationA)
  winnerA <- which.max(individualWinsA)
  winnerB <- which.max(individualWinsB)
  winner <- populationA[winnerA]
  if(individualWinsB[winnerB] > individualWinsA[winnerA]){
    winner <- populationB[winnerB]
  }
  coevolutionaryWinner = winner[[1]]

  # run games between best coevolutionary individual and random player
  testGames2 <- sapply(1:numberOfTestGames, function(i){ NNvsRandomPlayerGame(coevolutionaryWinner) })
  print("Games between coevolutionary AI and random player")
  print(GetPercentResult(testGames2))

  testGames2
}


GetPercentResult <- function(resTable){
	loses = length(resTable[resTable=="0"])
  wins = length(resTable[resTable=="3"])
  draws = length(resTable[resTable=="2"])
  all = loses + wins + draws
  result <- as.matrix(c(loses, draws, wins, all))
  rownames(result) <- c("Loses", "Draws", "Wins", "All")
  result
}

# TODO: FIX THIS FUNCTION TO SHOW RESULTS NICELY:)
DisplayPercantageResult <- function(percentResult){
	result <- sapply(percentResult, function(i){ paste( toString(i), "%" )})
	result <- t(result)
	colnames(result) <- c("Loses", "Draws", "Wins")
	t(result)
}

SetParallel <- function(){
  library(doParallel)
  library(plyr)
  
  nodes <- detectCores()
  cl <- makeCluster(nodes)
  registerDoParallel(cl)
  
  aaply(ozone, 1, mean,.parallel=TRUE)
  
  stopCluster(cl)
}
	
	