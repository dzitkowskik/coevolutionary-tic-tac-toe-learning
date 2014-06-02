hiddenNeuronsCount <- 9

# Creating an empty neural network which we represent as a matrix of weights
# Neural network should have (n^2) input neurons, n hidden neurons and (n^2) output neurons
# input and output weights will allways be 1 so we don't store them
InitNN <- function(){
  n <- board.size
  n.weights <- 2*(n*n) + hiddenNeuronsCount
  matrix(runif(n.weights),nrow=1)
}

# Calculating the network
RunNN <- function(nn, board){
  n <- board.size
  # output neuron weights
  x <- n*n
  network.out <- nn[,1:x]
  
  # hidden neuron weights
  x <- x+1
  network.hidden <- nn[,x:(x+hiddenNeuronsCount)]
  
  # input neuron weights
  x <- x + hiddenNeuronsCount
  network.in <- nn[,x:ncol(nn)]

  # calculate output
  result <- network.out * sum(tanh((network.in %*% as.vector(board)) + network.hidden))
  tanh(result)
}


# Running AI to choose best move
RunAI <- function(ai, board, side){
  res <- RunNN(ai,b)

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



NNvsRandomPlayer <- function(tic.ai){
  side <- sample(c(-1,1),1)
  board <- GenerateEmptyBoard() 
  eval <- 0

  while(eval == 0 && IsMovePossible(board)){
    if(side==1){
      board <- Move(tic.ai, board, side)
    } else {
      # Make a valid move completely at random and see what happens
      move <- sample(which(board==0),1)
      board[[move]] <- side
    }
    eval <- EvaluateBoard(board)
    side <- side * -1
  }
  eval
}


TrainAI <- function(){
  # Function to evaluate how good is our network
  Eval <- function(w, side){
    tic.ai <- matrix(w, nrow=neurons)

    # Playing against a random player is nondeterministic, so we need to stabilise the results
    ev <- median(sapply(1:20,function(j)mean(sapply(1:20, function(i)NNvsRandomPlayer(tic.ai)))))

    ev <- -1*(ev)
  }

  len <- length(InitNN())

  # This is a global optimisation method, so using we need an appropriate method - a differential evolution 
  # algorithm seems sufficient
  res <- DEoptim::DEoptim(Eval, rep(-0.1,len), rep(0.1,len), 
      DEoptim::DEoptim.control(trace=FALSE, parallelType=1, NP=20, VTR=-1.0,
       parVar=neededForEval, itermax = 10, storepopfrom = 1, storepopfreq = 2), 1)


  plot(res, plot.type = "storepop")

  #res2 <- DEoptim::DEoptim(Eval, rep(-0.1,len), rep(0.1,len), 
  #    DEoptim::DEoptim.control(storepopfrom=20, trace=1, parallelType=1, NP=20, VTR=-1.0, parVar=neededForEval), 2)

  matrix(res$optim$bestmem, nrow=neurons)
}


GetPercentResult <- function(result, times=100){
	factor <- times/100;
	resTable <- sapply(1:times, function(i)NNvsRandomPlayer(result))
	frequences <- as.data.frame(table(resTable))
	sapply(frequences$Freq, function(i){i/factor})
}

DisplayPercantageResult <- function(res, times=100){
	result <- sapply(GetPercentResult(res, times), function(i){ paste( toString(i), "%" )})
	result <- t(result)
	colnames(result) <- c("Loses", "Draws", "Wins")
	t(result)
}
	
	