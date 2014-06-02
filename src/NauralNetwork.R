neededForEval <- list("neurons", "NNvsRandomPlayer", "GenerateEmptyBoard", "board.size",
          "IsMovePossible", "Move", "RunAI", "InitNN", "EvaluateBoard", "RunNN", "winningSeries",
          "FlipMatrix", "EvaluateBoardSmall", "GetSubBoard")


neurons <- 1

# Creating an empty neural network which we represent as a matrix of weights
InitNN <- function(){
  n.weights <- neurons * board.size ^ 2 + 2 * neurons
  matrix(runif(n.weights),nrow=neurons)
}

# Calculating the network
RunNN <- function(nn, board){
  w.out <- nn[,1]
  w0.in <- nn[,2]
  w.in <- nn[,3:ncol(nn)]
  t(w.out) %*% tanh(w.in %*% as.vector(board) + w0.in)
}


# Evaluating every move possible using the network
RunAI <- function(ai, board, side){
  res <- sapply(1:length(board), function(i){
    b <- board
    b[[i]] <- side
    RunNN(ai,b) 
  })
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
  Eval <- function(w){
    tic.ai <- matrix(w, nrow=neurons)

    # Playing against a random player is nondeterministic, so we need to stabilise the results
    ev <- median(sapply(1:20,function(j)mean(sapply(1:20, function(i)NNvsRandomPlayer(tic.ai)))))

    ev <- -1*(ev)
  }

  len <- length(InitNN())

  # This is a global optimisation method, so using we need an appropriate method - a differential evolution 
  # algorithm seems sufficient
  res <- DEoptim::DEoptim(Eval, rep(-0.1,len), rep(0.1,len), 
      DEoptim::DEoptim.control(trace=1, parallelType=1, NP=20, VTR=-1.0, parVar=neededForEval))

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
	
	