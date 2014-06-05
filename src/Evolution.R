gameNumberPerIndividual <- 25
mutationProbability <- 0.5
populationSize <- 60
useCrossoverVersion <- 1

# Creates populationSize new individuals for population
# Returns a list of matrices
InitPopulation <- function(){
	lapply(1:populationSize, function(i)InitNN())
}

# Returns sum of points from games for every individual from populationA
# Games are played for each individual from populationA with gameNumberPerIndividual
# random individuals from populationB
Compete <- function(populationA, populationB){

  Play <- function(individual, oponents){
    sum(sapply(oponents, function(x)NNvsNN2Games(individual,x)))
  }

  results <- sapply(populationA, function(i)Play(i,sample(populationB,gameNumberPerIndividual)))
  minResult <- min(results)
  sapply(results, function(x)(x - minResult))
}

GetProbability <- function(x, s) {
  (x+1)/(s+populationSize)
}

# creates new generation
# params: population and number of wins
NextGeneration <- function(population, IndividualWins){
  #add the best of individuals for next generation
  nextGeneration <- list()
  nextGeneration[[1]] <- population[[which.max(IndividualWins)]]

  #crossover
  #1. draw two individuals for every crossover - probabilitiy based on winnings
  #2. draw for every neuron (from two individuals)
  #and get every inpout connection for that neuron from drawn individual
  #eg. I want to create individualNew. In first step i draw individualC and individualE
  #Draw for first neuron of individualNew - drew individualC.
  # => Copy every input connection of first neruonfrom individualC to individualNew
  #same for other neurons...
  prob <- sapply(IndividualWins, FUN=GetProbability, sum(IndividualWins))
  for (i in 2:populationSize){
    drawn <- sample(population, 2, FALSE, prob)
    if(useCrossoverVersion == 2){
      nextGeneration[[i]] <- Mutate(Crossover2(drawn[[1]], drawn[[2]]))
    } else {
      nextGeneration[[i]] <- Mutate(Crossover(drawn[[1]], drawn[[2]]))
    }
  }
  #mutation
  #for every individual draw a neuron (from hidden and output layer)
  #mutate every input connection of that neuron
  
  nextGeneration
}

# Creates a new individual with input connections from hidden layer and input connections 
# from output neurons from individualA or individualB (randomly choosed) each copied neuron 
# stays with his input and output connection weights
Crossover <- function(individualA, individualB){
  test<-individualA
  bool <- c(TRUE, FALSE)
  test[1,] <- sample(bool, hiddenNeuronsCount, TRUE)
  test[1:(2*n*n),] <- apply(test[1:(2*n*n),], 2, function(col){ sapply(col, function(y)col[1]) })
  test[(2*n*n + 1):(3*n*n),1] <- sample(bool, n*n, TRUE)
  test[(2*n*n + 1):(3*n*n),] <- t(apply(test[(2*n*n + 1):(3*n*n),], 1, function(row){ sapply(row, function(y)row[1]) }))
  ifelse(test, individualA, individualB)
}

# Creates a new individual with neurons from hidden layer from individualA
# or individualB (randomly choosed) each copied neuron stays with his input
# and output connection weights
Crossover2 <- function(individualA, individualB){
  test<-individualA
  bool <- c(TRUE, FALSE)
  test[1,] <- sample(bool, hiddenNeuronsCount, TRUE)
  test <- apply(test, 2, function(col){ sapply(col, function(y)col[1]) })
  crossoverResult <- ifelse(test, individualA, individualB)
  NormalizeConnections(crossoverResult)
}


# Chooses one neuron from hidden layer from individual and with a probability
# of mutationProbability mutates every input and output connection
# A sum of input connectios as well as a sum of output connections must always be equal to 1 
Mutate <- function(individual){
  n <- board.size

  # decide whether to mutate
  decision <- runif(1, min=0, max=1)
  if(decision > mutationProbability){
    return(individual)
  }

  # choose neuron
  neuron <- sample(1:hiddenNeuronsCount, 1)

  # mutate
  individual[,neuron] <- matrix(runif(3*n*n, min=0, max=1),ncol=1)
  NormalizeConnections(individual)
}
