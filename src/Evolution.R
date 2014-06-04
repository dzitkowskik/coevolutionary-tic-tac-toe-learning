gameNumberPerIndividual <- 10
mutationProbability <- 0.5

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
    sum(sapply(oponents, function(x)NNvsNNGame(individual,x)))
  }

  sapply(populationA, function(i)Play(i,sample(populationB,gameNumberPerIndividual)))
}

# creates new generation
# params: population and number of wins
NextGeneration <- function(population, IndividualWins){
  #add the best of individuals for next generation
  
  #crossover
  #1. draw two individuals for every crossover - probabilitiy based on winnings
  #2. draw for every neuron (from two individuals)
  #and get every inpout connection for that neuron from drawn individual
  #eg. I want to create individualNew. In first step i draw individualC and individualE
  #Draw for first neuron of individualNew - drew individualC.
  # => Copy every input connection of first neruonfrom individualC to individualNew
  #same for other neurons...
  
  #mutation
  #for every individual draw a neuron (from hidden and output layer)
  #mutate every input connection of that neuron
}

# Creates a new infividual with neurons from hidden layer from individualA
# or individualB (randomly choosed) each copied neuron stays with his input
# and output connection weights
Crossover <- function(individualA, individualB){

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
