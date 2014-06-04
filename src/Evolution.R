gameNumberPerIndividual <- 10

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
