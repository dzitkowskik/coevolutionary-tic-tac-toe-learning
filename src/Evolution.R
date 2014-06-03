InitPopulation <- function(populationSize){
	lapply(1:populationSize, function(i)InitNN())
}

###########################################
# TODO: Implement this function
###########################################
# Runs a match between two individuals;
# function should return 1 if first won, -1 is second and 0 for draw
EvalFunc <- function(firstIndividual, secondIndividual){

}

# returns number of wins every individual from populationA
Compete <- function(populationA, populationB){
  
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
