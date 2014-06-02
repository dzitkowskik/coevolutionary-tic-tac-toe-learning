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


# Eval - function to minimize
EvolutionOptim <- function(EvalFunc){

}