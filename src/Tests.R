# This file contains all the tests performed


# This test runs TrainAI with iterations number from 1 to max
HiddenLayerSize_Test <- function(max){
	result <- list()
	for (i in 1:max) {
		evolutionIterations <- i
		result[[i]] <- TrainAI()
	}
	result
}

# This test runs TrainAI with gameNumberPerIndividual from 1 to max
gameNumberPerIndividual_Test <- function(max){
  result <- list()
  for (i in 1:max) {
    gameNumberPerIndividual <- i
    result[[i]] <- TrainAI()
  }
  result
}

# This test runs TrainAI with populationSize from 1 to max
populationSize_Test <- function(max){
  result <- list()
  for (i in 1:max) {
    populationSize <- i
    result[[i]] <- TrainAI()
  }
  result
}

