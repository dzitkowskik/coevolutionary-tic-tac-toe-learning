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

