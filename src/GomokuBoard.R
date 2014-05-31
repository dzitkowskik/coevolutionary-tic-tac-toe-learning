board.size <- 3	#default size is 3
winningSeries <- 3 #default winning series is 3

# The game board is a matrix with: 1(tic/black, 0(empty), -1(tac/white)
GenerateEmptyBoard <- function(){
  matrix(0, ncol=board.size, nrow=board.size)    
}
 
DisplayBoard <- function(board){
  b <- factor(board, levels=c(-1,0,1),labels=c("X","*","O"))
  dim(b) <- dim(board)
  b
}
 
FlipMatrix <- function(m){
  apply(m, 2, rev)
}
 
# Checking whether somebody has won
# for board size (3 to 4) we need to have (board size) Os or Xs in row
# to win and for board size 5+ we constantly have to have 5 in a row
EvaluateBoard <- function(board){
	if(board.size == winningSeries ){
		return(EvaluateBoardSmall(board))
	}
	N <- winningSeries
	x <- floor(winningSeries/2)
	
	# foreach submatrix of size NxN we check if somebody is winning
	centers <- expand.grid(c((x+1):(board.size-x)),c((x+1):(board.size-x)))
	result <- sum(apply(centers, 1, function(c){ 
		EvaluateBoardSmall(GetSubBoard(c, board)) 
		}))

	return(result)
}

EvaluateBoardSmall <- function(board){
	sums <- c(colSums(board), 
            rowSums(board),
            sum(diag(board)),
            sum(diag(FlipMatrix(board)))
            )
   
  if(max(sums) == winningSeries){
    return(1)
  }
  if(min(sums) == -winningSeries){
    return(-1)
  }
  0
}

# Returns a subboard with size NxN, where N = winningSeries
# with the specified center
GetSubBoard <- function(center, board){
	center = t(center)
	N <- winningSeries
	x <- floor(winningSeries/2)
	board[((center[,1]-x):(center[,1]+x)),((center[,2]-x):(center[,2]+x))]
}

SignMatrix <- function(m)
{
	apply(m, 1:2, function(x){
		if(x < 0){ x = -1 }
		if(x > 0){ x = 1 }
		if(x == 0){ x = 0}
		x
		})
}

ClearMatrix <- function(m)
{
	apply(m, 1:2, function(x){
		x=0
		})
}












