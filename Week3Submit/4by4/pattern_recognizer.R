##list of stable blocks identified by nrows and ncols
#stable44Blocks <- 
 # list(##block
  #matrix(c(rep(0,4),rep(c(0,1,1,0),2),rep(0,4)),nrow=4))
#stable56Blocks <- 
 # list(##beehive
  #  matrix(c(rep(0,8),1,1,0,0,0,1,0,0,1,0,0,0,1,1,rep(0,8)),nrow=5),
    ##aircraft carrier
   # matrix(c(rep(0,7),1,1,rep(0,4),1,0,0,1,rep(0,4),1,1,rep(0,7)),nrow=5))
#stable66Blocks <- 
  #list(##pond
    #matrix(c(rep(0,8),1,1,0,0,rep(c(0,1,0,0,1,0),2),0,0,1,1,rep(0,8)),nrow=6), 
    ##loaf
    #matrix(c(rep(0,8),1,1,0,0,0,1,0,0,1,0,0,0,1,0,1,rep(0,4),1,rep(0,8)),nrow=6))

##list of stable blocks identified by nrows and ncols
    
stable44Blocks <- 
  list(##block
  matrix(c(rep(0,4),rep(c(0,1,1,0),2),rep(0,4)),nrow=4))    
stable56Blocks <- 
  list(##aircraft carrier
    matrix(c(rep(0,7),1,1,rep(0,4),1,0,0,1,rep(0,4),1,1,rep(0,7)),nrow=5))
stable66Blocks <- 
  list(##pond
    matrix(c(rep(0,8),1,1,0,0,rep(c(0,1,0,0,1,0),2),0,0,1,1,rep(0,8)),nrow=6), 
    ##loaf
    matrix(c(rep(0,8),1,1,0,0,0,1,0,0,1,0,0,0,1,0,1,rep(0,4),1,rep(0,8)),nrow=6))    
stable76Blocks <-
  list(##beehive
    matrix(c(rep(0,11),1,1,0,0,0,0,0,1,0,0,0,0,1,0,0,0,0,0,1,1,rep(0,11)),nrow=7))

##list of all stable patterns
stableBlocks <- 
  list(##block
    matrix(c(rep(0,4),rep(c(0,1,1,0),2),rep(0,4)),nrow=4),
    ##beehive
    matrix(c(rep(0,11),1,1,0,0,0,0,0,1,0,0,0,0,1,0,0,0,0,0,1,1,rep(0,11)),nrow=7),
    ##aircraft carrier
    matrix(c(rep(0,7),1,1,rep(0,4),1,0,0,1,rep(0,4),1,1,rep(0,7)),nrow=5),
    ##pond
    matrix(c(rep(0,8),1,1,0,0,rep(c(0,1,0,0,1,0),2),0,0,1,1,rep(0,8)),nrow=6), 
    ##loaf
    matrix(c(rep(0,8),1,1,0,0,0,1,0,0,1,0,0,0,1,0,1,rep(0,4),1,rep(0,8)),nrow=6))


##list of list of oscillating patterns
oscillate55Blocks <- 
  list(##blinker
    list(matrix(c(rep(0,5),rep(c(0,0,1,0,0),3),rep(0,5)),nrow=5),
    ##blinker osc
    matrix(c(rep(0,11),1,1,1,rep(0,11)),nrow=5)))
oscillate66Blocks <-
  list(##beacon
    list(matrix(c(rep(0,7),1,1,rep(0,4),1,1,rep(0,6),1,1,rep(0,4),1,1,rep(0,7)),nrow=6),
    ## beacon osc
    matrix(c(rep(0,7),1,1,rep(0,4),1,rep(0,8),1,rep(0,4),1,1,rep(0,7)),nrow=6)),
    ##toad
    list(matrix(c(rep(0,14),1,1,1,0,0,1,1,1,rep(0,14)),nrow=6),
    ##toad osc
    matrix(c(rep(0,9),1,0,0,0,1,0,0,1,0,0,1,0,0,1,0,0,0,1,rep(0,9)),nrow=6)))

##oscillating blocks
oscillateBlocks <- 
  list(##blinker
    list(matrix(c(rep(0,5),rep(c(0,0,1,0,0),3),rep(0,5)),nrow=5),
      ##blinker osc
      matrix(c(rep(0,11),1,1,1,rep(0,11)),nrow=5)),
    ##beacon
    list(matrix(c(rep(0,7),1,1,rep(0,4),1,1,rep(0,6),1,1,rep(0,4),1,1,rep(0,7)),nrow=6),
      ## beacon osc
      matrix(c(rep(0,7),1,1,rep(0,4),1,rep(0,8),1,rep(0,4),1,1,rep(0,7)),nrow=6)),
    ##toad
    list(matrix(c(rep(0,14),1,1,1,0,0,1,1,1,rep(0,14)),nrow=6),
      ##toad osc
      matrix(c(rep(0,9),1,0,0,0,1,0,0,1,0,0,1,0,0,1,0,0,0,1,rep(0,9)),nrow=6)))

## rotate function for matrix. found from 
## http://stackoverflow.com/questions/16496210/rotate-a-matrix-in-r
rotate <- function(x) t(apply(x, 2, rev))

## this is a function that takes in a board and searches
## for all stable patterns and gliders, returning both
## the predicted board and the portions it "guarantees"
## as correct.
detectPatterns <- function(board){
    nr <- nrow(board)
    nc <- ncol(board)
    resBoard <- matrix(0,nrow=nr,ncol=nc)
    predAreas <- matrix(0,nrow=nr,ncol=nc)
    for(i in 1:length(stableBlocks)){
      pattern <- stableBlocks[[i]]
      for(j in 1:4)
      {
        pattern <- rotate(pattern)
        nrr <- nrow(pattern)
        ncr <- ncol(pattern)
        for(r in 1:(nr-nrr))
        {
          for(c in 1:(nc-ncr))
          {
            if(all(board[r:(r+nrr-1),c:(c+ncr-1)]==pattern))
            {
              resBoard[r:(r+nrr-1),c:(c+ncr-1)] <- pattern
              predAreas[(r):(r+nrr-1),(c):(c+ncr-1)] <- matrix(c(rep(1,(nrr)*(ncr))),nrow=nrr)
            }
          }
        }
      }
    }
    ## now the oscillating blocks
    for(i in 1:length(oscillateBlocks)){
      patterns <- oscillateBlocks[[i]]
      l = length(patterns)
      for(j in 1:4)
      {
        for(k in 1:l)
        {
          patterns[[k]] <- rotate(patterns[[k]])
        }
        nrr <- nrow(patterns[[1]])
        ncr <- ncol(patterns[[1]])
        for(r in 1:(nr-nrr))
        {
          for(c in 1:(nc-ncr))
          {
            for(k in 1:l)
            {
              if(all(board[r:(r+nrr-1),c:(c+ncr-1)]==patterns[[k]]))
              {
                resBoard[r:(r+nrr-1),c:(c+ncr-1)] <- patterns[[((k %% l)+1)]]
                predAreas[(r):(r+nrr-1),(c):(c+ncr-1)] <- matrix(c(rep(1,(nrr)*(ncr))),nrow=nrr)
              }
            }
          }
        }
      }
    }
    return(list("resBoard" = resBoard, "predAreas" = predAreas))
}


predictStableBoard <- function(board.vec,steps){
  board <- matrix(board.vec,nrow=20,ncol=20)
  res<- detectPatterns(board)
  as.vector(res[[1]])
}