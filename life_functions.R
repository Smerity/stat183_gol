getLiveNeighbors <- function(board,idx){
    ncols <- ncol(board)
    nrows <- nrow(board)

    r.idx <- (idx-1)%%nrows+1
    c.idx <- ceiling(idx/ncols)
    neighbors <- c()
    for(i in -1:1){
        for(j in -1:1){
            if((r.idx+i)>0 & (r.idx+i)<=nrows & (c.idx+j)>0 & (c.idx+j)<=ncols & !(i==0 & j==0)){
                n <- idx+i+nrows*j
                neighbors <- c(neighbors,n)
            }
        }
    }
    sum(board[neighbors])

}

updateState <- function(board,idx){

    ## 0 or 1 live neighbors dies
    ## 2 or 3 live neighbors lives
    ## 4 or more neighbors dies (overcrowding)
    ## Exactly 3 live cells, lives/born

    cur <- board[idx]
    numLive <- getLiveNeighbors(board,idx)
   if(numLive<2){
        state <- 0
    }
    else if(numLive==2){
        if(cur==1)
            state <- 1
        else
            state <- 0
    }
    else if(numLive>=4){
        state <- 0
    }
    else if(numLive==3){
        state <- 1
    }

    state
}

updateBoard <- function(board){
    new.board <- board
    for(i in 1:length(board)){
        new.board[i] <- updateState(board,i)
    }

    new.board
}

## This takes a vector
predictBoard <- function(board.vec,steps){

    board <- matrix(board.vec,nrow=20,ncol=20)
    live <- sum(board)
    if(steps == 1)
    {
      return(board.vec)
    }
    else if(steps>1)
    {
      if (steps==2 & live<50)
      {
        return(board.vec)
      }
      
      else if (steps==3 & live<30)
      {
        return(board.vec)
      }
      
      else if (steps==4 & live<25)
      {
        return(board.vec)
      }
      
      else if (steps==5 & live<15)
      {
        return(board.vec)
      }
      
      else
      {
        new.board <- matrix(0,nrow=20,ncol=20)
        blist <- detectBlocks(board)
        if(length(blist)>0){
          for(b in 1:length(blist)){
            i <- blist[[b]][1]
            j <- blist[[b]][2]
            new.board[i:(i+3),j:(j+3)] <- matrix(c(rep(0,4),rep(c(0,1,1,0),2),rep(0,4)),nrow=4)
          }}
        return(as.vector(new.board))
      }
    }
}

detectBlocks <- function(board){
   blockList <- list()
   nblocks <- 0
   nr <- nrow(board)
   nc <- ncol(board)
   target <- matrix(c(rep(0,4),rep(c(0,1,1,0),2),rep(0,4)),nrow=4)
   for(i in 1:(nc-3)){
       for(j in 1:(nr-3)){
           if(all(board[i:(i+3),j:(j+3)] == target)){
               blockList[[nblocks+1]] <- c(i,j)
               nblocks <- nblocks+1
           }
       }
   }

   blockList
}
