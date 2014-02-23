## helper function to display memory usage
.ls.objects <- function (pos = 1, pattern, order.by = "Size", decreasing=TRUE, head = TRUE, n = 10) {
  # based on postings by Petr Pikal and David Hinds to the r-help list in 2004
  # modified by: Dirk Eddelbuettel (http://stackoverflow.com/questions/1358003/tricks-to-manage-the-available-memory-in-an-r-session) 
  # I then gave it a few tweaks (show size as megabytes and use defaults that I like)
  # a data frame of the objects and their associated storage needs.
  napply <- function(names, fn) sapply(names, function(x)
    fn(get(x, pos = pos)))
  names <- ls(pos = pos, pattern = pattern)
  obj.class <- napply(names, function(x) as.character(class(x))[1])
  obj.mode <- napply(names, mode)
  obj.type <- ifelse(is.na(obj.class), obj.mode, obj.class)
  obj.size <- napply(names, object.size) / 10^6 # megabytes
  obj.dim <- t(napply(names, function(x)
    as.numeric(dim(x))[1:2]))
  vec <- is.na(obj.dim)[, 1] & (obj.type != "function")
  obj.dim[vec, 1] <- napply(names, length)[vec]
  out <- data.frame(obj.type, obj.size, obj.dim)
  names(out) <- c("Type", "Size", "Rows", "Columns")
  out <- out[order(out[[order.by]], decreasing=decreasing), ]
  if (head)
    out <- head(out, n)
  out
}

## get the index of a cell on the board
## when the board is vectorized
getIndex = function(row, column, width = 20) {
  return(((column-1) %% width)*width + ((row-1) %% width) + 1);
}

## get the row and column of a cell given
## the index of a vectorized board
getPos = function(index, width = 20) {
  row = (index - 1) %% width + 1;
  col = (index - 1) %/% width + 1;
  return(c(row,col));
}

## shift a vector circularly
shiftVector = function(vec,shift, direction = "right") {
  N = length(c(vec));
  if (direction == "right") {
    return(vec[c((N-shift+1):N,1:(N-shift))]);
  } else {
    return(vec[c((shift+1):N,1:shift)]);
  }
}

## shift a matrix circularly up
shiftUp = function(matrix,n=1) {
  Nrow = nrow(matrix);
  return(matrix[c((n+1):Nrow,1:n),]);
}

## shift a matrix circularly down
shiftDown = function(matrix,n=1) {
  Nrow = nrow(matrix);
  return(matrix[c((Nrow-n+1):Nrow,1:(Nrow-n)),]);
}

## shift a matrix circularly left
shiftLeft = function(matrix,n=1) {
  Ncol = ncol(matrix);
  return(matrix[,c((n+1):Ncol,1:n)]);
}

## shift a matrix circularly right
shiftRight = function(matrix,n=1) {
  Ncol = ncol(matrix);
  return(matrix[,c((Ncol-n+1):Ncol,1:(Ncol-n))]);
}

## extract d-by-d squares from the board as features
## NOTE: setting "with_border = TRUE" will pad a border
## of dead cells around the board.
getNeighbors = function(board,delta = 1,width = 20, with_border = TRUE,border = 0) {
  # add a border of 0's around the board
  if (with_border) {
    new_board = matrix(border,width+2*delta,width+2*delta);
    new_board[(1+delta):(width+delta),(1+delta):(width+delta)] = matrix(board,width,width);
    board = as.numeric(new_board);
    width = width+2*delta;
  }
  
  features = matrix(0,length(board),(delta*2+1)^2);
  for (row_offset in -delta:delta) {
    for (col_offset in -delta:delta) {
      temp = matrix(board,width,width);
      if (row_offset < 0) {
        temp = shiftUp(temp,abs(row_offset));
      } else if (row_offset > 0) {
        temp = shiftDown(temp,abs(row_offset));
      }
      
      if (col_offset < 0) {
        temp = shiftLeft(temp,abs(col_offset));
      } else if (col_offset > 0) {
        temp = shiftRight(temp,abs(col_offset));
      }
      features[,getIndex(row_offset+delta+1,col_offset+delta+1,width = delta*2+1)] = 
        as.numeric(temp);
    }
  }
  
  if (with_border) {
    inner_index = as.numeric(matrix(1:(width^2),width,width)[(1+delta):(width-delta),(1+delta):(width-delta)]);
    features = features[inner_index,];
  }
  return(features);
}

## stack all features from different boards into a big matrix
## NOTE: getNeighbors is a function that takes in a board
## and return the features 
stackFeatures = function(boards, FUN = getNeighbors, ...) {
  N = nrow(boards);
  featureFirst = FUN(boards[1,], ...);
  frow = nrow(featureFirst);
  fcol = ncol(featureFirst);
  featureAll = matrix(0,N * frow,fcol);
  featureAll[1:frow,] = featureFirst;
  
  for (i in 1:(N-1)) {
    featureAll[(i*frow+1):(i*frow+frow),] = FUN(boards[i+1,], ...);
  }
  return(featureAll);
}

# train the model to get the average state of the cells 
# corresponding to each d-by-d square pattern
getBin = function(features.train, label.train, gridWidth = 1) {
  Ntrain = round(length(label.train) / 400);
  patch_size = (gridWidth*2+1)^2;
  base2 = 2^((patch_size-1):0);
  train.pattern = features.train %*% base2 + 1;
  labels.train = stackFeatures(matrix(label.train,ncol=400,byrow=TRUE),getNeighbors,delta=gridWidth,width=20);
  
  index_board = getNeighbors(1:400,delta=gridWidth,width=20,with_border=TRUE,border=0);
  indices = matrix(0,400,patch_size);
  for (i in 1:400) {
    for (j in 1:patch_size) {
      index_val = which(index_board[,j] == i);
      if (length(index_val) != 0) {
        indices[i,j] = index_val;
      } else {
        indices[i,j] = 0;
      }
    }
  }
  
  binList = list();
  for (i in  1:patch_size) {
    trainIndex = rep(indices[,i],Ntrain);
    patternAgg = aggregate(labels.train[which(trainIndex != 0),i],
                           list(train.pattern[which(trainIndex != 0)]),mean);
    binList[[i]] = rep(0,sum(base2)+1);
    binList[[i]][patternAgg$Group.1] = patternAgg$x;
    #   plot(patternAgg$Group.1,patternAgg$x);
  }
  return(binList);
}

## get the predicted state of the cells given the features
## and the "binList" obtained from the training data
getBinPrediction = function(features.train,binList,gridWidth=1) {
  predict.raw = rep(0,nrow(features.train));
  patch_size = (gridWidth*2+1)^2;
  base2 = 2^((patch_size-1):0);
  train.pattern = features.train %*% base2 + 1;
  index_board = getNeighbors(1:400,delta=gridWidth,width=20,with_border=TRUE,border=0);
  indices = matrix(0,400,patch_size);
  for (i in 1:400) {
    for (j in 1:patch_size) {
      index_val = which(index_board[,j] == i);
      if (length(index_val) != 0) {
        indices[i,j] = index_val;
      } else {
        indices[i,j] = 0;
      }
    }
  }
  for (k in 1:round(nrow(features.train)/400)) {
    patterns = train.pattern[((k-1)*400+1):(k*400)];
    if (k == 1) {
      patternCount = rep(0,400);
    }
    for (i in 1:patch_size) {
      addIndices = which(indices[,i] != 0);
      if (k == 1) {
        patternCount[addIndices] = patternCount[addIndices] + 1;
      }
      predict.raw[((k-1)*400+1):(k*400)][addIndices] = 
        predict.raw[((k-1)*400+1):(k*400)][addIndices] + binList[[i]][patterns[indices[addIndices,i]]];
    }
    predict.raw[((k-1)*400+1):(k*400)] = predict.raw[((k-1)*400+1):(k*400)] / patternCount;
  }
  return(predict.raw);
}

## get the prediction of 1-step backward
get1Step = function(features.test,binList,gridWidth=1,thres=0.5,asraw=FALSE) {
  predict.test.raw = getBinPrediction(features.test,binList,gridWidth);
  if (asraw) {
    return(predict.test.raw);
  } else {
    return(as.numeric(predict.test.raw > thres));
  }
}

## get the mean error given the raw prediction and the threshold(s)
getAccuracy = function(predict.raw,label,thresVector=0.5) {
  for (thres in thresVector) {
    predict.class = as.numeric(predict.raw > thres);
    cat("---------------------\n","THRESHOLD = ",thres,"\n");
    cat(prop.table(table(label == predict.class)),"\n");
  }
}

##list of all stable patterns
stableBlocks <- 
  list(##block
    matrix(c(rep(0,4),rep(c(0,1,1,0),2),rep(0,4)),nrow=4),
    ##beehive
    matrix(c(rep(0,8),1,1,0,0,0,1,0,0,1,0,0,0,1,1,rep(0,8)),nrow=5),
    ##aircraft carrier
    matrix(c(rep(0,7),1,1,rep(0,4),1,0,0,1,rep(0,4),1,1,rep(0,7)),nrow=5),
    ##pond
    matrix(c(rep(0,8),1,1,0,0,rep(c(0,1,0,0,1,0),2),0,0,1,1,rep(0,8)),nrow=6), 
    ##loaf
    matrix(c(rep(0,8),1,1,0,0,0,1,0,0,1,0,0,0,1,0,1,rep(0,4),1,rep(0,8)),nrow=6))


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
              resBoard(r:(r+nrr-1),c:(c+ncr-1)) <- patterns[[((k %% l)+1)]]
              predAreas((r):(r+nrr-1),(c):(c+nrc-1)) <- matrix(c(rep(1,(nrr)*(ncr))),nrow=nrr)
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