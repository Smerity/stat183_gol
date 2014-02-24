require(compiler)
enableJIT(3)
require(R.utils)
library('Rcpp')
Sys.setenv("PKG_CXXFLAGS"="-I/usr/local/boost/1.49.0 -std=c++11")
sourceCpp("gol_R3.cpp")
require(stringr)

DisplayBoard = function(X)
{ 
  # load lattic for levelplot function
  if(!require(lattice)) stop("lattice package could not be loaded")   
  
  # define a color palette
  pal  = colorRampPalette(c("lightcyan1", "lightcyan4"))
  
  # make the plot
  print(levelplot(t(X[nrow(X):1,]), colorkey=TRUE, col.regions=pal, xlab = "Column", ylab = "Row"))
} 


OverlayBoards= function(X1, X2, more = FALSE)
{ 
  # load lattic for levelplot function
  if(!require(lattice)) stop("lattice package could not be loaded")   
  
  # define a color palette
  pal  = colorRampPalette(c("lightcoral", "gray98", "lightcyan3"))
  
  # make the plot
  Z = X1-X2;
  print(levelplot(t(Z[nrow(Z):1,]), colorkey=TRUE, col.regions=pal, xlab = "Column", ylab = "Row"), more = more)
}


error = function(X,Y)
{
  sum(X != Y)/400
}

## find a submatrix of the 20x20 matrix
GetSubMatrix = function(X, row = 1, col = 1,  L = 5)
{
  
  # build interval
  if( L %% 2 != 0)  # odd case
  {
    i_row = max(row-floor(L/2), 1):min(row+floor(L/2), 20)
    i_col = max(col-floor(L/2), 1):min(col+floor(L/2), 20)
  }  
  else # even case  
  {
    i_row = max(row-L/2, 1):min(row+L/2-1, 20)
    i_col = max(col-L/2, 1):min(col+L/2-1, 20)
  }  
  
  # consider row edges
  X_sub   =  X[i_row, i_col]
  if( row == 1) 
  {
    X_sub = rbind( matrix(0, ncol = ncol(X_sub), nrow = ifelse(L %% 2 != 0, floor(L/2), L/2)), X_sub)
  }
  else if( row == 20)
  {
    X_sub = rbind(X_sub, matrix(0, ncol = ncol(X_sub), nrow = ifelse(L %% 2 != 0, floor(L/2), L/2-1)) )
  }
  
  # consider column edges
  if( col == 1 )
  {
    X_sub = cbind( matrix(0, nrow = nrow(X_sub), ncol = floor(L/2)), X_sub)
  }
  else if( col == 20)
  {
    X_sub = cbind(X_sub, matrix(0, nrow = nrow(X_sub), ncol = ifelse(L %% 2 != 0, floor(L/2), L/2-1)) )
  }
  return(X_sub)
}

GetTag = function(X_sub)
{
  sub_string = str_replace_all( toString(as.integer(t(X_sub))), ", ", "")  
  strtoi(sub_string, base = 2)
}

GetTagString = function(X_sub)
{
  sub_string = str_replace_all( toString(as.integer(t(X_sub))), ", ", "")
}

number2binary = function(number, noBits) {
  binary_vector = rev(as.numeric(intToBits(number)))
  if(missing(noBits)) {
    return(binary_vector)
  } else {
    binary_vector[-(1:(length(binary_vector) - noBits))]
  }
}


