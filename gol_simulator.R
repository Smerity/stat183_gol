find_neighbors = function(X)
{
    # Generate a matrix that gives the number of neighbors for each 
    # entry in X, including diagonal entries.  ranges from 1:8
    n_row = nrow(X)
    n_col = ncol(X)
    
    if( n_row < 2 || n_col < 2)
    {
      stop("n_row or n_col is less than 2, make them bigger")
    }

    # add zeros on top (aka "north")
    X_N   = rbind( rep(0, n_col),   X[-n_row, ])
    X_E   = cbind( X[ , -1],  rep(0, n_col)) 
    X_S   = rbind( X[-1, ],   rep(0, n_col)) 
    X_W   = cbind( rep(0, n_col),    X[,-n_col])
   
    # add a row of zero top and left ("north-west")
    X_NW  = rbind( rep(0, n_col), cbind( rep(0, n_col-1),  X[-n_row,-n_col]))
    X_SE  = cbind( rbind( X[-1,-1], rep(0, n_col-1)), rep(0, n_col) )
    X_NE  = rbind( rep(0, n_col), cbind( X[-n_row,-1], rep(0, n_col-1)))
    X_SW  = cbind( rep(0, n_col), rbind( X[-1,-n_col], rep(0, n_col-1)))
   
    # compute neighbors
    N = X_N + X_S + X_E + X_W + X_NW + X_SE + X_NE + X_SW
    return(N)
}

update_board = function(X)
{
    # find the neighbors 
    N = find_neighbors(X)

    # update the board using the logic
    X_update = X
    
    # Any live cell with fewer than two live neighbours dies, as if caused by under-population.
    X_update[X == 1 & N < 2 ] = 0

    # Any live cell with more than three live neighbours dies, as if by overcrowding.
    X_update[X == 1 & N > 3 ] = 0

    # Any dead cell with exactly three live neighbours becomes a live cell, as if by reproduction.
    X_update[X == 0 & N == 3] = 1

    # Any live cell with two or three live neighbours lives on to the next generation.
    # nothing to do
    return(X_update)
}

display_board = function(X)
{ 
    # load lattic for levelplot function
    if(!require(lattice)) stop("lattice package could not be loaded")   

    # define a color palette
    pal  = colorRampPalette(c("lightcyan1", "lightcyan4"))

    # make the plot
    print(levelplot(t(X[nrow(X):1,]), colorkey=FALSE, col.regions=pal, xlab = "Column", ylab = "Row"))
} 

gol_sim = function(
    N_steps    = 10,   # number of steps of evolution 
    n_row      = 20,   # number of rows in the board 
    n_col      = 20,   # number of cols in the board 
    verbose    = FALSE,# TRUE will display print outs of boards 
    graphics   = TRUE, # TRUE will display animation 
    prob       = 0.5,  # probability of the initial cell being occcupied 
    time_delay = 0.25) # time delay between animation frames
{
  
    # check that the rows and/or columns are bigger than 1
    if( n_row < 2 || n_col < 2)
    {
      stop("n_row or n_col is less than 2, make them bigger")
    }
    
    # Generate the board using Bernoulli Random variables Binom(p = prob, n = 1) 
    X0 =  matrix(rbinom(n_row*n_col, prob = prob, size = 1), nrow=n_row, ncol=n_col)
    
    # initalize arrary for storage of the boards
    X_stored      =  array(0, c(n_row, n_col, N_steps))
    X_stored[,,1] = X0
    
    # print the initial X 
    if(verbose)
    { 
        printf("\n step = 1, X = \n") 
        print(X0) 
    }
    
    # show the initial board
    if(graphics) display_board(X0)
    
    # evolve the board
    if( N_steps > 1)
    {
        for( i in 2:N_steps)
        {
            # print previous board  
            if(verbose)
            { 
                printf("\n step = %i, X = \n", i)
                print(X) 
            }
        
            # store the updated board
            X_stored[,,i]  = update_board(X_stored[,,i-1])
       
            # display boards
            if(graphics)
            {   
                display_board(X_stored[,,i])
                Sys.sleep(time_delay)
            }
        
        } # evolution loop
    } # N_steps conditional
    return(X_stored)
}



