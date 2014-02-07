require(compiler)
enableJIT(3)

source("gol_simulator.R")
require(R.utils)
require(stringr)


## find a submatrix of the 20x20 matrix
get_submatrix = function(X, row = 1, col = 1,  L = 3)
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

## build a distribution for a point in the grid
build_distribution = function(
  L= 3,             # size of the classifying grid
  n_row = 20,       # number of rows
  n_col = 20,       # number of columns
  point = c(9,10),  # the point that we want to consider
  N_samples = 1e3,  # the number of sample games to play 
  save_file = TRUE, # toggle for saving the file to disk 
  ##
  file_ext = "",       # name of the file
  file_path = "data/"  # path to the file 
)
{

  ## initialize data frame for storing results
  prob_dist = data.frame( character(),  integer(),  integer())
  names(prob_dist) = c("name", "is_on", "count")
  
  for( i in 1:N_samples)
  {
    ## simulate a board
    set.seed(i)
    p = runif(1)
    X = gol_sim(n_row= n_row, n_col = n_col, N_steps=6, prob = p, graphics = FALSE)
    if( sum(X) == 0) next
    
    ## print out status updates
    if( i %% 1e3 == 0) cat("\n", i , "complete\n")
  
    # determine wether board is on from start point
    is_on      = X[point[1], point[2], 5]
    
    ## select a subset of the ending board
    X_stop = get_submatrix(X[,,6], row = point[1], col = point[2], L = L)
    
    # classify end board
    # flatten the matrix, by row
    tag   = str_replace_all( toString(as.integer(X_stop)), ", ", "")
  
    # find entry in the distribution the cooresponds to the given submatrix
    index = which(prob_dist$name == tag)

    # if the length of the index array is zero, then add a new entry to the data frame
    if( length(index) == 0)
    {
      prob_dist = rbind(prob_dist,  data.frame(name = tag, is_on = is_on, count = 1 ))
    }
    # otherwise, increment the counter and update the is_on field
    else
    {
      prob_dist$is_on[index] = prob_dist$is_on[index]+ is_on
      prob_dist$count[index] = prob_dist$count[index]+ 1
    }
  }

  # compute the probability, that given the configuration, the count is on
  prob = with(prob_dist, is_on/count)
  prob_dist = cbind(prob_dist, prob = prob)
  
  # the the distribution and the size of the submatrix in a list to be returned
  prob_list = list( dist = prob_dist, L = L)
  
  # save the data frame
  if( save_file)
  {
    file_name = paste0("prob_dist", file_ext, ".Rda")
    path_name = paste0(file_path, file_name)
    save(prob_list, file = path_name)
  }
  return(prob_list)
}

## make a prediction
# using an error threshold
pred_start_board = function(X_stop, prob_list, prob_thr = 0.5, error_thr = .1, N_max = 50, verbose = FALSE)
{
    # X_stop: the stop board that we are trying to evolve backwards
    # prob_list: list that contains a data.frame (probability distribution) and the size of the submatrix classifier
    # prob threshold: use to set the inital guess of the submatrices
    # error_thr:  the threshold for error we are willing to except
    # N_max:  if the while loop continues and we have not achieved the desired threshold, kill the loop
    # verbose:  TRUE means printouts
    
    # retrieve the size of the submatrix from the prob_list
    L = prob_list$L
    
    # retrieve the prob_dist from the list
    prob_dist = prob_list$dist
    
    # define forward error, that the error the prediction makes when it's evolved forward one step
    # and this compared to the known stop board.
    forward_error_curr = 1  # for each sample, compute the forward error
    forward_error_best = 1  # keep track of the sample with the best error
    X_curr  = matrix(0, 20,20 )  # current prediction using a random throw
    X_best  = matrix(0, 20,20 )  # the best one of the samples 
    
    # initalize the counter
    count = 0;
    while(forward_error_curr > error_thr && count < N_max)
    {
        # keep track of how many iterations
        count = count + 1;
        
        #scan through the board and select sub-matrices
        for( row in 1:20)
        {
            for( col in 1:20)
            {
                # get sub matrix
                X_sub = get_submatrix(X_stop, row = row, col = col, L=L)
                
                # flatten the matrix, by row and cast it to string
                tag   = str_replace_all( toString(as.integer(X_sub)), ", ", "")
                
                # find the index where the tag occures in the probability data-frame
                index = with(prob_dist, which(name == tag))

                # if the sub-matrix does not have an entry in the prob-data frame, 
                # assume the cell is zero (possible mistake here)
                if( length(index) == 0)
                {
                    X_curr[row,col] = 0
                    if(count == 1) X_best[row, col] = 0
                } 
                else
                {
                    # sample the boards using the emperical pdf 
                  prob  = with(prob_dist, prob[index])
                  X_curr[row, col] = rbinom(1, prob = prob, size = 1)

                  # for the first iteration, assume the best is the one given by
                  # using prob > prob_thr (=0.5) to decide
                  if(count == 1) X_best[row, col] = ifelse(prob < prob_thr, 0, 1)
                }
            } # end col loop
        }# end row loop
        
        # evolve prediction and compare with the start board.
        if(count == 1) forward_error_best = sum(as.integer( update_board(X_best) != X_stop))/400
        forward_error_curr = sum(as.integer( update_board(X_curr) != X_stop))/400
        
        if( forward_error_curr < forward_error_best )
        {
            X_best = X_curr
            forward_error_best = forward_error_curr
        } 
    } # end while loop
    
    if(verbose){ cat( '\n\tForward error = ',  forward_error_best, "\t N_throws = ",  count)}
    return(X_best)
}


#
