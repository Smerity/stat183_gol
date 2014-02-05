source("~/Development/stat183/packages/gol_simulator/gol_simulator.R")

## build a distribution for a point in the grid
build_distribution = function(
  L= 3,             # size of the classifying grid
  n_row = 20,       # number of rows
  n_col = 20,       # number of columns
  N_steps = 1,            # number of time steps
  point = c(9,10),  # the point that we want to consider
  N_samples = 1e3,  # the number of sample games to play 
  save_file = TRUE, # toggle for saving the file to disk 
  ##
  file_name = "p_dist.Rda",   # name of the file
  file_path = "data/"                # path to the file 
)
{
  source("~/Development/stat183/packages/gol_simulator/gol_simulator.R")
  require(stringr)
  
  # check if the point is in the grid
  if( L %% 2 == 0) stop("L must be odd")
  
  # select the intervals of the subgrid to analyze
  i_row = max(point[1]-floor(L/2), 1):min(point[1]+floor(L/2), n_row)
  i_col = max(point[2]-floor(L/2), 1):min(point[2]+floor(L/2), n_col)
  
  ## initialize data frame for storing results
  p_dist = data.frame(name = character(), is_on = integer(), count = integer())
  
  for( i in 1:N_samples)
  {
    ## simulate a board
    set.seed(i)
    p = runif(1)
    X = gol_sim(n_row= n_row, n_col = n_col, N_steps=N_steps+5, prob = p, graphics = FALSE)
    if( sum(X) == 0) next
    
    ## print out status updates
    if( i %% 1e3 == 0) cat("\n", i , "complete\n")
    
    ## select a subblock of cose
    is_on      = X[point[1], point[2], 5]
    X_subset_0 = X[i_row, i_col,5]
    X_subset_N = X[i_row, i_col,N_steps+5]
    
    # classify end board
    # flatten the matrix, by row
    tag   = str_replace_all( toString(as.integer(X_subset_N)), ", ", "")
    
    index = which(p_dist$name == tag)
    
    if( length(index) == 0)
    {
      p_dist = rbind(p_dist,  data.frame(name = tag, is_on = is_on, count = 1 ))
    } else
    {
      p_dist$is_on[index] = p_dist$is_on[index]+ is_on
      p_dist$count[index] = p_dist$count[index]+ 1
    }
  }
  
  prob = with(p_dist, is_on/count)
  p_dist = cbind(p_dist, prob = prob)
  
  # save the data frame
  if( save_file)
  {
    path_name = paste0(file_path, file_name)
    saveRDS(p_dist, file = path_name)
  }
  return(p_dist)
  
}

# build 5 support tables, one for each number of steps
build_dists = function( N_samples = 1e3, L = 3)
{
  # initalize list of distributions
  prob_dist = list()
  for(N in 1:5)
  {
    cat("\nN = ", N)
    dist_name = sprintf("D%i_%1.0e", N, N_samples)
    prob_dist[[N]] =  build_distribution( L = L, N_samples = N_samples, N_steps = N, file_name = dist_name)
  }
  
  prob_dist = c(prob_dist, L = L)
}

# build 5 support tables, one for each number of steps
load_dists = function( size = "1e6", path = "data/", L = 3 )
{
  # initalize list of distributions
  prob_dist = list()
  for(N in 1:5)
  {
    dist_name = sprintf("D%i_%s",N, size)
    cat("\nloading:", dist_name)
    prob_dist[[N]] = readRDS(paste0(path, dist_name))
  }
  
  prob_dist = c(prob_dist, L = L)
  return(prob_dist)
}

pred_start_board = function(X_end, N_steps = 1, prob_dist, verbose = FALSE)
{
  L = prob_dist$L
  
  if(verbose){ cat("\n L = ", L)}
  X_start = matrix(0, 20,20)
  
  # double loop over the board
  for( row in 1:20)
  {
    for( col in 1:20)
    { 
      if(verbose){cat("\nrow =", row, "col = ", col)}
      # build interval
      i_row = max(row-floor(L/2), 1):min(row+floor(L/2), 20)
      i_col = max(col-floor(L/2), 1):min(col+floor(L/2), 20)
      
      if(verbose)
      {
        cat("\ni_row = ", i_row)
        cat("\ni_col = ", i_col)
      }
      
      # consider row edges
      X_sub   =  X_end[i_row, i_col]
      if( row == 1) 
      {
        X_sub = rbind( matrix(0, ncol = ncol(X_sub), nrow = floor(L/2)), X_sub)
      }
      else if( row == 20)
      {
        X_sub = rbind(X_sub, matrix(0, ncol = ncol(X_sub), nrow = floor(L/2)) )
      }
      
      # consider column edges
      if( col == 1 )
      {
        X_sub = cbind( matrix(0, nrow = L, ncol = floor(L/2)), X_sub)
      }
      else if( col == 20)
      {
        X_sub = cbind(X_sub, matrix(0, nrow = L, ncol = floor(L/2)) )
      }
      
      # flatten the matrix, by row
      tag   = str_replace_all( toString(as.integer(X_sub)), ", ", "")
      
      index = with(prob_dist[[N_steps]], which(name == tag))
      if(verbose)
      {
        cat("\nindex = ", index)
        cat("\nlength(index) = ", length(index))
      }
      if( length(index) == 0)
      {
        if(verbose){cat("\nno match, setting value to zero")}
        X_start[row,col] = 0
      } 
      else
      {
        prob  = with(prob_dist[[N_steps]], prob[index])
        X_start[row, col] = ifelse(prob < .7, 0, 1)
      }
    } # end col loop
  } # end row loop
  
  return(X_start)
}