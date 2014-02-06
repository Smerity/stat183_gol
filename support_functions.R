require(compiler)
enableJIT(3)

source("~/Development/stat183/packages/gol_simulator/gol_simulator.R")

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
  source("~/Development/stat183/packages/gol_simulator/gol_simulator.R")
  require(stringr)
  
  # check if the point is in the grid
  if( L %% 2 == 0) stop("L must be odd")
  
  # select the intervals of the subgrid to analyze
  i_row = max(point[1]-floor(L/2), 1):min(point[1]+floor(L/2), n_row)
  i_col = max(point[2]-floor(L/2), 1):min(point[2]+floor(L/2), n_col)
  
  ## initialize data frame for storing results
  prob_dist = data.frame(name = character(), is_on = integer(), count = integer())
  
  for( i in 1:N_samples)
  {
    ## simulate a board
    set.seed(i)
    p = runif(1)
    X = gol_sim(n_row= n_row, n_col = n_col, N_steps=6, prob = p, graphics = FALSE)
    if( sum(X) == 0) next
    
    ## print out status updates
    if( i %% 1e3 == 0) cat("\n", i , "complete\n")
    
    ## select a subblock of cose
    is_on      = X[point[1], point[2], 5]
    X_subset_1 = X[i_row, i_col,5]
    X_subset_1 = X[i_row, i_col,6]
    
    # classify end board
    # flatten the matrix, by row
    tag   = str_replace_all( toString(as.integer(X_subset_1)), ", ", "")
    
    index = which(prob_dist$name == tag)
    
    if( length(index) == 0)
    {
      prob_dist = rbind(prob_dist,  data.frame(name = tag, is_on = is_on, count = 1 ))
    } else
    {
      prob_dist$is_on[index] = prob_dist$is_on[index]+ is_on
      prob_dist$count[index] = prob_dist$count[index]+ 1
    }
  }
  
  prob = with(prob_dist, is_on/count)
  prob_dist = cbind(prob_dist, prob = prob)
  
  prob_list = list( dist = prob_dist, L = L)
  
  # save the data frame
  if( save_file)
  {
    file_name = paste0("prod_dist", file_extension, ".Rda")
    path_name = paste0(file_path, file_name)
    #saveRDS(prob_list, file = path_name)
    save(prob_list, file = path_name)
  }
  return(prob_list)
  
}

pred_start_board = function(X_end, prob_list, prob_thr = .5,  verbose = FALSE)
{
  L = prob_list$L
  prob_dist = prob_list$dist
  if(verbose)
  { 
    cat("\n\nL = ", L)
    print(prob_dist)
  }
  
  
  X_start = matrix(0, 20,20)
  
  # double loop over the board
  for( row in 1:20)
  {
    for( col in 1:20)
    { 
      if(verbose){cat("\n\nrow =", row, "col = ", col)}
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
      
      index = with(prob_dist, which(name == tag))
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
        prob  = with(prob_dist, prob[index])
        X_start[row, col] = ifelse(prob < prob_thr, 0, 1)
      }
    } # end col loop
  } # end row loop
  
  return(X_start)
}