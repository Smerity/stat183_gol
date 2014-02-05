source("/Users/kelley/Development/stat183/programs/gol/support_functions.R")

# initlize error matrix
N_test = 100
count = 0
errors = numeric()

## load prob distributions
load("data/prob_dist.Rda")

for(n in 1:N_test)
{
  ## print out status updates
  if(  n %% 1e2 == 0) cat("\n", n , "complete")
  
  # generate a random board
  as.numeric(Sys.time())-> t; 
  set.seed((t - floor(t)) * 1e8 -> seed);
  N_steps = 1
  p = runif(1)
  X = gol_sim(N_steps = N_steps+5, prob = p, graphics = FALSE)
  if( sum(X[,,5])  == 0) next
  
  # get start and end board
  X_start = X[,,5]
  X_end   = X[,,6]
  
  # make a prediction
  X_pred = pred_start_board(X_end, prob_list, prob_thr = 0.8, verbose = FALSE)

  # store error and counter
  count = count + 1
  errors[count] = sum(as.integer( X_pred != X_start))/400
}

cat("\naverage error = ", mean(errors))