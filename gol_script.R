source("support_functions.R")

# initlize error matrix
N_test = 20
prob_thresholds = 0.5

## load prob distributions
load("data/prob_dist_L3_1e6.Rda")

count = 0
for(i in 1:N_test)
{
  ## print out status updates
  cat("\n\ i = ", i )
  
  # generate a random board
  as.numeric(Sys.time())-> t; 
  set.seed((t - floor(t)) * 1e8 -> seed);
  N_steps = sample(1:5, 1)
  p = runif(1)
  X = gol_sim(N_steps = N_steps+5, prob = p, graphics = FALSE)
  if( sum(X[,,5])  == 0)
  {  
    cat("\n\t Board is empty, throw it out")
    next
  }

  # get true start board
  X_start = X[,,5]

  # initialize the predictions array 
  X_pred = array(0, c(20,20, N_steps+1))
  X_pred[,,N_steps+1] = X[,,N_steps + 5]
  
  for(n in N_steps:1)
  {
    # make a prediction
    X_pred[,,n] = pred_start_board(X_pred[,,n+1], prob_list, prob_thr = prob_thresholds[e], error_thr = .02, 
                                   verbose = TRUE, N_max =5)
  }

  # store error and counter
  count = count + 1
  errors[count] = sum(as.integer( X_pred[,,1] != X_start))/400
  cat("\n\n\tFinal error = ", errors[count])

}
avg_error = mean(errors)
cat("\n\n\tp =",  prob_thresholds[e], "\t error = ", avg_error,"\n")