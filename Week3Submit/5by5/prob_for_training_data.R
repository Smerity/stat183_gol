require(compiler)
enableJIT(3)
library('Rcpp')
Sys.setenv("PKG_CXXFLAGS"="-I/usr/local/boost/1.49.0 -std=c++11")
sourceCpp("gol_R3.cpp")
# source("support_functions.R")

# load train data
n_rows = 
train  = read.csv( file = "data/train.csv", header = TRUE, sep=",", nrow = n_rows, stringsAsFactors = FALSE )
train.out  = as.matrix(train[,3:402])

dist_file_name = "data/output_1e+07.RData"
cat('\nLoading distributions from file :', dist_file_name, '\n')
load(dist_file_name)


for( i in 1:n_rows)
{
  if( i %% 100 == 0) cat("\n i = ", i, "\n")
  
  # get the number of steps and the stop board
  delta      = as.numeric(train[i,2])
  stop_board = matrix( as.numeric(train[i,-c(1,2)]), nrow = 20, ncol = 20)  
  
  # initialize the predictions array
  pred_board = PredictBoardProbs(stop_board, prob_dist_5by5, delta)
  
  train.out[i,] = as.vector(pred_board)
}         

## Add board id's and make sure column names match the requirements
train.submission <- cbind(train$id,train.out)
colnames(train.submission) <- c("id",colnames(train)[grep("start",colnames(train))])
write.csv(x=train.submission,file="data/probs_for_train_set.csv",row.names=FALSE)