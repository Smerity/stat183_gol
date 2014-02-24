require(compiler)
enableJIT(3)
library('Rcpp')
Sys.setenv("PKG_CXXFLAGS"="-I/usr/local/boost/1.49.0 -std=c++11")
sourceCpp("gol_R3.cpp")
# source("support_functions.R")

# load test data
n_rows = 50000
train  = read.csv(file = "data/train.csv", header = TRUE, nrow = 1)  
test = read.csv( file = "data/test.csv", header = TRUE, sep=",", nrow = n_rows, stringsAsFactors = FALSE )
test.out  = as.matrix(test[,3:402])

dist_file_name = "data/output_1e+07.RData"
cat('\nLoading distributions from file :', dist_file_name, '\n')
load(dist_file_name)


for( i in 1:n_rows)
{
  if( i %% 100 == 0) cat("\n i = ", i, "\n")
  
  # get the number of steps and the stop board
  delta      = as.numeric(test[i,2])
  stop_board = matrix( as.numeric(test[i,-c(1,2)]), nrow = 20, ncol = 20)  
  
  # initialize the predictions array
  pred_board = PredictBoardCombine(stop_board, prob_dist_5by5, prob_dist_3by3, delta)
  
  test.out[i,] = as.vector(pred_board)
}         

## Add board id's and make sure column names match the requirements
test.submission <- cbind(test$id,test.out)
colnames(test.submission) <- c("id",colnames(train)[grep("start",colnames(train))])
write.csv(x=test.submission,file="data/RJSSubmission.csv",row.names=FALSE)