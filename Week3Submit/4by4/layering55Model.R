#
#Sys.setenv("PKG_CXXFLAGS"="-fopenmp")
#Sys.setenv("PKG_LIBS"="-fopenmp")
# we use the Rcpp library for C++-to-R integration
library("Rcpp");

# source the C++ helper functions
sourceCpp("layeringCtoR.cpp");

# source the R helper functions
source("gameoflife_functions.R")

args <- commandArgs(trailingOnly = TRUE)
threshold <- as.double(args[1])
print(threshold)

# load the test data
load("test.rds");
#load("data/train__train_only.rds")
#test <- train_only_test

# number of randomly generated training boards
N_BOARD = 5e+06
N_BOARD = 5e+06 * 4
test.boards = as.matrix(test[,3:402]);
test.deltas = as.numeric(test$delta);
rm(test);gc();

testPredTotal = rep(0,400*50000);
totalBlocks <- 0
for (i_offset in 0:4) {
  for (j_offset in 0:4) {
    # only use offsets with Manhattan distance <= 2 from the center
    distToCenter = abs(i_offset-2) + abs(j_offset-2);
    if (distToCenter > 2) {
      next;
    }
    totalBlocks <- totalBlocks + 1;

    # get the raw predictions
    saveFile <- sprintf("patterns/guess_%d_%d.rds", i_offset, j_offset)
    ##
    guesses <- getFrequencyCpp(N_BOARD, i_offset, j_offset)
    save(guesses, file=saveFile)
    ##
    load(file=saveFile)
    ##
    test.pred = getPredictionCpp(guesses, test.boards, test.deltas, i_offset, j_offset);

    # aggregate result
    testPredTotal = testPredTotal + as.numeric(test.pred > 0.5);

    # clean garbage to save memory
    rm(guesses);
    rm(test.pred);
    gc();
  }
}

# normalize prediction to [0,1]
#testPredTotal = testPredTotal / max(testPredTotal);
testPredTotal = testPredTotal / totalBlocks;

# create the submission file
submitName = names(read.csv("sampleSubmission.csv"));
submitData = matrix(0,nrow=50000,ncol=402);
# Threshold is originally 0.45
#submitData[,3:402] = matrix(as.numeric(testPredTotal > threshold), nrow=50000, ncol=400, byrow=TRUE);
submitData[,3:402] = matrix(testPredTotal, nrow=50000, ncol=400, byrow=TRUE);
submitData = submitData[,-2];
names(submitData) = submitName;
write.csv(x=submitData,file="submit_final.csv",row.names=FALSE);
