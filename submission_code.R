# The JIT compiler substantially improves the speed
require(compiler)
enableJIT(3)

## The code library initially supplied by Alex, modified by us
source("life_functions.R")
train <- read.csv("data/train.csv")
test <- read.csv("data/test.csv", stringsAsFactors = FALSE)

## Predict board
test.out <- as.matrix(test[, 3:402])
delta <- test[, 2]
for (i in 1:nrow(test)) {
  if (i%%100 == 0) {
    print(i)
  }

  ## predict board takes a board (as a vector) and the number of steps backward
  ## to predict and returns a the predicted board
  test.out[i, ] <- predictBoard(test.out[i, ], steps = delta[i])
}

## Add board id's and make sure column names match the requirements
test.submission <- cbind(test$id, test.out)
colnames(test.submission) <- c("id", colnames(train)[grep("start", colnames(train))])
write.csv(x = test.submission, file = "to_submit.csv", row.names = FALSE)
