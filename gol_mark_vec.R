# This version uses both a JIT and vectorized commands
# This takes execution time from 56 minutes to 18 seconds
# Unfortunately, constructing the equivalent of "train" will be quite slow...
require(compiler)
enableJIT(3)

cat("Reading in the Conway data\n")
###
train <- read.csv(file="data/train.csv", header=TRUE, sep=",")
cat("Finished reading in data\n")

mark_gol = function(guess, gold, total=400) {
  incorrect <- sum(xor(guess, gold))
  return(incorrect / total)
}

train <- train[, 3:ncol(train)]
starts <- train[, 1:400]
ends <- train[, 401:800]

# As a baseline, compare end to start
print(mark_gol(starts, ends, total=20000000))
# As a baseline, compare starting to "all off"
print(mark_gol(starts, ends * 0, total=20000000))
