cat("Reading in the Conway data\n")
####
train <- read.csv(file="data/train.csv", header=TRUE, sep=",")

mark_gol = function(guess, gold, total=400) {
  incorrect <- sum(xor(guess, gold))
  return(incorrect / total)
}

results <- c()
cols <- ncol(train)
for (i in 1:nrow(train)) {
  # Ignore id and delta
  pixels <- train[i, 3:cols]
  #
  start <- pixels[1:400]
  end <- pixels[401:800]
  #
  results <- append(results, mark_gol(start, end))
  #results <- append(results, mark_gol(train[i, 3:13], train[i, 15:25]))
  if (i %% 100 == 0) {
    cat(i, "\n")
    print(mean(results))
  }
}
print(mean(results))
