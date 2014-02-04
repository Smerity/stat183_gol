cat("Reading in the Conway data\n")
###
train <- read.csv(file="data/train.csv", header=TRUE, sep=",")

mark_gol = function(guess, gold, total=400) {
  incorrect <- sum(xor(guess, gold))
  return(incorrect / total)
}

results <- c()
cols <- ncol(train)
for (i in 1:nrow(train)) {
  cat(i, "\n")
  # Ignore id and delta
  pixels <- train[i, 3:cols]
  #
  start <- pixels[1:400]
  end <- pixels[401:800]
  #
  results <- append(results, mark_gol(start, end))
  if (i %% 100 == 0) {
    print(mean(results))
  }
}
print(mean(results))
