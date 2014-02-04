require(gtools)
allPos <- permutations(2, 9, c(0, 1), repeats.allowed=T)
neighbours <- rowSums(allPos) - allPos[, 5]
print(neighbours)
