## Build distributions

library("R.utils")
require(compiler)
enableJIT(3)
library('Rcpp')
Sys.setenv("PKG_CXXFLAGS"="-I/usr/local/boost/1.49.0 -std=c++11")
sourceCpp("gol_R3.cpp")

N = 1e4
cat("\nBuilding 3x3 Distribution\n")
prob_dist_3by3 = BuildDistribution3by3(N)

cat("\nBuilding 5x5 Distribution\n")
prob_dist_5by5 = BuildDistribution5by5(N)

cat("\nwriting to file: \n")
file_name = sprintf("data/output_%2.0e.Rdata", N)
cat("file name: ", file_name, "\n\n")
save(file = file_name, list = c("prob_dist_5by5", "prob_dist_3by3"))
