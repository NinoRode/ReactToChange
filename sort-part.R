suppressPackageStartupMessages({
  library(tidyverse)
  library(microbenchmark)
})

n <- 100  # Number of samples
k <- 2  # How many to select

x <- rnorm(n = n)  # samples

# x_selected <- sort(x, partial = k)
# print(x_selected[])
x_selected <- sort(x, partial = length(x) - k)
print(x_selected[(length(x) - k + 1):length(x)])

microbenchmark(
  sort(x, partial = length(x) - k),
  sort(x, partial = k),
  sort(x), times = 100000
)

