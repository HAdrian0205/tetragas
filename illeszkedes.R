calc_n <- function(k) {
  sum <- 0
  for(i in 1:length(k)) {
    sum <- sum + k[i]
  }
  return(sum)
}

calc_chi_square <- function(k, p, n) {
  chi_square <- 0
  for(i in 1:length(k)) {
    numerator <- (k[i] - n*p[i])^2
    denominator <- n*p[i]
    chi_square <- numerator/denominator
  }
  return(chi_square)
}

k = c(72, 122, 200, 83, 33, 90)
p = 1/6

chi_square <- calc_chi_square(k,p,calc_n(k))

print(chi_square)