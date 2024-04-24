calc_function <- function(x) {
  y <- c()
  a <- 3
  b <- 5
  n <- length(x)
  
  for(i in 1:n) {
    y[i] <- a*x[i]+b
  }
  
  return(y)
}

sum_x <- function(x,h) {
  sum <- 0
  
  for(i in 1:length(x)) {
    sum <- sum + x[i]^h
  }
  
  return(sum)
}

sum_y <- function(y) {
  sum <- 0
  
  for(i in 1:length(y)) {
    sum <- sum + y[i]
  }
  
  return(sum)
}

sum_xy <- function(x,y) {
  sum <- 0
  
  for(i in 1:length(x)) {
    sum <- sum + x[i] * y[i]
  }
  
  return(sum)
}

calc_ab <- function(x,y) {
  sum_x <- sum_x(x,1)
  sum_x_square <- sum_x(x,2)
  sum_y <- sum_y(y)
  sum_xy <- sum_xy(x,y)
  n <- length(x)
  
  b <- (n * sum_xy - sum_x * sum_y) / (n * sum_x_square - sum_x^2)
  a <- (sum_y - b * sum_x) / n
  
  return(list(a,b))
}

x <- 1:5
y <- c(2.9, 5.1, 7.1, 9, 11.2)

print(calc_ab(x,y))