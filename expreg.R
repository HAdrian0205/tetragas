sum_1 <- function(x) {
  sum <- 0
  for (i in 1:length(x)) {
    sum <- sum + x[i]
  }
  return(sum)
}

sum_2 <- function(x,y) {
  sum <- 0
  for (i in 1:length(x)) {
    sum <- sum + x[i]*y[i]
  }
  return(sum)
}

linear_reg <- function(x,y) {
  sum_x <- sum_1(x)
  sum_y <- sum_1(y)
  sum_x2 <- sum_2(x,x)
  sum_xy <- sum_2(x,y)
  
  b = (n * sum_xy - sum_x * sum_y)/(n * sum_x2 - sum_x * sum_x)
  a = (sum_y - b * sum_x)/n
  
  return(list(exp(a),b))
}

x <- 1:5
y <- c(2.9, 5.1, 7.1, 9, 11.2)

print(linear_reg(x,log(y)))
