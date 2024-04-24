function_values <- function(x){
  y = c()
  for(i in 1:length(x)){
    y[i] = 5*x[i]*x[i]+3*x[i]+2
  }
  return(y);
}

sum_x <- function(x,h) {
  sum <- 0
  n <- length(x)
  
  for(i in 1:n) {
    sum <- sum + x[i]^h
  }
  
  return(sum)
}

sum_y <- function(y) {
  sum <- 0
  n <- length(y)
  
  for(i in 1:n) {
    sum <- sum + y[i]
  }
  
  return(sum)
}

sum_xy <- function(x,y,h) {
  sum <- 0
  n <- length(x)
  
  for(i in 1:n) {
    sum <- sum + x[i]^h * y[i]
  }
  
  return(sum)
}

calc_det <- function(m) {
  det <- m[1,1] * (m[2,2] * m[3,3] - m[2,3] * m[3,2]) - m[1,2] * (m[2,1] * m[3,3] - m[2,3] * m[3,1]) + m[1,3] * (m[2,1] * m[3,2] - m[2,2] * m[3,1])
  
  return(det)
}

denominator <- function(x) {
  sum_x <- sum_x(x,1)
  sum_x_2 <- sum_x(x,2)
  sum_x_3 <- sum_x(x,3)
  sum_x_4 <- sum_x(x,4)
  n <- length(x)
  
  m <- matrix(nrow=3, ncol=3)
  
  m[1, ] <- c(sum_x_4, sum_x_3, sum_x_2)
  m[2, ] <- c(sum_x_3, sum_x_2, sum_x)
  m[3, ] <- c(sum_x_2, sum_x, n)
  
  return(calc_det(m))
}

calc_a2 <- function(x,y) {
  sum_1 = sum_x(x,1)
  sum_2 = sum_x(x,2)
  sum_3 = sum_x(x,3)
  sum_4 = sum_x(x,4)
  sum_x2y = sum_xy(x,y,2)
  sum_xy = sum_xy(x,y,1)
  sum_y = sum_y(y)
  n <- length(x)
  
  m <- matrix(nrow=3, ncol=3)
  
  m[1, ] <- c(sum_x2y, sum_xy, sum_y)
  m[2, ] <- c(sum_3, sum_2, sum_1)
  m[3, ] <- c(sum_2, sum_1, n)
  
  return(calc_det(m)/denominator(x))
}

calc_a1 <- function(x,y) {
  sum_1 = sum_x(x,1)
  sum_2 = sum_x(x,2)
  sum_3 = sum_x(x,3)
  sum_4 = sum_x(x,4)
  sum_x2y = sum_xy(x,y,2)
  sum_xy = sum_xy(x,y,1)
  sum_y = sum_y(y)
  n <- length(x)
  
  m <- matrix(nrow=3, ncol=3)
  
  m[1, ] <- c(sum_4, sum_3, sum_2)
  m[2, ] <- c(sum_x2y, sum_xy, sum_y)
  m[3, ] <- c(sum_2, sum_1, n)
  
  return(calc_det(m)/denominator(x))
}

calc_a0 <- function(x,y) {
  sum_1 = sum_x(x,1)
  sum_2 = sum_x(x,2)
  sum_3 = sum_x(x,3)
  sum_4 = sum_x(x,4)
  sum_x2y = sum_xy(x,y,2)
  sum_xy = sum_xy(x,y,1)
  sum_y = sum_y(y)
  n <- length(x)
  
  m <- matrix(nrow=3, ncol=3)
  
  m[1, ] <- c(sum_4, sum_3, sum_2)
  m[2, ] <- c(sum_3, sum_2, sum_1)
  m[3, ] <- c(sum_x2y, sum_xy, sum_y)
  
  return(calc_det(m)/denominator(x))
}

x = 1:4
y = function_values(x)

print(denominator(x))

print("a0:")
print(calc_a0(x,y))
print("a1:")
print(calc_a1(x,y))
print("a2:")
print(calc_a2(x,y))