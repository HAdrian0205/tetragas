row_sums <- function(m) {
  sums <- numeric(nrow(m))
  for(i in 1:nrow(m)) {
    for(j in 1:ncol(m)) {
      sums[i] <- sums[i] + m[i,j]
    }
  }
  
  return(sums)
}

col_sums <- function(m) {
  sums <- numeric(ncol(m))
  
  for(i in 1:ncol(m)) {
    for(j in 1:nrow(m)) {
      sums[i] <- sums[i] + m[j,i]
    }
  }
  
  return(sums)
}

total_sum <- function(m) {
  sum <- 0
  
  for(i in m) {
    sum <- sum + i
  }
  
  return(sum)
}

expected <- function(m, rows, cols, total_sum) {
  expected_data <- matrix(nrow=2, ncol=3)
  
  for(i in 1:nrow(m)) {
    for(j in 1:ncol(m)) {
      expected_data[i,j] <- rows[i] * cols[j] / total_sum
    }
  }
  
  return(expected_data)
}

chi_square <- function(m, expected_data) {
  chi_square <- 0
  
  for(i in 1:nrow(m)) {
    for(j in 1:ncol(m)) {
      chi_square <- chi_square + (m[i,j] - expected_data[i,j])^2 / expected_data[i,j]
    }
  }
  
  return(chi_square)
}

data <- matrix(nrow=2, ncol=3)
data[1, ] <- c(42, 28, 3)
data[2, ] <- c(17, 89, 21)

print(row_sums(data))
print(col_sums(data))
print(total_sum(data))
print(expected(data, row_sums(data), col_sums(data), total_sum(data)))
print(chi_square(data, expected(data, row_sums(data), col_sums(data), total_sum(data))))