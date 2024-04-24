x <- c(2, 5, 3, 1, 10, 9)

n <- length(x)
szumma <- 0

for (i in 1:n) {
  szumma <- szumma + x[i]
}

atlag <- szumma/n

s <- 0

for (i in 1:n) {
  s <- s + (x[i]-atlag)^2
}

s <- 1/(n-1)*s

print(s)