set.seed(3006)

# Q1
N <- 5000
i <- 1
X <- NULL
while (i <= N) {
  U <- runif(1)
  X <- c(X, qpois(U, 20))
  i <- i + 1
}
print(X)
hist(X)

# Q2
f <- function(x) {
  return(exp(-x^2/2)/(sqrt(2*pi)*pnorm(-4)))
}

g <- function(x) {
  return(4*exp(4*(x+4)))
}

M <- exp(-8)/(4*sqrt(2*pi)*pnorm(-4))

ratio <- function(x) {
  return(f(Y)/(M*g(Y)))
}

N <- 5000
i <- 1
X <- NULL
while (i <= N) {
  U <- runif(1)
  Y <- -rexp(1, 4) - 4
  if (U <= ratio(Y)) {
    X <- c(X, Y)
    i <- i + 1
  }
}
print(X)
hist(X)

# Q3a
# X stores the samples
N <- 5000
i <- 1
Y <- 0
while (i <= N) {
  Y <- Y + cos(2*X[i]+3)
  i <- i + 1
}
Y <- sqrt(2*pi)*pnorm(-4)*Y/N
print(Y)

# Q3a Short ver.
Y <- mean(cos(2*X+3))*sqrt(2*pi)*pnorm(-4)
print(Y)

#Q3b
N <- 5000
i <- 1
Y <- 0
while (i <= N) {
  X <- -rexp(1, 4) - 4
  Y <- Y + cos(2*X+3)*f(X)/g(X)
  i <- i + 1
}
Y <- sqrt(2*pi)*pnorm(-4)*Y/N
print(Y)

Y <- -rexp(5000, 4) - 4
mean(cos(2*Y+3)*f(Y)/g(Y))*sqrt(2*pi)*pnorm(-4)
