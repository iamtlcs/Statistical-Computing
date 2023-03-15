#Q1
f <- function(x) {
  return (x^3 + 5.9*x^2 - 32.46*x- 29.97)
}

bisection_f <- function(a, b) {
  if (a*b>0) {
    return ("a*b < 0 is not fulfilled")
  }
  epsilon <- 0.005
  t <- 0
  while (abs(a - b) > epsilon) {
    t <- t + 1
    c <- (a+b)/2
    if (f(c)*f(a) < 0) {
      b <- c
    } else if (f(c)*f(b) < 0) {
      a <- c
    }
  }
  return (c(a, b, c, t))
}

#Consider I : [-20, 20]
bisection_f(-20, 20)
#Consider I : [0, 20]
bisection_f(0, 20)
#Consider I : [-10, 0]
bisection_f(-10, 0)
#Consider I : [-5, 0]
bisection_f(-5, 0)

# Roots are 4.018555, -9.106445, -0.8154297

#Q2
x <- "-0.30 0.32 0.41 0.62 -0.21 0.31 0.41 0.81 0.50 -0.21 -0.20 0.70 0.10 0.13 0.69"
y <- "1 8 5 23 1 8 9 33 12 1 2 19 6 6 23"
x <- as.numeric(unlist(strsplit(x, " ")))
y <- as.numeric(unlist(strsplit(y, " ")))

poi_reg_newton <- function(a, b, t) {
  J <- c(0,0,0,0)
  nabla <- c(0,0)
  result <- c(a, b)
  for (k in 1:t) {
    q <- result[1] + x*result[2]
    J[1] <- J[1] - sum(exp(q))
    J[2] <- J[2] - sum(x*exp(q))
    J[3] <- J[3] - sum(x*exp(q))
    J[4] <- J[4] - sum(x^2*exp(q))
    nabla[1] <- nabla[1] - sum(exp(q)) + sum(y)
    nabla[2] <- nabla[2] - sum(x*exp(q)) + sum(x*y)
    Jmat <- matrix(J, nrow = 2, ncol = 2, byrow = TRUE)
    J_inv <- solve(Jmat)
    result <- result - J_inv %*% nabla
  }
  return (result)
}
result <- poi_reg_newton(1, 1, 100000)
glm(y ~ x, family = poisson)

#Q3
x <- "-0.30 0.32 0.41 0.62 -0.21 0.31 0.41 0.81 0.50 -0.21 -0.20 0.70 0.10 0.13 0.69"
y <- "0 1 1 0 0 0 0 1 0 0 0 1 0 0 1"
x <- as.numeric(unlist(strsplit(x, " ")))
y <- as.numeric(unlist(strsplit(y, " ")))

logit_reg_newton <- function(a, b, t) {
  J <- c(0,0,0,0)
  nabla <- c(0,0)
  result <- c(a, b)
  for (k in 1:t) {
    q <- result[1] + x*result[2]
    J[1] <- J[1] - sum(exp(q))
    J[2] <- J[2] - sum(x*exp(q))
    J[3] <- J[3] - sum(x*exp(q))
    J[4] <- J[4] - sum(x^2*exp(q))
    nabla[1] <- nabla[1] - sum(exp(q)) + sum(y)
    nabla[2] <- nabla[2] - sum(x*exp(q)) + sum(x*y)
    Jmat <- matrix(J, nrow = 2, ncol = 2, byrow = TRUE)
    J_inv <- solve(Jmat)
    result <- result - J_inv %*% nabla
  }
  return (result)
}
result <- logit_reg_newton(1, 1, 100000)
glm(y ~ x, family = binomial)

#Q4
height <- read.table("~/Downloads/stat3006/40197366.txt", header = TRUE)
height <- height$height

EM_algo <- function(y, p, mu1, mu2, sigma1, sigma2, t) {
  for (k in 1:t) {
    denom <- p*dnorm(y, mu1, sigma1) + (1-p)*dnorm(y, mu2, sigma2)
    w <- (p*dnorm(y, mu1, sigma1)+2*(1-p)*dnorm(y, mu2, sigma2))/denom
    p <- sum(2-w)/length(y)
    mu1 <- sum((2-w)*y)/sum(2-w)
    mu2 <- sum((w-1)*y)/sum(w-1)
    sigma1 <- sqrt(sum((2-w)*(y-mu1)^2)/sum(2-w))
    sigma2 <- sqrt(sum((w-1)*(y-mu2)^2)/sum(w-1))
  }
  result <- c(p, mu1, mu2, sigma1, sigma2)
  return (result)
}

pred <- function(y, p, mu1, mu2, sigma1, sigma2) {
  denom <- p*dnorm(y, mu1, sigma1) + (1-p)*dnorm(y, mu2, sigma2)
  w <- (p*dnorm(y, mu1, sigma1)+2*(1-p)*dnorm(y, mu2, sigma2))/denom
  return (w)
}

para <- EM_algo(height, p=0.5, mu1=160, mu2=170, sigma1=2, sigma2=2, t = 100000)
learning <- pred(height, para[1], para[2], para[3], para[4], para[5])
#First 10 students' genders
learning[1:10]
#First 10 students' heights
height[1:10]