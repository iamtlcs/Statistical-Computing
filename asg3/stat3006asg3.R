# Q1
rm(list = ls())

N = 2000 # iteration number
lambda_t = rep(NA, N) # the parameter to be inferred
Y_t = matrix(NA, N, 68) # 68 unobserved variables (y_i >= 4)

# Initialization
lambda_t[1] = 1.5
Y_t[1, ] = rep(4, 68)

# Hybrid Gibbs Sampler
accept_num = 0
set.seed(3006)
for (i in 2:N) {
  # sample lambda
  lambda_t[i] = rgamma(1, shape = 432 + sum(Y_t[i-1, ]), rate = 500)
  
  # sample 68 unobserved variables (MH-Step)
  for (j in 1:68) {
    # the proposal distribution of y's
    y_proposal = Y_t[i-1, j] + sample(c(-1, 0, 1), 1)
    # set the accept-reject ratio
    r = min( (lambda_t[i])^(y_proposal - Y_t[i-1, j]) * factorial(Y_t[i-1, j]) / factorial(y_proposal), 1)
    if((runif(1) < r) & (y_proposal >= 4)){
      Y_t[i, j] = y_proposal # accept
      accept_num = accept_num + 1
    }
    else{
      Y_t[i, j] = Y_t[i-1, j] # reject
    }
  }
  
}

accept_ratio = accept_num / (68*N) 

# first 1000 iterations are burn-in
B = 1000
# the estimation for lambda is the posterior mean
estimated_lambda = mean(lambda_t[(B+1):N])

# Q2
rm(list = ls())

# Read Data
Q2Data = read.delim("./Downloads/STAT3006/asg3/41409105.txt", 
                    header = TRUE, sep = " ")

#function used to sample Dirichlet distributed r.v.
rDirichlet <- function(alpha_vec){
  num <- length(alpha_vec)
  temp <- NULL
  for(i in 1:num){
    temp <- c(temp, rgamma(1, shape = alpha_vec[i], rate = 1))
  }
  return(temp/sum(temp))
} 

N = 10000 # iteration number
alpha_t = matrix(NA, N, 3) # alpha values for Dirichlet Distribution
pi_t = matrix(NA, N, 3)
theta_t = matrix(NA, N, 6) # theta_[11, 12, 13, 21, 22, 23] for each iteration
z_t = matrix(NA, N, 1000) # to store Z for each iteration

set.seed(3006)

# initialization
alpha_t[1,] = rep(2, 1*3)
pi_t[1, ] = rDirichlet(alpha_t[1,])
theta_t[1, ] = rbeta(6, 1, 1)

binom_k_1 = pi_t[1, 1] * dbinom(Q2Data[,1], 10 * 1, theta_t[1,1]) * dbinom(Q2Data[,2], 10 * 2, theta_t[1,4])
binom_k_2 = pi_t[1, 2] * dbinom(Q2Data[,1], 10 * 1, theta_t[1,2]) * dbinom(Q2Data[,2], 10 * 2, theta_t[1,5])
binom_k_3 = pi_t[1, 3] * dbinom(Q2Data[,1], 10 * 1, theta_t[1,3]) * dbinom(Q2Data[,2], 10 * 2, theta_t[1,6])

prob_1 = binom_k_1 / (binom_k_1 + binom_k_2 + binom_k_3)
prob_2 = binom_k_2 / (binom_k_1 + binom_k_2 + binom_k_3)
prob_3 = binom_k_3 / (binom_k_1 + binom_k_2 + binom_k_3)

z_df <- data.frame (p1 = prob_1,
                    p2 = prob_2,
                    p3 = prob_3,
                    cluster = NA
)

for (i in 1:1000) {
  z_df[i, 4] = sample(1:3, 1, prob=c(z_df[i,1],z_df[i,2],z_df[i,3]), replace=TRUE)
}

z_t[1, ] = z_df$cluster

for (i in 2:N) {
  alpha_t[i, ] = alpha_t[i-1, ] + c(sum(z_t[i-1, ] == 1), sum(z_t[i-1, ] == 2), sum(z_t[i-1, ] == 3))
  pi_t[i, ] = rDirichlet(alpha_t[i, ])
  
  z = z_t[i-1, ]
  Data = cbind(Q2Data, z)
  
  for (j in 1:2) {
    Data_to_compute = Data[, c(j, 3)]
    for (k in 1:3) {
      sum_x = sum(Data_to_compute[which(Data_to_compute$z == k), 1])
      theta_t[i, (j-1)*3 + k] = rbeta(1, 1+sum_x, 1+10*j*sum(z == k) - sum_x)
    }
  }
  
  binom_k_1 = pi_t[i, 1] * dbinom(Q2Data[,1], 10 * 1, theta_t[i,1]) * dbinom(Q2Data[,2], 10 * 2, theta_t[i,4])
  binom_k_2 = pi_t[i, 2] * dbinom(Q2Data[,1], 10 * 1, theta_t[i,2]) * dbinom(Q2Data[,2], 10 * 2, theta_t[i,5])
  binom_k_3 = pi_t[i, 3] * dbinom(Q2Data[,1], 10 * 1, theta_t[i,3]) * dbinom(Q2Data[,2], 10 * 2, theta_t[i,6])
  
  prob_1 = binom_k_1 / (binom_k_1 + binom_k_2 + binom_k_3)
  prob_2 = binom_k_2 / (binom_k_1 + binom_k_2 + binom_k_3)
  prob_3 = binom_k_3 / (binom_k_1 + binom_k_2 + binom_k_3)
  
  z_df <- data.frame (p1 = prob_1,
                      p2 = prob_2,
                      p3 = prob_3,
                      cluster = NA
  )
  
  for (p in 1:1000) {
    z_df[p, 4] = sample(1:3, 1, prob=c(z_df[p,1],z_df[p,2],z_df[p,3]), replace=TRUE)
  }
  
  z_t[i, ] = z_df$cluster
  
}

B = 2000 # burn-in period
pi_used = pi_t[(B+1):N, ]
estimated_pi = apply(pi_used, 2, mean)

theta_used = theta_t[(B+1):N, ]
estimated_theta = apply(theta_used, 2, mean)

estimated_z = matrix(NA, 1000, 1)
for(j in 1:1000){
  temp <- table(z_t[(B+1):N, j])
  ind <- which.max(temp)
  estimated_z[j] = names(temp)[ind]
}

# plot
dev.off()

plot(theta_t[, 1], type = "l", main="trace plot of theta11",xlab="iterations",ylab="theta11")
abline(v=2000,col="red")

plot(z_t[, 1], type = "l", main="trace plot of Z1",xlab="iterations",ylab="Z1")
abline(v=2000,col="red")

plot(pi_t[ ,1], type = "l", main="trace plot of pi1",xlab="iterations",ylab="pi1")
abline(v=2000,col="red")

# Q3
rm(list = ls())
set.seed(3006)

N = 10000 #iteration number
p_t = matrix(NA, N, 4) # prob. for multinomial dist.
y_t = matrix(NA, N, 4) # col 1 for y12, 2 for y13, 3 for y22, 4 for y24

rDirichlet <- function(alpha_vec){
  num <- length(alpha_vec)
  temp <- NULL
  for(i in 1:num){
    temp <- c(temp, rgamma(1, shape = alpha_vec[i], rate = 1))
  }
  return(temp/sum(temp))
}

# initialization
p_t[1, ] = rDirichlet(c(2,2,2,2))
y_t[1, ] = c(9, 35, 9, 30)

# Hybrid Gibbs Sampler
accept_num = 0
for (i in 2:N) {
  # sample p's
  p_t[i, ] = rDirichlet(c(99, 2+y_t[i-1, 1]+y_t[i-1, 3], 17+y_t[i-1, 2], 7+y_t[i-1, 4]))
  
  # sample unobserved y's
  # sample yi2 (i = 1,2)
  for (j in c(1,3)) {
    if (j==1){
      c = 35
    }
    if (j==3){
      c = 30
    }
    # get a proposal distribution depends on its last iteration
    if (y_t[i-1, j] == 9){
      y_proposal = y_t[i-1, j] + 1
    }
    if (y_t[i-1, j] == c){
      y_proposal = y_t[i-1, j] - 1
    }
    if ( (y_t[i-1, j]>9) & (y_t[i-1, j]<c) ){
      y_proposal = y_t[i-1, j] + sample(c(-1, 1), 1, prob = c(1/2, 1/2))
    }
    
    p_2 = p_t[i, 2]
    p_3 = p_t[i, 3]
    p_4 = p_t[i, 4]
    
    # MH step ratio
    if (j==1){
      orig_ratio = ( (p_2^y_proposal * p_3^(44-y_proposal)) / (factorial(y_proposal) * factorial(44-y_proposal)) ) / ( (p_2^ y_t[i-1, j] * p_3^(44-y_t[i-1, j])) / (factorial( y_t[i-1, j]) * factorial(44-y_t[i-1, j])) )
    }
    
    if (j==3){
      orig_ratio =( (p_2^y_proposal * p_4^(48-y_proposal)) / (factorial(y_proposal) * factorial(48-y_proposal)) ) / ( (p_2^ y_t[i-1, j] * p_4^(48-y_t[i-1, j])) / (factorial( y_t[i-1, j]) * factorial(48-y_t[i-1, j])) )
    }
    
    ratio = orig_ratio
    
    if ((y_proposal == c) | (y_proposal == 9)){
      ratio = 2*orig_ratio
    }
    if ((y_t[i-1, j] == c) | (y_t[i-1, j] == 9)){
      ratio = orig_ratio/2
    }
    
    
    r = min ( ratio , 1 )
    
    if(runif(1) < r){
      y_t[i, j] = y_proposal # accept proposal
      accept_num = accept_num + 1
    }else{
      y_t[i, j] = y_t[i-1, j] # reject proposal
    }
    
    
  }
  
  y_t[i, 2] = 44 - y_t[i, 1]
  y_t[i, 4] = 48 - y_t[i, 3]
  
}

accept_ratio = accept_num / (2*N) # to check if the acceptance rate is good enough

B = 2000 # burn-in period
estimated_p = rep(NA, 4)
for (j in 1:4) {
  estimated_p[j] = mean(p_t[(B+1):N, j])
}
