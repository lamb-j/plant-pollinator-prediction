# R interpretation of IFMF, collaborative filtering for implicit feedback systems (Hu, Koren, and Volinsky 2008)
# Applied to HJA plant-pollinator interaction dataset

library(graphics)
source("IFMF_funcs.R")
   # printf
   # conf1
   # conf2
   # MPR
   # print_rec

# dataset
   # subtrainMat2011 (plots 2,5,8)
   # validateMat2011 (plots 3,6,9)
   # trainMat2011    (plots 2,3,5,6,8,9)
   # testMat2011     (plots 1,4,7,10)
load("2011split.RData")

# observation matrix (mxn)
r <- trainMat2011
#r <- subtrainMat2011
rownames(r) <- NULL
colnames(r) <- NULL

#variables
m <- nrow(r)     # number of users (insects)
n <- ncol(r)     # number of items (plants)
alpha  <- 20
epsil  <- 10^-8
lambda <- 0.5    # training parameters
k <- 30           # number of factors

# preference matrix (mxn)
p <- r 
p[p > 0] <- 1

# confidence matrix (mxn)
c <- apply(r, 1:2, conf2, alpha, epsil)
# c <- apply(r, 1:2, conf1, alpha)

# list of user factors (U) and item factors (V)
U <- matrix(0.5, nrow = m, ncol = k) # m x k matrix
V <- matrix(0.5, nrow = k, ncol = n) # k x n matrix

# Compute the user and item factors, sweeping for stability
for (sweep in 1:10) {

  printf("sweep: %d\n", sweep)
  
  
  # Compute the user factor for each user
  for (i in 1:m) {
    
    # print(i)
    # u_i denotes row i of U
    U[i,] <- solve(V%*%diag(c[i,])%*%t(V) + lambda*diag(1,k) )%*%
             V%*%diag(c[i,])%*%p[i,]
  }

  # Compute the item factor for each item
   for (j in 1:n) {
    
    # print(j)
    # v_j denotes column j of V
    V[,j] <- solve(t(U)%*%diag(c[,j])%*%U + lambda*diag(1,k) )%*%
             t(U)%*%diag(c[,j])%*%p[,j]
  }

  # print(U)
  # print(V)
} 

# Predicted preferences
pf <- U%*%V

# dataset
   # subtrainMat2011 (plots 2,5,8)
   # validateMat2011 (plots 3,6,9)
   # trainMat2011    (plots 2,3,5,6,8,9)
   # testMat2011     (plots 1,4,7,10)
# MPR(trained preferences, testing matrix)   
printf("val")
MPR(pf, validateMat2011 )
printf("sub")
MPR(pf, subtrainMat2011 )
printf("test")
MPR(pf, testMat2011 )

