# R interpretation of IFMF, collaborative filtering for implicit feedback systems (Hu, Koren, and Volinsky 2008)
# Applied to HJA plant-pollinator interaction dataset
source("IFMF_funcs.R")
   # printf
   # conf1
   # conf2
   # fact_gather
   # fact_comp
   # MPR

# dataset
   # subtrainMat2011 (plots 2,5,8)
   # validateMat2011 (plots 3,6,9)
   # trainMat2011    (plots 2,3,5,6,8,9)
   # testMat2011     (plots 1,4,7,10)
#load("2011split.RData")
load("output.RData")

# observation matrix (mxn)
#r <- subtrainMat2011
r <- testR
rownames(r) <- NULL
colnames(r) <- NULL

#variables
m <- nrow(r)     # number of users (insects)
n <- ncol(r)     # number of items (plants)
f <- 2           # number of factors
lambda <- 0.1     # training parameters
alpha  <- 1
epsil  <- 1

# preference matrix (mxn)
p <- r 
p[p > 0] <- 1

# confidence matrix (mxn)
# c <- apply(r, 1:2, conf2, alpha, epsil)
# c <- apply(r, 1:2, conf1, alpha)
c <- W1

# list of user factors (mxf) and item factors (nxf)
#set.seed(0)
#x <- replicate(m, rexp(f, rate=.1), simplify=F) # user factors
#y <- replicate(n, rexp(f, rate=.1), simplify=F) # item factors
#x <- replicate(m, rep(0.5, f), simplify=F) # user factors
#y <- replicate(n, rep(0.5, f), simplify=F) # item factors
k = f
U <- matrix(0.5, nrow = m, ncol = k) # m x k matrix
V <- matrix(0.5, nrow = k, ncol = n) # k x n matrix
# list of user diagonal matricies (one for each user) mx(nxn)
   # Cu[[u]][i,i] = c[u,i] 
   # Ci[[i]][u,u] = c[u,i]
   # u - 1:m
   # i - 1:n 
# Cu <- replicate(m, diag(n), simplify=F)
# Ci <- replicate(n, diag(m), simplify=F)

# for (u in 1:m) {                            # find a way to vectorize these loops
#   for (i in 1:n) {
#     Cu[[u]][i,i] = c[u,i]
#     Ci[[i]][u,u] = c[u,i]
#   }
# }

# user-factor (mxf) and item-factor (nxf) matrices
# X <- matrix(0, nrow = m, ncol = f)
# Y <- matrix(0, nrow = n, ncol = f)
# X <- matrix(0, nrow = m, ncol = f)
# Y <- matrix(0, nrow = f, ncol = n)

# Compute the user and item factors, sweeping for stability
for (sweep in 1:10) {

  printf("sweep: %d\n", sweep)
  # Y <- fact_gather(y);
  # X <- fact_gather(x);
  # for (i in 1:m) X[i,] <- x[[i]]
  # for (i in 1:n) Y[,i] <- y[[i]]
  
  # Compute the user factor for each user
  for (i in 1:m) {
    # printf("x%d:\n", u)
    # x[[u]] <- fact_comp(Y, Cu[[u]], lambda, p[u,])
    # x[[u]] <- solve(Y%*%Cu[[u]]%*%t(Y) + lambda*diag(ncol(t(Y))) )%*%
    #    Y%*%Cu[[u]]%*%p[u,]
    # print(x[[u]])
    
    # u_i denotes row i of U
    U[i,] <- solve(V%*%diag(c[i,])%*%t(V) + lambda*diag(1,f) )%*%
             V%*%diag(c[i,])%*%p[i,]


  }

  # Compute the item factor for each item
   for (j in 1:n) {
    # printf("y%d:\n", i)
    # y[[i]] <- fact_comp(X, Ci[[i]], lambda, p[,i])
    # y[[i]] <- solve(t(X)%*%Ci[[i]]%*%X + lambda*diag(ncol(X)) )%*%
    #    t(X)%*%Ci[[i]]%*%p[,i]

    
    # v_j denotes column j of V
    V[,j] <- solve(t(U)%*%diag(c[,j])%*%U + lambda*diag(1,f) )%*%
             t(U)%*%diag(c[,j])%*%p[,j]


  }
} 

# --------------------------------------
# Decomp validation stuff

# Y-factor matrix that is 5x8  (fxn)


#R1 <- X%*%Y  # Reconstruction of the decomposed matrix

#R2 <- testR  # Lazy intialization of comparison matricies
#R3 <- testR 

#for (i in 1:m) R2[i,] <- rank(testR[i,])     # ranked version of R
#for (i in 1:m) R3[i,] <- rank(R1[i,])    # ranked version of R!

#print("R - original matrix")
#print(testR)

#print("R1 - reconstructed matrix (this should match R)")
#print(R1)

#print("R2 - ranked original matrix")
#print(R2)

#print("R3 - ranked reconstructed matrix")
#print(R3)

# --------------------------------------


# Create the final preference matrix (mxn)
#pf <- matrix(0, nrow = m, ncol = n)

#for (u in 1:m) {
#  for (i in 1:n) {
#    pf[u,i] <- (t(x[[u]]) %*% (y[[i]]) )
#  }
#}

# Print some recommendations
#printf("Top Recommendations:\n")
#printf("\n")

#for (u in 1:m) {
  
#  insect <- rownames(r)[u]
#  printf("  %s\n", insect)
  
#  rec_order <- order(pf[u,], decreasing=T)
#  printf("  1. %s\n", colnames(r)[rec_order[1]] )
#  printf("  2. %s\n", colnames(r)[rec_order[2]] )
#  printf("  3. %s\n\n", colnames(r)[rec_order[3]] )
#}

# dataset
   # subtrainMat2011 (plots 2,5,8)
   # validateMat2011 (plots 3,6,9)
   # trainMat2011    (plots 2,3,5,6,8,9)
   # testMat2011     (plots 1,4,7,10)

# Compare the results of our trained preferences against different datasets
#printf("\nTraining set: subtrainMat2011\nTesting  set: validateMat2011\n")
#MPR(pf, validateMat2011)
#
#printf("\nTraining set: subtrainMat2011\nTesting  set: trainMat2011\n")
#MPR(pf, trainMat2011)
#
#printf("\nTraining set: subtrainMat2011\nTesting  set: testMat2011\n")
#MPR(pf, testMat2011)
#
#printf("\nTraining set: subtrainMat2011\nTesting  set: subtrainMat2011\n")
#MPR(pf, subtrainMat2011)
