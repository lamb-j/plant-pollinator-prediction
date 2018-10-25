# Recognize this is an R file please
m <- 7 
f <- 4
n <- 5

x <- replicate(m, rep(.5, f), simplify=F)  # 10x5 <=> mxf
y <- replicate(n, rep(.5, f), simplify=F)  # 8x5  <=> nxf

set.seed(0)

for (u in 1:m) {
  for (i in 1:f) {
    x[[u]][i] <- sample(0:10, 1)
  }
}

for (u in 1:n) {
  for (i in 1:f) {
    y[[u]][i] <- sample(0:10, 1)
  }
}

# X-factor matrix that is 10x5 (mxf)
X <- matrix(0, nrow = m, ncol = f)
for (i in 1:m) X[i,] <- x[[i]]

# Y-factor matrix that is 5x8  (fxn)
Y <- matrix(0, nrow = f, ncol = n)
for (i in 1:n) Y[,i] <- y[[i]]

R <- X%*%Y
save(R, file = "R_Mx.RData")


# for (i in 1:10) R2[i,] <- rank(R[i,])
