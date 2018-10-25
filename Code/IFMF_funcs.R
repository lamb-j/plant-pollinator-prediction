# R functions for IFMF

# C-style printf
printf <- function(...) invisible(cat(sprintf(...)))

# Confidence matrix calculation function 1
conf1 <- function(x, alpha) {
  return(1 + alpha*x)
}

# Confidence matrix calculation function 2
conf2 <- function(x, alpha, epsil) {
  rv <- 1 + alpha*log(1 + x/epsil)
  return (rv)
}  

# Ranking function for evaluation using mean percentile rank
   # pf - trained preferences from IFMF
   # rt - set of test observations 

MPR <- function(pf, rt) {
  m = nrow(rt)  # users
  n = ncol(rt)  # items

  # matrix for storing the percentile values of the ranks
    # 0 is most desirable for users
    # 1 is least desirable for users
  rank_pf <- matrix(0.0, nrow = m, ncol = n) # preference-based ranking
  rank_lb <- matrix(0.0, nrow = m, ncol = n) # lower bound ranking 

  for (u in 1:m) {
    pf_row_r <- rank(pf[u,], ties.method = "first")
    rt_row_r <- rank(rt[u,], ties.method = "first")

    for (i in 1:n) {
      rank_pf[u,i] <- (n - pf_row_r[i]) / (n)
      rank_lb[u,i] <- (n - rt_row_r[i]) / (n)
    }
  }
  
  rv_pnum <- 0  # preference-based 
  rv_bnum <- 0  # lower-bound (best case)
  rv_wnum <- 0  # upper-bound (worst case)
  rv_den  <- 0  # denominator same in all cases
 
  # calculation of single-valued rank based on comparison of test-data with preferences
  for (u in 1:m) {
    for (i in 1:n) {
      rv_pnum <- rv_pnum + rt[u,i]*rank_pf[u,i]      # preference  multiplies by pf_percents
      rv_bnum <- rv_bnum + rt[u,i]*rank_lb[u,i]      # lower bound multiplies by rt_percents
      rv_wnum <- rv_wnum + rt[u,i]*(1-rank_lb[u,i])  # upper bound multiplies by 1 - rt_percents
      rv_den <- rv_den + rt[u,i]
    }
  }
  ndiff <- abs(rv_pnum/rv_den - rv_bnum/rv_den) 
  pdiff <- ndiff / ((rv_pnum + rv_bnum) / (2*rv_den))

  printf("  Single-valued performance ranking\n")
  printf("     upper bound    : %f\n", rv_wnum/rv_den)
  printf("     pref-based     : %f\n", rv_pnum/rv_den)
  printf("     lower bound    : %f\n", rv_bnum/rv_den)
  # printf("     numeric diff   : %f\n", ndiff)
  # printf("     percent diff   : %f\n", pdiff)
}

# Print some recommendations
print_rec <- function(r, pf) {
  
  printf("Top Recommendations:\n")
  printf("\n")
  
  for (u in 1:nrow(r)) {
  
    insect <- rownames(r)[u]
    printf("  %s\n", insect)
  
    rec_order <- order(pf[u,], decreasing=T)
    printf("  1. %s\n", colnames(r)[rec_order[1]] )
    printf("  2. %s\n", colnames(r)[rec_order[2]] )
    printf("  3. %s\n\n", colnames(r)[rec_order[3]] )
  }
} 
