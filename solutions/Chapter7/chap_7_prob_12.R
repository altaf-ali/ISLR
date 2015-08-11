#
# Written by:
# -- 
# John L. Weatherwax                2009-04-21
# 
# email: wax@alum.mit.edu
# 
# Please send comments and especially bug reports to the
# above email address.
#
# EPage 314
#
#-----

if( ! require("ISLR") ){ install.packages("ISLR") }
if( ! require("boot") ){ install.packages("boot") }

vt100ClearScreen <- function(...) cat("\033[2J") 

vt100ClearScreen()

set.seed(0)

p = 100
n = 1000

# Generate some regression coefficients beta_0, beta_1, ..., beta_p
beta_truth = rnorm( p+1 )

# Generate some data (append a column of ones):
Xs = c( rep(1,n), rnorm( n*p ) )
X = matrix( data=Xs, nrow=n, ncol=(p+1), byrow=FALSE )

# Produce the response: 
Y = X %*% beta_truth + 0.1 * rnorm(n)

# Get the true estimated coefficient estimates using lm:
#
m = lm( Y ~ X - 1 )
beta_lm = m$coeff 

# Estimate beta_i using backfitting: 
#
beta_hat = rnorm( p+1 ) # initial estimate of beta's is taken to be random 

n_iters = 10

beta_estimates = matrix( data=rep(NA,n_iters*(p+1)), nrow=n_iters, ncol=(p+1) )
beta_differences_with_truth = rep(NA,n_iters)
beta_differences_with_LS = rep(NA,n_iters)
for( ii in 1:n_iters ){
  for( pi in 0:p ){ # for beta_0, beta_1, ... beta_pi ... beta_p

    # Perform simple linear regression on the variable X_pi (assuming we know all other values of beta_pi): 
    # 
    a = Y - X[,-(pi+1)] %*% beta_hat[-(pi+1)] # remove all predictors except beta_0 

    if( pi==0 ){
      m = lm( a ~ 1 ) # estimate a constant 
      beta_hat[pi+1] = m$coef[1] 
    }else{
      m = lm( a ~ X[,pi+1] ) # estimate the slope on X_pi
      beta_hat[pi+1] = m$coef[2] 
    }

  }
  beta_estimates[ii,] = beta_hat
  beta_differences_with_truth[ii] = sqrt( sum( ( beta_hat - beta_truth )^2 ) )
  beta_differences_with_LS[ii] = sqrt( sum( ( beta_hat - beta_lm )^2 ) )
}

#postscript("../../WriteUp/Graphics/Chapter7/prob_12_convergence_plot.eps", onefile=FALSE, horizontal=FALSE)

plot( 1:n_iters, beta_differences_with_LS, main='||beta_truth-beta_LS||_2', pch=19, type='b' )
grid()

#dev.off()




