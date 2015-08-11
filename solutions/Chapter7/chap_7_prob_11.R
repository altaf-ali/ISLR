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

n = 100

X1 = rnorm( n )
X2 = rnorm( n )

# the true values of beta_i:
beta_0 = 3.0
beta_1 = 5.0
beta_2 = -0.2

Y = beta_0 + beta_1 * X1 + beta_2 * X2 + 0.1 * rnorm( n )

# Part (b):
#
beta_1_hat = -3.0

n_iters = 10

beta_0_estimates = c()
beta_1_estimates = c()
beta_2_estimates = c()
for( ii in 1:n_iters ){

  a = Y - beta_1_hat * X1
  beta_2_hat = lm( a ~ X2 )$coef[2] 

  a = Y - beta_2_hat * X2
  m = lm( a ~ X1 )
  beta_1_hat = m$coef[2]

  beta_0_hat = m$coef[1] 

  beta_0_estimates = c(beta_0_estimates, beta_0_hat)
  beta_1_estimates = c(beta_1_estimates, beta_1_hat)
  beta_2_estimates = c(beta_2_estimates, beta_2_hat)
}

# Get the coefficient estimates using lm:
#
m = lm( Y ~ X1 + X2 )

#postscript("../../WriteUp/Graphics/Chapter7/prob_11_convergence_plot.eps", onefile=FALSE, horizontal=FALSE)

old_par = par(mfrow=c(1,3))

plot( 1:n_iters, beta_0_estimates, main='beta_0', pch=19, ylim=c(beta_0*0.999,max(beta_0_estimates)) )
abline(h=beta_0,col='green',lwd=4)
abline(h=m$coefficients[1],col='gray',lwd=4)
grid()

plot( 1:n_iters, beta_1_estimates, main='beta_1', pch=19 )
abline(h=beta_1,col='green',lwd=4)
abline(h=m$coefficients[2],col='gray',lwd=4)
grid()

plot( 1:n_iters, beta_2_estimates, main='beta_2', pch=19 )
abline(h=beta_2,col='green',lwd=4)
abline(h=m$coefficients[3],col='gray',lwd=4)
grid()

par(old_par)

#dev.off()




