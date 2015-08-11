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
# EPage 387
#
#-----

save_plots = F

if( ! require("ISLR") ){ install.packages("ISLR") }
if( ! require("e1071") ){ install.packages("e1071") }

set.seed(0)

vt100ClearScreen <- function(...) cat("\033[2J") 
vt100ClearScreen()

DF = data.frame( x1=c(3,2,4,1,2,4,4), x2=c(4,2,4,4,1,3,1), y=as.factor(c(rep(1,4),rep(0,3))) )
colors = c( rep('red',4), rep('blue',3) )

# Use the svm code to find the linear boundary (take the cost of a margin violation to be very large): 
svm.fit = svm( y ~ ., data=DF, kernel="linear", cost=1e6, scale=FALSE )
print( summary( svm.fit ) )

# Use the output from svm.fit to compute/extract the linear decision boundary:
#
# For some reason this did not work ... not sure why.  If anyone knows please contact me.
#
# Its based on the decomposition:
#
# f(x) = beta_0 + \sum_{i \in S} \alpha_i < x, x_i >
#
# given in the text and using the results from the svm call to get the support vectors via "index".
#
if( FALSE ){
  beta_0 = svm.fit$coef0
  beta_x1 = sum( svm.fit$coefs * DF[ svm.fit$index, ]$x1 )
  beta_x2 = sum( svm.fit$coefs * DF[ svm.fit$index, ]$x2 )

  # Plot the decision boundary with 
  #abline( a=-beta_0/beta_x1, b=-beta_x2/beta_x1 )
}

# From a plot of the points the decision boundary is given by the line with:
#
slope = ( 3.5 - 1.5 ) / ( 4 - 2 )
intercept = -2 * slope + 1.5

# Compute the two margin lines:
#
slope_m_upper = slope
intercept_m_upper = -2 * slope_m_upper + 2 

slope_m_lower = slope
intercept_m_lower = -2 * slope_m_lower + 1 

if( save_plots ){ postscript("../../WriteUp/Graphics/Chapter9/prob_3_plot.eps", onefile=FALSE, horizontal=FALSE) }
plot( DF$x1, DF$x2, col=colors, pch=19, cex=2.0, xlab='X_1', ylab='X_2', main='' )
grid()
if( save_plots ){ dev.off() }

if( save_plots ){ postscript("../../WriteUp/Graphics/Chapter9/prob_3_plot_with_SVM_lines.eps", onefile=FALSE, horizontal=FALSE) }
plot( DF$x1, DF$x2, col=colors, pch=19, cex=2.0, xlab='X_1', ylab='X_2', main='' )
abline(a=intercept,b=slope,col='black')
abline(a=intercept_m_upper,b=slope_m_upper,col='gray',lty=2,lwd=2)
abline(a=intercept_m_lower,b=slope_m_lower,col='gray',lty=2,lwd=2)
grid()
if( save_plots ){ dev.off() }
