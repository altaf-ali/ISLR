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
#-----

save_plots = F

if( ! require("MASS") ){ install.packages("MASS") }
if( ! require("e1071") ){ install.packages("e1071") }

set.seed(1)

vt100ClearScreen <- function(...) cat("\033[2J") 
vt100ClearScreen()

x = matrix( rnorm(20*2), ncol=2 )
y = c( rep(-1,10), rep(+1,10) )
x[y==1,] = x[y==1,] + 1 
x[y==1,] = x[y==1,] + 0.5

if( save_plots ){ postscript("../../WriteUp/Graphics/Chapter9/prob_6_original_data.eps", onefile=FALSE, horizontal=FALSE) }
plot( x[,1], x[,2], col=(y+3), pch=19, cex=1.25, xlab='x1', ylab='x2', main='initial data' )
grid()
if( save_plots ){ dev.off() }

dat = data.frame( x1=x[,1], x2=x[,2], y=as.factor(y) )
tune.out = tune( svm, y ~ ., data=dat, kernel="linear", ranges=list( cost=c(0.001, 0.01, 0.1, 1, 5, 10, 100, 1000) ) )
print( summary(tune.out) ) # <- use this output to select the optimal cost value

# Generate test data:
#
x_test = matrix( rnorm(20*2), ncol=2 )
y_test = c( rep(-1,10), rep(+1,10) )
x_test[y_test==1,] = x_test[y_test==1,] + 1 
x_test[y_test==1,] = x_test[y_test==1,] + 0.5

dat_test = data.frame( x1=x_test[,1], x2=x_test[,2], y=as.factor(y_test) )

y_hat = predict( tune.out$best.model, newdata=dat_test )
y_hat = as.numeric( as.character( y_hat ) ) # convert factor responses into numerical values 
print( sprintf('Linear SVM test error rate= %10.6f',  1 - sum( y_hat == y )/length(y) ) )

