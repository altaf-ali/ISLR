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
if( ! require("MASS") ){ install.packages("MASS") }
if( ! require("e1071") ){ install.packages("e1071") }

set.seed(0)

vt100ClearScreen <- function(...) cat("\033[2J") 
vt100ClearScreen()

# Part (a):
#
n = dim(OJ)[1]
n_train = 800
train_inds = sample(1:n,n_train)
test_inds = (1:n)[-train_inds]
n_test = length(test_inds)

# Part (b) Use a linear kernel to start with:
#
svm.fit = svm( Purchase ~ ., data=OJ, kernel="linear", cost=0.01 )
print( summary( svm.fit ) )

# Part (c) Use this specific SVM to estimate training/testing error rates:
#
y_hat = predict( svm.fit, newdata=OJ[train_inds,] )
print( table( predicted=y_hat, truth=OJ[train_inds,]$Purchase ) )
print( sprintf('Linear SVM training error rate (cost=0.01)= %10.6f',  1 - sum( y_hat == OJ[train_inds,]$Purchase ) / n_train ) )

y_hat = predict( svm.fit, newdata=OJ[test_inds,] )
print( table( predicted=y_hat, truth=OJ[test_inds,]$Purchase ) )
print( sprintf('Linear SVM testing error rate (cost=0.01)= %10.6f',  1 - sum( y_hat == OJ[test_inds,]$Purchase ) / n_test ) )

# Part (d): Use tune to select an optimal value for cost when we have a linear kernel:
#
tune.out = tune( svm, Purchase ~ ., data=OJ, kernel="linear", ranges=list( cost=c(0.001, 0.01, 0.1, 1, 5, 10, 100, 1000) ) )
print( summary(tune.out) ) # <- use this output to select the optimal cost value

# Part (e): Predict the performance on training and testing using the best linear model: 
# 
y_hat = predict( tune.out$best.model, newdata=OJ[train_inds,] )
print( table( predicted=y_hat, truth=OJ[train_inds,]$Purchase ) )
print( sprintf('Linear SVM training error rate (optimal cost=1)= %10.6f',  1 - sum( y_hat == OJ[train_inds,]$Purchase ) / n_train ) )

y_hat = predict( tune.out$best.model, newdata=OJ[test_inds,] )
print( table( predicted=y_hat, truth=OJ[test_inds,]$Purchase ) )
print( sprintf('Linear SVM testing error rate (optimal cost=1)= %10.6f',  1 - sum( y_hat == OJ[test_inds,]$Purchase ) / n_test) )

# Part (f): Use a radial kernel: 
# 
tune.out = tune( svm, Purchase ~ ., data=OJ, kernel="radial", ranges = list( cost=c(0.001, 0.01, 0.1, 1, 5, 10, 100, 1000), gamma=c(0.5,1,2,3,4) ) )
print( summary(tune.out) ) # <- use this output to select the optimal cost value

y_hat = predict( tune.out$best.model, newdata=OJ[train_inds,] )
print( table( predicted=y_hat, truth=OJ[train_inds,]$Purchase ) )
print( sprintf('Radial SVM training error rate (optimal)= %10.6f',  1 - sum( y_hat == OJ[train_inds,]$Purchase ) / n_train ) )

y_hat = predict( tune.out$best.model, newdata=OJ[test_inds,] )
print( table( predicted=y_hat, truth=OJ[test_inds,]$Purchase ) )
print( sprintf('Radial SVM testing error rate (optimal)= %10.6f',  1 - sum( y_hat == OJ[test_inds,]$Purchase ) / n_test) )

# Part (g): Use a polynomial kernel: 
# 
tune.out = tune( svm, Purchase ~ ., data=OJ, kernel="polynomial", ranges = list( cost=c(0.001, 0.01, 0.1, 1, 5, 10, 100, 1000), degree=c(1,2,3) ) )
print( summary(tune.out) ) # <- use this output to select the optimal cost value

y_hat = predict( tune.out$best.model, newdata=OJ[train_inds,] )
print( table( predicted=y_hat, truth=OJ[train_inds,]$Purchase ) )
print( sprintf('Polynomial SVM training error rate (optimal)= %10.6f',  1 - sum( y_hat == OJ[train_inds,]$Purchase ) / n_train ) )

y_hat = predict( tune.out$best.model, newdata=OJ[test_inds,] )
print( table( predicted=y_hat, truth=OJ[test_inds,]$Purchase ) )
print( sprintf('Polynomial SVM testing error rate (optimal)= %10.6f',  1 - sum( y_hat == OJ[test_inds,]$Purchase ) / n_test) )


