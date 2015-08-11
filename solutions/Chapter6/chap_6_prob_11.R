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
# EPage 138
# 
#-----

if( ! require("ISLR") ){ install.packages("ISLR") }
if( ! require("glmnet") ){ install.packages("glmnet") }
if( ! require("pls") ){ install.packages("pls") }
library(MASS)

set.seed(0)

n = dim(Boston)[1]
p = dim(Boston)[2]

train = sample(c(TRUE,FALSE), n, rep=TRUE) # will roughly assign TRUE to one-half of the data (FALSE to the other half).
test = (!train)

Boston_train = Boston[train,]
Boston_test = Boston[test,]

# The full linear model:
# 
m = lm( crim ~ ., data=Boston_train )

Y_hat = predict( m, newdata=Boston_test )
MSE = mean( ( Boston_test$crim - Y_hat )^2 )
print( sprintf( "Linear model test MSE= %10.3f", MSE ) ) 


# Ridge regression: 
#
Y = Boston_train$crim 
MM = model.matrix( crim ~ ., data=Boston_train )
cv.out = cv.glmnet( MM, Y, alpha=0 )
plot( cv.out ) 
bestlam = cv.out$lambda.1se
#print( "ridge regression CV best value of lambda (one standard error)" )
#print( bestlam )

ridge.mod = glmnet( MM, Y, alpha=0 )

Y_hat = predict( ridge.mod, s=bestlam, newx=model.matrix( crim ~ ., data=Boston_test ) )
MSE =mean( ( Boston_test$crim - Y_hat )^2 ) 
print( sprintf( "Ridge regression test MSE= %10.3f", MSE ) ) 


# The Lasso: 
#
cv.out = cv.glmnet( MM, Y, alpha=1 )
plot( cv.out ) 
bestlam = cv.out$lambda.1se
#print( "lasso CV best value of lambda (one standard error)" )
#print( bestlam )

lasso.mod = glmnet( MM, Y, alpha=1 )

Y_hat = predict( lasso.mod, s=bestlam, newx=model.matrix( crim ~ ., data=Boston_test ) )
MSE =mean( ( Boston_test$crim - Y_hat )^2 )
print( sprintf( "Lasso regression test MSE= %10.3f", MSE ) )
print( "lasso coefficients" )
print( predict( lasso.mod, type="coefficients", s=bestlam ) )

# Principle Component Regression:
#
pcr.mod = pcr( crim ~ ., data=Boston_train, scale=TRUE, validation="CV" )

# Use this to select the number of components to include ... looks like CV suggests
# we should use 3 predictors
# 
validationplot( pcr.mod, val.type="MSEP" ) 

ncomp = 3
Y_hat = predict( pcr.mod, Boston_test, ncomp=ncomp )
MSE = mean( ( Boston_test$crim - Y_hat )^2 )
print( sprintf( "PCR (with ncomp= %5d) test MSE= %10.3f", ncomp, MSE ) )

# Paritial Least Squares: 
#
pls.mod = plsr( crim ~ ., data=Boston_train, scale=TRUE, validation="CV" )

# Use this to select the number of components to include ... looks like CV suggests
# the best is to use 5 predictors
# 
validationplot( pls.mod, val.type="MSEP" ) 

ncomp=5
Y_hat = predict( pls.mod, Boston_test, ncomp=ncomp )
MSE = mean( ( Boston_test$crim - Y_hat )^2 ) 
print( sprintf( "PLS (with ncomp= %5d) test MSE= %10.3f", ncomp, MSE ) )



