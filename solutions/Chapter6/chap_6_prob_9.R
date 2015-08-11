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

set.seed(0)

n = dim(College)[1]
p = dim(College)[2]

train = sample(c(TRUE,FALSE), n, rep=TRUE) # will roughly assign TRUE to one-half of the data (FALSE to the other half).
test = (!train)

College_train = College[train,]
College_test = College[test,]

# Part (b):
# 
m = lm( Apps ~ ., data=College_train )

Y_hat = predict( m, newdata=College_test )
MSE = mean( ( College_test$Apps - Y_hat )^2 )
print( sprintf( "Linear model test MSE= %10.3f", MSE ) ) 


# Part (c):
#
Y = College_train$Apps 
MM = model.matrix( Apps ~ ., data=College_train )
cv.out = cv.glmnet( MM, Y, alpha=0 )
plot( cv.out ) 
bestlam = cv.out$lambda.1se
#print( "ridge regression CV best value of lambda (one standard error)" )
#print( bestlam )

ridge.mod = glmnet( MM, Y, alpha=0 )

Y_hat = predict( ridge.mod, s=bestlam, newx=model.matrix( Apps ~ ., data=College_test ) )
MSE =mean( ( College_test$Apps - Y_hat )^2 ) 
print( sprintf( "Ridge regression test MSE= %10.3f", MSE ) ) 


# Part (d):
#
cv.out = cv.glmnet( MM, Y, alpha=1 )
plot( cv.out ) 
bestlam = cv.out$lambda.1se
#print( "lasso CV best value of lambda (one standard error)" )
#print( bestlam )

lasso.mod = glmnet( MM, Y, alpha=1 )

Y_hat = predict( lasso.mod, s=bestlam, newx=model.matrix( Apps ~ ., data=College_test ) )
MSE =mean( ( College_test$Apps - Y_hat )^2 )
print( sprintf( "Lasso regression test MSE= %10.3f", MSE ) )
print( "lasso coefficients" )
print( predict( lasso.mod, type="coefficients", s=bestlam ) )

# Part (e):
#
pcr.mod = pcr( Apps ~ ., data=College_train, scale=TRUE, validation="CV" )

# Use this to select the number of components to include ... looks like CV suggests
# we should use ALL predictors
# 
validationplot( pcr.mod, val.type="MSEP" ) 

ncomp = 17
Y_hat = predict( pcr.mod, College_test, ncomp=ncomp )
MSE = mean( ( College_test$Apps - Y_hat )^2 )
print( sprintf( "PCR (with ncomp= %5d) test MSE= %10.3f", ncomp, MSE ) )

# Part (f):
#
pls.mod = plsr( Apps ~ ., data=College_train, scale=TRUE, validation="CV" )

# Use this to select the number of components to include ... looks like CV suggests
# the best is to use ALL predictors but there is not much change in moving from
# ~ 5 predictors to 17 so we will take 10 (somewhere in the middle)
# 
validationplot( pls.mod, val.type="MSEP" ) 

ncomp=10
Y_hat = predict( pls.mod, College_test, ncomp=ncomp )
MSE = mean( ( College_test$Apps - Y_hat )^2 ) 
print( sprintf( "PLS (with ncomp= %5d) test MSE= %10.3f", ncomp, MSE ) )



