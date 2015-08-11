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
if( ! require("leaps") ){ install.packages("leaps") }
if( ! require("glmnet") ){ install.packages("glmnet") }

save_plots = F

set.seed(0)

n = 100
X = rnorm(n)
epsilon = 0.1 * rnorm(n)

beta_0 = 1.0
beta_1 = -0.1
beta_2 = +0.05
beta_3 = 0.75

Y = beta_0 + beta_1 * X + beta_2 * X^2 + beta_3 * X^3 + epsilon

DF = data.frame( Y=Y, X=X, X2=X^2, X3=X^3, X4=X^4, X5=X^5, X6=X^6, X7=X^7, X8=X^8, X9=X^9, X10=X^10 )

# Use the validation approach with regsubsets
#
train = sample(c(TRUE,FALSE), n, rep=TRUE) # will roughly assign TRUE to one-half of the data (FALSE to the other half).
test = (!train)



#--
# Apply best subset selection: 
#--
regfit.full = regsubsets( Y ~ ., data=DF[train,], nvmax=10 )
print( summary( regfit.full ) )

reg.summary = summary( regfit.full )

# Test models on the validation set:
#
test.mat = model.matrix( Y ~ ., data=DF[test,] )
val.errors = rep(NA,10)
for( ii in 1:10 ){
  coefi = coef( regfit.full, id=ii )
  pred = test.mat[,names(coefi)] %*% coefi
  val.errors[ii] = mean( ( DF$Y[test] - pred )^2 ) 
}
print( "best subset validation errors" )
print( val.errors )
k = which.min( val.errors ) 
print( k )
print( coef( regfit.full, id=k ) )

if( save_plots ){ postscript("../../WriteUp/Graphics/Chapter6/prob_8_exhaustive_summary_plots.eps", onefile=FALSE, horizontal=FALSE) }

old.par = par( mfrow=c(1,4) )
#plot( reg.summary$rss, xlab="Number of variables", ylab="RSS" )
plot( reg.summary$cp, xlab="Number of variables", ylab="Cp", pch=19 )
plot( reg.summary$bic, xlab="Number of variables", ylab="BIC", pch=19 )
plot( reg.summary$adjr2, xlab="Number of variables", ylab="adjusted R2", pch=19 )

plot( val.errors, xlab="Number of variables", ylab="Validation Errors", pch=19 )

par( mfrow=c(1,1) )

if( save_plots ){ dev.off() }



#--
# Now apply foward selection on the training set:
#--
regfit.forward = regsubsets( Y ~ ., data=DF[train,], nvmax=10, method="forward" )
print( summary( regfit.forward ) )

reg.summary = summary( regfit.forward )

# Test models on the validation set:
#
test.mat = model.matrix( Y ~ ., data=DF[test,] )
val.errors = rep(NA,10)
for( ii in 1:10 ){
  coefi = coef( regfit.forward, id=ii )
  pred = test.mat[,names(coefi)] %*% coefi
  val.errors[ii] = mean( ( DF$Y[test] - pred )^2 ) 
}
print( "forward selection validation errors" )
print( val.errors )
k = which.min( val.errors ) 
print( k )
print( coef( regfit.forward, id=k ) )

if( save_plots ){ postscript("../../WriteUp/Graphics/Chapter6/prob_8_forward_summary_plots.eps", onefile=FALSE, horizontal=FALSE) }

old.par = par( mfrow=c(1,4) )
#plot( reg.summary$rss, xlab="Number of variables", ylab="RSS" )
plot( reg.summary$cp, xlab="Number of variables", ylab="Cp", pch=19 )
plot( reg.summary$bic, xlab="Number of variables", ylab="BIC", pch=19 )
plot( reg.summary$adjr2, xlab="Number of variables", ylab="adjusted R2", pch=19 )

plot( val.errors, xlab="Number of variables", ylab="Validation Errors", pch=19 )

par( mfrow=c(1,1) )

if( save_plots ){ dev.off() }


#--
# Now apply backwards selection to the training set:
#--
regfit.backward = regsubsets( Y ~ ., data=DF[train,], nvmax=10, method="backward" )
print( summary( regfit.backward ) )

reg.summary = summary( regfit.backward )

# Test models on the validation set:
#
test.mat = model.matrix( Y ~ ., data=DF[test,] )
val.errors = rep(NA,10)
for( ii in 1:10 ){
  coefi = coef( regfit.backward, id=ii )
  pred = test.mat[,names(coefi)] %*% coefi
  val.errors[ii] = mean( ( DF$Y[test] - pred )^2 ) 
}
print( "backwards selection validation errors" )
print( val.errors )
k = which.min( val.errors ) 
print( k )
print( coef( regfit.backward, id=k ) )

if( save_plots ){ postscript("../../WriteUp/Graphics/Chapter6/prob_8_backward_summary_plots.eps", onefile=FALSE, horizontal=FALSE) }

old.par = par( mfrow=c(1,4) )
#plot( reg.summary$rss, xlab="Number of variables", ylab="RSS" )
plot( reg.summary$cp, xlab="Number of variables", ylab="Cp", pch=19 )
plot( reg.summary$bic, xlab="Number of variables", ylab="BIC", pch=19 )
plot( reg.summary$adjr2, xlab="Number of variables", ylab="adjusted R2", pch=19 )

plot( val.errors, xlab="Number of variables", ylab="Validation Errors", pch=19 )
par( mfrow=c(1,1) )

if( save_plots ){ dev.off() }



#--
# Now apply the lasso to our training set:
#--

# First fit the lasso model for all of the given lambda values :
# 
grid = 10^seq(10,-2,length=100) # a grid of lambda values 
Y = DF$Y
MM = model.matrix( Y ~ ., data=DF ) # the predictors as a datamatrix 
lasso.mod = glmnet( MM, Y, alpha=1, lambda=grid ) 
plot(lasso.mod) # plots the extracted coefficients as a function of lambda 

# Apply cross validation (to pick the best value of lambda):
# 
cv.out = cv.glmnet( MM, Y, alpha=1 )
bestlam = cv.out$lambda.1se
print( "lasso CV best value of lambda (one standard error)" )
print( bestlam )

if( save_plots ){ postscript("../../WriteUp/Graphics/Chapter6/prob_8_lasso_cv_errors.eps", onefile=FALSE, horizontal=FALSE) }
plot(cv.out)
if( save_plots ){ dev.off() }

# Extract the optimal coefficients used:
#
lasso.coef = predict( lasso.mod, type="coefficients", s=bestlam )
print( lasso.coef ) 



#
# Part (f) Try a different regression function:
#
X = rnorm(n)
epsilon = 0.1 * rnorm(n)

beta_0 = 1.0 
beta_7 = 2.5
Y = beta_0 + beta_7 * X^7 + epsilon
DF = data.frame( Y=Y, X=X, X2=X^2, X3=X^3, X4=X^4, X5=X^5, X6=X^6, X7=X^7, X8=X^8, X9=X^9, X10=X^10 )

train = sample(c(TRUE,FALSE), n, rep=TRUE) # will roughly assign TRUE to one-half of the data (FALSE to the other half).
test = (!train)

# Best subset selection:
# 
regfit.full = regsubsets( Y ~ ., data=DF[train,], nvmax=10 )
print( summary( regfit.full ) )

# Test best subset models on the validation set:
#
test.mat = model.matrix( Y ~ ., data=DF[test,] )
val.errors = rep(NA,10)
for( ii in 1:10 ){
  coefi = coef( regfit.full, id=ii )
  pred = test.mat[,names(coefi)] %*% coefi
  val.errors[ii] = mean( ( DF$Y[test] - pred )^2 ) 
}
print( "best subsets validation errors" )
print( val.errors )
k = which.min( val.errors )
print( k ) 
print( "best subsets optimal coefficients" )
print( coef( regfit.full, id=k ) ) # print the coefficients of the best model 
print( val.errors[k] ) 

# Using the lasso technique:
#

# First apply cross validation (to find the optimal value of lambda):
# 
MM = model.matrix( Y ~ ., data=DF )

cv.out = cv.glmnet( MM, Y, alpha=1 )
plot( cv.out ) 
bestlam = cv.out$lambda.1se
print( "best lambda (1 se)" )
print( bestlam ) 

# Now fit the lasso with this value of lambda: 
lasso.mod = glmnet( MM, Y, alpha=1 )

lasso.coef = predict( lasso.mod, type="coefficients", s=bestlam )
print( "lasso optimal coefficients" )
print( lasso.coef )

print( "I do not think the predict method is working correctly..." )
lasso.predict = predict( lasso.mod, s=bestlam, newx=MM )
print( "lasso RSS error" )
print( mean( ( Y - lasso.predict )^2 ) )



