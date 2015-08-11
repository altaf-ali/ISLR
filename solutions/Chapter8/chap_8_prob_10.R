#
# EPage 348
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

save_plots = FALSE

if( ! require("MASS") ){ install.packages("MASS") }
if( ! require("ISLR") ){ install.packages("ISLR") }
if( ! require("gbm") ){ install.packages("gbm") }
if( ! require("glmnet") ){ install.packages("glmnet") }
if( ! require("randomForest") ){ install.packages("randomForest") }

set.seed(0)

vt100ClearScreen <- function(...) cat("\033[2J") 
vt100ClearScreen()

Hitters = na.omit(Hitters)
Hitters$Salary = log(Hitters$Salary)

n = nrow(Hitters)
p = ncol(Hitters) - 1 # one column is the response we are trying to model i.e. "Salary" 

train = 1:200
test = 201:n

lambda_set = seq( 1.e-4, 0.04, by=0.001 )

# 
training_set_mse = rep(NA,length(lambda_set))
test_set_mse = rep(NA,length(lambda_set))
for( lmi in 1:length(lambda_set) ){
  lm = lambda_set[lmi]
  
  boost.hitters = gbm( Salary ~ ., data=Hitters[train,], distribution="gaussian", n.trees=1000, interaction.depth=4, shrinkage=lm )

  y_hat = predict( boost.hitters, newdata=Hitters[train,], n.trees=1000 )
  training_set_mse[lmi] = mean( ( y_hat - Hitters[train,]$Salary )^2 )
  
  y_hat = predict( boost.hitters, newdata=Hitters[test,], n.trees=1000 )
  test_set_mse[lmi] = mean( ( y_hat - Hitters[test,]$Salary )^2 )
}

if( save_plots ){ postscript("../../WriteUp/Graphics/Chapter8/prob_10_test_N_train_MSE_as_fn_of_lambda.eps", onefile=FALSE, horizontal=FALSE) }
plot( lambda_set, training_set_mse, type='b', pch=19, col='red', xlab='Lambda Value', ylab='MSE' )
lines( lambda_set, test_set_mse, type='b', pch=19, col='green', xlab='Lambda Value', ylab='Test Set MSE' )
grid()
if( save_plots ){ dev.off() }

# Looks like the test MSE results are insensitive to the exact value of lambda as long as its small enough:
lm = 0.01
boost.hitters = gbm( Salary ~ ., data=Hitters[train,], distribution="gaussian", n.trees=1000, interaction.depth=4, shrinkage=lm )
y_hat = predict( boost.hitters, newdata=Hitters[test,], n.trees=1000 )
print( 'regression boosting test MSE:' )
print( mean( ( y_hat - Hitters[test,]$Salary )^2 ) )

# Try linear regression:
# 
m = lm( Salary ~ ., data=Hitters[train,] )
y_hat = predict( m, newdata=Hitters[test,] )
print( 'linear regression test MSE:' )
print( mean( ( y_hat - Hitters[test,]$Salary )^2 ) )

# Try the lasso:
#
MM = model.matrix( Salary ~ ., data=Hitters[train,] )
cv.out = cv.glmnet( MM, Hitters[train,]$Salary, alpha=1 )
bestlam = cv.out$lambda.1se
print( "lasso CV best value of lambda (one standard error)" )
print( bestlam )

lasso.mod = glmnet( MM, Hitters[train,]$Salary, alpha=1 )

MM_test = model.matrix( Salary ~ ., data=Hitters[test,] )
y_hat = predict( lasso.mod, s=bestlam, newx=MM_test )
print( 'lasso regression test MSE:' )
print( mean( ( y_hat - Hitters[test,]$Salary )^2 ) )

# Try ridge regression:
#
cv.out = cv.glmnet( MM, Hitters[train,]$Salary, alpha=0 )
plot( cv.out ) 
bestlam = cv.out$lambda.1se
print( "ridge CV best value of lambda (one standard error)" )
print( bestlam )

ridge.mod = glmnet( MM, Hitters[train,]$Salary, alpha=0 )
Y_hat = predict( ridge.mod, s=bestlam, newx=MM_test )
print( 'ridge regression test MSE:' )
print( mean( ( y_hat - Hitters[test,]$Salary )^2 ) )

# What are the most important variables:
# 
summary( boost.hitters )

# Try randomForests on the Hitters dataset (not asked for in the problem statement): 
#
rf.hitters = randomForest( Salary ~ ., data=Hitters, mtry=p/3, ntree=1000, importance=TRUE, subset=train )
y_hat = predict( rf.hitters, newdata=Hitters[test,] )
mse.rf = mean( ( Hitters[test,]$Salary - y_hat )^2 )
print( 'randomForest test MSE:' )
print( mse.rf )

# Try BAGGING on the Hitters dataset: 
#
bag.hitters = randomForest( Salary ~ ., data=Hitters, mtry=p, ntree=1000, importance=TRUE, subset=train )
y_hat = predict( bag.hitters, newdata=Hitters[test,] )
mse.bag = mean( ( Hitters[test,]$Salary - y_hat )^2 )
print( 'Bagging test MSE:' )
print( mse.bag )
