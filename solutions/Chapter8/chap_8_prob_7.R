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
# EPage 348 problem 
# EPage 342
# EPage 335 figure 8.10
#
#-----

save_plots = F

if( ! require("MASS") ){ install.packages("MASS") }
if( ! require("randomForest") ){ install.packages("randomForest") }

set.seed(0)

vt100ClearScreen <- function(...) cat("\033[2J") 
vt100ClearScreen()

n = nrow(Boston)
p = ncol(Boston) - 1 # one column is the response we are trying to model i.e. "medv" 

train = sample(1:n, n/2)
test = (1:n)[-train]

ntree_to_test = seq( from=1, to=500, by=10 )

#
# For a number of mtry values and a number of trees look at the test error rate:
#


# For mtry == p:
# 
mse.bag = rep(NA,length(ntree_to_test))
for( nti in 1:length(ntree_to_test) ){
     nt = ntree_to_test[nti]
     
     # Grow a tree with "nt" trees:
     # 
     boston.bag = randomForest( medv ~ ., data=Boston, mtry=p, ntree=nt, importance=TRUE, subset=train )

     # Make predictions with this tree on the test dataset:
     # 
     y_hat = predict( boston.bag, newdata=Boston[test,] )

     mse.bag[nti] = mean( ( Boston[test,]$medv - y_hat )^2 )
}



# For mtry=p/2:
#
mse.p_over_two = rep(NA,length(ntree_to_test))
for( nti in 1:length(ntree_to_test) ){
     nt = ntree_to_test[nti]
     
     # Grow a tree with "nt" trees:
     # 
     boston.bag = randomForest( medv ~ ., data=Boston, mtry=p/2, ntree=nt, importance=TRUE, subset=train )

     # Make predictions with this tree on the test dataset:
     # 
     y_hat = predict( boston.bag, newdata=Boston[test,] )

     mse.p_over_two[nti] = mean( ( Boston[test,]$medv - y_hat )^2 )
}


# Run random forest with mtry=sqrt(p) and test on test set
#
mse.sqrt_p = rep(NA,length(ntree_to_test))
for( nti in 1:length(ntree_to_test) ){
     nt = ntree_to_test[nti]
     
     # Grow a tree with "nt" trees:
     # 
     boston.bag = randomForest( medv ~ ., data=Boston, mtry=p/2, ntree=nt, importance=TRUE, subset=train )

     # Make predictions with this tree on the test dataset:
     # 
     y_hat = predict( boston.bag, newdata=Boston[test,] )

     mse.sqrt_p[nti] = mean( ( Boston[test,]$medv - y_hat )^2 )
}



if( save_plots ){ postscript("../../WriteUp/Graphics/Chapter8/prob_7_rf_various_mtry_values.eps", onefile=FALSE, horizontal=FALSE) }

plot( ntree_to_test, mse.bag, xlab='Number of Trees', ylab='Test MSE', col='red', type='l' )
lines( ntree_to_test, mse.p_over_two, xlab='Number of Trees', ylab='Test MSE', col='blue', type='l' )
lines( ntree_to_test, mse.sqrt_p, xlab='Number of Trees', ylab='Test MSE', col='green', type='l' )
grid()

if( save_plots ){ dev.off() }

