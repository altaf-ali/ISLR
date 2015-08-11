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

save_plots= F

if( ! require("ISLR") ){ install.packages("ISLR") }
if( ! require("leaps") ){ install.packages("leaps") }
if( ! require("glmnet") ){ install.packages("glmnet") }
if( ! require("pls") ){ install.packages("pls") }

set.seed(0)

# The sample size and the number of features:
# 
n = 1000
p = 20

# Create the true value of beta (and zero out half of the entries):
# 
beta_truth = rnorm(p+1) # add one for the constant beta_0 
zero_locations = c(2,3,4,7,8,11,12,15,17,20)
beta_truth[zero_locations] = 0
# For debugging lets check that we can recover our coefficients: 
#beta_truth = rep(0,p+1); beta_truth[1] = 1.5; beta_truth[10] = 3.5; beta_truth[15] = -3.4
print( "True values for beta (beta_0-beta_20):" )
print( beta_truth )

# Generate some input features and an output response:
# 
X = c( rep(1,n), rnorm( n*p ) ) # make leading column of ones 
X = matrix( X, nrow=n, ncol=(p+1), byrow=FALSE )

Y = X %*% beta_truth + rnorm( n )

# Create a dataframe with this data:
# 
DF = data.frame( Y, X[,-1] ) # drop the column of ones 

train_inds = sample( 1:n, 100 )
test_inds = ( 1:n )[ -train_inds ]

#--
# Apply best subset selection using the training data: 
#--
regfit.full = regsubsets( Y ~ ., data=DF[train_inds,], nvmax=20 )
#print( summary( regfit.full ) )
reg.summary = summary( regfit.full )

# Plot the in-sample MSE:
#
training.mat = model.matrix( Y ~ ., data=DF[train_inds,] )
training.errors = rep(NA,20)
for( ii in 1:20 ){
  coefi = coef( regfit.full, id=ii )
  pred = training.mat[,names(coefi)] %*% coefi
  training.errors[ii] = mean( ( DF$Y[train_inds] - pred )^2 ) 
}
print( "best subset training MSE" )
print( training.errors )
if( save_plots ){
  postscript("../../WriteUp/Graphics/Chapter6/prob_10_train_test_MSE.eps", onefile=FALSE, horizontal=FALSE)
}
plot( 1:20, training.errors, xlab='number of predictors', ylab='training MSE', type='o', col='red', ylim=c(0,9) )

# Test models on the validation set:
#
test.mat = model.matrix( Y ~ ., data=DF[test_inds,] )
val.errors = rep(NA,20)
for( ii in 1:20 ){
  coefi = coef( regfit.full, id=ii )
  pred = test.mat[,names(coefi)] %*% coefi
  val.errors[ii] = mean( ( DF$Y[test_inds] - pred )^2 ) 
}
print( "best subset validation MSE" )
print( val.errors )
k = which.min( val.errors ) 
print( k )
print( coef( regfit.full, id=k ) )
points( 1:20, val.errors, xlab='number of predictors', ylab='testing MSE', type='o', col='green' )

grid()
legend( 11, 9.25, c('Training MSE','Testing MSE'), col=c('red','green'), lty=c(1,1) )

if( save_plots ){
  dev.off()
}

# Part (g):
#
nms = colnames(DF)
nms[1] = "(Intercept)" 
names(beta_truth) = nms

norm.beta.diff = rep(NA,20)
for( ii in 1:20 ){
  coefi = coef( regfit.full, id=ii )
  norm.beta.diff[ii] = sqrt( sum( ( beta_truth[ names( coefi ) ] - coefi )^2 ) ) 
}

if( save_plots ){ postscript("../../WriteUp/Graphics/Chapter6/prob_10_beta_norm_plot.eps", onefile=FALSE, horizontal=FALSE) }
plot( 1:20, norm.beta.diff, xlab='number of predictors', ylab='||beta_truth - beta^r||', type='o', col='green' )
grid()
if( save_plots ){ dev.off() }
