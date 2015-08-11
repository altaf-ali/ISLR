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
# EPage 313
# EPage 63
#
#-----

save_plots = F

if( ! require("ISLR") ){ install.packages("ISLR") }
if( ! require("leaps") ){ install.packages("leaps") }
if( ! require("glmnet") ){ install.packages("glmnet") }
if( ! require("gam") ){ install.packages("gam") }

set.seed(0)

# Part (a):
#

# Divide the dataset into three parts: training==1, validation==2, and test==3
# 
dataset_part = sample( 1:3, nrow(College), replace=T, prob=c(0.5,0.25,0.25) )

p = ncol(College)-1

# Fit subsets of various sizes:
#
regfit.forward = regsubsets( Outstate ~ ., data=College[dataset_part==1,], nvmax=p, method="forward" )
print( summary( regfit.forward ) )

reg.summary = summary( regfit.forward )

# Test the trained models on the validation set:
#
validation.mat = model.matrix( Outstate ~ ., data=College[dataset_part==2,] )
val.errors = rep(NA,p)
for( ii in 1:p ){
  coefi = coef( regfit.forward, id=ii )
  pred = validation.mat[,names(coefi)] %*% coefi
  val.errors[ii] = mean( ( College$Outstate[dataset_part==2] - pred )^2 ) 
}
print( "forward selection validation errors" )
print( val.errors )
k = which.min( val.errors ) 
print( sprintf("smallest validation error for index= %d, with coefficients given by", k) )
print( coef( regfit.forward, id=k ) )

if( save_plots ){ postscript("../../WriteUp/Graphics/Chapter7/prob_10_forward_summary_plots.eps", onefile=FALSE, horizontal=FALSE) }
plot( val.errors, xlab="Number of variables", ylab="Validation MSE", pch=19, type='b' )
abline(v=k,col='red')
grid()
if( save_plots ){ dev.off() }

# Predict the best model found on the testing set:
#
test.mat = model.matrix( Outstate ~ ., data=College[dataset_part==3,] )
coefi = coef( regfit.forward, id=k )
pred = test.mat[,names(coefi)] %*% coefi
test.error = mean( ( College$Outstate[dataset_part==3] - pred )^2 ) 
print( "test error on the optimal subset" )
print( test.error )

k = 3
coefi = coef( regfit.forward, id=k )
pred = test.mat[,names(coefi)] %*% coefi
test.error = mean( ( College$Outstate[dataset_part==3] - pred )^2 ) 
print( "test error on the k=3 subset" )
print( test.error )



# Part (b):
#

# Combine the training and validation into one "training" dataset
#
dataset_part[dataset_part==2] = 1 
dataset_part[dataset_part==3] = 2

gam.model = gam( Outstate ~ s(Expend,4) + s(Room.Board,4) + Private, data=College[dataset_part==1,] )

if( save_plots ){ postscript("../../WriteUp/Graphics/Chapter7/prob_10_gam_plot.eps", onefile=FALSE, horizontal=FALSE) }
par(mfrow=c(1,3))
plot(gam.model, se=TRUE, col="blue")
par(mfrow=c(1,1))
if( save_plots ){ dev.off() }

# Predict the GAM performance on the test dataset:
# 
y_hat = predict( gam.model, newdata=College[dataset_part==2,] )
MSE = mean( ( College[dataset_part==2,]$Outstate - y_hat )^2 )
print( "gam testing set (MSE) error" )
print( MSE )







