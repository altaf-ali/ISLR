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
# EPage 384
#
#-----

save_plots = F

if( ! require("MASS") ){ install.packages("MASS") }
if( ! require("e1071") ){ install.packages("e1071") }

set.seed(0)

vt100ClearScreen <- function(...) cat("\033[2J") 
vt100ClearScreen()

# Part (a):
#
n = 5000
p = 2 

x1 = runif(n) - 0.5
x2 = runif(n) - 0.5
y = 1*( x1^2 - x2^2 > 0 )
#y = 1*( x1^2 + x2^2 > 1 )
#y = 1*( abs(x1) - abs(x2) > 0 )
DF = data.frame( x1=x1, x2=x2, y=as.factor(y) )

# Part (b):
#
if( save_plots ){ postscript("../../WriteUp/Graphics/Chapter9/prob_5_original_data.eps", onefile=FALSE, horizontal=FALSE) }
plot( x1, x2, col=(y+1), pch=19, cex=1.05, xlab='x1', ylab='x2', main='initial data' )
grid()
if( save_plots ){ dev.off() }

# Part (c):  
#
m = glm( y ~ x1 + x2, data=DF, family=binomial )

# Part (d): Predict the class label of the training data.
#
# When type="response" in predict we output probabilities:
# 
y_hat = predict( m, newdata=data.frame(x1=x1,x2=x2), type="response" )
predicted_class = 1 * ( y_hat > 0.5 ) 
print( sprintf('Linear logistic regression training error rate= %10.6f',  1 - sum( predicted_class == y )/length(y) ) )

if( save_plots ){ postscript("../../WriteUp/Graphics/Chapter9/prob_5_linear_LR_predicted_class.eps", onefile=FALSE, horizontal=FALSE) }
plot( x1, x2, col=(predicted_class+1), pch=19, cex=1.05, xlab='x1', ylab='x2', main='logistic regression: y ~ x1 + x2' )
grid()
if( save_plots ){ dev.off() }

# Part (e-f): Using logistic regression to fit a nonlinear model: 
#
m = glm( y ~ x1 + x2 + I(x1^2) + I(x2^2) + I(x1*x2), data=DF, family="binomial" )
y_hat = predict( m, newdata=data.frame(x1=x1,x2=x2), type="response" )
predicted_class = 1 * ( y_hat > 0.5 ) 
print( sprintf('Non-linear logistic regression training error rate= %10.6f',  1 - sum( predicted_class == y )/length(y) ) )

if( save_plots ){ postscript("../../WriteUp/Graphics/Chapter9/prob_5_nonlinear_LR_predicted_class.eps", onefile=FALSE, horizontal=FALSE) }
plot( x1, x2, col=(predicted_class+1), pch=19, cex=1.05, xlab='x1', ylab='x2' )
grid()
if( save_plots ){ dev.off() }

# Part (g): Fit a linear SVM to the data and report the error rate: 
#
dat = data.frame( x1=x1, x2=x2, y=as.factor(y) )

# Do CV to select the value of cost using the "tune" function:
#
tune.out = tune( svm, y ~ ., data=dat, kernel="linear", ranges=list( cost=c(0.001, 0.01, 0.1, 1, 5, 10, 100, 1000) ) )
print( summary(tune.out) ) # <- use this output to select the optimal cost value

# The best model is stored in "tune.out$best.model" 
# 
print(tune.out$best.model)

y_hat = predict( tune.out$best.model, newdata=data.frame(x1=x1,x2=x2) )
y_hat = as.numeric( as.character( y_hat ) ) # convert factor responses into numerical values 
print( sprintf('Linear SVM training error rate= %10.6f',  1 - sum( y_hat == y )/length(y) ) )

if( save_plots ){ postscript("../../WriteUp/Graphics/Chapter9/prob_5_linear_SVM_predicted_class.eps", onefile=FALSE, horizontal=FALSE) }
plot( x1, x2, col=(y_hat+1), pch=19, cex=1.05, xlab='x1', ylab='x2' )
grid()
if( save_plots ){ dev.off() }

# Part (h-i): Fit a linear SVM to the data and report the error rate: 
#

# Do CV to select the value of cost using the "tune" function:
#
tune.out = tune( svm, y ~ ., data=dat, kernel="radial", ranges = 
	         list( cost=c(0.001, 0.01, 0.1, 1, 5, 10, 100, 1000),
                       gamma=c(0.5,1,2,3,4) ) )
print( summary(tune.out) ) # <- use this output to select the optimal cost value

# The best model is stored in "tune.out$best.model" 
# 
print(tune.out$best.model)

y_hat = predict( tune.out$best.model, newdata=data.frame(x1=x1,x2=x2) )
y_hat = as.numeric( as.character( y_hat ) ) # convert factor responses into numerical values 
print( sprintf('Nonlinear SVM training error rate= %10.6f',  1 - sum( y_hat == y )/length(y) ) )

if( save_plots ){ postscript("../../WriteUp/Graphics/Chapter9/prob_5_nonlinear_SVM_predicted_class.eps", onefile=FALSE, horizontal=FALSE) }
plot( x1, x2, col=(y_hat+1), pch=19, cex=1.05, xlab='x1', ylab='x2' )
grid()
if( save_plots ){ dev.off() }

