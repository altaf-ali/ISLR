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
# EPage 386
#
#-----

save_plots = F

if( ! require("ISLR") ){ install.packages("ISLR") }
if( ! require("MASS") ){ install.packages("MASS") }
if( ! require("class") ){ install.packages("class") }

set.seed(0)

vt100ClearScreen <- function(...) cat("\033[2J") 
vt100ClearScreen()

# Part (a):
Direction = Weekly$Direction
Weekly$Direction = NULL 
Weekly$NumericDirection = as.numeric( Direction ) # Maps Down=>1 and Up=>2
Weekly$NumericDirection[ Weekly$NumericDirection==1 ] = -1 # Maps Down=>-1 and Up=>2
Weekly$NumericDirection[ Weekly$NumericDirection==2 ] = +1 # Maps Down=>-1 and Up=>+1

# Look at the correlation between the output and the input lags:
#
Weekly.cor = cor(Weekly)

# b: logistic regression to predict Direction as a function of 5 lag variables + volume:
#
Weekly$NumericDirection = NULL 
Weekly$Direction = Direction

five_lag_model = glm( Direction ~ Lag1 + Lag2 + Lag3 + Lag4 + Lag5 + Volume, data=Weekly, family=binomial )
print( summary(five_lag_model) )

print( contrasts( Weekly$Direction ) )

# c: the confusion matrix:
#
p_hat = predict( five_lag_model, newdata=Weekly, type="response" )
y_hat = rep( "Down", length(p_hat) )
y_hat[ p_hat > 0.5 ] = "Up"
CM = table( predicted=y_hat, truth=Weekly$Direction )
print( CM )
print( sprintf( "LR (all features): overall fraction correct= %10.6f", ( CM[1,1] + CM[2,2] ) / sum(CM) ) )


# d: logistic regression using only Lag2 as the predictor (since it is the most significant predictor)
#
Weekly.train = ( Weekly$Year >= 1990 ) & ( Weekly$Year <= 2008 ) # our training set 
Weekly.test = ( Weekly$Year >= 2009 ) # our testing set 
lag2_model = glm( Direction ~ Lag2, data=Weekly, family=binomial, subset=Weekly.train )

# CM on test data :
#
p_hat = predict( lag2_model, newdata=Weekly[Weekly.test,], type="response" )
y_hat = rep( "Down", length(p_hat) )
y_hat[ p_hat > 0.5 ] = "Up"
CM = table( predicted=y_hat, truth=Weekly[Weekly.test,]$Direction )
print( CM )
print( sprintf( "LR (only Lag2): overall fraction correct= %10.6f", ( CM[1,1] + CM[2,2] ) / sum(CM) ) )

#e: Use LDA
#
lda.fit = lda( Direction ~ Lag2, data=Weekly, subset=Weekly.train )

lda.predict = predict( lda.fit, newdata=Weekly[Weekly.test,] )
CM = table( predicted=lda.predict$class, truth=Weekly[Weekly.test,]$Direction )
print( CM )
print( sprintf( "LDA (only Lag2): overall fraction correct= %10.6f", ( CM[1,1] + CM[2,2] ) / sum(CM) ) )


#f: Use QDA
#
qda.fit = qda( Direction ~ Lag2, data=Weekly, subset=Weekly.train )

qda.predict = predict( qda.fit, newdata=Weekly[Weekly.test,] ) 
CM = table( predicted=qda.predict$class, truth=Weekly[Weekly.test,]$Direction )
print( CM )
print( sprintf( "QDA (only Lag2): overall fraction correct= %10.6f", ( CM[1,1] + CM[2,2] ) / sum(CM) ) )

#g: KNN 
#
X.train = data.frame( Lag2=Weekly[ Weekly.train, ]$"Lag2" )
Y.train = Weekly[ Weekly.train, ]$"Direction" 

X.test = data.frame( Lag2=Weekly[ Weekly.test, ]$"Lag2" )

y_hat_k_1 = knn( X.train, X.test, Y.train, k=1 )

CM = table( predicted=y_hat_k_1, truth=Weekly[ Weekly.test, ]$Direction ) 
print( CM )
print( sprintf( "KNN (k=1): overall fraction correct= %10.6f", ( CM[1,1] + CM[2,2] ) / sum(CM) ) )

y_hat_k_3 = knn( X.train, X.test, Y.train, k=3 )
CM = table( predicted=y_hat_k_3, truth=Weekly[ Weekly.test, ]$Direction ) 
print( CM )
print( sprintf( "KNN (k=1): overall fraction correct= %10.6f", ( CM[1,1] + CM[2,2] ) / sum(CM) ) )
